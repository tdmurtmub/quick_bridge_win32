// (C) 2005 Wesley Steiner

{$MODE FPC}

unit bridge;

{$I std.inc}

interface

uses
	std;

const
	NORTH=0;
	EAST=1;
	SOUTH=2;
	WEST=3;

	NOT_DOUBLED=0;
	DOUBLED=1;
	REDOUBLED=2;

type
	teamId=(WE,THEY);
	gameIndex=1..3;
	playerType=NORTH..WEST;
	trumptype=(TTCLUB,TTDIAMOND,TTHEART,TTSPADE,TTNT);
	doublingStatus=NOT_DOUBLED..REDOUBLED;
	contractLevel=1..7;
	lengthtype=0..13;

type
	ScoreType=record
		GameRecord:array[gameIndex] of integer;
		OverLine:integer;
		m_nGamesWon:integer; { not persisted }
	end;

	ScoreKeeper=object
		constructor Construct;
		constructor ConstructFromData(data:ansistring);
		function IsBlank:boolean;
		function CurrentGameIndex:gameIndex;
		function GameOver:boolean;
		function GamesPlayed:word;
		function GameScore(team:teamId;n:gameIndex):word;
		function GamesWon(team:teamId):word;
		function OverLineScoreCount(team:teamId):quantity;
		function OverlineTotal(team:teamId):word;
		function TeamTotal(team:teamId):word;
		function ToString:ansistring;
		function WonRubber(team:teamId):boolean;
		procedure AddResult(team:teamId;level:contractLevel;trump:trumpType;doubling:doublingStatus;nMade:lengthType);
		procedure Clear;
		procedure FromData(data:ansistring);
		procedure OnCleared; virtual;
		procedure OnAdded; virtual;
	test_private
		my_data:array[teamId] of ScoreType;
		as_string:ansistring;
		function GameWon(aGameIndex:gameIndex):boolean;
		procedure DoAddResult(team:teamId;level:contractLevel;trump:trumpType;doubling:doublingStatus;nMade:lengthType);
		procedure DoClear;
	end;

implementation

uses 
	{$ifdef TEST} bridgeTests, {$endif}
	sysutils,strutils;

const
	BOOK_SIZE=6;
	MINOR_TRICK_POINTS=20;
	MAJOR_TRICK_POINTS=30;
	FIRST_NT_TRICK_POINTS=40;
	NEXT_NT_TRICK_POINTS=30;
	GAME_POINTS=100;

procedure ScoreKeeper.DoClear;
begin
	FillChar(my_data,SizeOf(my_data),chr(0));
	as_string:='';
end;

procedure ScoreKeeper.Clear;
begin
	DoClear;
	OnCleared;
end;

procedure ScoreKeeper.OnCleared;
begin
	PlaceHolder;
end;

function ScoreKeeper.IsBlank:boolean;
begin
	IsBlank:=(TeamTotal(WE)=0) and (TeamTotal(THEY)=0);
end;

function ScoreKeeper.OverlineTotal(team:teamId):word;
begin
	OverlineTotal:=my_data[team].Overline
end;

function ScoreKeeper.GameScore(team:teamId;n:gameIndex):word;
begin
	GameScore:=my_data[team].GameRecord[n]
end;

function ScoreKeeper.GamesWon(team:teamId):word;

var
	aCount:word;
	aGameIndex:gameIndex;

begin
	aCount:= 0;
	for aGameIndex:=Low(gameIndex) to High(gameIndex) do if GameScore(team, aGameIndex)>=100 then Inc(aCount);
	GamesWon:=aCount;
end;

function ScoreKeeper.GameWon(aGameIndex:gameIndex):boolean;

begin
	GameWon:=
		(my_data[WE].GameRecord[aGameIndex] >= GAME_POINTS)
		or
		(my_data[THEY].GameRecord[aGameIndex] >= GAME_POINTS);
end;

function ScoreKeeper.GamesPlayed:word;

var
	aGameIndex:gameIndex;
	aPlayedCount:word;

begin
	aPlayedCount:= 0;
	for aGameIndex:= Low(gameIndex) to High(gameIndex) do begin
		if GameWon(aGameIndex) then Inc(aPlayedCount);
	end;
	GamesPlayed:= aPlayedCount;
end;

function ScoreKeeper.CurrentGameIndex:gameIndex;

begin
	CurrentGameIndex:=GamesPlayed+1;
end;

function ScoreKeeper.GameOver:boolean;

begin
	GameOver:= (GamesWon(WE) > 1) or (GamesWon(THEY) > 1);
end;

function ScoreKeeper.TeamTotal(team:teamId):word;

	function GameTotal(team:teamId):word;

	begin
		GameTotal:= GameScore(team, 1) + GameScore(team, 2) + GameScore(team, 3);
	end;

begin
	TeamTotal:= OverlineTotal(team) + GameTotal(team);
end;

constructor ScoreKeeper.Construct;

begin
	DoClear;
end;

function ScoreKeeper.WonRubber(team:teamId):boolean;

begin
	WonRubber:=GamesWon(team)>1;
end;

procedure CalculateScore(var aScoreKeeper:ScoreKeeper;nMade,level:word;double:DoublingStatus;trump:TrumpType;dummy:playerType);
var
	aDeclarer, aDefender:ScoreType;

	function DefenderPts(vulnerable:boolean;aDblStatus:DoublingStatus):word;
		function UnderTricks(nOddTricks:word; nTricksWon:word):integer;
		begin
			UnderTricks:=BOOK_SIZE+nOddTricks-nTricksWon;
		end;

		function OverlinePts(aDblStatus:DoublingStatus; nUnderTricks:integer;nNotDbld,nDbldFirst,nDbldNext:word):word;
		begin
			if aDblStatus=NOT_DOUBLED then
				OverlinePts:=nNotDbld*nUnderTricks
			else
				OverlinePts:=(nDbldFirst+nDbldNext*(nUnderTricks-1))*aDblStatus;
		end;
	begin
		DefenderPts:=OverlinePts(aDblStatus,UnderTricks(level,nMade),Q(vulnerable,100,50), Q(vulnerable,200,100), Q(vulnerable,300,200))
	end;

	procedure CaseAssign(fromSrc:boolean; var declarer,defender:ScoreType;dummy:playerType);
	begin
		if fromSrc then case Dummy of
			North, South: begin
				declarer:= aScoreKeeper.my_data[WE];
				defender:= aScoreKeeper.my_data[THEY];
			end;
			East, West: begin
				declarer:= aScoreKeeper.my_data[THEY];
				defender:= aScoreKeeper.my_data[WE];
			end;
		end else case Dummy of
			North, South: begin
				aScoreKeeper.my_data[WE]:= declarer;
				aScoreKeeper.my_data[THEY]:= defender;
			end;
			East, West: begin
				aScoreKeeper.my_data[THEY]:= declarer;
				aScoreKeeper.my_data[WE]:= defender;
			end;
		end;
	end;

	procedure CalcVulnerabilityPnts(vulnerable:boolean;aTricksWon:word;var W:ScoreType);
	begin
		if vulnerable then begin
			if level=6 then Inc(W.OverLine,750)
			else if level=7 then Inc(W.OverLine,1500)
			else if double>0 then W.OverLine:=W.OverLine+50+(((aTricksWon-(level+BOOK_SIZE))*100)*(double*2));
		end 
		else begin
			if level=6 then Inc(W.OverLine,500)
			else if level=7 then Inc(W.OverLine,1000)
			else if double>0 then W.OverLine:=W.OverLine+50+(((aTricksWon-(level+BOOK_SIZE))*50)*(double*2));
		end;
	end;

	procedure CalculatePoints(vulnerable:boolean;var W:ScoreType;CurGame:integer);
		procedure CaseCalc(Value,CurGame:integer);
		begin
			if double=0 then begin
				W.GameRecord[CurGame]:=W.GameRecord[CurGame] + (level*Value);
				W.OverLine:=W.OverLine+((nMade-(level+6)) * Value);
			end
			else
				W.GameRecord[CurGame]:=W.GameRecord[CurGame]+((level*Value)*(double*2));
		end;
	begin
		case Trump of
			TTCLUB,TTDIAMOND:CaseCalc(MINOR_TRICK_POINTS, CurGame);
			TTHEART,TTSPADE:CaseCalc(MAJOR_TRICK_POINTS, CurGame);
			else {  No_Trump }
				if double=0 then begin
					W.GameRecord[CurGame]:=W.GameRecord[CurGame]+FIRST_NT_TRICK_POINTS+((level-1)*NEXT_NT_TRICK_POINTS);
					W.OverLine:=W.OverLine+((nMade-6-level)*NEXT_NT_TRICK_POINTS);
				end
				else begin
					W.GameRecord[CurGame]:=W.GameRecord[CurGame]+((FIRST_NT_TRICK_POINTS+((level-1)*NEXT_NT_TRICK_POINTS)) * (double*2));
				end
		end;
		CalcVulnerabilityPnts(vulnerable,nMade,W);
		if W.GameRecord[CurGame]>=GAME_POINTS then W.m_nGamesWon:=W.m_nGamesWon+1;
	end;

	function IsVulnerable(var declarer:ScoreType):boolean;
	begin
		IsVulnerable:=(declarer.m_nGamesWon>0);
	end;

begin { CalculateScore }
	CaseAssign(true,aDeclarer,aDefender,dummy);
	if nMade>=(BOOK_SIZE+level) 
		then CalculatePoints(IsVulnerable(aDeclarer),aDeclarer,aScoreKeeper.CurrentGameIndex)
		else Inc(aDefender.Overline,DefenderPts(IsVulnerable(aDeclarer), double));
	CaseAssign(false,aDeclarer,aDefender,dummy);
	if aDeclarer.m_nGamesWon>1 then begin
		case aScoreKeeper.GamesPlayed of
			2:Inc(aDeclarer.Overline,700);
			3:Inc(aDeclarer.Overline,500);
		end;
		CaseAssign(false,aDeclarer,aDefender,dummy);
	end;
end;

procedure ScoreKeeper.DoAddResult(team:teamId;level:contractLevel;trump:trumpType;doubling:doublingStatus;nMade:lengthType);
begin
	CalculateScore(self,nMade,level,doubling,trump,Q(team=WE,NORTH,WEST));
	if Length(as_string)>0 then as_string:=as_string+',';
	as_string:=as_string+IntToStr(Ord(team))+','+IntToStr(level)+','+IntToStr(Ord(trump))+','+IntToStr(doubling)+','+IntToStr(nMade);
end;

procedure ScoreKeeper.AddResult(team:teamId;level:contractLevel;trump:trumpType;doubling:doublingStatus;nMade:lengthType);
begin
	DoAddResult(team,level,trump,doubling,nMade);
	OnAdded;
end;

function ScoreKeeper.ToString:ansistring;
begin
	ToString:=as_string;
end;

procedure ScoreKeeper.OnAdded;
begin
	PlaceHolder;
end;

procedure ScoreKeeper.FromData(data:ansistring);
var
	n,val:integer;
	s:ansistring;
	team:teamId;
	bid:contractLevel;
	trump:trumpType;
	doubling:doublingStatus;

	function CheckRange(minVal,maxVal:integer):boolean;
	var
		b:boolean;
	begin
		b:=(val<minVal) or (val>maxVal);
		if b then Clear;
		CheckRange:=b;
	end;

begin //WriteLn('ScoreKeeper.ConstructFromData("',data,'")');
	n:=1;
	s:=ExtractDelimited(n,data,[',']);
	while Length(s)>0 do begin
		val:=StrToIntDef(s,-1);
		if CheckRange(0,1) then Exit; 
		team:=teamId(val);
		Inc(n);
		val:=StrToIntDef(ExtractDelimited(n,data,[',']),-1);
		if CheckRange(Low(ContractLevel),High(contractLevel)) then Exit; 
		bid:=contractLevel(val);
		Inc(n);
		val:=StrToIntDef(ExtractDelimited(n,data,[',']),-1);
		if CheckRange(Ord(Low(trumpType)),Ord(High(trumpType))) then Exit; 
		trump:=trumpType(val);
		Inc(n);
		val:=StrToIntDef(ExtractDelimited(n,data,[',']),-1);
		if CheckRange(Low(doublingStatus),High(doublingStatus)) then Exit;
		doubling:=val;
		Inc(n);
		val:=StrToIntDef(ExtractDelimited(n,data,[',']),-1);
		if CheckRange(Low(lengthtype),High(lengthtype)) then Exit;
		DoAddResult(team,bid,trump,doubling,val);
		Inc(n);
		s:=ExtractDelimited(n,data,[',']);
	end;
end;

constructor ScoreKeeper.ConstructFromData(data:ansistring);
begin //WriteLn('ScoreKeeper.ConstructFromData("',data,'")');
	Construct;
	FromData(data);
end;

function ScoreKeeper.OverLineScoreCount(team:teamId):quantity;
begin
	OverLineScoreCount:=0;
end;

end.
