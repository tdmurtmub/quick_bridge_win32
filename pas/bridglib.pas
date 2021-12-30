{ (C) 1992 Wesley Steiner }

{$MODE TP}

{$V-}

unit bridglib;

interface

uses
	std,
	cards,
	bridge;

const
	ACEVAL = 14;
	KINGVAL = 13;
	sl_LibLine=80;
	sl_LibHandName=16;
	sl_LibKeyWord=16;
	sl_LibKeyValue=80;
	maxdeals=5; { Maximum number of deals in the search }
	branchvalue=10; { Branch-value controlling the branching }

 type
	valuetype= 2..ACEVAL;
	pcSuitType = TSuit;
	cardInfo=record
		suit:pcSuitType;
		value:valuetype;
		known, { Card is known to player }
		played:boolean; { Card has been played }
	end;
	levelType = 0..7; { Bid levels }
	bidType =record { Representation of bids }
		Level:levelType;
		Trump:trumpType;
	end;

const
	x_aSuitString:array[trumptype] of string = ('Club', 'Diamond', 'Heart', 'Spade', 'No-Trump');
	{ Representation of the special bids }
	PASS_BID:bidtype=(level:0;trump:TTNT);
	dbl:bidtype=(level:0;trump:TTCLUB);
	rdbl:bidtype=(level:0;trump:TTDIAMOND);

const
	SearchWght=8; { Weight of search evaluation }
	HeuristicWght=3; { Weight of direct heuristics }

type
	str4=string[4];
	cardnotype = 0..12; { Index in DIST }
	indextype = 0..51; { Used as index in DIST }
	ExpandCardNoType = -1..12;
	ExpandValueType = -1..15;

	{ INFORMATION about the real deal, known by all players.
		This information is obtained mainly through the biddings.
		MINL contains the minimum length of the suits.
		Minus 1 means that the player have shown a void suit in the play.
		MINP and MAXP contains the minimum and maximum number of points, including distribution points.
		MINL is updated for each bid and when a card is played.
		MINP and MAXP are only updated at biddings.
		Thus MINP and MAXP refer to the hand before any cards were played }
	infotype=record
		minl:array[pcSuitType] of -1..13;
		minp,maxp:integer;
	end;

	PlayerHand=array[PlayerType] of boolean;
	CardDist=array[PlayerType, CardNoType] of cardInfo; // cards are in descending rank order (see SortCards)
	DataRecord=record
		l:array[pcSuitType] of lengthtype;
		p:integer;
	end;
	CardData=array[PlayerType] of DataRecord;
	TCardStat=array[CardNoType] of { Statistics about cards } record
		Try:boolean;{ Card should be tried }
		Stat:integer;{ Statistical Evaluation }
	end;

var
	{ The DISTribution of the cards, and the state of the game.
	[rel] and [RelInfo] contains the real situation.
	[sim] and [SimInfo] contains a simulated situation used in the search }
	RelInfo,SimInfo:CardDist;
	{ DATA about each of the 4 hands.
	L contains the length of the 4 suits.
	P contains number of points, including distribution points.
	L but not P is updated when a card is played.
	Thus P refers to the hand before any cards were played }
	RelData,SimData:CardData;
	info:array[PlayerType] of infotype;
	{ The BIDS made in the game, indexed by BIDNO. BIDNO contains the number of bids made }
	theBids:array[-4..51] of bidtype;
	bidno:0..52;
	contract:bidtype;
	theDoubledStatus:DoublingStatus;
	{ The played game, card by card }
	game:array[indextype] of record
		hand:PlayerType; { Hand }
		no:cardnotype; { Index to RelInfo }
	end;
	rel,sim:record{ State of the game during play }
		Round:0..52; { Number of played cards }
		WonTricks:0..13; { Tricks won by declarer }
		leadhand:PlayerType; { Leading hand in this trick }
		leadsuit:pcSuitType;
		bestcard:cardInfo; { Best card in the trick }
		Besthand:PlayerType; { Player of best card }
		playinghand:PlayerType; { Hand to play }
	end;
	TrickNo,
	firstno:0..13; { Trick number counters }
	computer:PlayerHand;{ Indicates which hands the computer plays }
	TopHand:PlayerType;
	Dealer:PlayerType; { First player to bid }
	dummy:PlayerType;
	Cardstat:TCardStat;

{ Contains the different lead Suits used in the search }

var
	Count :array[0..13] of record
		{ Sorted table of Suits }
		Suits :array[1..4] of pcSuitType;
		{ Evaluation of Suits }
		Evaluation :array[pcSuitType] of integer;
		{ Counter for Suit table }
		Cnt :0..4;
	end;

const
	CommandLength=4;

type
	CommandString=string[CommandLength];

var
	Command:CommandString;

{ Names of hands, Suits and values }

const
	HandNameLen=5;
	TeamNameLen=HandNameLen*2+1; { "xxxxx/xxxxx" }
	HandName  :array[PlayerType] of string[HandNameLen]=('North', 'East', 'South', 'West');
	TrumpName :array[TrumpType] of string[2]=('Cl', 'Di', 'He', 'Sp', 'NT');
	ValueName :array[ValueType] of CHAR=('2', '3', '4', '5', '6', '7', '8','9', 'T', 'J', 'Q', 'K', 'A');

var
	TrumpStr :array[TrumpType] of string[2];

type  BidClassType =
				 (OpenPass,{ Open Pass }
					OPEN1S,{ Open 1 Suit    (1 He) }
					open2S,{ Open 2 Suit    (2 He) }
					OpenNT,{ Open   NT      (1 NT) }

					Resppass,{ Response Pass  (1 He, Pass) }
					resp2S,{ Response 2 S   (1 He, 2 He) }
					resp3S,{ Response 3 S   (1 He, 3 He) }
					resp1o1, { Response 1-o-1 (1 He, 1 Sp) }
					resp2o1, { Response 2-o-1 (1 He, 2 Cl) }
					resp1NT, { Response 1 NT  (1 He, 1 NT) }
					resp2NT, { Response 2 NT  (1 He, 2 NT) }

					resp2S2NT, { Response 2 S - 2 NT  (2 He, 2 NT) }
					resp2Snorm,{ Response 2 S - Norm  (2 He, Others) }

					Stayman, { Stayman  (1 NT, 2 Cl) }
					RespStayman, { Stayman response }
					Blackwood, { Blackwood (4 NT or 5 NT) }
					RespBlackwood, { Blackwood response }

					resp1NT2S, { Response 1 NT - 2 S  (1 NT, 2 He) }
					SecondBidNT, { 2nd Bid NT     (1 He, 2 Cl, 2 NT) }
					Shutout, { Shutout Bids   (Open 3 He) }
					OVERCALL,{ Opponent OverCall }

					NORMAL_PASS,{ Normal Pass }
					NormDBL, { Normal Dbl }
					NormRDBL,{ Normal RDbl }
					NORMAL_BID,
					ILLEGAL_BID);

var
	theBidTypes:array[-4..51] of BidClassType;

type
	TaskType = (CalcInfo, Evaluate);
	TTeamName = String[11];
	TeamNameStr = string[3];

type
	BridgeModeType=(TutorialMode,ContractMode);
	DealType=(DealRandom,DealLib);
	{ logical state of the bridge game }
	TrickIndex=1..13;
	TBridgeState=record
		Mode:BridgeModeType;
		DealFlag:DealType;
		MakeNSDeclarer:boolean; { when playing "TutorialMode" the high card hands are rotated until N/S gets the high points }
	end;

const
	BridgeState:TBridgeState=(
		Mode:TutorialMode;
		DealFlag:DealRandom;
		MakeNSDeclarer:True
		);

var
	DealLibLine:integer; { current line # in the the file }
	Comp:boolean; { Computer controls Hand }
	Hint:boolean;
	CanPlayAll:boolean; { All cards can be Played }
	FB_BestVal,FB_Val:integer;
	winning_bidder:PlayerType; { team that has the current bidding contract }
	BestBid:BidType; { Chosen bid }
	NextBidder:PlayerType;
	TmpBid:BidType;

function BidSystem(Bid:BidType;Task:TaskType):integer;
function BidStr(Bid:BidType):str4;
function CardCount(Who:PlayerType):integer;
function CardType2TCard(Crd:cardInfo):TCard;
function CountTry:LengthType;
function DiscardCard:CardNoType;
function DistPoints(Hand:PlayerType):integer;
function EndGameMsg(wonRubber,wonGame:boolean):string;
function EqBid(A,B:BidType):boolean;
function findHigh(Hand:PlayerType; s:pcSuitType):CardNoType;
function findLow(Hand:PlayerType; s:pcSuitType):CardNoType;
function GetHandPoints(s,h,d,c:pchar):integer;
function Highest(hand4:integer; s:pcSuitType):ExpandValueType;
function Highmax(hand4:integer; s:pcSuitType; MaxVal:ExpandValueType):ExpandValueType;
function Lead:boolean;
function LongTeamName(Who:PlayerType;S:PChar):PChar;
function Lowest(hand4:integer; s:pcSuitType):ExpandValueType;
function NextHand(Who:PlayerType):PlayerType;
function Opponent(CurHand :PlayerType):PlayerType;
function PCPip2Value(aPip:TPip):ValueType;
function Partner(CurHand :PlayerType):PlayerType;
function Practicing:boolean; { true if we are playing in practice mode }
function RandomDealMode:boolean;
function SelectCard:CardNoType;
function ShortTeamName(Who:PlayerType;S:PChar):PChar;
function ShouldRotateHands:boolean; { true if high card hands should be rotated to N/S }
function SuitString(a_aSuit:trumptype):string;
function Team(who:PlayerType):TeamId;
function TeamName(Player:PlayerType):TeamNameStr;
function Threepass:boolean;
function VoidIn(H:PlayerType;S:pcSuitType):boolean;

procedure Analyse(var Result:integer);
procedure InitBids;
procedure InitDefaults;
procedure InitializeSimHandInfo;
procedure ChangeCards;
procedure DealNewCards;
procedure FindCard(var BestChoice:CardNoType;var Hint:boolean);
procedure InitGames;
procedure InitSearch;
procedure InitTrumpStr;
procedure MakeBid(Bid:BidType);
procedure PreAuctionSetup;
procedure PlayCard(Hand:PlayerType;   No:CardNoType);
procedure ProcessLastBid;
procedure ReadPrepare(var Command:CommandString);
procedure RotateHands; { rotate hands to give N/S the high hands in random deals }
procedure SetupBid(T:TrumpType);
procedure SetDealMode(aMode:DealType);
procedure SetupSimHand(target:playerType;s,h,d,c:pchar);
procedure SortCards;

{$ifdef TEST} 
function GetBidClass(Bid:BidType):BidClassType;
function Honours(var aCardDist:CardDist):word;
function Jumps(const Bid:BidType):integer;
{$endif}

{$ifdef UNUSED}
function DealLibLoad(const aLibFile,aLibHand:TString):integer;
function LibraryDealMode:boolean; { true if next hand should be loaded from a deal library file. }

procedure DealLibFirst(const aLibFile:PChar;aDealName,aDealComment:PChar);
procedure DealLibNext(aDealName,aDealComment:PChar);
{$endif UNUSED}

implementation

uses
	{$ifdef TEST} bridglibTests, {$endif}
	strings, stringsx;

type
	DefaultRec=record
		Computer:PlayerHand;
	end;

var
	Defaults:DefaultRec;

procedure InitTrumpStr;

begin
	TrumpStr[TTCLUB   ]:=chr(5);
	TrumpStr[TTDIAMOND]:=chr(4);
	TrumpStr[TTHEART  ]:=chr(3);
	TrumpStr[TTSPADE  ]:=chr(6);
	TrumpStr[TTNT]:='NT';
end;

function EqBid;

{ Return true if bid "A" = bid "B". }

begin
	EqBid:=(a.Level=b.Level) and (a.Trump=b.Trump);
end;

function DistributionPoints(const hand:DataRecord):integer;

var
	Pts:integer;
	s:pcSuitType;

begin
	Pts:=0;
	with hand do for s:=TCLUB to TSPADE do if l[s]<=2 then Pts:=Pts+3-l[s];
	DistributionPoints:=Pts;
end;

function DistPoints(Hand:PlayerType):integer;
begin
	DistPoints:=DistributionPoints(SimData[Hand]);
end;

function CardStr(Crd:cardInfo):str4;

{ Converts a Card to a string }

	begin

		with Crd do
			CardStr:=ValueName[Value] + ' ' + TrumpStr[TrumpType(ord(Suit))];

	end; { CardStr }

function BidStr(Bid:BidType):str4;
begin
	if EqBid(Bid,PASS_BID) then BidStr:='Pass'
	else if EqBid(Bid,Dbl) then BidStr:='Dbl'
	else if EqBid(Bid,RDbl) then BidStr:='RDbl'
	else with Bid do BidStr:=chr(Ord('0')+Level)+TrumpName[Trump][1];
end;

function Opponent(CurHand :PlayerType):PlayerType;

{ returns an Opponent of the passed in Hand }

begin

  if CurHand < South then
    Opponent:=CurHand + 1
  else
	  Opponent:=CurHand - 1;

end; { Opponent }

procedure InitDefaults;

begin
	Defaults.Computer[NORTH]:=FALSE;
	Defaults.Computer[SOUTH]:=TRUE;
	Defaults.Computer[EAST]:=FALSE;
	Defaults.Computer[WEST]:=FALSE;
end;

procedure GetDefaults;

begin
	Computer[NORTH]:=not Defaults.Computer[NORTH];
	Computer[SOUTH]:=not Defaults.Computer[SOUTH];
	Computer[EAST]:=not Defaults.Computer[EAST];
	Computer[WEST]:=not Defaults.Computer[WEST];
end;

function HighCardPoints(aValue:ValueType):integer;

begin
	HighCardPoints:=Max(0,aValue-10);
end;

procedure Exchange(a,b:IndexType);
{ Exchange cards with index A and B in SimInfo and updates SimData }
var
	ah,bh:PlayerType;
	an,bn:CardNoType;
	Card:cardInfo;
	
	procedure Update(Hand:PlayerType;Sign:integer);
	{ Update SimData[Hand] }
	begin
		with SimData[Hand], Card do begin
			l[Suit]:=l[Suit] +  Sign;
			if Value > 10 then p:=p + (Value - 10) * Sign; { High Card points }
			if l[Suit] * 2 - Sign <=5 then p:=p - Sign; { Distribution points }
		end;
	end;

begin //writeln('Exchange(',a,'{hand:',a and 3,';card:',a shr 2,'},',b,'{hand:',b and 3,';card:',b shr 2,'})');
	ah:=a and 3;
	an:=a shr 2;
	bh:=b and 3;
	bn:=b shr 2;
	Card:=SimInfo[ah, an];
	Update(ah, -1);
	Update(bh, 1);
	SimInfo[ah, an]:=SimInfo[bh, bn];
	SimInfo[bh, bn]:=Card;
	Card:=SimInfo[ah, an];
	Update(bh, -1);
	Update(ah, 1);
end;

procedure ChangeCards;
{ Mixes some of the unknown cards in SimInfo in such a way that SimData still corresponds with Info afterwards }
var
  Cnt:integer;
  a, b   :IndexType;
  ah, bh :PlayerType;
	ac, bc :cardInfo;
begin { ChangeCards }
  for Cnt:=1 to 26 do begin
    repeat
      a:=Random(52);{ 1st Card }
      ah:=a and 3;
      ac:=SimInfo[ah, a shr 2];
    until not ac.Known;
    repeat
      b:=Random(52); { 2nd Card }
      bh:=b and 3;
      bc:=SimInfo[bh, b shr 2];
    until not bc.Known;
    if ah <> bh then begin
      Exchange(a, b);{ Exchange cards }
 { if the the difference between SimData and Info was made
           larger, then Exchange the cards Back again }
      if (Info[ah].Minl[ac.Suit] > SimData[ah].l[ac.Suit])
				 or (Info[bh].Minl[ac.Suit] < 0)
         or (Info[bh].Minl[bc.Suit] > SimData[bh].l[bc.Suit])
         or (Info[ah].Minl[bc.Suit] < 0)
         or (ac.Value > 10)
					 and ((SimData[ah].p < Info[ah].MinP)
						 or (SimData[ah].p > Info[ah].MaxP))
         or (bc.Value > 10)
					 and ((SimData[bh].p < Info[bh].MinP)
						 or (SimData[bh].p > Info[bh].MaxP)) then
               Exchange(a, b);
    end; { if }
  end;
end; { ChangeCards }

procedure DealCards;
{ Deals the unknown cards in SimInfo at Random. if BidNo=0, All cards will be changed }
var
    i, j :IndexType;
    Hand :PlayerType;
    No   :CardNoType;
begin
	{ Calculate which cards are known }
	if BidNo <> 0 then
		for Hand:=North to West do
		for No:=0 to 12 do
		  with Sim, SimInfo[Hand, No] do
			{ Played and own cards and Dummy are known, and Dummy knows partners Hand }
			Known:=Played or (Hand=PlayingHand)
					 or (Round>0)
					   and ((Hand=Dummy) or (PlayingHand=Dummy)
						 and not odd(Hand + Dummy));
	for i:=0 to 50 do { Mix unKNOWN cards }
		if not SimInfo[i and 3, i shr 2].Known then { Get All unknown positions }
		begin
			repeat{ Pick an unknown Card }
				j:=i + Random(52 - i);
			until not SimInfo[j and 3, j shr 2].Known;
			Exchange(i, j);
		end;
end;

procedure DealNewCards;
{ Deals the cards in "SimInfo" at Random. }
var
	i, j :IndexType;
begin
	for i:=0 to 50 do { Mix cards } begin
		repeat{ Pick an unknown Card }
			j:=i + Random(52 - i);
		until not SimInfo[j and 3, j shr 2].Known;
		Exchange(i, j);
	end;
end;

procedure SortCards;
{ Sorts each Suit in each Hand in SimInfo with largest card of each suit first. Maintains the position of suits in the hand. }
var
	h:PlayerType;
	i,j:CardNoType;
	Card:cardInfo;
begin
	for h:=North to West do
		for i:=0 to 11 do with SimInfo[h, i] do
			if not Known then
				for j:=i + 1 to 12 do
					if
						(not SimInfo[h, j].Known)
						and
						(Suit=SimInfo[h, j].Suit)
						and
						(Value<SimInfo[h, j].Value)
					then begin
						Card:=SimInfo[h, i];
						SimInfo[h, i]:=SimInfo[h, j];
						SimInfo[h, j]:=Card;
					end;
end; { SortCards }

procedure InitializeRelHands;
var
	h:PlayerType;
	No:CardNoType;
	s,s0:pcSuitType;
begin
	for s:=TCLUB to TSPADE do begin
		h:=Ord(s);
		for No:=0 to 12 do with RelInfo[h,No] do begin
			Suit:=s;
			Value:=No+2;
			Played:=TRUE;
        end;
		with RelData[h] do begin
			for s0:=TCLUB to TSPADE do l[s0]:=0;   
			p:=10+3*3;
		end;
    end;
end;

procedure InitGames; // called by: PreRubber
begin
    Randomize;
	InitTrumpStr;
	InitializeRelHands;
	Dealer:=NORTH;
	Dummy:=NORTH;
	GetDefaults;
end;

function Partner(CurHand :PlayerType):PlayerType;
{ returns the team Partner of the passed in Hand }
begin
    if CurHand < South then
		Partner:=CurHand + 2
    else
		Partner:=CurHand - 2;
end;

function TeamName(Player:PlayerType):TeamNameStr;

	{ Returns the abbreviated team name that "Player" belongs to. }

	begin

		if Player<South then
			TeamName:=HandName[Player][1]+'/'+HandName[Partner(Player)][1]
		else
			TeamName:=HandName[Partner(Player)][1]+'/'+HandName[Player][1];

	end;

function ShortTeamName;

	{ Return in "S" the team name that Player "Who" belongs to. }

	var
		P1,P2:PlayerType;

	begin
		if Who<South then begin
			P1:=Who;
			P2:=Partner(Who);
		end
		else begin
			P1:=Partner(Who);
			P2:=Who;
		end;
		S[0]:=HandName[P1][1];
		S[1]:='/';
		S[2]:=HandName[P2][1];
		S[3]:=#0;
		ShortTeamName:=S;
	end;

function LongTeamName;

	{ Return in "S" the team name that Player "Who" belongs to. }

	var
		P1,P2:PlayerType;
		tS:array[0..HandNameLen] of Char;

	begin
		if Who<South then begin
			P1:=Who;
			P2:=Partner(Who);
		end
		else begin
			P1:=Partner(Who);
			P2:=Who;
		end;
		StrPCopy(S,HandName[P1]);
		StrCat(S,'/');
		StrPCopy(tS,HandName[P2]);
		StrCat(S,tS);
		LongTeamName:=S;
	end;

function Lead:boolean;

	{ True if the current hand is to lead }

	begin

	  Lead:=(Rel.Round and 3)=0;

	end;

function VoidIn(H:PlayerType;S:pcSuitType):boolean;

	{ Return true if player "H" is void in suit "S". }

  var

  	i:integer;

	begin

  	VoidIn:=True;
		for i:=0 to 12 do if (not RelInfo[H,i].Played) and (RelInfo[H,i].Suit=S) then begin
	  	VoidIn:=False;
      Exit;
	 end;

	end;

function CardCount(Who:PlayerType):integer;

{ Return the number of unplayed cards remaining in "Who"'s hand. }

var
	i,n:integer;

begin
	n:=0;
	for i:=0 to 12 do if not RelInfo[Who,i].Played then	Inc(n);
	CardCount:=n;
end;

procedure InitBids;
{ Initialize theBids[-4..-1] and theBidTypes[-4..-1] }
var
	i:integer;
begin
	for i:=-4 to -1 do begin
		theBids[i]:=PASS_BID;
		theBidTypes[i]:=OpenPass;
	end;
end;

function Jumps(const Bid:BidType):integer;
{ Count Number of Jumps from Contract to Bid }
begin
	if Bid.Trump<=Contract.Trump 
		then Jumps:=Bid.Level-Contract.Level-1
		else Jumps:=Bid.Level-Contract.Level;
end;

type
	NumberType =0..4;
	PCPipType = TPip;

function PCPip2Value(aPip:TPip):ValueType;

begin
	case aPip of
		TAce:PCPip2Value:=ACEVAL;
		else PCPip2Value:=Ord(aPip)+1;
	end;
end;

function Number(Player:PlayerType;   v:ValueType):NumberType;

{ Count how many cards Player has of the Value }

var
  n :NumberType;
  i :CardNoType;
  
begin
  n:=0;
  for i:=0 to 12 do
    if SimInfo[Player,i].Value=v then
      n:=n + 1;
  Number:=n;
end; { Number }

function GetBidClass(Bid:BidType):BidClassType;
var
	PartnerBidtyp:BidClassType;
begin
	with Bid do begin
	    GetBidClass:=NORMAL_BID;
	    if Jumps(Bid) >=2 then { Double Jumps are Illegal } GetBidClass:=ILLEGAL_BID;
	    PartnerBidtyp:=theBidTypes[BidNo-2];
		if (PartnerBidtyp=STAYMAN) and EqBid(theBids[BidNo - 1],PASS_BID) 
			then GetBidClass:=RESPSTAYMAN
			else if (PartnerBidtyp=BLACKWOOD) and EqBid(theBids[BidNo - 1],PASS_BID) 
				then GetBidClass:=RESPBLACKWOOD
				else if PartnerBidtyp=open2S then begin
					GetBidClass:=RESP2SNORM;
					if EqBid(theBids[BidNo-1],PASS_BID) then begin
						if (Level=2) and (Trump=TTNT) 
							then GetBidClass:=resp2S2NT
							else if EqBid(Bid,PASS_BID) then GetBidClass:=ILLEGAL_BID
					end
					else if EqBid(Bid,PASS_BID) then GetBidClass:=resp2S2NT;
				end
				else if EqBid(Bid,PASS_BID) then if PartnerBidtyp=OpenPass 
					then GetBidClass:=OpenPass
					else if PartnerBidtyp=OPEN1S 
						then GetBidClass:=Resppass
						else GetBidClass:=NORMAL_PASS
				else if EqBid(Bid,Dbl) 
					then { Doubles } GetBidClass:=NormDBL
					else if EqBid(Bid,RDbl) 
						then { Redoubles } GetBidClass:=NormRDBL 
						else if (Trump=TTNT) and { Blackwood } ((Level=4) or (Level=5) and (theBidTypes[BidNo-4]=Blackwood)) 
							then GetBidClass:=Blackwood 
							else if PartnerBidtyp=OpenNT then { Response to opening in NT } begin
								if Level=theBids[BidNo-2].Level + 1 then if Trump=TrumpType(Ord(TCLUB)) 
									then { Club is Stayman } GetBidClass:=Stayman 
									else { 2 in Suit is very weak } if (Level=2) and (Trump <> TTNT) then GetBidClass:=resp1NT2S
							end
							else if (Trump=TTNT) and { Opening 1 in Suit, second Bid TTNT } ((PartnerBidtyp=resp1o1) and (Level <=2) or (PartnerBidtyp=resp2o1) and (Level <=3)) 
								then GetBidClass:=SecondBidNT
								else { Response to opening 1 in Suit } if PartnerBidtyp=OPEN1S then begin
									if Trump=theBids[BidNo-2].Trump then begin
										if Level=2 
											then GetBidClass:=resp2S
											else if Level=3 then GetBidClass:=resp3S;
									end
						            else if Trump=TTNT then begin
										if Level=1 
											then GetBidClass:=resp1NT
											else if Level=2 then GetBidClass:=resp2NT;
							        end
					            else if Level=1 
									then GetBidClass:=resp1o1
									else if Level=2 then GetBidClass:=resp2o1;
								end
							else if BidNo < 4 then if (Trump=TTNT) and (PartnerBidtyp=OPENPASS) 
								then GetBidClass:=OPENNT
								else if EqBid(Contract,PASS_BID) then if Level=1 
									then { Opening in Suit } GetBidClass:=OPEN1S
									else if Level=2 
										then GetBidClass:=open2S
										else GetBidClass:=Shutout
								else if Trump<>TTNT then { Opponents first Bid } if Jumps(Bid)=0 
									then GetBidClass:=OVERCALL
									else GetBidClass:=SHUTOUT;
	end; { with }
end; { GetBidClass }

procedure PreAuctionSetup;
begin
	BidNo:=0;
	Contract:=PASS_BID;
	theDoubledStatus:=0;
end;

function BidSystem(Bid:BidType;Task:TaskType):integer; // called by: MakeBid, ProcessLastBid
{ Depending on Task, BidSystem will
  Task CalcInfo:Update Info according to the Bid.
  Task Evaluate:Evaluate how the Bid fits with the present Hand }
var
  player,Partner:PlayerType; { Player to Bid and his Partner }
  InfoPlayer:InfoType; { Updated Info for Player }
  BidVal:integer;{ The Evaluation of Bid }
  Test:BOOLEAN;{ Evaluate the Bid by testing the Hand with InfoPlayer }

 { Necessary points to win a Contract }
const MinTab:array[BOOLEAN,LevelType] of integer =
				((0,20,20,23,26,29,33,37),{ Suit contracts }
				 (0,20,20,26,29,32,35,39)); { NT   contracts }

	procedure NormalPass;
	{ Pass if the present Contract is correct }
	var
	  p :integer;
	begin
		with InfoPlayer,Contract do begin
			p:=Info[Partner].MinP;
		    if Level=7 
				then MaxP:=40
				else MaxP:=MinTab[false,Level + 1]-p-2;
		    if not odd(Player + Dummy) then begin
		      if (Level < 4) and not ((Level=3) and (Trump=TTNT)) then
		        MaxP:=Max(MaxP,24-p){ Check Game chance }
		      else
		        if (Level > 4) or (Trump >=TrumpType(Ord(THEART))) then
		        begin
		          if Level < 6 then { Check slam chance } MaxP:=Max(MaxP,32-p);
		          BidVal:=9;
				end;
						{ Check the Suit }
		        if Trump <> TTNT then
		          Minl[pcSuitType(Ord(Trump))]:=7-Info[Partner].Minl[pcSuitType(Ord(Trump))];
		    end;
			{ if the Opponent Bids then Pass means weak Hand }
			if not EqBid(theBids[BidNo-1],PASS_BID) then MaxP:=Max(MaxP,MinP + 2);
		end;
	end; { NormalPass }

	procedure NormalBid; { All normal Bids }
		function MinPoints(Level:integer;Trump:TrumpType; Jump:BOOLEAN):integer; { Minimum points for Bidding Level in TRUMPH }
		var
			Pts, MinPts :integer;
			FoundTrump  :TrumpType;
			function FoundSuit:TrumpType; { Returns the eventual chosen trumpsuit }
			var
			  s:pcSuitType;
			begin
			  FoundSuit:=TTNT;
			  for s:=TCLUB to TSPADE do if Info[Player].Minl[s] + Info[Partner].Minl[s] >=8 then FoundSuit:=TrumpType(Ord(s));
			end;
		begin { MinPoints }
			with Info[Partner] do begin
				{ Make sure Partner can Bid the right Trump without getting too High }
			    FoundTrump:=FoundSuit;
			    if (Trump > FoundTrump) and (Trump <> TTNT) then Level:=Level + 1;
			    Level:=Min(Level,7);
			    MinPts:=InfoPlayer.MinP;
				{ Bidding when the Opponent has bidden means extra strength }
				if not EqBid(theBids[BidNo-1],PASS_BID) then MinPts:=MinPts + 3;
			    Pts:=MinTab[Trump=TTNT,Level]-MinP-1;
			    if odd(Player + Dummy) and not Jump then Pts:=Pts-2;
			    MinPts:=Max(MinPts,Pts);
			    if Jump or (Trump=Contract.Trump) or
			      (FoundTrump <> TTNT) and (FoundTrump=Contract.Trump) then
			      if Level > 5-Ord(Trump) div 2 then { Above Game means slam interest }
			         Pts:=33-MaxP
					else { Jump means Game demand }
			        if (Level=5-Ord(Trump) div 2) or Jump 
						then Pts:=25-MinP
						else Pts:=26-MaxP;
			    MinPoints:=Max(MinPts,Pts);
			end
		end;
	var
	  s :pcSuitType;
	  i :integer;
	  n :LevelType;
	begin { NormalBid }
		with SimData[Player],InfoPlayer,Bid do begin
			if Trump <> TTNT then { Trump Length }
			  Minl[pcSuitType(Ord(Trump))]:=Min(Max(Minl[pcSuitType(Ord(Trump))] + 1,4),
								 8-Info[Partner].Minl[pcSuitType(Ord(Trump))])
			else if Level <> 3 then { NT means No singletons } begin
				for s:=TCLUB to TSPADE do Minl[s]:=2;
				BidVal:=-9;
				i:=BidNo-2;
				while i>=0 do begin 
					with theBids[i] do if (Trump=TTNT) and (Level > 0) then BidVal:=0;
					i:=i-4;
				end;
			end;
			n:=Level-Jumps(Bid);{ Calculate MinP and MaxP }
			MinP:=MinPoints(n  ,Trump,false);
			MaxP:=MinPoints(n + 1,Trump,true);
			MaxP:=Max(MaxP,MinP + 3);
			if n=Level 
				then MaxP:=MaxP-1
				else begin
					MinP:=MaxP;
					MaxP:=Info[Player].MaxP;
				end;
			{ Bid in major when possible }
			if (l[THEART] + Info[Partner].Minl[THEART]  >=8) or
			 (l[TSPADE] + Info[Partner].Minl[TSPADE]  >=8) then begin
				if Trump=TTNT 
					then BidVal:=-100
					else if l[pcSuitType(Ord(Trump))] + Info[Partner].Minl[pcSuitType(Ord(Trump))]  <  8 then BidVal:=-100;
			end
			else { do not Bid if Partner Bids 3 NT }
				if (Contract.Level=3) and (Contract.Trump=TTNT) then BidVal:=-100;
			if Level >=5 then { do not Bid if Partner has used Blackwood } begin
				i:=BidNo-2;
				while i >=0 do begin
					if theBidTypes[i]=Blackwood then BidVal:=-300;
					i:=i-4;
				end;
			end;
		end;
	end; { NormalBid }

	procedure NormalDBL;
	{ Double if the Contract will go at least 2 down }
	var
		Tricks:integer;
	begin
		with SimData[Player],Contract do begin
			Tricks:=(p-DistPoints(Player) + Max(Info[Partner].MinP-2,0)) div 4-1;
			if Trump <> TTNT then
			if l[pcSuitType(Ord(Trump))] > 3 then Tricks:=Tricks + l[pcSuitType(Ord(Trump))]-3;
			BidVal:=10;
			if Tricks < 2 + 7-Level then BidVal:=-140;
		end;
	end; { NormalDBL }

var
	s:pcSuitType;
	Val:integer;
	Len:integer;
begin //writeln('BidSystem(BidType(',bid.Level,',',Ord(bid.trump),'),TaskType(',Ord(task),'))');
	Player:=(Dealer + BidNo) and 3;
	Partner:=(Player + 2) and 3;
	InfoPlayer:=Info[Player];
	Test:=true;
	BidVal:=0;
	with SimData[Player],InfoPlayer,Bid do begin 
		case GetBidClass(Bid) of
			{ Opening Bids }
			OpenPass:begin { Opening Pass, 0-12 p }
			  MaxP:=12;
			end;
			OPEN1S  :begin { Opening 1 S, 13-23 p, 4 trumps }
			  MinP:=13;
			  MaxP:=23;
			  Minl[pcSuitType(Ord(Trump))]:=4;
			end;
			open2S:begin { Opening 2 S, 24- ? p, 4 trumps }
				MinP:=24;
				Minl[pcSuitType(Ord(Trump))]:=4;
			end;
			OpenNT:begin { Opening NT }
				if Level=1 
					then MinP:=16{ 1 NT  16-18 p }
					else MinP:=16 + Level*3; { 2 NT  22-24 p }
				MaxP:=MinP + 2;
				for s:=TCLUB to TSPADE do{ No singletons }
				Minl[s]:=2;
				if DistPoints(Player) > 1 then BidVal:=-100;
			end;

			{ Response to 1 in Suit }
			Resppass:begin { 1 He, Pass   0- 5 p }
				if EqBid(theBids[BidNo-1],PASS_BID) 
					then MaxP:=5
					else MaxP:=8;
            end;
			resp2S:begin { 1 He, 2 He   6- 9 p, 4 trumps }
				MinP:=6;
				MaxP:=9;
				Minl[pcSuitType(Ord(Trump))]:=4;
			end;
			resp3S:begin { 1 He, 3 He  13-16 p, 4 trumps }
				MinP:=13;
				MaxP:=16;
				Minl[pcSuitType(Ord(Trump))]:=4;
            end;
			resp1o1:begin { 1 He, 1 Sp   6- ? p }
                MinP:=6;
				Minl[pcSuitType(Ord(Trump))]:=4;
			end;
			resp2o1  :begin { 1 He, 2 Cl  10- ? p }
                MinP:=10;
                Minl[pcSuitType(Ord(Trump))]:=4;
            end;
			resp1NT  :begin { 1 He, 1 NT   6- 9 p, any distribution }
                   MinP:=6;
                   MaxP:=9;
                   BidVal:=-20;
                 end;
			resp2NT:begin { 1 He, 2 NT  13-16 p, NT distribution }
				 MinP:=13;
                MaxP:=16;
				for s:=TCLUB to TSPADE do Minl[s]:=2;
                if DistPoints(Player) > 1 then
				BidVal:=-100;
			end;

			{ Response to 2 in Suit }
			resp2S2NT :begin{ 2 He, 2 NT   0- 8 p }
				MaxP:=8;
				BidVal:=-20;
			end;
			resp2Snorm:begin { 2 He, anything else, an Ace and a King }
				MinP:=7;
				if (Number(Player,ACEVAL)*2 +
					 Number(Player,KingVal) < 3) or (Jumps(Bid) >=1) then
					BidVal:=-200;
					if Trump <> TTNT then
						Minl[pcSuitType(Ord(Trump))]:=4;
			end;

			{ Conventions }
			Stayman:begin { Stayman  7- ? p }
				if Level=2 then
					MinP:=7;
				if (l[THEART] <> 4) and (l[TSPADE] <> 4) then
					BidVal:=-200
				else
					BidVal:=20;
			end;
			RespStayman   :begin { Response, He or Sp 4 trumps, otherwise Di }
				if ((Trump=TTNT) or
					 (Level <> theBids[BidNo-2].Level)) and
					 EqBid(theBids[BidNo-1],PASS_BID) then
					BidVal:=-100
				else
					if (Trump<>TTNT) and (Trump>=TrumpType(Ord(THEART))) then
					begin
						Minl[pcSuitType(Ord(Trump))]:=4;
						BidVal:=10;
					end;
			end;
			Blackwood:begin { Blackwood, not used }
				MinP:=MinTab[false,Level + 2]-Info[Partner].MinP-1;
				BidVal:=-200;
			end;
			RespBlackwood:begin { Response, show Number of aces or kings }
				Test:=false;   
				BidVal:=-200;
				if (Level=theBids[BidNo-2].Level + 1) then if Number(Player,ACEVAL-(Level-5))=Ord(Trump) then BidVal:=10;
				if Level=0 then BidVal:=-40;
			end;
			{ Other special Bids }
			resp1NT2S:begin { 1 NT, 2 He, 0- 6 p, odd distribution }
				MaxP:=6;
				Minl[pcSuitType(Ord(Trump))]:=6;
			end;
			SecondBidNT:begin { 1 He, 2 Cl, 2 NT  13-15 p, 3 NT 19-21 p }
				if Level=Contract.Level then
					MaxP:=15
				else
				begin
					MinP:=19;
					MaxP:=21;
				end;
				for s:=TCLUB to TSPADE do
					Minl[s]:=2;
				if DistPoints(Player) > 1 then
					BidVal:=-100;
			end;
			Shutout:begin { Shut out Bid, 7 trumps, weak Hand }
				MaxP:=12;
				Minl[pcSuitType(Ord(Trump))]:=7;
				if l[pcSuitType(Ord(Trump))]-1 + Number(Player,ACEVAL)  >=6 + Level-2 then
					BidVal:=Level
				else
					 BidVal:=-300;
			end;
			OVERCALL:begin { Opponent OVERCALL, 8- ? p, 5 trumps }
				MinP:=8;
				Minl[pcSuitType(Ord(Trump))]:=5;
				if l[pcSuitType(Ord(Trump))] + p div 4  <  Level + 6 then
				BidVal:=-120;
			end;
			NORMAL_PASS:NormalPass;
			NORMAL_BID:NormalBid;
			NormDBL:NormalDBL;
			NormRDBL,ILLEGAL_BID:begin { Never redouble }
				BidVal:=-600;
				Test:=false;
			end;
		end { case };

		{ Add the new information to the old }
	    MinP:=Max(MinP,Info[Player].MinP);
	    MaxP:=Min(MaxP,Info[Player].MaxP);
	    MaxP:=Max(MaxP,MinP);
		for s:=TCLUB to TSPADE do Minl[s]:=Max(Minl[s],Info[Player].Minl[s]);
		case Task of
			CalcInfo:Info[Player]:=InfoPlayer; { Update Info[Player] }
			Evaluate:if Test then begin
				{ Adjust BidVal according to the Test }
				Val:=0;
				{ Test the Hand against Info }
				for s:=TCLUB to TSPADE do
	            if l[s] < Minl[s] then Val:=Val + (Minl[s]-l[s])*40;
				if p > MaxP then Val:=Val + (p-MaxP)*12;
				if p < MinP then Val:=Val + (MinP-p)*16;
				if Val > 0 then BidVal:=BidVal-100-Val;
				{ Try to give Partner as much information as possible }
				if Level>0 then begin
					if Trump=TTNT
						then BidVal:=BidVal + 11
						else begin
							BidVal:=BidVal + (l[pcSuitType(Ord(Trump))]-Info[Player].Minl[pcSuitType(Ord(Trump))])*2;
							if (Trump >=TrumpType(Ord(THEART))) and (l[pcSuitType(Ord(Trump))] >=5) then
							BidVal:=BidVal + 1;
							if Trump < TrumpType(Ord(TSPADE)) then
							for s:=pcSuitType(Ord(Succ(Trump))) to TSPADE do
								if l[s] >=5 then BidVal:=BidVal-1;
							Len:=8-Info[Partner].Minl[pcSuitType(Ord(Trump))];
							if (Info[Player].Minl[pcSuitType(Ord(Trump))] < Len) and (Minl[pcSuitType(Ord(Trump))] >=Len) then BidVal:=BidVal + 5;
						end;
					if (Level <=3) and not ((Level=3) and (Trump=TTNT)) then BidVal:=BidVal + 8;
				end;
				if Level > 5-Ord(Trump) div 2 then BidVal:=BidVal + 10;
	       	end;
	    end { case };
	end; { with }
	BidSystem:=BidVal;
end; { BidSystem }

function Threepass:BOOLEAN;

	{ Test if last three Bids were Pass }

	var

		i :integer;

	begin
		if BidNo < 4 then
			Threepass:=false
		else begin
			Threepass:=true;
			for i:=BidNo-3 to BidNo-1 do
				if not EqBid(theBids[i],PASS_BID) then
					Threepass:=false;
		end;
	end; { Threepass }

function NextHand;

	{ Returns the player to the 'left' of "Who" }

	begin
		if Who=West then
			NextHand:=North
		else
			NextHand:=Succ(Who);
	end;

{$ifdef UNUSED}

const
	LibFileExt='.BBH';

procedure ParseLibLine(const aLine:String;var aKeyWord:String;var aKeyValue:String);

	var
		i:integer;

	begin
		i:=Pos('=',aLine);
		aKeyWord:=Copy(aLine,1,i-1);
		AKeyValue:=Copy(aLine,i+1,sl_LibLine);
		UpStr(AKeyWord);
	end;

function DealLibLoad;

	{ Load the library hand identified by "aLibHand" from the library
		file "aLibFile" as the current hand to be played.

		"aLibHand" is the string that identifies the
		hand, case-insensitive. This string is enclosed in [] in the file.

		"aLibFile" can include a path and extension. If no extension
		is specified the default extension is used.

		Return 0 if the hand was loaded succesfully. }

	var
		LibFile:Text;
		sLibFile:String[sl_Path];
		sLibHand:String[sl_LibHandName];
		aDir:String[sl_FileDir];
		aName:String[sl_FileName];
		aExt:String[sl_FileExt];
		aLine:String[sl_LibLine];
		FoundHand:boolean;
		Current:integer;
		KeyWord:String[sl_LibKeyWord];
		KeyValue:String[sl_LibKeyValue];
		TheSuit:PCSuitType;
		ThePip:PCPipType;
		i,j:integer;
		Used:array[PCSuitType,PCPipType] of boolean;
		CIndex:array[North..West] of integer;
		Player:PlayerType;
		aBid:String[2];
		aCard:TCard;

	function AllHandsFull:boolean;

		begin
			AllHandsFull:=
				(CIndex[North]=13)
				and
				(CIndex[East]=13)
				and
				(CIndex[South]=13)
				and
				(CIndex[West]=13);
		end;

	begin
		FillChar(Used,SizeOf(Used),#0);
		FillChar(CIndex,SizeOf(CIndex),#0);
		FillChar(SimData,SizeOf(SimData),#0);
		sLibFile:={$ifdef WINDOWS} StrPas(aLibFile) {$else} aLibFile {$endif};
		sLibHand:={$ifdef WINDOWS} StrPas(aLibHand) {$else} aLibHand {$endif};
		UpStr(sLibHand);
		if not HasExtension(sLibFile) then sLibFile:=sLibFile+LibFileExt;
		Assign(LibFile,sLibFile);
		{$I-} Reset(LibFile); {$I+}
		if IOResult=0 then begin
			FoundHand:=False;
			DealLibLine:=1;
			Read(LibFile,aLine);
			while (not FoundHand) and (not eof(LibFile)) do begin
				UpStr(aLine);
				ReadLn(LibFile);
				Inc(DealLibLine);
				FoundHand:=(aLine='['+sLibHand+']');
				Read(LibFile,aLine);
			end;
			if FoundHand then begin
				Current:=-1; { don't have a hand yet }
				while (not eof(LibFile)) and (aLine[1]<>'[') do begin
					ReadLn(LibFile);
					Inc(DealLibLine);
					if aLine[1]='{' then begin
						UpStr(aLine);
						if aLine='{NORTH}' then
							Current:=North
						else if aLine='{EAST}' then
							Current:=East
						else if aLine='{SOUTH}' then
							Current:=South
						else if aLine='{WEST}' then
							Current:=West
						else begin
							Close(LibFile);
							DealLibLoad:=1;
							Exit;
						end;
					end
					else begin
						{ parse out the keyword }
						i:=Pos('=',aLine);
						if i>0 then begin
							ParseLibLine(aLine,KeyWord,KeyValue);
							i:=Pos(KeyWord,'CDHS');
							if (Current<>-1) and (i>0) then begin
								{ one of the suits }
								TheSuit:=PCSuitType(Ord(TCLUB)+i-1);
								UpStr(KeyValue);
								for i:=1 to Length(KeyValue) do begin
									j:=Pos(KeyValue[i],'A23456789TJQK');
									if (j>0) then begin
										ThePip:=PCPipType(Ord(TAce)+j-1);
										if (not Used[TheSuit,ThePip]) then begin
											Used[TheSuit,ThePip]:=True;
											if CIndex[Current]<13 then with SimInfo[Current,CIndex[Current]] do begin
												Suit:=TheSuit;
												Value:=PCPip2Value(ThePip);
												Inc(SimData[Current].P,HighCardPoints(Value));
												Inc(CIndex[Current]);
												Inc(SimData[Current].L[TheSuit]);
											end
											else begin
												{ too many cards for this player }
												Close(LibFile);
												DealLibLoad:=1;
												Exit;
											end;
										end
										else begin
											Close(LibFile);
											DealLibLoad:=1;
											Exit;
										end;
									end
									else if not (KeyValue[i] in [Space,Tab]) then begin
										Close(LibFile);
										DealLibLoad:=1;
										Exit;
									end;
								end;
							end
							else if KeyWord='DEALER' then begin
								UpStr(KeyValue);
								if KeyValue='NORTH' then
									Dealer:=North
								else if KeyValue='EAST' then
									Dealer:=East
								else if KeyValue='SOUTH' then
									Dealer:=South
								else if KeyValue='WEST' then
									Dealer:=West;
							end
							(*
							else if KeyWord='LEAD' then begin
								aCard:=Text2Card(KeyValue);
							end
							*)
							else if KeyWord='AUCTION' then begin
								{ parse out and assign the bids }
								UpStr(KeyValue);
								j:=Pos(',',KeyValue);
								if j=0 then j:=255;
								aBid:=Copy(KeyValue,1,j-1);
								while not (aBid='') do begin
									case abid[1] of
										'P':begin
											theBids[BidNo]:=PASS_BID;
											Inc(BidNo);
										end;
										'D':begin
											theBids[BidNo]:=Dbl;
											theDoubledStatus:=1;
											Inc(BidNo);
										end;
										'R':begin
											theBids[BidNo]:=RDbl;
											theDoubledStatus:=2;
											Inc(BidNo);
										end;
										'1'..'7':begin
											theBids[BidNo].Level:=Ord(aBid[1])-Ord('0');
											case aBid[2] of
												'C','D','H','S','N':begin
													case aBid[2] of
														'C':theBids[BidNo].Trump:=TTCLUB;
														'D':theBids[BidNo].Trump:=TTDIAMOND;
														'H':theBids[BidNo].Trump:=TTHEART;
														'S':theBids[BidNo].Trump:=TTSPADE;
														'N':theBids[BidNo].Trump:=TTNT;
													end;
													Contract:=theBids[BidNo];
													Dummy:=((Dealer+BidNo) and 3);
												end;
												else begin
													BidNo:=0;
													Close(LibFile);
													DealLibLoad:=1;
													Exit;
												end;
											end;
											Inc(BidNo);
										end;
										else begin
											BidNo:=0;
											Close(LibFile);
											DealLibLoad:=1;
											Exit;
										end;
									end;
									Delete(KeyValue,1,j);
									j:=Pos(',',KeyValue);
									if j=0 then j:=255;
									aBid:=Copy(KeyValue,1,j-1);
								end;
							end;
						end;
					end;
					Read(LibFile,aLine);
				end;
				if AllHandsFull then begin
					SortCards;
					for Player:=North to West do with SimData[Player] do P:=P+DistPoints(Player);
					RelInfo:=SimInfo;
					RelData:=SimData;
					DealLibLoad:=0;
				end
				else
					{ missing some cards }
					DealLibLoad:=1;
			end
			else
				DealLibLoad:=1;
			Close(LibFile);
		end
		else
			DealLibLoad:=1;
	end;

{$endif UNUSED}

procedure RotateHands;

	{ rotate the current hands so that N/S will have the higher
		point total and most likely get the contract }

	procedure Exchange(a,b:PlayerType);

		var
			Tmp:array[0..12] of cardInfo;
			TmpData:DataRecord;

		begin
			Move(RelInfo[a],Tmp,SizeOf(Tmp));
			Move(RelInfo[b],RelInfo[a],SizeOf(Tmp));
			Move(Tmp,RelInfo[b],SizeOf(Tmp));
			TmpData:=RelData[a];
			RelData[a]:=RelData[b];
			RelData[b]:=TmpData;
		end;

	begin
		with BridgeState do if (DealFlag=DealRandom) and (Mode=TutorialMode) and MakeNSDeclarer then
			if RelData[North].P+RelData[South].P<RelData[East].P+RelData[West].P then begin
				Exchange(North,West);
				Exchange(East,South);
				SimData:=RelData;
				SimInfo:=RelInfo;
			end;
	end;

function Practicing:boolean;

	begin
		Practicing:=(BridgeState.Mode=TutorialMode);
	end;

function findHigh(Hand:PlayerType;   s:pcSuitType):CardNoType;

{ Find Highest Card }

var
	High:CardNoType;
begin
	High:=0;
	while (SimInfo[Hand,High].Suit <> s) or
					 SimInfo[Hand,High].Played do
		High:=High+1;
	findHigh:=High;
end; { findHigh }

function findLow(Hand:PlayerType;   s:pcSuitType):CardNoType;
{ Find Lowest Card }
var
	Low:CardNoType;
begin
	Low:=12;
	while (SimInfo[Hand, Low].Suit <> s) or
				 SimInfo[Hand, Low].Played do
		Low:=Low-1;
	findLow:=Low;
end; { findLow }

function Highest(hand4:integer;   s:pcSuitType):ExpandValueType;
{ Return Value of Highest Card }
var
	Hand:PlayerType;
begin
	Hand:=hand4 and 3;
	if SimData[Hand].l[s]=0 then
		Highest:=-1
	else
		Highest:=SimInfo[Hand,findHigh(Hand,s)].Value;
end; { Highest }

function Lowest(hand4:integer;   s:pcSuitType):ExpandValueType;

{ Return Value of Lowest Card }

var
	Hand :PlayerType;

begin
	Hand:=hand4 and 3;
	if SimData[Hand].l[s]=0 then
		Lowest:=ACEVAL + 1
	else
		Lowest:=SimInfo[Hand,findLow(Hand,s)].Value;
end; { Lowest }

function Highmax(hand4:integer;   s:pcSuitType;
								 MaxVal:ExpandValueType):ExpandValueType;

{ Return Value of the Highest Card, which is
	lower or equal to MaxVal }

var

	Hand:PlayerType;
	No:CardNoType;

begin

	Hand:=hand4 and 3;
	Highmax:=-1;
	for No:=0 to 12 do
		if not SimInfo[Hand,No].Played then
			if (SimInfo[Hand,No].Suit=s) then
				if (SimInfo[Hand,No].Value<=MaxVal) then
				begin
					Highmax:=SimInfo[Hand,No].Value;
					Exit;
				end;

end; { Highmax }

procedure TestSuit(PlayingHand:PlayerType;   s:pcSuitType;
									 var TopTricks:integer;
									 var winning:boolean);

{ TopTricks is the Number of TopTricks in the Suit for PlayingHand.
	winning is true if All cards in the Suit are winners }

var
	MaxVal      :ExpandValueType;{ Highest Card }
	HighHand    :PlayerType; { Hand with Highest Card }
	OpponentVal :ExpandValueType;{ Opponents Highest Card }
	Highval     :array[PlayerType] of{ Table of Highest cards }
									ExpandValueType;
	h           :PlayerType;
	i           :CardNoType;
	Len         :LengthType;

begin

	TopTricks:=0;
	winning:=false;
	MaxVal:=-1;
	for h:=North to West do
	begin
		Highval[h]:=Highest(h,s); { Find Highest Card }
		if MaxVal <  Highval[h] then
		begin
			MaxVal:=Highval[h];
			HighHand:=h;
		end;
	end;

	if MaxVal>=0 then
		if not odd(HighHand+PlayingHand) then begin

			{ Find opponents Highest Card }

			OpponentVal:=Max(Highval[(HighHand+1) and 3],
											 Highval[(HighHand-1) and 3]);
			for i:=0 to 12 do with SimInfo[HighHand,i] do { Count TopTricks }
				if not Played and (Suit=s) and (Value > OpponentVal) then
					TopTricks:=TopTricks+1;
			for i:=0 to 12 do
				with SimInfo[(HighHand+2) and 3,i] do
				 if not Played and (Suit=s) and (Value > OpponentVal) then
					 TopTricks:=TopTricks+1;
			Len:=Max(SimData[HighHand].l[s], SimData[(HighHand+2) and 3].l[s]);
			TopTricks:=Min(TopTricks,Len);
			{ The Suit is winning if PlayingHand can take All Tricks from Top }
			winning:=TopTricks >=Max(SimData[(HighHand+1) and 3].l[s],
															SimData[(HighHand-1) and 3].l[s]);
			if winning then TopTricks:=Len;
		end;
end; { TestSuit }

function SelectLead:ExpandCardNoType;

{ Selects a lead in the position.
	The function will return a new suggestion for a lead
	every time it is called (-1 means No more suggestions).
	The function uses the record Count[TrickNo]
	(suggestion Number Cnt is in the Suit Suits[Cnt])
	All calculations are performed in the Sim record }

function SelectSuit(Number:integer):pcSuitType;
{ Selects the Suit in which the lead should be Played.
	Number is the suggestion Number.
	The Suits and the evaluations are placed in Count[TrickNo].
	All calculations are performed in the Sim records }

procedure SortSuits;
{ Sort Suits in Count }
var
	t1,t2 :1..4;
  Suit  :pcSuitType;
begin
  with Count[TrickNo] do
  for t1:=1 to 3 do for t2:=t1+1 to 4 do
    if Evaluation[Suits[t1]] < Evaluation[Suits[t2]] then
    begin
      Suit:=Suits[t2];
			Suits[t2]:=Suits[t1];
      Suits[t1]:=Suit;
    end;
end; { SortSuits }

function DeclarerLeadEvalu(s:pcSuitType):integer;
{ Evaluate the Suit as lead from declarer or Dummy }
var
  Evalu    :integer;
  Short    :PlayerType;
  ShortLen :LengthType;
  Val      :integer;
begin

  with Sim,SimData[PlayingHand],Contract do begin
    Evalu:=0;
    if s=pcSuitType(Ord(Trump)) then
		begin

			{ Draw Trump until the opponents have No more }

			if (SimData[(PlayingHand+1) and 3].l[pcSuitType(Ord(Trump))] > 0) or
         (SimData[(PlayingHand-1) and 3].l[pcSuitType(Ord(Trump))] > 0) then
         Evalu:=200;
    end
    else
		if Trump <> TTNT then
			begin

				{ Try to establish a Ruff trick }

		  Short:=(PlayingHand + 2) and 3;
		  if SimData[Short].l[s] > l[s] then
			 Short:=PlayingHand;
		  ShortLen:=SimData[Short].l[s];
		  Val:=SimData[(Short+2) and 3].l[s]-2*ShortLen;
		  if (Val > 0) and (SimData[Short].l[pcSuitType(Ord(Trump))] > 0) then
				begin
			 with SimData[(Short+1) and 3] do
				if (l[s] <=ShortLen) and (l[pcSuitType(Ord(Trump))] > 0) then
				  Val:=0;
			 with SimData[(Short-1) and 3] do
				if (l[s] <=ShortLen) and (l[pcSuitType(Ord(Trump))] > 0) then
				  Val:=0;
				if Val > 0 then
							Evalu:=200+Val;
		  end;
		end;
		DeclarerLeadEvalu:=Evalu;
  end;
end; { DeclarerLeadEvalu }

function OpponentLeadEvalu(s:pcSuitType):integer;

{ Evaluate the Suit as lead from Opponent to declarer }

var
  Evalu     :integer;
  RuffRisk  :boolean;
  TopTricks :integer;
  winning   :boolean;
begin
  with Sim,SimData[PlayingHand],Contract do
  begin
    Evalu:=0;
		RuffRisk:=false;
    if (Trump <> TTNT) and (s <> pcSuitType(Ord(Trump))) then
		begin

			{ Check if declarer or Dummy can Ruff Suit }

      with SimData[(PlayingHand+1) and 3] do
        if (l[s]=0) and (l[pcSuitType(Ord(Trump))] > 0) then
          RuffRisk:=true;
      with SimData[(PlayingHand-1) and 3] do
        if (l[s]=0) and (l[pcSuitType(Ord(Trump))] > 0) then
          RuffRisk:=true;
    end;
    if not RuffRisk then begin
			TestSuit(PlayingHand,s,TopTricks,winning);
      if TopTricks > 0 then
      begin
        Evalu:=200+TopTricks;{ Take TopTricks }
        if winning then
          Evalu:=Evalu+4;
      end
      else
        if Trump <> TTNT then begin

					{ Try to establish a Ruff trick }

          with SimData[PlayingHand] do
            if (l[s] <=1) and (l[pcSuitType(Ord(Trump))] > 0) then
              Evalu:=200;
          with SimData[(PlayingHand+2) and 3] do
            if (l[s] <=1) and (l[pcSuitType(Ord(Trump))] > 0) then
              Evalu:=200;
				end;
    end;
    OpponentLeadEvalu:=Evalu;
  end; { with }
end; { OpponentLeadEvalu }

function SuitEvalu(s:pcSuitType):integer;

{ Evaluate the strength of the Suit }

var
  Evalu     :integer;
  TopTricks :integer;
	winning   :boolean;
  m,n       :integer;
begin
  with Sim,SimData[PlayingHand],Contract do
  begin
    if s=pcSuitType(Ord(Trump)) then
      Evalu:=100 { Play Trump }
    else
    begin
		m:=SimData[North].l[s]+SimData[South].l[s]; { Play best Suit }
      n:=SimData[East ].l[s]+SimData[West ].l[s];
      if not odd(PlayingHand) then
        Evalu:=96+m-2*n
      else
        Evalu:=96+n-2*m;
      if not odd(PlayingHand+Dummy) then
      begin
				Evalu:=Evalu+24;
        TestSuit(PlayingHand,s,TopTricks,winning);
        if TopTricks > 0 then
        begin
          Evalu:=128+TopTricks;{ Take TopTricks }
          if winning then
            Evalu:=Evalu+4;
        end;
			end;
    end;
    SuitEvalu:=Evalu;
  end;
end; { SuitEvalu }

var
  Lastlead :TrumpType;{ The Suit which was  last lead by the side }
  LastNo   :integer;{ The trick Number of last lead by the side }
  s        :pcSuitType;
  Evalu    :integer;
  i        :integer;

begin

  with Sim,SimData[PlayingHand],Count[TrickNo] do
  begin
		{ When the procedure is called for the first suggestion,
      All Suits are evaluated, sorted and stored in the record
      Count[TrickNo]. Suggestion Number Number is then simply
			the Suit Suits[Number] }
    if Number=1 then
		begin
			{ Find Suit of last lead by this side }
      Lastlead:=TTNT;
      for i:=FirstNo to TrickNo-1 do with Game[i*4] do
		  if not odd(PlayingHand+Hand) then
        begin
          Lastlead:=TrumpType(Ord(SimInfo[Hand,No].Suit));
          LastNo:=i;
        end;
		for s:=TCLUB to TSPADE do
      begin
        if l[s]=0 then{ Evaluate each Suit }
          Evalu:=-1000
		  else
        begin
          if not odd(PlayingHand+Dummy) then{ Most important rules }
            Evalu:=DeclarerLeadEvalu(s)
          else
            Evalu:=OpponentLeadEvalu(s);
          if s=pcSuitType(Ord(Lastlead)) then{ Continue playing same Suit as in last lead }
            Evalu:=Max(Evalu+40,Count[LastNo].Evaluation[pcSuitType(Ord(Lastlead))]);
          if Evalu=0 then { Second most important rules }
            Evalu:=SuitEvalu(s);
				end;
        Evaluation[s]:=Evalu;
        Suits[Ord(s)+1]:=s;
      end;
      SortSuits;{ Sort Suits }
    end;
    SelectSuit:=Suits[Number];{ Return suggested Suit }
	end { with };

end; { SelectSuit }

function SuitTreatment(CardSuit:pcSuitType):CardNoType;

  { Decides which Card in CardSuit should be Played }

  var

    Low        :boolean;{ program plays Lowest Card in CardSuit }
    Highval,{ Players  Highest Card }
    PartnerVal :ExpandValueType; { Partners Highest Card }

	begin

    with Sim,SimData[PlayingHand] do
    begin
      Low:=true;{ Play Low normally }
      if l[CardSuit] > 1 then
      begin { Play High if both opponents have singletons }
        if (SimData[(PlayingHand+1) and 3].l[CardSuit] <=1) and
			  (SimData[(PlayingHand-1) and 3].l[CardSuit] <=1) then
          Low:=false
        else
		  begin
          Highval:=Highest(PlayingHand  ,CardSuit);
          PartnerVal:=Highest(PlayingHand+2,CardSuit);
          if PartnerVal < Highval then
          begin
						{ Play High if Partner cannot take trick }
            if (Highmax(PlayingHand+1,CardSuit,Highval) > PartnerVal) or
               (Highmax(PlayingHand-1,CardSuit,Highval) > PartnerVal) then
              Low:=false;
			 end
          else
					  { Play High to make a finesse }
            if (SimData[(PlayingHand+2) and 3].l[CardSuit] >=2) and
							 (SimData[(PlayingHand+1) and 3].l[CardSuit] >=3) then
              if Highest(PlayingHand+1,CardSuit)=PartnerVal-1 then
                if Highest(PlayingHand-1,CardSuit) > Highval then
                  Low:=false;
          if not Low then
          begin
					  { Play Low if the Opponent has Highest Card singleton }
						if SimData[(PlayingHand+1) and 3].l[CardSuit]=1 then
              if Highest(PlayingHand+1,CardSuit) > Highval then
                Low:=true;
            if SimData[(PlayingHand-1) and 3].l[CardSuit]=1 then
              if Highest(PlayingHand-1,CardSuit) > Highval then
                Low:=true;
          end;{ if }
        end; { else }
      end; { if }
		if Low then
				SuitTreatment:=findLow(PlayingHand,CardSuit)
      else
        SuitTreatment:=findHigh(PlayingHand,CardSuit);
		end { with };
	end; { SuitTreatment }

var
	CardSuit:pcSuitType;

begin { SelectLead }
  with Count[TrickNo] do
{ Decide if any more suggestions should be made, and
    calculate eventually next suggestion }
  if Cnt >=2 then
    SelectLead:=-1
  else
  begin
    Cnt:=Cnt+1;
	 CardSuit:=SelectSuit(Cnt);
    if (Cnt > 1) and (Evaluation[Suits[1]]-
        Evaluation[CardSuit] >=BranchValue) then
		SelectLead:=-1
		else
			SelectLead:=SuitTreatment(CardSuit);
  end;
end; { SelectLead }

function TopTrick(PlayingHand:PlayerType;   s:pcSuitType):integer;

{ Returns the Number of TopTricks for the side.
	A negative Number means that the opponents has TopTricks.
	The Number is calculated by using TestSuit }

var

	Top     :integer;
	winning :boolean;

begin

	TestSuit(PlayingHand,s,Top,winning);
	if Top=0 then
	begin
		TestSuit((PlayingHand+1) and 3,s,Top,winning);
		Top:=-Top;
	end;
	TopTrick:=Top;

end; { TopTrick }

function DiscardCard:CardNoType;

{ Find the Card with Lowest Value }

var

	CardSuit   :pcSuitType;{ Chosen Suit }
	Minval, { Minimal Card Value }
  Evalu      :integer; { Card Value }
	Hold       :boolean; { Hold opponents Suit }
  Highval,
  LowVal,
	OpponentVal,
  Val        :ExpandValueType;
  s          :pcSuitType;
  Hand       :PlayerType;
  top0,top1  :integer;
	OwnVal,
  OppVal,
  v          :integer;

function Trickval(Top:integer):integer;

{ Calculate Value of TopTricks.
	OwnVal is Value of a Player trick.
	OppVal is Value of an Opponent trick }

begin

	if Top >=0 then
     Trickval:=Top*OwnVal
   else
		 Trickval:=Top*OppVal;

end; { Trickval }

begin
	with Sim,SimData[PlayingHand],Contract do
	begin
    Minval:=MaxInt;
		for s:=TCLUB to TSPADE do
      if l[s] > 0 then
      begin
        if s=pcSuitType(Ord(Trump)) then { Evaluate the importance of each of the Suits }
          Evalu:=500{ do not discard Trump }
				else
        begin
          LowVal:=Lowest(PlayingHand,s);{ do not discard High Card }
          if LowVal > 10 then
				Evalu:=8+LowVal
          else
          begin

						{ Penalty for leaving High Card unprotected. Otherwise Hold the longest Suit }

            Highval:=Highest(PlayingHand,s);
				if (Highval > 10) and (Highval <> ACEVAL) and
					(l[s] <=ACEVAL+1-Highval) then
              Evalu:=Highval
            else
              Evalu:=l[s];
            if odd(PlayingHand+Dummy) then { if Player have No more trumps,
                                             then discard longest Suit }
						if Trump<>TTNT then
              if l[pcSuitType(Ord(Trump))]=0 then
								Evalu:=-l[s];
					end;
					Hold:=false;{ Hold opponents Suit unless the Player has
									 trumps or Partner leads next trick }
          if odd(PlayingHand+Dummy) or (Trump=TTNT) then
          begin
				Hold:=odd(PlayingHand+BestHand);
            with BestCard do
              if Suit=LeadSuit then
                Val:=Value
              else
					 Val:=ACEVAL+1;
            Hand:=(PlayingHand+1) and 3;
						while Hand <> LeadHand do
            begin
				  OpponentVal:=Highest(Hand,LeadSuit);
              if (OpponentVal < 0) and (Trump <> TTNT) then
								if SimData[Hand].l[pcSuitType(Ord(Trump))] > 0 then
						OpponentVal:=ACEVAL+1;
              if Val <=OpponentVal then
              begin
								Val:=OpponentVal;
                Hold:=odd(PlayingHand+Hand);
              end;
              Hand:=(Hand+1) and 3;
						end;
          end;
					top1:=TopTrick(PlayingHand,s);
					{ TOP1 is the present Number of TopTricks for side.
						TOP0 is the Number of TopTricks if PlayingHand
            discards a Card in the Suit }
          with SimInfo[PlayingHand,findHigh(PlayingHand,s)],
							 SimData[PlayingHand] do
          begin
            Played:=true;
				l[s]:=l[s]-1;
            top0:=TopTrick(PlayingHand,s);
            Played:=false;   l[s]:=l[s]+1;
          end;
          OwnVal:=40; { Evaluate the importance of the lost TopTricks }
          OppVal:=20;
          if (Trump <> TTNT) and
             ((top1 > 0) =odd(PlayingHand+Dummy)) then
            if (abs(top1) > SimData[(Dummy+1) and 3].l[s]) or
							 (abs(top1) > SimData[(Dummy-1) and 3].l[s]) then
							OppVal:=8;
          if Hold then
          begin
            v:=OppVal;
            OppVal:=OwnVal;
						OwnVal:=v;
          end;
          Evalu:=Evalu+(Trickval(top1)-Trickval(top0));
        end;
		  if Evalu < Minval then
        begin
          Minval:=Evalu;
					CardSuit:=s;
      end;
    end { for Suit };
    DiscardCard:=findLow(PlayingHand,CardSuit);
		{ Discard Lowest Card in the chosen Suit }
	end { with };
end { DiscardCard };

function SelectCard:CardNoType;

	{ Selects a Card in the position.
		The procedure does not handle leads in the search
		(this is Done by SelectLead).
		The procedure can only be called once, and gives thus
		only one suggestion (in contrary to SelectLead).
		All calculations are performed in the Sim records }

	function FollowSuit:CardNoType;

		{ Follow Suit and play a Card in the lead Suit }

		var

			Low         :boolean;{ program plays Lowest Card in CardSuit }
      findLower   :boolean;{ program plays Lowest Card,
                                  which is higher than OpponentVal }
      OpponentVal,{ Opponents Highest Card }
      Highval,{ Players Highest Card }
      PartnerVal  :ExpandValueType; { Partners Highest Card }
			No          :CardNoType;

		begin

			with Sim,SimData[PlayingHand] do
      begin
        Low:=true;
        if (BestCard.Suit=LeadSuit) and (l[LeadSuit] > 1) then
        begin
					Highval:=Highest(PlayingHand,LeadSuit);
          if BestCard.Value <=Highval then

  				  { It is possible to play the Highest Card }

            case Round and 3+1 of
							2 :begin { 2nd Hand }
						    { Play Low  if Partner can take trick, else
                      play High if it will get the trick, else
							 play High if it will press the Opponent, else
                      play Low  in 2nd Hand }
                    OpponentVal:=Max(Highest(PlayingHand+1,LeadSuit),
																			 BestCard.Value);
                    PartnerVal:=Highest(PlayingHand+2,LeadSuit);
                    Low:=(PartnerVal > OpponentVal)
                           or (OpponentVal > Highval)
                             and ((PartnerVal > BestCard.Value) or
																 (SimData[(PlayingHand+1) and 3].l[LeadSuit]=1));
										findLower:=(Round <=Rel.Round+2)
                                 and (Highval > OpponentVal);
									end;
				  3:begin { 3rd Hand }
							  { Play Low  if Partner is sure to win trick }
                   OpponentVal:=Highest(PlayingHand+1,LeadSuit);
									 Low:=not odd(BestHand+PlayingHand) and
															(OpponentVal < BestCard.Value);
                   if not Low then
									 begin
                     findLower:=true;{ Play High in 3rd Hand }
										 OpponentVal:=Highmax(PlayingHand+1,LeadSuit,Highval);
                     if OpponentVal < BestCard.Value then
                       if odd(BestHand+PlayingHand) then
                         OpponentVal:=BestCard.Value
                       else
                         Low:=true;
									 end;
                 end;
              4:begin { 4th Hand }
		 { Get trick unless Partner has it }
                   if odd(BestHand+PlayingHand) then
									 begin
										 Low:=false;
                     findLower:=true;
                     OpponentVal:=BestCard.Value;
                   end;
                 end;
            end { case };
        end; { if }
        if Low then
					FollowSuit:=findLow(PlayingHand, LeadSuit)
				else
        begin
          FollowSuit:=findHigh(PlayingHand, LeadSuit);
					if findLower then
            for No:=0 to 12 do
							with SimInfo[PlayingHand,No] do
                if not Played and
                  (Suit=LeadSuit) and (Value > OpponentVal) then
                  FollowSuit:=No;
        end;
		end { with };

    end;

	function SelectRuff:ExpandCardNoType;

  	{ Check whether and with which trumph the trick should be ruffed }

	var

		Ruff        :boolean;{ Ruff the trick }
    Low         :boolean;{ program plays Lowest Card in CardSuit }
	 findLower   :boolean;{ program plays Lowest Card,
															which is higher than OpponentVal }
    OpponentVal :ExpandValueType;{ Opponents Highest Trump }
    No          :CardNoType;

  begin

    with Sim,SimData[PlayingHand],Contract do
    begin
      if Trump=TTNT then
				Ruff:=false
			else
        Ruff:=(SimData[PlayingHand].l[pcSuitType(Ord(Trump))] > 0)
								and ((BestCard.Suit <> pcSuitType(Ord(Trump))) or
										 (Highest(PlayingHand,pcSuitType(Ord(Trump))) > BestCard.Value));
        if Ruff then
        begin
  { Ruff if it gets the trick }
          case Round and 3+1 of
            2 :Ruff:=Highest(PlayingHand+2,LeadSuit)  <=
							  Max(Highest(PlayingHand+1,LeadSuit),BestCard.Value);
						3 :Ruff:=odd(PlayingHand+BestHand) or
                        (BestCard.Suit=LeadSuit) and
                        (Highest(PlayingHand+1,LeadSuit) > BestCard.Value);
						4 :Ruff:=odd(PlayingHand+BestHand);
          end { case };
          if Ruff then
					begin
            Low:=true;
            with BestCard do
              if Suit=pcSuitType(Ord(Trump)) then
                if Highest(PlayingHand,pcSuitType(Ord(Trump))) > Value then
								begin
                  Low:=false;
                  findLower:=true;
									OpponentVal:=Value;
								end
								else
                  Ruff:=false;
					end;
        end;
        if not Ruff then
			 SelectRuff:=-1
				else
          if Low then
            SelectRuff:=findLow(PlayingHand,pcSuitType(Ord(Trump)))
          else
					begin
            SelectRuff:=findHigh(PlayingHand,pcSuitType(Ord(Trump)));
						if findLower then
							for No:=0 to 12 do
					 with SimInfo[PlayingHand,No] do
                  if not Played and (Suit=pcSuitType(Ord(Trump)))
                     and (Value > OpponentVal) then
										SelectRuff:=No;
					end;
		end { with };
	end { SelecTrump };

	var
		CardNo:ExpandCardNoType;

	begin { SelectCard }
		with Sim,SimData[PlayingHand] do
			if Round and 3=0 then begin
				{ Find lead by calling SelectLead (must not be used in the search) }
				Count[TrickNo].Cnt:=0;
				FirstNo:=TrickNo;
				SelectCard:=SelectLead;
			end
			else begin
				if l[LeadSuit]>0 then
					{ Follow Suit and play a Card in the lead Suit }
					SelectCard:=FollowSuit
				else begin
					CardNo:=SelectRuff; { Void Suit, Ruff the trick or Discard a Card }
					if CardNo>=0 then
						SelectCard:=CardNo
					else
						SelectCard:=DiscardCard;
				end;
			end;
	end;

procedure ResetTrick;

{ Reestablishes the situation in Sim, SimInfo, SimData and TrickNo
	one trick earlier (inverts 4 calls of PlayCard) }

var

	Cnt:0..3;

begin

  with Sim do
  begin
		if not odd(LeadHand+Dummy) then
			WonTricks:=WonTricks-1;
    for Cnt:=3 downto 0 do
    begin
      Round:=Round-1;
		with Game[Round],SimInfo[Hand,No] do
      begin
        Played:=false;
				with SimData[Hand] do l[Suit]:=l[Suit]+1;
			end;
		end;
    with Game[Round] do
    begin
			LeadHand:=Hand;
			PlayingHand:=LeadHand;
			LeadSuit:=SimInfo[Hand,No].Suit;
		end;
		TrickNo:=TrickNo-1;
  end;

end; { ResetTrick }

procedure PlayCard(Hand:PlayerType;   No:CardNoType);

{ Plays the Card and updates Game, SimInfo, SimData, Sim and TrickNo }

begin

	with SimInfo[Hand,No],Sim do
	begin
		Played:=true;
		with SimData[Hand] do
			l[Suit]:=l[Suit]-1;
		Game[Round].Hand:=Hand;
		Game[Round].No:=No;
		if Round and 3=0 then
		begin
			LeadSuit:=Suit;{ Lead }
			BestCard:=SimInfo[Hand,No];
			BestHand:=Hand;
		end
		else
			if (Suit=BestCard.Suit) and (Value > BestCard.Value) or
				 (Suit <> BestCard.Suit) and (Suit=pcSuitType(Ord(Contract.Trump))) then
			begin
				BestCard:=SimInfo[Hand,No];{ Best Card }
				BestHand:=Hand;
			end;
			Round:=Round+1;
			if Round and 3=0 then
			begin
				if not odd(BestHand+Dummy) then
					WonTricks:=WonTricks+1;
				LeadHand:=BestHand;
				TrickNo:=TrickNo+1;
			end;
			PlayingHand:=(LeadHand+Round) and 3;
	end; { with }
end; { PlayCard }

{$GOTO+}

procedure Analyse(var Result:integer);

	{ Performs the analysis of the given position (in the Sim records).
		When the analysis is finished the Number of won Tricks
		for the declarer is placed in Result }

	var

		MaxVal:array[-1..13] of integer; { Alfa-Beta values }
		CardNo:ExpandCardNoType; { Counters }
		Back:-1..13;
		i:IndexType;
		h:1..3;

	label Call,Loop;

	begin

		with Sim do
		begin
			if Round and 3 <> 0 then{ Finish this trick }
				for h:=Round and 3 to 3 do
					PlayCard(PlayingHand,SelectCard);
			FirstNo:=TrickNo;
			Call: { Initialize MaxVal[TrickNo] }
			if odd(PlayingHand+Dummy) then
				MaxVal[TrickNo]:=-WonTricks+TrickNo-14
			else
        MaxVal[TrickNo]:=WonTricks+1;
      if TrickNo=FirstNo then
        MaxVal[FirstNo-1]:=TrickNo-13-MaxVal[FirstNo]
      else
      begin
        Back:=TrickNo;
				repeat
			 Back:=Back-1
				until (MaxVal[TrickNo] > 0) =(MaxVal[Back] > 0);
        if MaxVal[TrickNo] <  MaxVal[Back] then
			 MaxVal[TrickNo]:=MaxVal[Back];
      end;
			Count[TrickNo].Cnt:=0;{ Initiate SelectLead }
		Loop:{ Test cut-off }
      Back:=TrickNo;
			repeat
        Back:=Back-1
			until (MaxVal[TrickNo] > 0) <> (MaxVal[Back] > 0);
      if MaxVal[TrickNo]+MaxVal[Back] >=0 then
		begin
        Back:=Back+1; { Take Back All Tricks lead by the Player }
        MaxVal[Back]:=MaxVal[TrickNo];
				while TrickNo > Back do ResetTrick;
			end
			else
			begin
				CardNo:=SelectLead; { Find next lead }
        if CardNo >=0 then
				begin
					PlayCard(PlayingHand,CardNo);{ Play the whole trick }
          for h:=1 to 3 do
						PlayCard(PlayingHand,SelectCard);
          goto Call;
        end;
      end;

			if TrickNo > FirstNo then
      begin
        ResetTrick; { Backup MaxVal-Value }
		  if (MaxVal[TrickNo] > 0) =(MaxVal[TrickNo+1] > 0) then
          MaxVal[TrickNo]:=MaxVal[TrickNo+1]
        else
          MaxVal[TrickNo]:=-MaxVal[TrickNo+1];
        goto Loop;
      end;
		end; { with }
		Result:=abs(MaxVal[TrickNo])-1; { Save Result and reestablish situation }
		for i:=Sim.Round-1 downto Rel.Round do
			with Game[i],SimInfo[Hand,No],SimData[Hand] do
			begin
				Played:=false;
				l[Suit]:=l[Suit]+1;
			end;
		Sim:=Rel;
		TrickNo:=Rel.Round div 4;

	end; { Analyse }

{$GOTO-}

procedure ReadPrepare(var Command:CommandString);

	{ Sets up for reading an option }

	begin
		Command:='';
	end; { ReadPrepare }

function ShouldRotateHands;

	begin
		ShouldRotateHands:=BridgeState.MakeNSDeclarer;
	end;

{$ifdef UNUSED}

var
	TmpLibFile:Text;
	aLine:string[sl_LibLine];

function LibraryDealMode;
begin
	LibraryDealMode:=(Practicing and (BridgeState.DealFlag=DealLib));
end;

procedure SeekNextHand(var TmpLibFile:Text;var aLine:String;var aDealName:TString);

	{ Find the next hand description in "TmpLibFile" ("[xxx]") if any, store the
		name of it in "aDealName" and seek to the start of the line after it. }

	begin
		EmptyStr(aDealName);
		while (not (aLine[1]='[')) and (not eof(TmpLibFile)) do ReadLn(TmpLibFile,aLine);
		if (aLine[1]='[') then begin
			{$ifdef WINDOWS}
			StrPCopy(aDealName,
			{$else}
			aDealName:=
			{$endif} Copy(aLine,2,Pos(']',aLine)-2) {$ifdef WINDOWS} ) {$endif};
			ReadLn(TmpLibFile,aLine);
		end;
	end;

procedure FindComment(var TmpLibFile:Text;var aLine:String;var aDealComment:TString);

	var
		i:integer;
		KeyWord:String[sl_LibKeyWord];
		KeyValue:String[sl_LibKeyValue];

	begin
		EmptyStr(aDealComment);
		while (not eof(TmpLibFile)) and (aLine[1]<>'[') do begin
			ReadLn(TmpLibFile);
			{ parse out the keyword }
			i:=Pos('=',aLine);
			if i>0 then begin
				ParseLibLine(aLine,Keyword,KeyValue);
				if KeyWord='COMMENT' then begin
					{$ifdef WINDOWS}
					StrPCopy(
					{$endif}
					aDealComment
					{$ifdef WINDOWS} , {$else} := {$endif} KeyValue {$ifdef WINDOWS} ) {$endif};
					Exit;
				end;
			end;
			Read(TmpLibFile,aLine);
		end;
	end;

procedure DealLibFirst;

	{ Read the first deal header from "aLibFile" into "aDealName" and
		"aDealComment". This procedure opens the file for reading and
		leaves it open on exit to be closed by the last call to "DealLibNext".

		Set "aDealName" to '' if there are no more deals (eof). }

	var
		LibFilePath:{$ifdef WINDOWS} array[0..sl_Path] of Char {$else} PathStr {$endif};

	begin
		{$ifdef WINDOWS}
		StrCopy(LibFilePath,aLibFile);
		{$else}
		LibFilePath:=aLibFile;
		{$endif}
		if not HasExtension(LibFilePath) then
			{$ifdef WINDOWS}
			StrCat(LibFilePath,LibFileExt);
			{$else}
			LibFilePath:=LibFilePath+LibFileExt;
			{$endif}
		Assign(TmpLibFile,LibFilePath);
		{$I-} Reset(TmpLibFile); {$I+}
		if IOResult=0 then begin
			ReadLn(TmpLibFile,aLine);
			SeekNextHand(TmpLibFile,aLine,aDealName);
			if not IsEmptyString({$ifndef WINDOWS}@{$endif}aDealname) then FindComment(TmpLibFile,aLine,aDealComment);
		end
		else
			EmptyStr(aDealName);
	end;

procedure DealLibNext;

	{ read the next deal header from "TmpLibFile" into "aDealName" and
		"aDealComment". "DealLibFirst" must be called before a call to
		this procedure.

		See DealLibFirst. }

	begin
		SeekNextHand(TmpLibFile,aLine,aDealName);
		if not IsEmptyString({$ifndef WINDOWS}@{$endif}aDealname) then
			FindComment(TmpLibFile,aLine,aDealComment)
		else
			Close(TmpLibFile);
	end;

{$endif UNUSED}

procedure SetDealMode;

	begin
		BridgeState.DealFlag:=aMode;
	end;

function RandomDealMode;

	begin
		RandomDealMode:=(BridgeState.DealFlag=DealRandom);
	end;

function CardType2TCard(Crd:cardInfo):TCard;

	function CardType2TPip(Val:ValueType):TPip;

		begin
			if Val=ACEVAL then
				CardType2TPip:= TAce
			else
				CardType2TPip:= Val - 1
		end;

	begin
		CardType2TCard:=MakeCard(CardType2TPip(Crd.Value), Crd.Suit);
	end;

function SuitString(a_aSuit:trumptype):string;

begin
	SuitString:= x_aSuitString[a_aSuit];
end;

function EndGameMsg(wonRubber,wonGame:boolean):string;

var
	s:string;
	sz:StrToTextBuffer;
	OverTricks:integer;

begin
	s:= StrPas(ShortTeamName(Dummy, @sz));
	with Rel, Contract do if (WonTricks >= (Level + 6)) then begin
		s:=s+' made the contract';
		if wonRubber 
			then s:=s+' and the Rubber'
			else if wonGame then s:=s+' and Game';
		OverTricks:=WonTricks-(Level+6);
		if OverTricks>0 then begin
			s:=s+' with '+Int2Str(OverTricks)+' overtrick';
			if OverTricks>1 then s:=s+'s';
		end;
	end
	else
		s:=s+' down '+Int2Str(Level+6-WonTricks);
	s:=s+'.';
	EndGameMsg:=s;
end;

function Honours(var aCardDist:CardDist):word;

begin
	Honours:=0;
end;

function Team(who:PlayerType):TeamId;

begin
	Team:=TeamId(Q(who in [NORTH,SOUTH],Ord(WE),Ord(THEY)));
end;

function CountTry:LengthType;
{ Counts the Number of cards which should be tried. The variable
	"CardStat" is setup by a call to "InitSearch". }
var
	Count:LengthType;
	No:CardNoType;
begin
	Count:=0;
	for No:=0 to 12 do if Cardstat[No].Try then Inc(Count);
	CountTry:=Count;
end;

{$GOTO+}

procedure FindCard(var BestChoice:CardNoType;var Hint:boolean);
{ Finds a Card to play. Should call InitSearch before calling this
	function.

	The Rel record contains the actual situation.

	TrickNo, Game, Contract, BLIND and Info are alse updated.
	The Number of the chosen Card is returned in BestChoice.
	if the Hand is controlled by the human,
	the Card is Read from input. }
var
	DealNo    :integer;{ Deal counter }
	Result    :integer;{ Result of analysis }
	StatEvalu :integer;{ Evaluation of Result }
	MaxStat   :integer;{ Maximal statistical Evaluation }
	TryNo     :CardNoType;

label 10;
begin { findCard }
	{InitSearch;}
	with Sim do
		if CountTry=1 then begin
			for TryNo:=0 to 12 do { Find the only Card }
				if Cardstat[TryNo].Try then BestChoice:=TryNo;
		end
		else if Comp then begin
			DealNo:=0; { Heuristic Evaluation   }
			if HeuristicWght<>0 then while ((DealNo<MaxDeals) or (DealNo<1)) and (CountTry>1) do begin
				DealNo:=DealNo+1;
				SimInfo:=RelInfo;
				SimData:=RelData;
				DealCards;
				ChangeCards;
				SortCards;
				with SimInfo[PlayingHand,SelectCard] do
					for TryNo:=0 to 12 do
						with Cardstat[TryNo] do
							if Try and (SimInfo[PlayingHand,TryNo].Suit=Suit) and
								(SimInfo[PlayingHand,TryNo].Value <=Value) then
							begin
								Stat:=Stat+HeuristicWght;
								goto 10;
							end;
				10:
			end; { while }
			DealNo:=0; { Search }
			if SearchWght <> 0 then
				while ((DealNo < MaxDeals) or (DealNo < 1)) and (CountTry > 1) do begin
					DealNo:=DealNo+1;
					SimInfo:=RelInfo;
					SimData:=RelData;
					DealCards;
					ChangeCards;
					SortCards;
					for TryNo:=0 to 12 do
						if Cardstat[TryNo].Try then begin
							PlayCard(PlayingHand, TryNo);{ Analyse Card }
							Analyse(Result);
							StatEvalu:=Result-(6+Contract.Level); { Calculate Evaluation }
							if StatEvalu < 0 then
								StatEvalu:=StatEvalu-2;
							if odd(PlayingHand + Dummy) then
								StatEvalu:=-StatEvalu;
							with Cardstat[TryNo] do
								Stat:=Stat + StatEvalu * SearchWght;
						end; { if }
				end; { while }
			MaxStat:=-MaxInt; { Find best Choice }
			for TryNo:=12 downto 0 do
				with Cardstat[TryNo] do if Try then
					if Stat > MaxStat then begin
						BestChoice:=TryNo;
						MaxStat:=Stat;
					end;
		end;
end;

{$GOTO-}

procedure MakeBid(Bid:BidType);
{ Make the Bid and Update variables }
var
	i   :integer;
begin
	BidSystem(Bid,CalcInfo); { Update Info }
	theBids[BidNo]:=Bid;
	theBidTypes[BidNo]:=GetBidClass(Bid);
	if Bid.Level>0 then begin
		Contract:=Bid;
		theDoubledStatus:=0;
		i:=BidNo;
		repeat{ Find declarer and Dummy }
			with theBids[i] do if (Level>0) and (Trump=Contract.Trump) then Dummy:=(Dealer+i+2) and 3;
			i:=i-2;
		until i < 0;
	end
	else
		if not EqBid(Bid,PASS_BID) then theDoubledStatus:=Succ(theDoubledStatus);
	Inc(BidNo);
end;

procedure InitSearch;
{ Copies Rel in Sim, and finds the cards, which shall be analysed }

	procedure SelectTry;
	{ Selects the cards which should be analysed }
	var
		h        :PlayerType;
		i        :CardNoType;
	const
		LastCard :array[TSuit] of ValueType=(Low(ValueType),Low(ValueType),Low(ValueType),Low(ValueType)); { Strength of last chosen Card in the Suit }
	begin
		with Rel,RelData[PlayingHand],Contract do begin 
			for i:=12 downto 0 do with RelInfo[PlayingHand,i],Cardstat[i] do if Try then begin
				{ Always Try Lowest Card in a Suit }
				if Value <> Lowest(PlayingHand,Suit) then begin
					{ Never Try two different equally strong cards }
					Try:=false;
					if Value > LastCard[Suit]+1 then for h:=North to West do if Highmax(h,Suit,Value-1) > LastCard[Suit] then Try:=true;
					if not Try then if PlayingHand <> LeadHand then if (Suit=BestCard.Suit) and (LastCard[Suit] < BestCard.Value) then Try:=true;
					if not Try then LastCard[Suit]:=Value;
					if Try then begin
						if PlayingHand=LeadHand then
							Try:=Value > 8 { Try to lead All High cards }
						else
							if (Suit=BestCard.Suit) or (Suit=TSuit(Ord(Trump))) then begin
								{ Try cards which gets the trick }
								if Suit=BestCard.Suit 
									then Try:=Value > BestCard.Value
									else Try:=Suit=TSuit(Ord(Trump));
								{ in 4th Hand Try only Lowest Card, which gets the trick }
								if Try then if (Value <=8) or (PlayingHand=(LeadHand+3) and 3) then if not ((Suit=BestCard.Suit) and (LastCard[Suit] < BestCard.Value)) then Try:=false;
							end
							else Try:=false;
					end;
				end;
				if Try then LastCard[Suit]:=Value;
			end;
		end;
	end; { SelectTry }

var
	i :CardNoType;
	
begin { InitSearch }
	Sim:=Rel;
	with Rel,RelData[PlayingHand] do begin
		Comp:={$ifdef AUTOPLAY_TRICK} TRUE {$else} Computer[PlayingHand] {$endif};
		if PlayingHand=LeadHand then{ Check if All cards can be Played }
			CanPlayAll:=true
		else
			CanPlayAll:=(l[LeadSuit]=0);
		for i:=0 to 12 do { Check which cards can be Played }
			with RelInfo[PlayingHand,i],Cardstat[i] do begin
				Stat:=0;
				Try:=not Played and (CanPlayAll or (Suit=LeadSuit));
			end;
		if Comp then
			if CountTry>1 then SelectTry; { Select the cards to be analysed further }
	end;
end;

procedure ProcessLastBid;
var
	FB_Bid:BidType;
begin
	if not EqBid(Contract,PASS_BID) then
		if odd(NextBidder+Dummy) then begin
			if theDoubledStatus=0 then begin
				if Computer[NextBidder] then begin
					FB_Val:=BidSystem(Dbl,Evaluate);
					if FB_Val>FB_BestVal then begin
						BestBid:=Dbl;
						FB_BestVal:=FB_Val;
					end;
				end
				else
					if Pos(Command,'DBL')=1 then BestBid:=Dbl;
			end;
		end
		else
			if theDoubledStatus=1 then begin
				if Computer[NextBidder] then begin
					FB_Val:=BidSystem(RDbl,Evaluate);
					if FB_Val > FB_BestVal then begin
						BestBid:=RDbl;
						FB_BestVal:=FB_Val;
					end;
				end
				else
					if Pos(Command,'RDBL')=1 then BestBid:=RDbl;
		end;
	with FB_Bid do begin
		Level:=7;
		Trump:=TTNT;
		while not EqBid(FB_Bid,Contract) do begin
			if Computer[NextBidder] then begin
				FB_Val:=BidSystem(FB_Bid,Evaluate);
				if FB_Val >=FB_BestVal then
				begin
					BestBid:=FB_Bid;
					FB_BestVal:=FB_Val;
				end;
			end
			else if (Command=chr(Ord('0') + Level) + TrumpName[Trump][1]) then BestBid:=FB_Bid;
			if Trump<>TrumpType(Ord(TCLUB)) then
				Trump:=Pred(Trump)
			else begin
				Level:=Level-1;
				Trump:=TTNT;
			end;
		end;
	end;
	if BestBid.Level>0 then winning_bidder:=NextBidder;
end;

procedure InitializeSimHandInfo;
var
	s:TSuit;
	No:CardNoType;
	H:PlayerType;
begin
	for h:=North to West do with Info[h] do begin
		for No:=0 to 12 do with SimInfo[h, No], SimData[h] do begin
			if Played then Inc(l[Suit]);
			Known:=False;
			Played:=False;
		end;
		MinP:=0;
		MaxP:=40;
		for s:=TCLUB to TSPADE do Minl[s]:=0;
	end;
end;

procedure SetupSimHand(target:playerType;s,h,d,c:pchar);
const
	SPADEHAND=WEST;
	CLUBHAND=NORTH;
var
	info:CardDist;
	data:CardData;
begin
	System.Assert((StrLen(s)+StrLen(h)+StrLen(d)+StrLen(c))<=13,'More than 13 cards in the hand.');
	info:=RelInfo;
	data:=RelData;
	InitializeRelHands;
	SimInfo:=RelInfo;
	SimData:=RelData;
	InitializeSimHandInfo;
	SortCards;
//	for i:=0 to cardnotype(StrLen(s)-1) do Exchange(((12-i) shl 2)+SPADEHAND,((12-i) shl 2)+target);
//	Exchange(((9) shl 2)+CLUBHAND,((9) shl 2)+target);
	RelInfo:=info;
	RelData:=data;
end;

function GetHandPoints(s,h,d,c:pchar):integer;
const
	USE=NORTH;
begin
	SetupSimHand(USE,s,h,d,c);
	GetHandPoints:=SimData[USE].p;
end;

procedure SetupBid(T:TrumpType);
{ Increment the bid in "TmpBid" according to the trump type "T". }
begin
	with TmpBid do begin
		if Trump=T then begin
			{ next higher bid of the same suit }
			if Level=7 then
				if T>Contract.Trump then
					Level:=Contract.Level-1
				else
					Level:=Contract.Level;
			Level:=Level+1;
		end
		else begin
			{ switch to a different suit }
			if T>Contract.Trump then
				Level:=Contract.Level
			 else
				Level:=Contract.level+1;
			Trump:=T;
		end;
	end;
end;

end.
