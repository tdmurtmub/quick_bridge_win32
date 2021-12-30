{ (C) 2011 Wesley Steiner }

{$MODE TP}

{$V-}

unit bridglibTests;

interface

implementation

uses
	punit,
	cards,
	bridge,
	bridglib;
	
type
	DefaultRec=record
		Computer:PlayerHand;
	end;

procedure test_DistPoints;
begin
	with SimData[SOUTH] do begin
		l[TCLUB]:=4;
		l[TDIAMOND]:=3;
		l[THEART]:=3;
		l[TSPADE]:=3;
		AssertAreEqual(0,DistPoints(SOUTH));
		l[TDIAMOND]:=2;
		AssertAreEqual(1,DistPoints(SOUTH));
		l[TDIAMOND]:=1;
		AssertAreEqual(2,DistPoints(SOUTH));
		l[TDIAMOND]:=0;
		AssertAreEqual(3,DistPoints(SOUTH));
	end;
	with SimData[WEST] do begin
		l[TCLUB]:=13;
		l[TDIAMOND]:=0;
		l[THEART]:=0;
		l[TSPADE]:=0;
		AssertAreEqual(9,DistPoints(WEST));
	end;
end;

procedure Test_InitializeRelHands;
begin
	InitGames;
	AssertAreEqual(TCLUB,RelInfo[NORTH][0].Suit);
	AssertAreEqual(TCLUB,RelInfo[NORTH][12].Suit);
	AssertAreEqual(TDIAMOND,RelInfo[EAST][0].Suit);
	AssertAreEqual(TDIAMOND,RelInfo[EAST][12].Suit);
	AssertAreEqual(THEART,RelInfo[SOUTH][0].Suit);
	AssertAreEqual(THEART,RelInfo[SOUTH][12].Suit);
	AssertAreEqual(TSPADE,RelInfo[WEST][0].Suit);
	AssertAreEqual(TSPADE,RelInfo[WEST][12].Suit);
	AssertAreEqual(2,RelInfo[NORTH][0].Value);
	AssertAreEqual(2,RelInfo[SOUTH][0].Value);
	AssertAreEqual(2,RelInfo[EAST][0].Value);
	AssertAreEqual(2,RelInfo[WEST][0].Value);
	AssertAreEqual(14,RelInfo[NORTH][12].Value);
	AssertAreEqual(14,RelInfo[SOUTH][12].Value);
	AssertAreEqual(14,RelInfo[EAST][12].Value);
	AssertAreEqual(14,RelInfo[WEST][12].Value);
	AssertAreEqual(0,RelData[NORTH].l[TCLUB]);
	AssertAreEqual(0,RelData[SOUTH].l[THEART]);
	AssertAreEqual(0,RelData[EAST].l[TSPADE]);
	AssertAreEqual(0,RelData[WEST].l[TDIAMOND]);
	AssertAreEqual(19,RelData[SOUTH].p);
	AssertAreEqual(19,RelData[EAST].p);
	AssertAreEqual(19,RelData[WEST].p);
	AssertAreEqual(19,RelData[NORTH].p);
end;

function MakeBidType(trump:trumpType;level:levelType):bidType;
var
	aBid:bidType;
begin
	aBid.Trump:=trump;
	aBid.Level:=level;
	MakeBidType:=aBid;
end;

procedure test_Jumps;
var
	aBid:BidType;
begin
	Contract:=MakeBidType(TTCLUB,1);
	aBid:=MakeBidType(TTDIAMOND,4);
	AssertAreEqual(3,Jumps(aBid));
	Contract:=MakeBidType(TTSPADE,1);
	aBid:=MakeBidType(TTSPADE,4);
	AssertAreEqual(2,Jumps(aBid));
	Contract:=MakeBidType(TTSPADE,1);
	aBid:=MakeBidType(TTHEART,4);
	AssertAreEqual(2,Jumps(aBid));
end;

procedure GetBidClass_test;
var
	aBid:bidType;
begin
	PreAuctionSetup;
	aBid.Level:=1;
	aBid.Trump:=TTHEART;
	AssertAreEqual(Ord(OPEN1S),Ord(GetBidClass(aBid)));
end;

procedure BidSystem_CalcInfo_Task;
var
	bid:BidType;
begin
	Dealer:=NORTH;
	BidNo:=0;
	AssertAreEqual(0,BidSystem(bid,CalcInfo));
end;

function Bid(aOddTricks:word):word;
begin
	Bid:=aOddTricks;
end;

function Made(aOddTricks:integer):word;
begin
	Made:=6+aOddTricks;
end;

procedure NextHandResult(aDummy, aLevel:integer; aTrump:trumptype; aDblStatus:DoublingStatus;nTricksWon:integer);

begin
	Contract.Level:= aLevel;
	Contract.Trump:= aTrump;
	theDoubledStatus:=aDblStatus;
	dummy:= aDummy;
	rel.WonTricks:=nTricksWon;
end;

procedure FreshHandResult(aDummy, aLevel:integer; aTrump:trumptype; aDblStatus:DoublingStatus;nTricksWon:integer);

begin
	NextHandResult(aDummy, aLevel, aTrump, aDblStatus, nTricksWon);
end;

procedure Test_EndGameMsg;
begin
	FreshHandResult(North,Bid(3), TTNT, 0, 9);
	Assert.EqualStr('N/S made the contract and Game.', EndGameMsg(FALSE,TRUE));
	FreshHandResult(North,Bid(3), TTNT, 0, 9);
	NextHandResult(South,Bid(4), TTSPADE, 0, 10);
	Assert.EqualStr('N/S made the contract and the Rubber.',EndGameMsg(TRUE,TRUE));
end;

procedure Test_Honours;
var
	aDist:CardDist;
begin
	Assert.Equal(0, Honours(aDist));
end;

procedure Test_Team;
begin
	Assert.Equal(Ord(WE),Ord(Team(NORTH)));
	Assert.Equal(Ord(WE),Ord(Team(SOUTH)));
	Assert.Equal(Ord(THEY),Ord(Team(EAST)));
	Assert.Equal(Ord(THEY),Ord(Team(WEST)));
end;

procedure test_SetupSimHand;
begin
	SetupSimHand(NORTH,'','','','AKQJT98765432');
	AssertAreEqual(TCLUB,RelInfo[NORTH][0].Suit);
	AssertAreEqual(TCLUB,RelInfo[NORTH][12].Suit);
	AssertAreEqual(14,RelInfo[NORTH][0].Value);
	AssertAreEqual(2,RelInfo[NORTH][12].Value);
	SetupSimHand(NORTH,'AKQJT98765432','','','');
	AssertAreEqual(TSPADE,RelInfo[NORTH][5].Suit);
end;

procedure test_GetHandPoints;
begin
	AssertAreEqual(16,GetHandPoints('AKQJT98765','AK','A',''));
end;

procedure ProcessLastBid_pass_does_not_update_winning_bidder;
begin
	winning_bidder:=NORTH;
	Contract:=PASS_BID;
	BestBid:=PASS_BID;
	NextBidder:=EAST;
	Computer[NextBidder]:=FALSE;
	Command:='Pass';
	ProcessLastBid;
	AssertAreEqual(NORTH,winning_bidder);
end;

procedure ProcessLastBid_updates_winning_bid_info;
begin
	winning_bidder:=WEST;
	Contract:=PASS_BID;
	BestBid:=PASS_BID;
	NextBidder:=NORTH;
	Computer[NextBidder]:=FALSE;
	Command:='3H';
	ProcessLastBid;
	AssertAreEqual(NORTH,winning_bidder);
	AssertAreEqual(3,BestBid.Level);
	AssertAreEqual(THEART,Ord(BestBid.Trump));
end;

begin
//	Suite.Add(BidSystem_CalcInfo_Task);
	Suite.Add(GetBidClass_test);
	Suite.Add(ProcessLastBid_pass_does_not_update_winning_bidder);
	Suite.Add(ProcessLastBid_updates_winning_bid_info);
	Suite.Add(test_EndGameMsg);
	Suite.Add(test_Honours);
	Suite.Add(test_Team);
	Suite.Add(test_Jumps);
	Suite.Add(test_InitializeRelHands);
	Suite.Add(test_DistPoints);
//	Suite.Add(test_SetupSimHand);
//	Suite.Add(test_GetHandPoints);
	Suite.Run('bridglibTests');
end.
