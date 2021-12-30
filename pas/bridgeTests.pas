// (C) 2005 Wesley Steiner

{$MODE FPC}

unit bridgeTests;

interface

uses
	std,
	bridge;

implementation

uses 
	punit; 

procedure test_ScoreKeeper_OverLineTotal;
var
	sheet:ScoreKeeper;
begin
	sheet.Construct;
	sheet.AddResult(WE,1,TTCLUB,NOT_DOUBLED,8);
	sheet.AddResult(WE,1,TTCLUB,NOT_DOUBLED,9);
	Assert.AreEqualWord(60,sheet.OverlineTotal(WE));
	sheet.AddResult(THEY,2,TTSPADE,NOT_DOUBLED,9);
	sheet.AddResult(THEY,2,TTSPADE,NOT_DOUBLED,9);
	Assert.AreEqualWord(60,sheet.OverlineTotal(THEY));
end;

procedure SetGameScores(var gms:ScoreKeeper;we1,they1,we2,they2,we3,they3:integer);

begin
	gms.my_data[WE].GameRecord[1]:=we1;
	gms.my_data[WE].GameRecord[2]:=we2;
	gms.my_data[WE].GameRecord[3]:=we3;
	gms.my_data[THEY].GameRecord[1]:=they1;
	gms.my_data[THEY].GameRecord[2]:=they2;
	gms.my_data[THEY].GameRecord[3]:=they3;
end;

procedure TestScoreKeeperGameScore;

var
	sheet:ScoreKeeper;

begin
	sheet.Construct;
	sheet.my_data[WE].Gamerecord[2]:= 2123;
	sheet.my_data[THEY].Gamerecord[3]:= 4561;
	Assert.AreEqualWord(2123, sheet.GameScore(WE, 2));
	Assert.AreEqualWord(4561, sheet.GameScore(THEY, 3));
end;

procedure TestScoreKeeperGamesWon;

var
	sheet:ScoreKeeper;

begin
	sheet.Construct;
	SetGameScores(sheet, 0, 0, 100, 0, 99, 0);
	Assert.Equal(1, sheet.GamesWon(WE));
	SetGameScores(sheet, 0, 200, 100, 23, 99, 199);
	Assert.Equal(2, sheet.GamesWon(THEY));
end;

procedure TestScoreKeeperGamesPlayed;

var
	sheet:ScoreKeeper;

begin
	sheet.Construct;
	SetGameScores(sheet, 0, 0, 0, 0, 0, 0);
	Assert.Equal(0, sheet.GamesPlayed);
	SetGameScores(sheet, 100, 0, 0, 0, 0, 0);
	Assert.Equal(1, sheet.GamesPlayed);
	SetGameScores(sheet, 100 + 1, 0, 0, 0, 0, 0);
	Assert.Equal(1, sheet.GamesPlayed);
	SetGameScores(sheet, 100 - 1, 0, 0, 0, 0, 0);
	Assert.Equal(0, sheet.GamesPlayed);
	SetGameScores(sheet, 100, 0, 100, 0, 100, 0);
	Assert.Equal(3, sheet.GamesPlayed);
	SetGameScores(sheet, 0, 100, 0, 100, 0, 100);
	Assert.Equal(3, sheet.GamesPlayed);
	SetGameScores(sheet, 100, 0, 0, 100, 0, 0);
	Assert.Equal(2, sheet.GamesPlayed);
end;

procedure TestScoreKeeperCurrentGameIndex;

var
	sheet:ScoreKeeper;

begin
	sheet.Construct;
	SetGameScores(sheet, 0, 0, 0, 0, 0, 0);
	Assert.Equal(1, sheet.CurrentGameIndex);
	SetGameScores(sheet, 100 - 1, 0, 0, 0, 0, 0);
	Assert.Equal(1, sheet.CurrentGameIndex);
	SetGameScores(sheet, 100, 0, 0, 0, 0, 0);
	Assert.Equal(2, sheet.CurrentGameIndex);
	SetGameScores(sheet, 0, 100, 100 + 1, 0, 20, 50);
	Assert.Equal(3, sheet.CurrentGameIndex);
end;

procedure TestScoreKeeperGameOver;

var
	sheet:ScoreKeeper;

begin
	sheet.Construct;
	SetGameScores(sheet, 0, 0, 0, 0, 0, 0);
	Assert.IsFalse(sheet.GameOver);
	SetGameScores(sheet, 100, 0, 0, 0, 0, 0);
	Assert.IsFalse(sheet.GameOver);
	SetGameScores(sheet, 0, 100, 0, 0, 0, 0);
	Assert.IsFalse(sheet.GameOver);
	SetGameScores(sheet, 100, 0, 100, 0, 0, 0);
	Assert.IsTrue(sheet.GameOver);
	SetGameScores(sheet, 100, 0, 0, 200, 0, 0);
	Assert.IsFalse(sheet.GameOver);
	SetGameScores(sheet, 100, 0, 0, 200, 100, 0);
	Assert.IsTrue(sheet.GameOver);
	SetGameScores(sheet, 0, 200, 100, 0, 0, 200);
	Assert.IsTrue(sheet.GameOver);
end;

procedure SetTeamScores(var rTeam:ScoreType; O, G1, G2, G3:word);

begin
	rTeam.Overline:= O;
	rTeam.GameRecord[1]:= G1;
	rTeam.GameRecord[2]:= G2;
	rTeam.GameRecord[3]:= G3;
end;

procedure TestTeamTotal;

var
	sheet:ScoreKeeper;

begin
	sheet.Construct;

	SetTeamScores(sheet.my_data[WE], 0, 0, 0, 0);
	Assert.Equal(0, sheet.TeamTotal(WE));

	SetTeamScores(sheet.my_data[THEY], 123, 0, 0, 0);
	Assert.Equal(123, sheet.TeamTotal(THEY));

	SetTeamScores(sheet.my_data[THEY], 200, 10, 0, 0);
	Assert.Equal(210, sheet.TeamTotal(THEY));

	SetTeamScores(sheet.my_data[WE], 200, 10, 20, 0);
	Assert.Equal(230, sheet.TeamTotal(WE));

	SetTeamScores(sheet.my_data[WE], 200, 10, 20, 30);
	Assert.Equal(260, sheet.TeamTotal(WE));
end;

procedure Test_ScoreKeeper_WonRubber;

var
	sheet:ScoreKeeper;

begin
	sheet.Construct;
	SetTeamScores(sheet.my_data[WE], 0, 100, 100, 0);
	Assert.IsTrue(sheet.WonRubber(WE));
	SetTeamScores(sheet.my_data[WE], 0, 100, 0, 0);
	Assert.IsFalse(sheet.WonRubber(WE));
end;

function Bid(aOddTricks:word):word;
begin
	Bid:=aOddTricks;
end;

function Made(aOddTricks:integer):word;
begin
	Made:=6+aOddTricks;
end;

procedure Test_ScoreKeeper_AddResult;
var
	sheet:ScoreKeeper;
begin
	sheet.Construct;
	sheet.Clear;
	sheet.AddResult(WE,1,TTCLUB,NOT_DOUBLED,Made(1));
	AssertAreEqual(20,sheet.GameScore(WE,1));
	sheet.Clear;
	sheet.AddResult(THEY,1,TTCLUB,NOT_DOUBLED,Made(1));
	AssertAreEqual(20,sheet.GameScore(THEY,1));
end;

var
	aTestScoreSheet:ScoreKeeper;

procedure FreshHandResult(aDummy:integer);
begin
	aTestScoreSheet.Clear;
end;

procedure TestTrickScoring;
begin
	aTestScoreSheet.Construct;

	FreshHandResult(North);
	aTestScoreSheet.AddResult(WE,contractLevel(0),TTCLUB,NOT_DOUBLED,0);
	Assert.Equal(0, aTestScoreSheet.GameScore(WE, 1));
	Assert.Equal(0, aTestScoreSheet.GameScore(THEY, 1));

	FreshHandResult(North);
	aTestScoreSheet.AddResult(WE,1,TTCLUB,NOT_DOUBLED,7);
	Assert.Equal(20, aTestScoreSheet.GameScore(WE, 1));
	aTestScoreSheet.AddResult(THEY,Bid(2),TTDIAMOND,NOT_DOUBLED,8);
	Assert.Equal(20 * 2, aTestScoreSheet.GameScore(THEY, 1));

	FreshHandResult(East);
	aTestScoreSheet.AddResult(THEY,Bid(1),TTNT,NOT_DOUBLED,Made(1));
	Assert.Equal(40, aTestScoreSheet.GameScore(THEY, 1));

	FreshHandResult(East);
	aTestScoreSheet.AddResult(THEY,Bid(1),TTNT,NOT_DOUBLED,Made(3));
	Assert.Equal(40, aTestScoreSheet.GameScore(THEY, 1));

	FreshHandResult(East);
	aTestScoreSheet.AddResult(THEY,Bid(3),TTNT,NOT_DOUBLED,Made(3));
	Assert.Equal(40+30*2, aTestScoreSheet.GameScore(THEY, 1));
end;

procedure Test2GameRubberBonus;
begin
	aTestScoreSheet.Construct;
	FreshHandResult(North);
	aTestScoreSheet.AddResult(WE,5,TTCLUB,NOT_DOUBLED,11);
	aTestScoreSheet.AddResult(WE,3,TTNT,NOT_DOUBLED,9);
	Assert.Equal(700, aTestScoreSheet.OverlineTotal(WE));
end;

procedure Test3GameRubberBonus;
begin
	aTestScoreSheet.Construct;
	FreshHandResult(East);
	aTestScoreSheet.AddResult(THEY,5,TTCLUB,NOT_DOUBLED,11);
	aTestScoreSheet.AddResult(WE,3,TTNT,NOT_DOUBLED,9);
	Assert.Equal(0, aTestScoreSheet.OverlineTotal(THEY));
	aTestScoreSheet.AddResult(THEY,4,TTHEART,NOT_DOUBLED,10);
	Assert.Equal(500, aTestScoreSheet.OverlineTotal(THEY));
end;

procedure TestOvertricks;

	procedure Tester(level, nMade:integer; aTrump:trumpType; aDblStatus:DoublingStatus; expect:integer);

	begin
		FreshHandResult(North);
		aTestScoreSheet.AddResult(WE,level,aTrump,aDblStatus,Made(nMade));
		Assert.Equal(expect, aTestScoreSheet.OverlineTotal(WE));
	end;

begin
	aTestScoreSheet.Construct;

	Tester(3, 4, TTCLUB, NOT_DOUBLED, 20);
	Tester(3, 5, TTCLUB, NOT_DOUBLED, 20 * 2);
	Tester(3, 4, TTSPADE, NOT_DOUBLED, 30);
	Tester(3, 5, TTHEART, NOT_DOUBLED, 30 * 2);
	Tester(3, 4, TTNT, NOT_DOUBLED, 30);
	Tester(3, 5, TTNT, NOT_DOUBLED, 30 * 2);

	Tester(3, 3, TTCLUB, DOUBLED, 50);
	Tester(3, 3, TTSPADE, DOUBLED, 50);
	Tester(3, 3, TTNT, DOUBLED, 50);
	Tester(3, 4, TTNT, DOUBLED, 50 + 100);
	Tester(3, 5, TTNT, DOUBLED, 50 + 100 * 2);
	Tester(3, 6, TTNT, DOUBLED, 50 + 100 * 3);

	Tester(3, 4, TTSPADE, REDOUBLED, 50 + 200);
	Tester(3, 5, TTSPADE, REDOUBLED, 50 + 200 * 2);
end;

procedure TestUndertricksNotVulnerable;
	procedure Tester(level, nMade:integer; aDblStatus:DoublingStatus; expect:integer);
	begin
		FreshHandResult(North);
		aTestScoreSheet.AddResult(WE,Bid(level),TTSPADE,aDblStatus,Made(nMade));
		Assert.Equal(expect, aTestScoreSheet.OverlineTotal(THEY));
	end;
begin
	aTestScoreSheet.Construct;

	Tester(3, 2, NOT_DOUBLED, 50);
	Tester(3, 1, NOT_DOUBLED, 100);
	Tester(3, 0, NOT_DOUBLED, 150);

	Tester(3, 2, DOUBLED, 100);
	Tester(3, 1, DOUBLED, 300);
	Tester(3, 0, DOUBLED, 500);
	Tester(3, -1, DOUBLED, 700);

	Tester(3, 2, REDOUBLED, 200);
	Tester(3, 1, REDOUBLED, 600);
	Tester(3, 0, REDOUBLED, 1000);
end;

procedure TestUndertricksVulnerable;
	procedure Tester(level, nMade:integer; aDblStatus:DoublingStatus; expect:integer);
	begin
		FreshHandResult(North);
		aTestScoreSheet.AddResult(WE,Bid(level),TTNT,aDblStatus,Made(level));
		aTestScoreSheet.AddResult(WE,Bid(level),TTNT,aDblStatus,Made(nMade));
		Assert.Equal(expect, aTestScoreSheet.OverlineTotal(THEY));
	end;
begin
	aTestScoreSheet.Construct;

	Tester(3, 2, NOT_DOUBLED, 100);
	Tester(3, 1, NOT_DOUBLED, 200);
	Tester(3, 0, NOT_DOUBLED, 300);

	Tester(3, 2, DOUBLED, 200);
	Tester(3, 1, DOUBLED, 500);
	Tester(3, 0, DOUBLED, 800);

	Tester(3, 2, REDOUBLED, 400);
	Tester(3, 1, REDOUBLED, 1000);
	Tester(3, 0, REDOUBLED, 1600);
end;

procedure TestSmallSlamNotVulnerable;
begin
	FreshHandResult(North);
	aTestScoreSheet.AddResult(WE,6,TTCLUB,NOT_DOUBLED,12);
	Assert.Equal(500, aTestScoreSheet.OverlineTotal(WE));
end;

procedure TestGrandSlamNotVulnerable;
begin
	FreshHandResult(East);
	aTestScoreSheet.AddResult(THEY,7,TTNT,NOT_DOUBLED,13);
	Assert.Equal(1000, aTestScoreSheet.OverlineTotal(THEY));
end;

procedure TestSmallSlamVulnerable;
begin
	FreshHandResult(North);
	aTestScoreSheet.AddResult(WE,3,TTNT,NOT_DOUBLED,9);
	aTestScoreSheet.AddResult(WE,6,TTSPADE,NOT_DOUBLED,12);
	Assert.Equal(700 + 750, aTestScoreSheet.OverlineTotal(WE));
end;

procedure TestGrandSlamVulnerable;

begin
	FreshHandResult(West);
	aTestScoreSheet.AddResult(THEY,3,TTNT,NOT_DOUBLED,9);
	aTestScoreSheet.AddResult(THEY,7,TTSPADE,NOT_DOUBLED,13);
	Assert.Equal(2200{700+1500}, aTestScoreSheet.OverlineTotal(THEY));
end;

procedure Test_Backwards_Compatibility;
begin
	AssertAreEqual(0,Ord(WE));
	AssertAreEqual(1,Ord(THEY));
	AssertAreEqual(0,Ord(TTCLUB));
	AssertAreEqual(1,Ord(TTDIAMOND));
	AssertAreEqual(2,Ord(TTHEART));
	AssertAreEqual(3,Ord(TTSPADE));
	AssertAreEqual(4,Ord(TTNT));
	AssertAreEqual(0,NOT_DOUBLED);
	AssertAreEqual(1,DOUBLED);
	AssertAreEqual(2,REDOUBLED);
end;

procedure Test_ScoreKeeper_ToString;
var
	aTester:ScoreKeeper;
begin
	aTester.Construct;
	aTester.AddResult(WE,1,TTCLUB,NOT_DOUBLED,2);
	punit.Assert.EqualStr('0,1,0,0,2',aTester.ToString);
	aTester.Clear;
	aTester.AddResult(THEY,1,TTCLUB,NOT_DOUBLED,2);
	punit.Assert.EqualStr('1,1,0,0,2',aTester.ToString);
	aTester.Clear;
	aTester.AddResult(THEY,5,TTCLUB,NOT_DOUBLED,2);
	punit.Assert.EqualStr('1,5,0,0,2',aTester.ToString);
	aTester.Clear;
	aTester.AddResult(THEY,5,TTSPADE,NOT_DOUBLED,2);
	punit.Assert.EqualStr('1,5,3,0,2',aTester.ToString);
	aTester.Clear;
	aTester.AddResult(THEY,5,TTSPADE,REDOUBLED,2);
	punit.Assert.EqualStr('1,5,3,2,2',aTester.ToString);
	aTester.Clear;
	aTester.AddResult(THEY,5,TTSPADE,REDOUBLED,13);
	punit.Assert.EqualStr('1,5,3,2,13',aTester.ToString);
	aTester.AddResult(WE,3,TTNT,DOUBLED,10);
	punit.Assert.EqualStr('1,5,3,2,13,0,3,4,1,10',aTester.ToString);
end;

procedure Test_ScoreKeeper_ConstructFromData;
var
	aTester:ScoreKeeper;
begin
	aTester.ConstructFromData('0,1,1,0,7');
	punit.Assert.IsTrue(aTester.TeamTotal(WE)>0);
	aTester.ConstructFromData('1,1,1,0,7');
	punit.Assert.IsTrue(aTester.TeamTotal(THEY)>0);
	aTester.ConstructFromData('1,1,1,0,7');
	AssertAreEqual(20,aTester.GameScore(THEY,1));
	aTester.ConstructFromData('1,7,1,0,7');
	AssertAreEqual(300,aTester.TeamTotal(WE));
	aTester.ConstructFromData('1,1,4,0,7');
	AssertAreEqual(40,aTester.TeamTotal(THEY));
	aTester.ConstructFromData('1,1,4,1,7');
	AssertAreEqual(130,aTester.TeamTotal(THEY));
	aTester.ConstructFromData('1,1,4,2,7');
	AssertAreEqual(210,aTester.TeamTotal(THEY));
	aTester.ConstructFromData('1,1,1,0,13');
	AssertAreEqual(140,aTester.TeamTotal(THEY));
	aTester.ConstructFromData('0,3,4,0,10,0,2,3,0,5');
	AssertAreEqual(300,aTester.OverlineTotal(THEY));
	AssertAreEqual(130,aTester.TeamTotal(WE));
end;

procedure Test_ScoreKeeper_ConstructFromData_bad_format;
var
	aTester:ScoreKeeper;
begin
	aTester.ConstructFromData('-1,3,4,0,9');
	punit.Assert.IsTrue(aTester.IsBlank);
	aTester.ConstructFromData('2,3,4,0,9');
	punit.Assert.IsTrue(aTester.IsBlank);
	aTester.ConstructFromData('1,0,4,0,9');
	punit.Assert.IsTrue(aTester.IsBlank);
	aTester.ConstructFromData('1,8,4,0,9');
	punit.Assert.IsTrue(aTester.IsBlank);
	aTester.ConstructFromData('1,3,-1,0,9');
	punit.Assert.IsTrue(aTester.IsBlank);
	aTester.ConstructFromData('1,3,5,0,9');
	punit.Assert.IsTrue(aTester.IsBlank);
	aTester.ConstructFromData('1,3,4,-1,9');
	punit.Assert.IsTrue(aTester.IsBlank);
	aTester.ConstructFromData('1,3,4,3,9');
	punit.Assert.IsTrue(aTester.IsBlank);
	aTester.ConstructFromData('1,3,4,0,-1');
	punit.Assert.IsTrue(aTester.IsBlank);
	aTester.ConstructFromData('1,3,4,0,14');
	punit.Assert.IsTrue(aTester.IsBlank);
	aTester.ConstructFromData('1,3,4,0,9,Bad');
	punit.Assert.IsTrue(aTester.IsBlank);
end;

procedure test_ScoreKeeper_OverLineScoreCount;
var
	testee:ScoreKeeper;
begin
	testee.Construct;
	AssertAreEqual(0,testee.OverLineScoreCount(WE));
	testee.AddResult(WE,1,TTCLUB,NOT_DOUBLED,8);
	testee.AddResult(WE,1,TTCLUB,NOT_DOUBLED,9);
	testee.AddResult(WE,1,TTCLUB,NOT_DOUBLED,10);
	AssertAreEqual(3,testee.OverLineScoreCount(WE));
end;

begin
	Suite.Add(@TestScoreKeeperGameScore);
	Suite.Add(@TestScoreKeeperGamesWon);
	Suite.Add(@TestScoreKeeperGamesPlayed);
	Suite.Add(@TestScoreKeeperCurrentGameIndex);
	Suite.Add(@TestScoreKeeperGameOver);
	Suite.Add(@TestTeamTotal);
	Suite.Add(@Test_ScoreKeeper_WonRubber);
	Suite.Add(@TestTrickScoring);
	Suite.Add(@Test2GameRubberBonus);
	Suite.Add(@Test3GameRubberBonus);
	Suite.Add(@TestOvertricks);
	Suite.Add(@TestUndertricksNotVulnerable);
	Suite.Add(@TestUndertricksVulnerable);
	Suite.Add(@TestSmallSlamNotVulnerable);
	Suite.Add(@TestSmallSlamVulnerable);
	Suite.Add(@TestGrandSlamNotVulnerable);
	Suite.Add(@TestGrandSlamVulnerable);
	Suite.Add(@Test_ScoreKeeper_AddResult);
	Suite.Add(@Test_Backwards_Compatibility);
	Suite.Add(@Test_ScoreKeeper_ToString);
	Suite.Add(@Test_ScoreKeeper_ConstructFromData);
	Suite.Add(@Test_ScoreKeeper_ConstructFromData_bad_format);
	Suite.Add(@test_ScoreKeeper_OverLineTotal);
//	Suite.Add(@test_ScoreKeeper_OverLineScoreCount);
	Suite.Run('bridgeTests');
end.
