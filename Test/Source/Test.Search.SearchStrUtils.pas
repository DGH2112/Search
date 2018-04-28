//: @nochecks for testing @nometrics for testing
Unit Test.Search.SearchStrUtils;

Interface

uses
  TestFramework;

Type
  TTestSearchStrUtils = Class(TTestCase)
  Published
    Procedure TestPosOfNthChar;
    Procedure TestLike;
    Procedure TestCharCount;
    Procedure TestGetField;
  End;

Implementation

uses
  Search.StrUtils;

Procedure TTestSearchStrUtils.TestCharCount;

Begin
  CheckEquals(4, TSearchStrUtils.CharCount(',', 'First,Second,Third,Fourth,Fifth'));
  CheckEquals(4, TSearchStrUtils.CharCount(',', '"First,Second",Third,"Fourth,Fifth"'));
  CheckEquals(4, TSearchStrUtils.CharCount(',', 'First,"Second,Third",Fourth,Fifth'));
  CheckEquals(2, TSearchStrUtils.CharCount(',', '"First,Second",Third,"Fourth,Fifth"', False));
  CheckEquals(3, TSearchStrUtils.CharCount(',', 'First,"Second,Third",Fourth,Fifth', False));
End;

Procedure TTestSearchStrUtils.TestGetField;

Begin
  CheckEquals('First', TSearchStrUtils.GetField('First,Second,Third,Fourth,Fifth', ',', 1), 'First');
  CheckEquals('Third', TSearchStrUtils.GetField('First,Second,Third,Fourth,Fifth', ',', 3), 'Third');
  CheckEquals('Fifth', TSearchStrUtils.GetField('First,Second,Third,Fourth,Fifth', ',', 5), 'Third');

  CheckEquals('Third', TSearchStrUtils.GetField('"First,Second",Third,"Fourth,Fifth"', ',', 3), 'Fourth');
  CheckEquals('"Fourth,Fifth"', TSearchStrUtils.GetField('"First,Second",Third,"Fourth,Fifth"', ',', 3, False), 'Fifth');
  CheckEquals('Third"', TSearchStrUtils.GetField('First,"Second,Third",Fourth,Fifth"', ',', 3), 'Sixth');
  CheckEquals('Fourth', TSearchStrUtils.GetField('First,"Second,Third",Fourth,Fifth"', ',', 3, False), 'Seventh');
End;

Procedure TTestSearchStrUtils.TestLike;

Const
  strText = 'Mary had a little lamb';

Begin
  Check(TSearchStrUtils.Like('mary*', strText), '1');
  Check(Not TSearchStrUtils.Like('mry*', strText), '2');
  Check(TSearchStrUtils.Like('*lamb', strText), '3');
  Check(Not TSearchStrUtils.Like('*lmb', strText), '4');
  Check(TSearchStrUtils.Like('*little*', strText), '5');
  Check(Not TSearchStrUtils.Like('*litle*', strText), '6');
  Check(TSearchStrUtils.Like('*had*little*', strText), '7');
  Check(Not TSearchStrUtils.Like('*had*litle*', strText), '8');
  Check(Not TSearchStrUtils.Like('*little*had*', strText), '9');
  Check(TSearchStrUtils.Like('*library*testprojectdll*;*', 'Library TestProjectDLL;'), '10');
  Check(TSearchStrUtils.Like('*', ''));
  Check(TSearchStrUtils.Like('*', strText));
End;

Procedure TTestSearchStrUtils.TestPosOfNthChar;

Begin
  CheckEquals(19, TSearchStrUtils.PosOfNthChar('First,Second,Third,Fourth,Fifth', ',', 3));
  CheckEquals(21, TSearchStrUtils.PosOfNthChar('"First,Second",Third,"Fourth,Fifth"', ',', 3));
  CheckEquals(21, TSearchStrUtils.PosOfNthChar('First,"Second,Third",Fourth,Fifth"', ',', 3));
  CheckEquals(21, TSearchStrUtils.PosOfNthChar('"First,Second",Third,"Fourth,Fifth"', ',', 2, False));
  CheckEquals(21, TSearchStrUtils.PosOfNthChar('First,"Second,Third",Fourth,Fifth"', ',', 2, False));
End;

initialization
  // Register any test cases with the test runner
  RegisterTest('Search String Utilities', TTestSearchStrUtils.Suite);
End.
