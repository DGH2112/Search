//: @nochecks for testing @nometrics for testing
Unit Test.Search.ConvertDate;

Interface

Uses
  TestFramework;

Type
  TTestConvertDate = Class(TTestCase)
  Strict Private
  Strict Protected
  Public
  Published
    Procedure TestConvertDate;
  End;

Implementation

uses
  Search.ConvertDate, 
  System.SysUtils;

Procedure TTestConvertDate.TestConvertDate;

Const
  dblTolerance = 1.0 / 24.0 / 60.0 / 60.0;

Var
  iYear, iMonth, iDay : Word;

Begin
  DecodeDate(Now(), iYear, iMonth, iDay);
  CheckEquals(EncodeDate(iYear, iMonth, 07), TConvertDate.ConvertDate('07'), dblTolerance);
  CheckEquals(EncodeDate(iYear, 03, 07), TConvertDate.ConvertDate('07 Mar'), dblTolerance);
  CheckEquals(EncodeDate(2007, 01, 07), TConvertDate.ConvertDate('07 Jan 2007'), dblTolerance);
  CheckEquals(EncodeDate(iYear, iMonth, 07) + EncodeTime(12, 34, 0, 0), TConvertDate.ConvertDate('07 12:34'), dblTolerance);
  CheckEquals(EncodeDate(iYear, 08, 07) + EncodeTime(12, 34, 0, 0), TConvertDate.ConvertDate('07 Aug 12:34'), dblTolerance);
  CheckEquals(EncodeDate(2007, 01, 07) + EncodeTime(12, 34, 56, 0), TConvertDate.ConvertDate('07 Jan 2007 12:34:56'), dblTolerance);
  CheckEquals(EncodeDate(iYear, 03, 07), TConvertDate.ConvertDate('Mon 07 Mar'), dblTolerance);
  CheckEquals(EncodeDate(2007, 01, 07) + EncodeTime(12, 34, 56, 0), TConvertDate.ConvertDate('Wed 07 Jan 2007 12:34:56'), dblTolerance);
End;

Initialization
  // Register any test cases with the test runner
  RegisterTest('Convert Date', TTestConvertDate.Suite);
End.
