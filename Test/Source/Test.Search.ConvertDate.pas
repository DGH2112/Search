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

Begin
  CheckEquals(EncodeDate(2007, 01, 07) + EncodeTime(12, 34, 56, 0), TConvertDate.ConvertDate('07 Jan 2007 12:34:56'),
    dblTolerance);
End;

Initialization
  // Register any test cases with the test runner
  RegisterTest('Convert Date', TTestConvertDate.Suite);
End.
