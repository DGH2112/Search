//: @stopdocumentation @nochecks Disabled for test @nometrics Disabled for test
Unit Test.Search.SearchFile;

Interface

Uses
  TestFramework,
  Search.Interfaces;

Type
  //
  // Test Class for the ISearchFile Class Methods.
  //
  TTestISearchFile = Class(TTestCase)
  Strict Private
    FISearchFile : ISearchFile;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAddRegExLine;
    Procedure TestClone;
    Procedure TestAttributes;
    Procedure TestCompressedSize;
    Procedure TestDate;
    Procedure TestFileName;
    Procedure TestOwner;
    Procedure TestRegExMatches;
    Procedure TestSize;
  End;

Implementation

uses
  Search.FileCls, 
  Search.Types, 
  System.DateUtils, 
  System.RegularExpressions;

//
// Test Methods for Class ISearchFile.
//
Procedure TTestISearchFile.Setup;

Var
  recSearchFile: TSearchFileRec;

Begin
  recSearchFile.Create(EncodeDateTime(2018, 1, 7, 14, 30, 15, 0), 123456, 23456, [sfaFile, sfaArchive,
    sfaCompressed], 'David.Hoyle', 'D:\Path\MyFileName.pas');
  FISearchFile := TSearchFile.Create(recSearchFile)
End;

Procedure TTestISearchFile.TearDown;

Begin
  FISearchFile := Nil;
End;

Procedure TTestISearchFile.TestAddRegExLine;

Const
  strText = 
    'This is some text with the word Hello'#13#10 +
    'in several times in different places.'#13#10 +
    ''#13#10 +
    'Yes, that is HELLO in many hello places.'#13#10 +
    ''#13#10 +
    'Hello!'#13#10 +
    'Goodbye'#13#10;

Var
  RE : TRegEx;
  MC: TMatchCollection;

Begin
  RE.Create('Hello', [roIgnoreCase, roCompiled]);
  MC := RE.Matches(strText);
  FISearchFile.AddRegExLine(1, MC);
  FISearchFile.AddRegExLine(10, MC);
  CheckEquals(2, FISearchFile.RegExMatches.Count);
End;

Procedure TTestISearchFile.TestClone;

Const
  dblTolerance = 1.0 / 24.0 / 60.0 / 60.0;

Var
  F: ISearchFile;

Begin
  F := FISearchFile.Clone;
  CheckEquals(EncodeDateTime(2018, 1, 7, 14, 30, 15, 0), F.Date, dblTolerance);
  CheckEquals(123456, F.Size);
  CheckEquals(23456, F.CompressedSize);
  Check([sfaFile, sfaArchive, sfaCompressed]= F.Attributes);
  CheckEquals('David.Hoyle', F.Owner);
  CheckEquals('D:\Path\MyFileName.pas', F.FileName);
End;

Procedure TTestISearchFile.TestAttributes;

Begin
  Check(FISearchFile.Attributes = [sfaFile, sfaArchive, sfaCompressed]);
End;

Procedure TTestISearchFile.TestCompressedSize;

Begin
  CheckEquals(23456, FISearchFile.CompressedSize);
End;

Procedure TTestISearchFile.TestDate;

Const
  dblTolerance = 1.0 / 234.0 / 60.0 / 60.0;

Begin
  CheckEquals(EncodeDateTime(2018, 1, 7, 14, 30, 15, 0), FISearchFile.Date, dblTolerance);
End;

Procedure TTestISearchFile.TestFileName;

Begin
  CheckEquals('D:\Path\MyFileName.pas', FISearchFile.FileName);
End;

Procedure TTestISearchFile.TestOwner;

Begin
  CheckEquals('David.Hoyle', FISearchFile.Owner);
End;

Procedure TTestISearchFile.TestRegExMatches;

Const
  strText = 
    'This is some text with the word Hello'#13#10 +
    'in several times in different places.'#13#10 +
    ''#13#10 +
    'Yes, that is HELLO in many hello places.'#13#10 +
    ''#13#10 +
    'Hello!'#13#10 +
    'Goodbye'#13#10;

Var
  RE : TRegEx;
  MC: TMatchCollection;

Begin
  RE.Create('Hello', [roIgnoreCase, roCompiled]);
  MC := RE.Matches(strText);
  FISearchFile.AddRegExLine(1, MC);
  FISearchFile.AddRegExLine(10, MC);
  CheckEquals(2, FISearchFile.RegExMatches.Count);
End;

Procedure TTestISearchFile.TestSize;

Begin
  CheckEquals(123456, FISearchFile.Size);
End;


Initialization
  RegisterTest('ISearchFile', TTestISearchFile.Suite);
End.