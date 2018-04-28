//: @stopdocumentation @nochecks Disabled for test @nometrics Disabled for test
Unit Test.Search.SearchFiles;

Interface

Uses
  TestFramework,
  Search.Interfaces;

Type
  //
  // Test Class for the ISearchFiles Class Methods.
  //
  TTestISearchFiles = Class(TTestCase)
  Strict Private
    FISearchFiles : ISearchFiles;
  Strict Protected
    Procedure ExceptionHandler(Const strMsg : String);
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAddFileInfo;
    Procedure TestAddSearchRec;
    Procedure TestOrderBy;
    Procedure TestOwnerWidth;
    Procedure TestCount;
    Procedure TestFileInfo;
    Procedure TestHasCompressed;
    Procedure TestPath;
  End;

Implementation

Uses
  Search.FileCls, 
  Search.FilesCls, 
  Winapi.Windows, 
  Search.Types, 
  System.DateUtils, 
  Test.Search.SearchFile;

Const
  dblTolerance = 1.0 / 24.0 / 60.0 / 60.0;

//
// Test Methods for Class ISearchFiles.
//
Procedure TTestISearchFiles.ExceptionHandler(Const strMsg: String);

Begin
  OutputDebugString(PChar(strMsg));
End;

Procedure TTestISearchFiles.Setup;

Var
  iGREPCount : Integer;

Begin
  FISearchFiles := TSearchFiles.Create(ExceptionHandler, '');
  FISearchFiles.Add(
    TSearchFileRec.Create(EncodeDateTime(2018, 1, 7, 14, 30, 15, 0), 123456, 23456,
      [sfaFile, sfaArchive, sfaCompressed], 'David.Hoyle', 'D:\Path\MyFileName.pas'),
    '',
    iGREPCount
  );
  FISearchFiles.Add(
    TSearchFileRec.Create(EncodeDateTime(2018, 1, 8, 15, 35, 20, 0), 234567, 0, [sfaFile, sfaSystem],
      'David.Hoyle', 'D:\Path\MyOtherFileName.pas'),
    '',
    iGREPCount
  );
End;

Procedure TTestISearchFiles.TearDown;

Begin
  FISearchFiles := Nil;
End;

Procedure TTestISearchFiles.TestAddFileInfo;

var
  F: ISearchFile;

Begin
  F := TSearchFile.Create(TSearchFileRec.Create(EncodeDateTime(2018, 1, 9, 16, 40, 25, 0), 1234567, 0, [sfaFile, sfaHidden],
      'David.Hoyle', 'D:\Path\MyOtherOtherFileName.pas'));
  FISearchFiles.Add(F);
  CheckEquals(3, FISearchFiles.Count);
  F := FISearchFiles.FileInfo[2];
  CheckEquals(EncodeDateTime(2018, 1, 9, 16, 40, 25, 0), F.Date, dblTolerance);
  CheckEquals(1234567, F.Size);
  CheckEquals(1234567, F.CompressedSize);
  Check([sfaFile, sfaHidden] = F.Attributes);
  CheckEquals('David.Hoyle', F.Owner);
  CheckEquals('D:\Path\MyOtherOtherFileName.pas', F.FileName);
  CheckEquals(0, F.RegExMatches.Count);
End;

Procedure TTestISearchFiles.TestAddSearchRec;

var
  iGREPCount: Integer;
  F: ISearchFile;

Begin
  FISearchFiles.Add(
    TSearchFileRec.Create(EncodeDateTime(2018, 1, 9, 16, 40, 25, 0), 1234567, 0, [sfaFile, sfaHidden],
      'David.Hoyle', 'D:\Path\MyOtherOtherFileName.pas'),
    '',
    iGREPCount
  );
  CheckEquals(3, FISearchFiles.Count);
  F := FISearchFiles.FileInfo[2];
  CheckEquals(EncodeDateTime(2018, 1, 9, 16, 40, 25, 0), F.Date, dblTolerance);
  CheckEquals(1234567, F.Size);
  CheckEquals(1234567, F.CompressedSize);
  Check([sfaFile, sfaHidden] = F.Attributes);
  CheckEquals('David.Hoyle', F.Owner);
  CheckEquals('D:\Path\MyOtherOtherFileName.pas', F.FileName);
  CheckEquals(0, F.RegExMatches.Count);
End;

Procedure TTestISearchFiles.TestOrderBy;

Begin
  CheckEquals('D:\Path\MyFileName.pas', FISearchFiles.FileInfo[0].FileName);
  CheckEquals('D:\Path\MyOtherFileName.pas', FISearchFiles.FileInfo[1].FileName);
  FISearchFiles.OrderBy(obName, odAscending);
  CheckEquals('D:\Path\MyFileName.pas', FISearchFiles.FileInfo[0].FileName);
  CheckEquals('D:\Path\MyOtherFileName.pas', FISearchFiles.FileInfo[1].FileName);
  FISearchFiles.OrderBy(obName, odDescending);
  CheckEquals('D:\Path\MyOtherFileName.pas', FISearchFiles.FileInfo[0].FileName);
  CheckEquals('D:\Path\MyFileName.pas', FISearchFiles.FileInfo[1].FileName);
  FISearchFiles.OrderBy(obDate, odAscending);
  CheckEquals('D:\Path\MyFileName.pas', FISearchFiles.FileInfo[0].FileName);
  CheckEquals('D:\Path\MyOtherFileName.pas', FISearchFiles.FileInfo[1].FileName);
  FISearchFiles.OrderBy(obDate, odDescending);
  CheckEquals('D:\Path\MyOtherFileName.pas', FISearchFiles.FileInfo[0].FileName);
  CheckEquals('D:\Path\MyFileName.pas', FISearchFiles.FileInfo[1].FileName);
  FISearchFiles.OrderBy(obSize, odAscending);
  CheckEquals('D:\Path\MyFileName.pas', FISearchFiles.FileInfo[0].FileName);
  CheckEquals('D:\Path\MyOtherFileName.pas', FISearchFiles.FileInfo[1].FileName);
  FISearchFiles.OrderBy(obSize, odDescending);
  CheckEquals('D:\Path\MyOtherFileName.pas', FISearchFiles.FileInfo[0].FileName);
  CheckEquals('D:\Path\MyFileName.pas', FISearchFiles.FileInfo[1].FileName);
End;

Procedure TTestISearchFiles.TestOwnerWidth;

Begin
  CheckEquals(11, FISearchFiles.OwnerWidth);
End;

Procedure TTestISearchFiles.TestCount;

Begin
  CheckEquals(2, FISearchFiles.Count);
End;

Procedure TTestISearchFiles.TestFileInfo;

Var
  F: ISearchFile;

Begin
  F := FISearchFiles.FileInfo[0];
  CheckEquals(EncodeDateTime(2018, 1, 7, 14, 30, 15, 0), F.Date, dblTolerance);
  CheckEquals(123456, F.Size);
  CheckEquals(23456, F.CompressedSize);
  Check([sfaFile, sfaArchive, sfaCompressed] = F.Attributes);
  CheckEquals('David.Hoyle', F.Owner);
  CheckEquals('D:\Path\MyFileName.pas', F.FileName);
  CheckEquals(0, F.RegExMatches.Count);
  F := FISearchFiles.FileInfo[1];
  CheckEquals(EncodeDateTime(2018, 1, 8, 15, 35, 20, 0), F.Date, dblTolerance);
  CheckEquals(234567, F.Size);
  CheckEquals(234567, F.CompressedSize);
  Check([sfaFile, sfaSystem] = F.Attributes);
  CheckEquals('David.Hoyle', F.Owner);
  CheckEquals('D:\Path\MyOtherFileName.pas', F.FileName);
  CheckEquals(0, F.RegExMatches.Count);
End;

Procedure TTestISearchFiles.TestHasCompressed;

Begin
  CheckEquals(True, FISearchFiles.HasCompressed);
End;

Procedure TTestISearchFiles.TestPath;

Begin
  CheckEquals('', FISearchFiles.Path);
End;


Initialization
  RegisterTest('ISearchFile', TTestISearchFiles.Suite);
End.
