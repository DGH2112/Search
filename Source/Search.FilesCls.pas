(**

  This module contains the classes for handling the file information in each
  directory searched.

  @Version 1.0
  @Author  David Hoyle
  @Date    15 Apr 2018

**)
Unit Search.FilesCls;

Interface

Uses
  SysUtils,
  Classes,
  Contnrs,
  Search.Types, 
  System.RegularExpressions, 
  Search.Interfaces;

Type
  (** This is a procedure declaration for an exception event handler. **)
  TFilesExceptionHandler = Procedure(Const strException : String) Of Object;

  (** A class to hold a collection of files. **)
  TSearchFiles = Class(TInterfacedObject, ISearchFiles)
  Private
    FFiles            : TInterfaceList;
    FExceptionHandler : TFilesExceptionHandler;
    FPath             : String;
    FRegEx            : TRegEx;
    FRegExSearch      : Boolean;
    FHasCompressed    : Boolean;
  Protected
    // ISearchFiles
    Function  GetCount : Integer;
    Function  GetFile(Const iIndex: Integer): ISearchFile;
    Function  GetPath: String;
    Function  GetHasCompressed : Boolean;
    Procedure SetPath(Const strPath: String);
    Function  Add(Const dtDate: TDateTime; Const iSize, iCompressedSize: Int64;
      Const setAttrs : TSearchFileAttrs; Const strOwner, strName, strSearchText: String): Boolean;
      Overload;
    Procedure OrderBy(Const OrderBy: TOrderBy; Const OrderDirection: TOrderDirection);
    Function  OwnerWidth: Integer;
    Function  Add(Const FileInfo : ISearchFile) : Boolean; Overload;
    // General Methods
    Function GrepFile(Const SearchFile : ISearchFile; Const strName, strSearchText : String) : Integer;
  Public
    Constructor Create(Const ExceptionHandler : TFilesExceptionHandler;
      Const strRegExSearchText : String);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  RegularExpressionsCore,
  Windows, 
  Search.FileCls;

(**

  This method adds a TFile to the files collection.

  @precon  FileInfo must be a valid instance.
  @postcon Adds a TFile to the files collection.

  @param   FileInfo as an ISearchFile as a constant
  @return  a Boolean

**)
function TSearchFiles.Add(Const FileInfo: ISearchFile): Boolean;

begin
  Result := True;
  FFiles.Add(FileInfo);
end;

(**

  This method added a file and its attributes to the collection.

  @precon  None.
  @postcon Added a file and its attributes to the collection.

  @param   dtDate          as a TDateTime as a constant
  @param   iSize           as an Int64 as a constant
  @param   iCompressedSize as an Int64 as a constant
  @param   setAttrs        as a TSearchFileAttrs as a constant
  @param   strOwner        as a String as a constant
  @param   strName         as a String as a constant
  @param   strSearchText   as a String as a constant
  @return  a Boolean

**)
Function TSearchFiles.Add(Const dtDate : TDateTime; Const iSize, iCompressedSize : Int64;
  Const setAttrs : TSearchFileAttrs; Const strOwner, strName, strSearchText : String) : Boolean;

Var
  FFile : ISearchFile;
  iMatchCount: Integer;

Begin
  Result := True;
  FFile := TSearchFile.Create(dtDate, iSize, iCompressedSize, setAttrs, strOwner, strName);
  If Not FHasCompressed And ([sfaFile, sfaCompressed] <= setAttrs) Then
    FHasCompressed := True;
  If FRegExSearch Then
    Begin
      If Not (sfaDirectory In setAttrs) Then
        Begin
          iMatchCount := GrepFile(FFile, strName, strSearchText);
          If iMatchCount > 0 Then
            FFiles.Add(FFile)
          Else
            Result := False;
        End Else
          Result := False;
      If Not Result Then
        FFile := Nil;
    End Else
      FFiles.Add(FFile);
End;

(**

  This is the constructor method for the TFiles class.

  @precon  None.
  @postcon Creates the file list.

  @param   ExceptionHandler   as a TFilesExceptionHandler as a constant
  @param   strRegExSearchText as a String as a constant

**)
Constructor TSearchFiles.Create(Const ExceptionHandler : TFilesExceptionHandler;
  Const strRegExSearchText : String);

Const
  strRegExError = 'Reg Ex Error: %s ("%s")';

Begin
  FFiles := TInterfaceList.Create;
  FExceptionHandler := ExceptionHandler;
  FRegExSearch := False;
  FHasCompressed := False;
  Try
    If strRegExSearchText <> '' Then
      Begin
        FRegEx := TRegEx.Create(strRegExSearchText, [roIgnoreCase, roCompiled, roSingleLine]);
        FRegExSearch := True;
      End;
  Except
    On E : ERegularExpressionError Do
      Begin
        If Assigned(FExceptionHandler) Then
          FExceptionHandler(Format(strRegExError, [E.Message, strRegExSearchText]));
      End;
  End;
End;

(**

  This is the destructor method for the TFiles class.

  @precon  None.
  @postcon Frees the list of files.

**)
Destructor TSearchFiles.Destroy;

Begin
  FFiles.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of files in the collectionn.

  @return  an Integer

**)
Function TSearchFiles.GetCount : Integer;

Begin
  Result := FFiles.Count;
End;

(**

  This is a getter method for the File property.

  @precon  iIndex must be a valid index.
  @postcon Returns the file specified by the index.

  @param   iIndex as an Integer as a constant
  @return  an ISearchFile

**)
Function TSearchFiles.GetFile(Const iIndex : Integer) : ISearchFile;

Begin
  Result :=  FFiles.Items[iIndex] As TSearchFile;
End;

(**

  This is a getter method for the HasCompressed property.

  @precon  None.
  @postcon Returns whether the collection contains any compressed files.

  @return  a Boolean

**)
Function TSearchFiles.GetHasCompressed: Boolean;

Begin
  Result := FHasCompressed;
End;

(**

  This is a getter method for the Path property.

  @precon  None.
  @postcon Returns the Path of the file collection.

  @return  a String

**)
Function TSearchFiles.GetPath: String;

Begin
  Result := FPath;
End;

(**

  This method searches the given text for match regular expressions.

  @precon  None.
  @postcon This method searches the given file for regular expression matches and retuns the number of
           matches found.

  @param   SearchFile    as an ISearchFile as a constant
  @param   strName       as a String as a constant
  @param   strSearchText as a String as a constant
  @return  an Integer

**)
Function TSearchFiles.GrepFile(Const SearchFile : ISearchFile; Const strName,
  strSearchText : String) : Integer;

Var
  iLine: Integer;
  slFile : TStringList;
  M: TMatchCollection;

Begin
  slFile := TStringList.Create;
  Try
    slFile.Text := strSearchText;
    Try
      For iLine := 0 To slFile.Count - 1 Do
        Begin
          M := FRegEx.Matches(slFile[iLine]);
          If M.Count > 0 Then
            SearchFile.AddRegExLine(iLine + 1, M);
        End;
    Except
      On E : ERegularExpressionError Do
        If Assigned(FExceptionHandler) Then
          FExceptionHandler(Format('%s (%s)', [E.Message, strName]));
    End;
  Finally
    slFile.Free;
  End;
  Result := SearchFile.RegExMatches.Count;
End;

(**

  This method orders the files in the collection by the given direction and attribute.

  @precon  None.
  @postcon Orders the files in the collection by the given direction and attribute.

  @param   OrderBy        as a TOrderBy as a constant
  @param   OrderDirection as a TOrderDirection as a constant

**)
Procedure TSearchFiles.OrderBy(Const OrderBy : TOrderBy; Const OrderDirection : TOrderDirection);

Var
  i, j : Integer;
  iMin : Integer;
  iFirst, iSecond : Integer;
  iCount: Integer;
  FFI: ISearchFile;
  SFI: ISearchFile;

Begin
  iCount := GetCount;
  For i := 0 To iCount - 1 Do
    For j := i + 1 To iCount - 1 Do
      Begin
        iMin := -1;
        If OrderDirection = odAscending Then
          Begin
            iFirst := i;
            iSecond := j;
          End Else
          Begin
            iFirst := j;
            iSecond := i;
          End;
        FFI := GetFile(iFirst);
        SFI := GetFile(iSecond);
        Case OrderBy Of
          obName:      If CompareText(SFI.FileName, FFI.FileName) < 0 Then iMin := j;
          obDate:      If SFI.Date < FFI.Date                         Then iMin := j;
          obSize:      If SFI.Size < FFI.Size                         Then iMin := j;
          obOwner:     If CompareText(SFI.Owner, FFI.Owner) < 0       Then iMin := j;
        End;
        If iMin > -1 Then FFiles.Exchange(i, iMin);
      End;
End;

(**

  This method determines the width of the widthest owner.

  @precon  None.
  @postcon Determines the width of the widthest owner.

  @return  an Integer

**)
Function TSearchFiles.OwnerWidth : Integer;

Var
  i : Integer;
  FI: ISearchFile;

Begin
  Result := 0;
  For i := 0 To GetCount - 1 Do
    Begin
      FI := GetFile(i);
      If Length(FI.Owner) > Result Then
        Result := Length(FI.Owner);
    End;
End;

(**

  This is a setter method for the Path property.

  @precon  None.
  @postcon Set the path for the file collection.

  @param   strPath as a String as a constant

**)
Procedure TSearchFiles.SetPath(Const strPath: String);

Begin
  FPath := strPath;
End;

End.
