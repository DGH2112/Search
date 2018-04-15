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
    FDirectories      : TInterfaceList;
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
    Function  Add(Const SearchFileRec : TSearchFileRec; Const strSearchText: String;
      Var iGREPCount : Integer): Boolean; Overload;
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

Var
  Collection: TInterfaceList;

begin
  Result := True;
  Collection := FFiles;
  If sfaDirectory In FileInfo.Attributes Then
    Collection := FDirectories;
  Collection.Add(FileInfo);
end;

(**

  This method added a file and its attributes to the collection.

  @precon  None.
  @postcon Added a file and its attributes to the collection.

  @param   SearchFileRec as a TSearchFileRec as a constant
  @param   strSearchText as a String as a constant
  @param   iGREPCount    as an Integer as a reference
  @return  a Boolean

**)
Function TSearchFiles.Add(Const SearchFileRec : TSearchFileRec; Const strSearchText : String;
  Var iGREPCount : Integer) : Boolean;
  
Var
  FFile : ISearchFile;
  iMatchCount: Integer;
  Collection : TInterfaceList;

Begin
  Result := True;
  FFile := TSearchFile.Create(SearchFileRec);
  If Not FHasCompressed And ([sfaFile, sfaCompressed] <= SearchFileRec.FAttrs) Then
    FHasCompressed := True;
  Collection := FFiles;
  If sfaDirectory In SearchFileRec.FAttrs Then
    Collection := FDirectories;
  If FRegExSearch Then
    Begin
      If Not (sfaDirectory In SearchFileRec.FAttrs) Then
        Begin
          iMatchCount := GrepFile(FFile, SearchFileRec.FName, strSearchText);
          If iMatchCount > 0 Then
            Begin
              Collection.Add(FFile);
              Inc(iGREPCount, iMatchCount);
            End Else
              Result := False;
        End Else
          Result := False;
      If Not Result Then
        FFile := Nil;
    End Else
      Collection.Add(FFile);
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
  FDirectories := TInterfaceList.Create;
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
  FDirectories.Free;
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
  Result := FDirectories.Count + FFiles.Count;
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
  If iIndex < FDirectories.Count Then
    Result := FDirectories.Items[iIndex] As ISearchFile
  Else
    Result := FFiles.Items[iIndex - FDirectories.Count] As ISearchFile;
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

  (**

    This method order the given collection by the OrderBy parameter.

    @precon  Collection must be a valid instance.
    @postcon The list is ordered.

    @param   Collection as a TInterfaceList as a constant

  **)
  Procedure OrderCollection(Const Collection : TInterfaceList);

    (**

      This method attempts to find the minimum value for the select sort.

      @precon  None.
      @postcon Returns the minium index value if found else return -1.

      @param   iOuter as an Integer as a constant
      @param   iInner as an Integer as a constant
      @return  an Integer

    **)
    Function FindMin(Const iOuter, iInner : Integer) : Integer;

    Var
      iFirst, iSecond : Integer;
      FFI: ISearchFile;
      SFI: ISearchFile;
      
    Begin
      Result := -1;
      If OrderDirection = odAscending Then
        Begin
          iFirst := iOuter;
          iSecond := iInner;
        End Else
        Begin
          iFirst := iInner;
          iSecond := iOuter;
        End;
      FFI := Collection.Items[iFirst] As ISearchFile;
      SFI := Collection.Items[iSecond] As ISearchFile;
      Case OrderBy Of
        obName:  If CompareText(SFI.FileName, FFI.FileName) < 0 Then Result := iInner;
        obDate:  If SFI.Date < FFI.Date                         Then Result := iInner;
        obSize:  If SFI.Size < FFI.Size                         Then Result := iInner;
        obOwner: If CompareText(SFI.Owner, FFI.Owner) < 0       Then Result := iInner;
      End;
    End;

  Var
    iOuter, iInner : Integer;
    iMin : Integer;
    iCount: Integer;

  Begin
    iCount := Collection.Count;
    For iOuter := 0 To iCount - 1 Do
      For iInner := iOuter + 1 To iCount - 1 Do
        Begin
          iMin := FindMin(iOUter, iInner);
          If iMin > -1 Then
            Collection.Exchange(iOuter, iMin);
        End;
  End;

Begin
  OrderCollection(FDirectories);
  OrderCollection(FFiles);
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
