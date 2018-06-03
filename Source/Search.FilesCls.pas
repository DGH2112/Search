(**

  This module contains the classes for handling the file information in each
  directory searched.

  @Version 1.0
  @Author  David Hoyle
  @Date    03 Jun 2018

**)
Unit Search.FilesCls;

Interface

Uses
  SysUtils,
  Classes,
  Search.Types, 
  System.RegularExpressions, 
  Search.Interfaces, 
  System.Generics.Collections;

Type
  (** This is a procedure declaration for an exception event handler. **)
  TFilesExceptionHandler = Procedure(Const strException : String) Of Object;

  (** A class to hold a collection of files. **)
  TSearchFiles = Class(TInterfacedObject, ISearchFiles)
  Private
    FFiles            : TList<ISearchFile>;
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
    Procedure OrderBy;
    Function  OwnerWidth: Integer;
    Function  Add(Const FileInfo : ISearchFile) : Boolean; Overload;
    // General Methods
    Function GrepFile(Const SearchFile : ISearchFile; Const strSearchText : String) : Integer;
  Public
    Constructor Create(Const OrderBy : TOrderBy; Const OrderDirection : TOrderDirection;
      Const ExceptionHandler : TFilesExceptionHandler; Const strRegExSearchText : String);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  RegularExpressionsCore,
  Windows, 
  Search.FileCls, 
  System.Generics.Defaults;

Type
  (** This isn an implementation of IComparer to allow sorting of the file collection. **)
  TSearchFileComparer = Class(TComparer<ISearchFile>)
  Strict Private
    FOrderBy : TOrderBy;
    FOrderDirection : TOrderDirection;
  Strict Protected
  Public
    Function Compare(Const Left, Right : ISearchFile) : Integer; Override;
    Constructor Create(Const OrderBy : TOrderBy; Const OrderDirection : TOrderDirection);
  End;

(**

  This is an IComparer Compare method for the file list.

  @precon  None.
  @postcon Order the files list first by path (if it exists) and then by the order asked for.

  @param   Left  as an ISearchFile as a constant
  @param   Right as an ISearchFile as a constant
  @return  an Integer

**)
Function TSearchFileComparer.Compare(Const Left, Right: ISearchFile): Integer;

Const
  iSecsInDay = 24 * 60 * 60;
  
Begin
  Result := CompareText(ExtractFilePath(Left.FileName), ExtractFilePath(Right.FileName));
  If Result = 0 Then
    Case FOrderBy Of
      obName:  Result := CompareText(Left.FileName, Right.FileName);
      obSize:  Result := Left.Size - Right.Size;
      obDate:  Result := Trunc(Left.Date  * iSecsInDay) - Trunc(Right.Date * iSecsInDay);
      obOwner: Result := CompareText(Left.Owner, Right.Owner);
    End;
  If FOrderDirection = odDescending Then
    Result := -Result;
End;

(**

  A constructor for the TSearchFileComparer class.

  @precon  None.
  @postcon Sets the sorting order type and direction.

  @param   OrderBy        as a TOrderBy as a constant
  @param   OrderDirection as a TOrderDirection as a constant

**)
Constructor TSearchFileComparer.Create(Const OrderBy: TOrderBy; Const OrderDirection: TOrderDirection);

Begin
  FOrderBy := OrderBy;
  FOrderDirection := OrderDirection;
End;

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

Begin
  Result := True;
  FFile := TSearchFile.Create(SearchFileRec);
  If Not FHasCompressed And ([sfaFile, sfaCompressed] <= SearchFileRec.FAttrs) Then
    FHasCompressed := True;
  If FRegExSearch Then
    Begin
      If Not (sfaDirectory In SearchFileRec.FAttrs) Then
        Begin
          iMatchCount := GrepFile(FFile, strSearchText);
          If iMatchCount > 0 Then
            Begin
              FFiles.Add(FFile);
              Inc(iGREPCount, iMatchCount);
            End Else
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

  @param   OrderBy            as a TOrderBy as a constant
  @param   OrderDirection     as a TOrderDirection as a constant
  @param   ExceptionHandler   as a TFilesExceptionHandler as a constant
  @param   strRegExSearchText as a String as a constant

**)
Constructor TSearchFiles.Create(Const OrderBy : TOrderBy; Const OrderDirection : TOrderDirection;
      Const ExceptionHandler : TFilesExceptionHandler; Const strRegExSearchText : String);

Begin
  FFiles := TList<ISearchFile>.Create(TSearchFileComparer.Create(OrderBy, OrderDirection));
  FExceptionHandler := ExceptionHandler;
  FRegExSearch := False;
  FHasCompressed := False;
  If strRegExSearchText <> '' Then
    Begin
      FRegEx := TRegEx.Create(strRegExSearchText, [roIgnoreCase, roCompiled, roSingleLine]);
      FRegExSearch := True;
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
  Result := FFiles.Items[iIndex] As ISearchFile;
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
  @param   strSearchText as a String as a constant
  @return  an Integer

**)
Function TSearchFiles.GrepFile(Const SearchFile : ISearchFile; Const strSearchText : String) : Integer;

Var
  iLine: Integer;
  slFile : TStringList;
  M: TMatchCollection;

Begin
  slFile := TStringList.Create;
  Try
    slFile.Text := strSearchText;
    For iLine := 0 To slFile.Count - 1 Do
      Begin
        M := FRegEx.Matches(slFile[iLine]);
    If M.Count > 0 Then
          SearchFile.AddRegExLine(iLine + 1, M);
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


**)
Procedure TSearchFiles.OrderBy;

Begin
  FFiles.Sort;
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
