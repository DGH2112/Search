(**

  This module contains the classes for handling the file information in each
  directory searched.

  @Version 1.0
  @Author  David Hoyle
  @Date    05 Apr 2018

**)
Unit FileHandling;

Interface

Uses
  SysUtils,
  Classes,
  Contnrs,
  RegularExpressions,
  Generics.Collections;

Type
  (** An enumerate to define the order for sorting the files. **)
  TOrderBy = (obNone, obName, obSize, obDate, obOwner, obAttribute);

  (** An enumberate to define whether the files should be ascending or
      descending **)
  TOrderDirection = (odAscending, odDescending);

  TRegExMatch = Record
    FIndex : Integer;
    FLength : Integer;
  End;

  TArrayOfMatch = Array Of TRegExMatch;

  TRegExMatches = Record
  Strict Private
    FLineNum    : Integer;
    FMatches    : TArrayOfMatch;
    FCount      : Integer;
    Function GetMatch(iIndex : Integer) : TRegExMatch;
  Public
    Constructor Create(iLine : Integer; Matches : TMatchCollection);
    Property LineNum : Integer Read FLineNum;
    Property Count : Integer Read FCount;
    Property Group[iIndex : Integer] : TRegExMatch Read GetMatch;
  End;

  (** A class to hold a files information. **)
  TFile = Class
  Strict Private
    FDate : TDateTime;
    FSize : Int64;
    FAttr : String;
    FOwner : String;
    FName : String;
    FRegExLines : TList<TRegExMatches>;
  Strict Protected
  Public
    Constructor Create(dtDate : TDateTime; iSize : Int64; strAttr, strOwner,
      strName : String);
    Procedure AddRegExLine(iLine : Integer; Matches : TMatchCollection);
    Destructor Destroy; Override;
    Function Clone : TFile;
    (**
      A property to read and write the files date and time.
      @precon  None.
      @postcon None.
      @return  a TDateTime
    **)
    Property Date : TDateTime Read FDate;
    (**
      A property to read and write the files size.
      @precon  None.
      @postcon None.
      @return  an Int64
    **)
    Property Size : Int64 Read FSize;
    (**
      A property to read and write the files Attributes.
      @precon  None.
      @postcon None.
      @return  a String
    **)
    Property Attr : String Read FAttr;
    (**
      A property to read and write the files owner.
      @precon  None.
      @postcon None.
      @return  a String
    **)
    Property Owner : String Read FOwner;
    (**
      A property to read and write the files name.
      @precon  None.
      @postcon None.
      @return  a String
    **)
    Property FileName : String Read FName;
    Property RegExMatches : TList<TRegExMatches> Read FRegExLines;
  End;

  (** This is a procedure declaration for an exception event handler. **)
  TFilesExceptionHandler = Procedure(strException : String) Of Object;

  (** A class to hold a collection of files. **)
  TFiles = Class
  Private
    FFiles : TObjectList;
    FExceptionHandler : TFilesExceptionHandler;
    FPath : String;
    FRegEx : TRegEx;
    FRegExSearch : Boolean;
  Protected
    Function GetFile(iIndex : Integer) : TFile;
    Function GetCount : Integer;
  Public
    Constructor Create(ExceptionHandler : TFilesExceptionHandler; strRegExSearchText : String);
    Destructor Destroy; Override;
    Function Add(dtDate : TDateTime; iSize : Int64; strAttr, strOwner,
      strName, strSearchText : String; iFileAttrs : Integer) : Boolean; Overload;
    Function OwnerWidth : Integer;
    Procedure OrderBy(OrderBy : TOrderBy; OrderDirection : TOrderDirection);
    Function Add(FileInfo : TFile) : Boolean; Overload;
    (**
      A property to return a specific file from the collection.
      @precon  iIndex must be a valid index.
      @postcon The file identified any the index is returned.
      @param   iIndex as       an Integer
      @return  a TFile
    **)
    Property FileInfo[iIndex : Integer] : TFile Read GetFile;
    (**
      A property to return the number of files in the collection.
      @precon  None.
      @postcon Returns the number of files in the collection.
      @return  an Integer
    **)
    Property Count : Integer Read GetCount;
    (**
      A property to read and write a path for the collection of files.
      @precon  None.
      @postcon Gets and sets the path of the file collection.
      @return  a String
    **)
    Property Path : String Read FPath Write FPath;
  End;

Implementation

Uses
  RegularExpressionsCore,
  Windows;

{ TRegExRecord }

Constructor TRegExMatches.Create(iLine: Integer; Matches : TMatchCollection);

Var
  iMatch: Integer;

Begin
  FLineNum := iLine;
  SetLength(FMatches, Matches.Count);
  For iMatch := 0 To Matches.Count - 1 Do
    Begin
      FMatches[iMatch].FIndex := Matches.Item[iMatch].Index;
      FMatches[iMatch].FLength := Matches.Item[iMatch].Length;
    End;
  FCount := Matches.Count;
End;

Function TRegExMatches.GetMatch(iIndex: Integer): TRegExMatch;

Begin
  Result := FMatches[iIndex];
End;

(**

  This method adds the given text as a RegEx line.

  @precon  None.
  @postcon Adds the given text as a RegEx line.

  @param   strText as a String
  @param   iLine   as an Integer
  @param   iIndex  as an Integer
  @param   iLength as an Integer

**)
Procedure TFile.AddRegExLine(iLine : Integer; Matches : TMatchCollection);

begin
  FRegExLines.Add(TRegExMatches.Create(iLine, Matches));
end;

(**

  This method returns a clone (copy) of the TFile instance.

  @precon  None.
  @postcon Returns a clone (copy) of the TFile instance.

  @return  a TFile

**)
function TFile.Clone: TFile;
var
  R: TRegExMatches;

begin
  Result := TFile.Create(FDate, FSize, FAttr, FOwner, ExtractFileName(FName));
  For R In FRegExLines Do
    Result.FRegExLines.Add(R);
end;

(**

  This is the constructor method for the TFile class.

  @precon  None.
  @postcon Constructs a TFile class.

  @param   dtDate   as a TDateTime
  @param   iSize    as an Int64
  @param   strAttr  as a String
  @param   strOwner as a String
  @param   strName  as a String

**)
constructor TFile.Create(dtDate: TDateTime; iSize: Int64; strAttr, strOwner,
  strName: String);

begin
  inherited Create;
  FRegExLines := TList<TRegExMatches>.Create;
  FDate := dtDate;
  FSize := iSize;
  FAttr := strAttr;
  FOwner := strOwner;
  FName := strName;
end;

(**

  This is the destructor method for the TFile class.

  @precon  None.
  @postcon Destroys the class instance.

**)
destructor TFile.Destroy;
begin
  FRegExLines.Free;
  inherited Destroy;
end;

(**

  This method adds a TFile to the files collection.

  @precon  FileInfo must be a valid instance.
  @postcon Adds a TFile to the files collection.

  @param   FileInfo as a TFile
  @return  a Boolean

**)
function TFiles.Add(FileInfo: TFile): Boolean;
begin
  Result := True;
  FFiles.Add(FileInfo);
end;

(**

  This is the constructor method for the TFiles class.

  @precon  None.
  @postcon Creates the file list.

  @param   ExceptionHandler   as a TFilesExceptionHandler
  @param   strRegExSearchText as a String

**)
Constructor TFiles.Create(ExceptionHandler : TFilesExceptionHandler; strRegExSearchText : String);

Const
  strRegExError = 'Reg Ex Error: %s ("%s")';

Begin
  FFiles := TObjectList.Create(True);
  FExceptionHandler := ExceptionHandler;
  FRegExSearch := False;
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
Destructor TFiles.Destroy;

Begin
  FFiles.Free;
  Inherited Destroy;
End;

(**

  This method added a file and its attributes to the collection.

  @precon  None.
  @postcon Added a file and its attributes to the collection.

  @param   dtDate        as a TDateTime
  @param   iSize         as an Int64
  @param   strAttr       as a String
  @param   strOwner      as a String
  @param   strName       as a String
  @param   strSearchText as a String
  @param   iFileAttrs    as an Integer
  @return  a Boolean

**)
Function TFiles.Add(dtDate : TDateTime; iSize : Int64; strAttr, strOwner,
  strName, strSearchText : String; iFileAttrs : Integer) : Boolean;

Var
  FFile : TFile;
  iLine: Integer;
  slFile : TStringList;
  M: TMatchCollection;

Begin
  Result := True;
  FFile := TFile.Create(dtDate, iSize, strAttr, strOwner, strName);
  If FRegExSearch Then
    Begin
      If  iFileAttrs And faDirectory = 0 Then
        Begin
          slFile := TStringList.Create;
          Try
            slFile.Text := strSearchText;
            Try
              For iLine := 0 To slFile.Count - 1 Do
                Begin
                  M := FRegEx.Matches(slFile[iLine]);
                  If M.Count > 0 Then
                    FFile.AddRegExLine(iLine + 1, M);
                End;
            Except
              On E : ERegularExpressionError Do
                If Assigned(FExceptionHandler) Then
                  FExceptionHandler(Format('%s (%s)', [E.Message, strName]));
            End;
          Finally
            slFile.Free;
          End;
          If FFile.RegExMatches.Count > 0 Then
            FFiles.Add(FFile)
          Else
            Result := False;
        End Else
          Result := False;
      If Not Result Then
        FFile.Free;
    End Else
      FFiles.Add(FFile);
End;

(**

  This is a getter method for the File property.

  @precon  iIndex must be a valid index.
  @postcon Returns the file specified by the index.

  @param   iIndex as an Integer
  @return  a TFile

**)
Function TFiles.GetFile(iIndex : Integer) : TFile;

Begin
  Result :=  FFiles.Items[iIndex] As TFile;
End;

(**

  This is a getter method for the Count property.

  @precon  None.
  @postcon Returns the number of files in the collectionn.

  @return  an Integer

**)
Function TFiles.GetCount : Integer;

Begin
  Result := FFiles.Count;
End;

(**

  This method determines the width of the widthest owner.

  @precon  None.
  @postcon Determines the width of the widthest owner.

  @return  an Integer

**)
Function TFiles.OwnerWidth : Integer;

Var
  i : Integer;

Begin
  Result := 0;
  For i := 0 To Count - 1 Do
    If Length(FileInfo[i].Owner) > Result Then
      Result := Length(FileInfo[i].Owner);
End;

(**

  This method orders the files in the collection by the given direction and
  attribute.

  @precon  None.
  @postcon Orders the files in the collection by the given direction and
  attribute.

  @param   OrderBy        as a TOrderBy
  @param   OrderDirection as a TOrderDirection

**)
Procedure TFiles.OrderBy(OrderBy : TOrderBy; OrderDirection : TOrderDirection);

Var
  i, j : Integer;
  iMin : Integer;
  iFirst, iSecond : Integer;

Begin
  For i := 0 To Count - 1 Do
    For j := i + 1 To Count - 1 Do
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
        Case OrderBy Of
          obName:
            If CompareText(FileInfo[iSecond].FileName, FileInfo[iFirst].FileName) < 0 Then
              iMin := j;
          obDate:
            If FileInfo[iSecond].Date < FileInfo[iFirst].Date Then
              iMin := j;
          obSize:
            If FileInfo[iSecond].Size < FileInfo[iFirst].Size Then
              iMin := j;
          obAttribute:
            If CompareText(FileInfo[iSecond].Attr,FileInfo[iFirst].Attr) < 0 Then
              iMin := j;
          obOwner:
            If CompareText(FileInfo[iSecond].Owner, FileInfo[iFirst].Owner) < 0 Then
              iMin := j;
        End;
        If iMin > -1 Then FFiles.Exchange(i, iMin);
      End;
End;

End.
