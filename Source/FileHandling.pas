(**

  This module contains the classes for handling the file information in each
  directory searched.

  @Version 1.0
  @Author  David Hoyle
  @Date    07 Mar 2010

**)
Unit FileHandling;

Interface

Uses
  SysUtils, Classes, Contnrs;

Type
  (** An enumerate to define the order for sorting the files. **)
  TOrderBy = (obNone, obName, obSize, obDate, obOwner, obAttribute);

  (** An enumberate to define whether the files should be ascending or
      descending **)
  TOrderDirection = (odAscending, odDescending);

  (** A class to hold a files information. **)
  TFile = Class
  Private
    FDate : TDateTime;
    FSize : Int64;
    FAttr : String;
    FOwner : String;
    FName : String;
    FGREPLines : TStringList;
    function GetGREPText(iIndex: Integer): String;
    function GetGREPLine(iIndex: Integer): Integer;
    function GetGREPLines: Integer;
  Protected
  Public
    Constructor Create(dtDate : TDateTime; iSize : Int64; strAttr, strOwner,
      strName : String);
    Procedure AddGREPLine(strText : String; iLine : Integer);
    Destructor Destroy; Override;
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
    (**
      A property to returns the number of GREP lines found.
      @precon  None.
      @postcon Returns the number of GREP lines found.
      @return  an Integer
    **)
    Property GREPLines : Integer Read GetGREPLines;
    (**
      A property to returns specific indexed GREP Text.
      @precon  iIndex must be a valid index.
      @postcon Returns an indexed GREP Text for the file.
      @param   iIndex as       an Integer
      @return  a String
    **)
    Property GREPText[iIndex : Integer] : String Read GetGREPText;
    (**
      A property to returns specific indexed GREP line.
      @precon  iIndex must be a valid index.
      @postcon Returns an indexed GREP line for the file.
      @param   iIndex as       an Integer
      @return  an Integer
    **)
    Property GREPLine[iIndex : Integer] : Integer Read GetGREPLine;
  End;

  (** This is a procedure declaration for an exception event handler. **)
  TFilesExceptionHandler = Procedure(strException : String) Of Object;

  (** A class to hold a collection of files. **)
  TFiles = Class
  Private
    FFiles : TObjectList;
    FExceptionHandler : TFilesExceptionHandler;
  Protected
    Function GetFile(iIndex : Integer) : TFile;
    Function GetCount : Integer;
  Public
    Constructor Create(ExceptionHandler : TFilesExceptionHandler);
    Destructor Destroy; Override;
    Function Add(dtDate : TDateTime; iSize : Int64; strAttr, strOwner,
      strName, strGREPText, strGREPSearchText : String;
      iFileAttrs : Integer) : Boolean;
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
    Function OwnerWidth : Integer;
    Procedure OrderBy(OrderBy : TOrderBy; OrderDirection : TOrderDirection);
  End;

Implementation

(**

  This method adds the given text as a GREP line.

  @precon  None.
  @postcon Adds the given text as a GREP line.

  @param   strText as a String
  @param   iLine   as an Integer

**)
procedure TFile.AddGREPLine(strText: String; iLine : Integer);
begin
  FGREPLines.AddObject(strText, TObject(iLine));
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
  FGREPLines := TStringList.Create;
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
  FGREPLines.Free;
  inherited Destroy;
end;

(**

  This is a getter method for the GREPText property.

  @precon  iIndex must be a valid index.
  @postcon Returns the GREP text specified by the index.

  @param   iIndex as an Integer
  @return  a String

**)
function TFile.GetGREPText(iIndex: Integer): String;
begin
  Result := FGREPLines[iIndex];
end;

(**

  This is a getter method for the GREPLine property.

  @precon  iIndex must be a valid index.
  @postcon Returns the GREP line specified by the index.

  @param   iIndex as an Integer
  @return  an Integer

**)
function TFile.GetGREPLine(iIndex: Integer): Integer;
begin
  Result := Integer(FGREPLines.Objects[iIndex]);
end;

(**

  This is a getter method for the GREPLines property.

  @precon  None.
  @postcon Returns the number of GREP lines in the file.

  @return  an Integer

**)
function TFile.GetGREPLines: Integer;
begin
  Result := FGREPLines.Count;
end;

(**

  This is the constructor method for the TFiles class.

  @precon  None.
  @postcon Creates the file list.

  @param   ExceptionHandler as a TFilesExceptionHandler

**)
Constructor TFiles.Create(ExceptionHandler : TFilesExceptionHandler);

Begin
  FFiles := TObjectList.Create(True);
  FExceptionHandler := ExceptionHandler;
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

  @param   dtDate            as a TDateTime
  @param   iSize             as an Int64
  @param   strAttr           as a String
  @param   strOwner          as a String
  @param   strName           as a String
  @param   strGREPText       as a String
  @param   strGREPSearchText as a String
  @param   iFileAttrs        as an Integer
  @return  a Boolean

**)
Function TFiles.Add(dtDate : TDateTime; iSize : Int64; strAttr, strOwner,
  strName, strGREPText, strGREPSearchText : String;
  iFileAttrs : Integer) : Boolean;

Var
  FFile : TFile;
  slFile: TStringList;
  iLine: Integer;

Begin
  Result := True;
  FFile := TFile.Create(dtDate, iSize, strAttr, strOwner, strName);
  strGREPText := Lowercase(strGREPText);
  If strGREPText <> '' Then
    Begin
      If  iFileAttrs And faDirectory = 0 Then
        Begin
          slFile := TStringList.Create;
          Try
            Try
              slFile.Text := strGREPSearchText;
              For iLine := 0 To slFile.Count - 1 Do
                If Pos(strGREPText, Lowercase(slFile[iLine])) > 0 Then
                  FFile.AddGREPLine(slFile[iLine], iLine + 1);
            Except
              On E : Exception Do
                If Assigned(FExceptionHandler) Then
                  FExceptionHandler(Format('%s (%s)', [E.Message, strName]));
            End;
          Finally
            slFile.Free;
          End;
          If FFile.GetGREPLines > 0 Then
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
