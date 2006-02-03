(**
  
  This module contains the classes for handling the file information in each
  directory searched.

  @Version 1.0
  @Author  David Hoyle
  @Date    03 Feb 2006
  
**)
Unit FileHandling;

Interface

Uses
  SysUtils, Contnrs;

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
  Public
    (**
      A property to read and write the files date and time.
      @precon  None.
      @postcon None.
      @return  a TDateTime
    **)
    Property Date : TDateTime Read FDate Write FDate;
    (**
      A property to read and write the files size.
      @precon  None.
      @postcon None.
      @return  an Int64
    **)
    Property Size : Int64 Read FSize Write FSize;
    (**
      A property to read and write the files Attributes.
      @precon  None.
      @postcon None.
      @return  a String
    **)
    Property Attr : String Read FAttr Write FAttr;
    (**
      A property to read and write the files owner.
      @precon  None.
      @postcon None.
      @return  a String
    **)
    Property Owner : String Read FOwner Write FOwner;
    (**
      A property to read and write the files name.
      @precon  None.
      @postcon None.
      @return  a String
    **)
    Property Name : String Read FName Write FName;
  End;

  (** A class to hold a collection of files. **)
  TFiles = Class
  Private
    FFiles : TObjectList;
  Protected
    Function GetFile(iIndex : Integer) : TFile;
    Function GetCount : Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(dtDate : TDateTime; iSize : Int64; strAttr, strOwner,
      strName : String);
    (**
      A property to return a specific file from the collection.
      @precon  iIndex must be a valid index.
      @postcon The file identified ny the index is returned.
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

  This is the constructor method for the TFiles class.

  @precon  None.
  @postcon Creates the file list.

**)
Constructor TFiles.Create;

Begin
  FFiles := TObjectList.Create(True);
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

  @param   dtDate   as a TDateTime
  @param   iSize    as an Int64
  @param   strAttr  as a String
  @param   strOwner as a String
  @param   strName  as a String

**)
Procedure TFiles.Add(dtDate : TDateTime; iSize : Int64; strAttr, strOwner,
  strName : String);

Var
  FFile : TFile;

Begin
  FFile := TFile.Create;
  FFiles.Add(FFile);
  FFile.Date := dtDate;
  FFile.Size := iSize;
  FFile.Attr := strAttr;
  FFile.Owner := strOwner;
  FFile.Name := strName;
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
            If CompareText(FileInfo[iSecond].Name, FileInfo[iFirst].Name) < 0 Then
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
