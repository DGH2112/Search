(**
  
  This module contains a class which imlpements the ISearchFile interface to provide a class that holds
  information about a file found in the search.

  @Author  David Hoyle
  @Version 1.0
  @Date    07 Apr 2018
  
**)
Unit Search.FileCls;

Interface

Uses
  Search.RegExMatches, 
  RegularExpressions,
  System.Generics.Collections, 
  Search.Interfaces;

Type
  (** A class to hold a files information. **)
  TSearchFile = Class(TInterfacedObject, ISearchFile)
  Strict Private
    FDate : TDateTime;
    FSize : Int64;
    FAttr : String;
    FOwner : String;
    FName : String;
    FRegExMatches : TList<TRegExMatches>;
  Strict Protected
    // ISearchFile
    Procedure AddRegExLine(Const iLine: Integer; Const Matches: TMatchCollection);
    Function  Clone: ISearchFile;
    Function  GetAttr: String;
    Function  GetDate: TDateTime;
    Function  GetName: String;
    Function  GetOwner: String;
    Function  GetRegExMatches: TList<TRegExMatches>;
    Function  GetSize: Int64;
  Public
    Constructor Create(Const dtDate : TDateTime; Const iSize : Int64; Const strAttr, strOwner,
      strName : String);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  System.SysUtils;

(**

  This method adds the given text as a RegEx line.

  @precon  None.
  @postcon Adds the given text as a RegEx line.

  @param   iLine   as an Integer as a constant
  @param   Matches as a TMatchCollection as a constant

**)
Procedure TSearchFile.AddRegExLine(Const iLine: Integer; Const Matches: TMatchCollection);

Begin
  FRegExMatches.Add(TRegExMatches.Create(iLine, Matches));
End;

(**

  This method returns a clone (copy) of the TFile instance.

  @precon  None.
  @postcon Returns a clone (copy) of the TFile instance.

  @return  an ISearchFile

**)
Function TSearchFile.Clone: ISearchFile;
Var
  R: TRegExMatches;

Begin
  Result := TSearchFile.Create(FDate, FSize, FAttr, FOwner, ExtractFileName(FName));
  For R In FRegExMatches Do
    Result.RegExMatches.Add(R);
End;

(**

  This is the constructor method for the TFile class.

  @precon  None.
  @postcon Constructs a TFile class.

  @param   dtDate   as a TDateTime as a constant
  @param   iSize    as an Int64 as a constant
  @param   strAttr  as a String as a constant
  @param   strOwner as a String as a constant
  @param   strName  as a String as a constant

**)
Constructor TSearchFile.Create(Const dtDate: TDateTime; Const iSize: Int64; Const strAttr, strOwner,
  strName: String);

Begin
  Inherited Create;
  FRegExMatches := TList<TRegExMatches>.Create;
  FDate := dtDate;
  FSize := iSize;
  FAttr := strAttr;
  FOwner := strOwner;
  FName := strName;
End;

(**

  This is the destructor method for the TFile class.

  @precon  None.
  @postcon Destroys the class instance.

**)
Destructor TSearchFile.Destroy;

Begin
  FRegExMatches.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the Attr property.

  @precon  None.
  @postcon Returns the attributes of the file as a string representation.

  @return  a String

**)
Function TSearchFile.GetAttr: String;

Begin
  Result := FAttr;
End;

(**

  This is a getter method for the Date property.

  @precon  None.
  @postcon Returns the date and time of the file.

  @return  a TDateTime

**)
Function TSearchFile.GetDate: TDateTime;

Begin
  Result := FDate;
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the name of the file.

  @return  a String

**)
Function TSearchFile.GetName: String;

Begin
  Result := FName;
End;

(**

  This is a getter method for the Owner property.

  @precon  None.
  @postcon Returns the login name of the owner of the file.

  @return  a String

**)
Function TSearchFile.GetOwner: String;

Begin
  Result := FOwner;
End;

(**

  This is a getter method for the RegExMatches property.

  @precon  None.
  @postcon Returns the regular expression matches for the file.

  @return  a TList<TRegExMatches>

**)
Function TSearchFile.GetRegExMatches : TList<TRegExMatches>;

Begin
  Result := FRegExMatches;
End;

(**

  This is a getter method for the Size property.

  @precon  None.
  @postcon Returns the size of the file in bytes.

  @return  an Int64

**)
Function TSearchFile.GetSize: Int64;

Begin
  Result := FSize;
End;

End.
