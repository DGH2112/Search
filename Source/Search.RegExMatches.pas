(**

  This module contains a record for managing regular expression matches in a generic collection.

  @Author  David Hoyle
  @Version 1.0
  @Date    07 Apr 2018

**)
Unit Search.RegExMatches;

Interface

Uses
  Search.Types,
  System.RegularExpressions;

Type
  (** A record to describe the index and length of a regex match. **)
  TRegExMatch = Record
    FIndex : Integer;
    FLength : Integer;
  End;

  (** A record to manage the regular expression matches in a grep search. @nohints Needs fixing **)
  TRegExMatches = Record
  Strict Private
    Type
      (** A type to define an array of regular expression matches. **)
      TArrayOfMatch = Array Of TRegExMatch;
  Strict Private
    FLineNum: Integer;
    FMatches: TArrayOfMatch;
    FCount: Integer;
    Function GetMatch(Const iIndex: Integer): TRegExMatch;
  Public
    Constructor Create(Const iLine: Integer; Const Matches: TMatchCollection);
    (**
      This property returns the line number of the regular expression match.
      @precon  None.
      @postcon Returns the line number of the regular expression match.
      @return  an Integer
    **)
    Property LineNum: Integer Read FLineNum;
    (**
      This property returns the number of matches in the collection.
      @precon  None.
      @postcon Returns the number of matches in the collection.
      @return  an Integer
    **)
    Property Count: Integer Read FCount;
    (**
      This property returns the indexed regular expression match.
      @precon  None.
      @postcon Returns the indexed regular expression match.
      @param   iIndex as an Integer as a constant
      @return  a TRegExMatch
    **)
    Property Group[Const iIndex: Integer]: TRegExMatch Read GetMatch;
  End;

Implementation

(**

  A constructor for the TRegExmatches record.

  @precon  Matches must be a valid instance.
  @postcon The collection is initialised with the results of the regular expression match.

  @param   iLine   as an Integer as a constant
  @param   Matches as a TMatchCollection as a constant

**)
Constructor TRegExMatches.Create(Const iLine: Integer; Const Matches: TMatchCollection);

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

(**

  This is a getter method for the Match property.

  @precon  None.
  @postcon Returns the indexed match.

  @nohints Needs fixing

  @param   iIndex as an Integer as a constant
  @return  a TRegExMatch

**)
Function TRegExMatches.GetMatch(Const iIndex: Integer): TRegExMatch;

Begin
  Result := FMatches[iIndex];
End;

End.

