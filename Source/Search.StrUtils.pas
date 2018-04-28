(**
  
  This module contains a record with static class methods to encapsulate string functions for use within
  this application.

  @Author  David Hoyle
  @Version 1.0
  @Date    22 Apr 2018
  
**)
Unit Search.StrUtils;

Interface

Type
  (** A record to encapsulate string function used within the application. **)
  TSearchStrUtils = Record
  Strict Private
  Public
    Class Function  CharCount(Const cChar : Char; Const strText : String;
      Const boolIgnoreQuotes : Boolean = True) : Integer; Static;
    Class Function  GetField(Const strText : String; Const Ch : Char; Const iIndex : Integer;
      Const boolIgnoreQuotes : Boolean = True): String; Static;
    Class Function IsKeyWord(Const strWord : String; Const strWordList : Array Of String): Boolean;
      Static;
    Class Function  Like(Const strPattern, strText : String) : Boolean; Static;
    Class Function  PosOfNthChar(Const strText : String; Const Ch : Char; Const iIndex : Integer;
      Const boolIgnoreQuotes : Boolean = True): Integer; Static;
  End;

Implementation

uses
  System.SysUtils;

(**

  This routine returns the number of occurrances of the char found in the string .

  @precon  None.
  @postcon Returns the number of occurrances of the char found in the string.

  @param   cChar            as a Char as a constant
  @param   strText          as a String as a constant
  @param   boolIgnoreQuotes as a Boolean as a constant
  @return  an Integer

**)
Class Function TSearchStrUtils.CharCount(Const cChar : Char; Const strText : String;
  Const boolIgnoreQuotes : Boolean = True) : Integer;

Var
  iCount : Integer;
  boolInQuotes : Boolean;

Begin
  Result := 0;
  boolInQuotes := False;
  For iCount := 1 to Length(strText) Do
    Begin
      If Not boolIgnoreQuotes Then
        If strText[iCount] = '"' Then
          boolInQuotes := Not boolInQuotes;
      If strText[iCount] = cChar Then
        If Not boolInQuotes Then
          Inc(Result);
    End;
End;

(**

  This function returns the contents of the specified field in the delimited text.

  @precon  None.
  @postcon Returns the contents of the specified field in the delimited text.

  @param   strText          as a String as a constant
  @param   Ch               as a Char as a constant
  @param   iIndex           as an Integer as a constant
  @param   boolIgnoreQuotes as a Boolean as a constant
  @return  a String

**)
Class Function TSearchStrUtils.GetField(Const strText : String; Const Ch : Char; Const iIndex : Integer;
  Const boolIgnoreQuotes : Boolean = True): String;

Var
  iNumOfFields : Integer;
  iStart, iEnd : Integer;

Begin
  Result := '';
  iNumOfFields := CharCount(Ch, strText, boolIgnoreQuotes) + 1;
  If iIndex = 1 Then
    Begin
      If iNumOfFields > 1  Then
        Begin
          iEnd := PosOfNthChar(strText, Ch, 1, boolIgnoreQuotes);
          Result := Copy(strText, 1, iEnd - 1);
        End Else
          Result := strText;
    End
  Else If (iIndex > 1) And (iIndex < iNumOfFields) Then
    Begin
      iStart := PosOfNthChar(strText, Ch, iIndex - 1, boolIgnoreQuotes);
      iEnd := PosOfNthChar(strText, Ch, iIndex, boolIgnoreQuotes);
      Result := Copy(strText, iStart + 1, iEnd - iStart - 1);
    End
  Else If iIndex = iNumOfFields Then
    Begin
      iStart := PosOfNthChar(strText, Ch, iIndex - 1, boolIgnoreQuotes);
      Result := Copy(strText, iStart + 1, Length(strText) - iStart);
    End;
End;

(**

  This function returns true if the given word is in the supplied word list. It uses a binary search, so 
  the word lists need to be sorted.

  @precon  strWord is the word to be searches for in the word list and strWordList is a static array of 
           words in lowercase and alphabetical order.
  @postcon Returns true if the word is found in the list.

  @param   strWord     as a String as a constant
  @param   strWordList as an Array Of String as a constant
  @return  a Boolean

**)
Class Function TSearchStrUtils.IsKeyWord(Const strWord : String;
  Const strWordList : Array Of String): Boolean;

Var
  l, m, r : Integer;
  str : String;

begin
  Result := False;
  str := LowerCase(strWord);
  l := Low(strWordList);
  r := High(strWordList);
  While l <= r Do
    Begin
      m := (l + r) Div 2;
      If strWordList[m] < str Then
        l := Succ(m)
      Else If strWordList[m] > str Then
        r:= Pred(m)
      Else
        Begin
          Result := True;
          Exit;
        End;
    End;
end;

(**

  This function returns true if the pattern matches the text.

  @precon  None.
  @postcon Returns true if the pattern matches the text.

  @param   strPattern as a String as a constant
  @param   strText    as a String as a constant
  @return  a Boolean

**)
Class Function TSearchStrUtils.Like(Const strPattern, strText : String) : Boolean;

Type
  TMatchType = (mtStart, mtEnd);
  TMatchTypes = Set Of TMatchType;

  (**

    This function checks that the last part of the pattern matches the end of the input string and
    returns true if so.

    @precon  strParts must be a valid array.
    @postcon Returns true if the last pattern matches the end of the input string.

    @param   strParts   as a TArray<String> as a constant
    @param   MatchTypes as a TMatchTypes as a constant
    @return  a Boolean

  **)
  Function CheckEnd(Const strParts : TArray<String>; Const MatchTypes : TMatchTypes) : Boolean;

  Begin
    Result := False;
    If Length(strParts) > 0 Then
      If mtEnd In MatchTypes Then
        If CompareText(strParts[Length(strParts) - 1], Copy(strText, Length(strText) -
          Length(strParts[Length(strParts) - 1]) + 1, Length(strParts[Length(strParts) - 1]))) <> 0 Then
          Result := True;
  End;

  (**

    This function checks that the middle parts of the pattern match the middle of the input string and 
    returns true if so.

    @precon  strParts must be a valid array.
    @postcon Returns true if the middle patterns match the middle of the input string.

    @param   strParts    as a TArray<String> as a constant
    @param   MatchTypes  as a TMatchTypes as a constant
    @param   iStartIndex as an Integer as a reference
    @return  a Boolean

  **)
  Function CheckInBetween(Const strParts : TArray<String>; Const MatchTypes : TMatchTypes;
    Var iStartIndex : Integer) : Boolean;

  Var
    iPos: Integer;
    i: Integer;
  
  Begin
    Result := False;
    For i := Integer(mtStart In MatchTypes) To Length(strParts) - 1 - Integer(mtEnd In MatchTypes) Do
      Begin
        iPos := Pos(strParts[i], LowerCase(strText));
        If (iPos = 0) Or (iPos < iStartIndex) Then
          Result := True;
        Inc(iStartIndex, Length(strParts[i]));
      End;
  End;

  (**

    This function checks that the first part of the pattern matches the start of the input string and 
    returns true if so.

    @precon  strParts must be a valid array.
    @postcon Returns true if the first pattern matches the startof the input string.

    @param   strParts    as a TArray<String> as a constant
    @param   MatchTypes  as a TMatchTypes as a constant
    @param   iStartIndex as an Integer as a reference
    @return  a Boolean

  **)
  Function CheckStart(Const strParts : TArray<String>; Const MatchTypes : TMatchTypes;
    Var iStartIndex : Integer) : Boolean;

  Begin
    Result := False;
    iStartIndex := 1;
    If Length(strParts) > 0 Then
      If mtStart In MatchTypes Then
        If CompareText(strParts[0], Copy(strText, 1, Length(strParts[0]))) <> 0 Then
          Result := True
        Else
          Inc(iStartIndex, Length(strParts[0]));
  End;

  (**

    This method processes the parts of the pattern for matches.

    @precon  None.
    @postcon Returns True if the parts are confirmed as matching the input else returns false.

    @param   strPattern as a String as a constant
    @param   MatchTypes as a TMatchTypes as a constant
    @return  a Boolean

  **)
  Function ProcessParts(Const strPattern : String; Const MatchTypes : TMatchTypes) : Boolean;

  Var
    strParts : TArray<String>;
    iStartIndex : Integer;
    
  Begin
    strParts := strPattern.Split(['*']);
    Result := Not(
      CheckStart(strParts, MatchTypes, iStartIndex) Or
      CheckInBetween(strParts, MatchTypes, iStartIndex) Or
      CheckEnd(strParts, MatchTypes));
  End;

Var
  MatchTypes : TMatchTypes;
  strLPattern : String;

Begin
  Result := False;
  MatchTypes := [];
  strLPattern := strPattern;
  If Length(strLPattern) = 0 Then
    Exit;
  If strLPattern = '*' Then
    Begin
      Result := True;
      Exit;
    End;
  If strLPattern[1] <> '*' Then
    Include(MatchTypes, mtStart)
  Else
    Delete(strLPattern, 1, 1);
  If Length(strLPattern) > 0 Then
    If strLPattern[Length(strLPattern)] <> '*' Then
      Include(MatchTypes, mtEnd)
    Else
      Delete(strLPattern, Length(strLPattern), 1);
  Result := ProcessParts(strLPattern, MatchTypes);
End;

(**

  This routine returns the position of the Nth occurrance of the character in the text.

  @precon  None.
  @postcon Returns the position of the Nth occurrance of the character in the text.

  @param   strText          as a String as a constant
  @param   Ch               as a Char as a constant
  @param   iIndex           as an Integer as a constant
  @param   boolIgnoreQuotes as a Boolean as a constant
  @return  an Integer

**)
Class Function TSearchStrUtils.PosOfNthChar(Const strText : String; Const Ch : Char;
  Const iIndex : Integer; Const boolIgnoreQuotes : Boolean = True): Integer;

Var
  i : Integer;
  iCount : Integer;
  boolInQuotes : Boolean;

Begin
  Result := 0;
  iCount := 0;
  boolInQuotes := False;
  For i := 1 To Length(strText) Do
    Begin
      If Not boolIgnoreQuotes Then
        If strText[i] = '"' Then
          boolInQuotes := Not boolInQuotes;
      If strText[i] = Ch Then
        If Not boolInQuotes Then
          Inc(iCount);
      If iIndex = iCount Then
        Begin
          Result := i;
          Exit;
        End;
    End;
End;

End.
