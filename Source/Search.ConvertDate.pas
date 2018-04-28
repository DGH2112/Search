(**
  
  This module contains a record that provides the ability convert a date as a string into a date time
  value.

  @Author  David Hoyle
  @Version 1.0
  @Date    28 Apr 2018
  
**)
Unit Search.ConvertDate;

Interface

Uses
  System.Classes;

Type
  (** A cord that can convert a string representation of a date and time to a date time value. **)
  TConvertDate = Record
  Strict Private
    Type
      (** This is a record that defined the date and time for a date. **)
      TDateRec = Record
        iDay, iMonth, iYear, iHour, iMinute, iSecond, iMilli : Word;
      End;
  Strict Private
    Class Var
      (** An internal class variable to hold the date to be converted. **)
      FDate : String;
      (** An internal class variable to hold the date components **)
      FDateRec : TDateRec;
      (** Indexes for the day, month and year parts of the date into the string list. **)
      FDayIndex, FMonthIndex, FYearIndex : Integer;
      (** A string list to contain the parts of the date when broken down. **)
      FDateParts : TStringList;
  Strict Private
    Class Procedure AddToken(Const slStringList : TStringList; Var strToken  : String); Static;
    Class Procedure AssignIndexes; Static;
    Class Procedure DecodeTime(Const iTime : Integer); Static;
    Class Procedure InitialiseDateParts; Static;
    Class Function  OutputResult : TDateTime; Static;
    Class Procedure ProcessValue(Const iIndex : Integer; Var iValue : Word;
      Const boolDelete : Boolean); Static;
    Class Procedure RemoveDayOfWeek; Static;
    Class  Procedure SplitDate(Var iTime : Integer); Static;
  Public
    Class Function ConvertDate(Const strDate : String) : TDateTime; Static;
  End;

Const
  (** Number of hours in a day **)
  iHoursInDay = 23;
  (** Number of minutes in an hour **)
  iMinutesInHour = 59;
  (** Number of seconds in a minute **)
  iSecondsInMinute = 59;
  (** Number of days in a month **)
  iDaysInMonth = 31;
  (** Number of months in a year **)
  iMonthsInYear = 12;
  
Implementation

Uses
  System.SysUtils,
  Search.Types, 
  Search.StrUtils;

ResourceString
  (** An exception message for a string that cannot be converted to a date time value. **)
  strErrMsg = 'Cannot convert the date "%s" to a valid date and time value!';

Const
  (** A set of AnsiChars for valid date and time format delimiters. **)
  Delimiters : Set Of AnsiChar = ['-', ' ', '\', '/', ':', '.'];
  (** A constant array of day of the week short names. **)
  Days : Array[1..7] Of String = ('fri', 'mon', 'sat', 'sun', 'thu', 'tue', 'wed');
  (** A constant array of short and long month names. **)
  Months : Array[1..24] Of String = (
    'apr', 'april',
    'aug', 'august',
    'dec', 'december',
    'feb', 'february',
    'jan', 'january',
    'jul', 'july',
    'jun', 'june',
    'mar', 'march',
    'may', 'may',
    'nov', 'november',
    'oct', 'october',
    'sep', 'september'
    );
  (** A constant array of month indexes for the month names. **)
  MonthIndexes : Array[1..24] Of Word = (
    4, 4,
    8, 8,
    12, 12,
    2, 2,
    1, 1,
    7, 7,
    6, 6,
    3, 3,
    5, 5,
    11, 11,
    10, 10,
    9, 9
  );
  (** A minimum year starting value. **)
  iMinYear = 1900;
  (** A next cenury year value. **)
  iNextCentury = 2000;

(**

  This procedure adds the token to the specified string list and clears the token.

  @precon  None.
  @postcon Adds the token to the specified string list and clears the token.

  @param   slStringList as a TStringList as a constant
  @param   strToken     as a String as a reference

**)
Class Procedure TConvertDate.AddToken(Const slStringList : TStringList; Var strToken  : String);

Begin
  If strToken <> '' Then
    Begin
      slStringList.Add(strToken);
      strToken := '';
    End;
End;

(**

  This procedure assigns string list indexes to the three index values according to the short date 
  format and what information is supplied.

  @precon  None.
  @postcon Assigns string list indexes to the three index values according to the short date format 
           and what information is supplied.

**)
Class Procedure TConvertDate.AssignIndexes;

Const
  iDefaultDayIdx = 0;
  iDefaultMonthIdx = 1;
  iDefaultYearIdx = 2;
  iDayOfMonthLen = 2;

Var
  slFormat : TStringList;
  str : String;
  j : Integer;

Begin
  FDayIndex := iDefaultDayIdx;
  FMonthIndex := iDefaultMonthIdx;
  FYearIndex := iDefaultYearIdx;
  slFormat := TStringList.Create;
  Try
    str := '';
    For j := 1 To Length(FormatSettings.ShortDateFormat) Do
      If CharInSet(FormatSettings.ShortDateFormat[j], Delimiters) Then
        AddToken(slFormat, str)
      Else
        str := str + FormatSettings.ShortDateFormat[j];
    AddToken(slFormat, str);
    // Remove day of week
    For j := slFormat.Count - 1 DownTo 0 Do
      If (CharInSet(slFormat[j][1], ['d', 'D'])) And (Length(slFormat[j]) > iDayOfMonthLen) Then
        slFormat.Delete(j);
    For j := 0 To slFormat.Count - 1 Do
      Begin
        If CharInSet(slFormat[j][1], ['d', 'D']) Then
          FDayIndex := j;
        If CharInSet(slFormat[j][1], ['m', 'M']) Then
          FMonthIndex := j;
        If CharInSet(slFormat[j][1], ['y', 'Y']) Then
          FYearIndex := j;
      End;
  Finally
    slFormat.Free;
  End;
End;

(**

  This function converts a freeform text string representing dates and times
  in standard formats in to a TDateTime value.

  @precon  strDate is the string to convert into a date.
  @postcon Returns a valid TDateTime value.

  @param   strDate as a String as a Constant
  @return  a TDateTime

**)
Class Function TConvertDate.ConvertDate(Const strDate : String) : TDateTime;

Const
  iDayOnly = 1;
  iDayAndMonthOnly = 2;
  iDayMonthAndYear = 3;

Var
  i : Integer;
  tmp : Word;

Begin
  FDate := strDate;
  fDateParts := TStringList.Create;
  Try
    InitialiseDateParts;
    // Decode date
    Case FDateParts.Count Of
      iDayOnly:
        Begin
          DecodeDate(Now, FDateRec.iYear, FDateRec.iMonth, tmp);
          ProcessValue(0, FDateRec.iDay, False);
        End;
      iDayAndMonthOnly, iDayMonthAndYear:
        Begin
          DecodeDate(Now, FDateRec.iYear, tmp, tmp);
          AssignIndexes;
          ProcessValue(FDayIndex, FDateRec.iDay, False); // Get day
          If TSearchStrUtils.IsKeyWord(FDateParts[FMonthIndex], Months) Then
            Begin
              For i := Low(Months) To High(Months) Do
                If CompareText(Months[i], FDateParts[FMonthIndex]) = 0 Then
                  Begin
                    FDateRec.iMonth := MonthIndexes[i];
                    Break;
                  End;
            End Else
              ProcessValue(FMonthIndex, FDateRec.iMonth, False); // Get Month
            If FDateParts.Count = iDayMonthAndYear Then
              Begin
                ProcessValue(FYearIndex, FDateRec.iYear, False); // Get Year
                If FDateRec.iYear < iMinYear Then
                  Inc(FDateRec.iYear, iNextCentury);
              End;
        End;
    Else
      If FDateParts.Count <> 0 Then
        Raise ESearchException.CreateFmt(strErrMsg, [FDate]);
    End;
    Result := OutputResult;
  Finally
    FDateParts.Free;
  End;
End;

(**

  This method extracts the date and time values from the string list into the given date record.

  @precon  None.
  @postcon The date and time components are extracted from the string list and placed in the record.

  @param   iTime   as an Integer as a constant

**)
Class Procedure TConvertDate.DecodeTime(Const iTime : Integer);

Begin
  If iTime > -1 Then
    Begin
      ProcessValue(iTime, FDateRec.iHour, True);
      ProcessValue(iTime, FDateRec.iMinute, True);
      ProcessValue(iTime, FDateRec.iSecond, True);
      ProcessValue(iTime, FDateRec.iMilli, True);
    End;
End;

(**

  This method initialises the date parts string list.

  @precon  None.
  @postcon Date Parts string list is initialised.

**)
Class Procedure TConvertDate.InitialiseDateParts;

Var
  iTime : Integer;

Begin
  iTime := -1;
  SplitDate(iTime);
  FillChar(FDateRec, SizeOf(FDateRec), 0);
  DecodeTime(iTime);
  RemoveDayOfWeek;
End;

(**

  This procedure outputs the results of the conversion of the date else raises an exception.

  @precon  None.
  @postcon Outputs the results of the conversion of the date else raises an exception.

  @return  a TDateTime

**)
Class Function TConvertDate.OutputResult : TDateTime;

Begin
  If Not (FDateRec.iHour In [0..iHoursInDay]) Then
    Raise ESearchException.CreateFmt(strErrMsg, [FDate]);
  If Not (FDateRec.iMinute In [0..iMinutesInHour]) Then
    Raise ESearchException.CreateFmt(strErrMsg, [FDate]);
  If Not (FDateRec.iSecond In [0..iSecondsInMinute]) Then
    Raise ESearchException.CreateFmt(strErrMsg, [FDate]);
  Result := EncodeTime(FDateRec.iHour, FDateRec.iMinute, FDateRec.iSecond, FDateRec.iMilli);
  If FDateRec.iYear * FDateRec.iMonth * FDateRec.iDay <> 0 Then
    Begin
      If Not (FDateRec.iDay In [1..iDaysInMonth]) Then
        Raise ESearchException.CreateFmt(strErrMsg, [FDate]);
      If Not (FDateRec.iMonth In [1..iMonthsInYear]) Then
        Raise ESearchException.CreateFmt(strErrMsg, [FDate]);
      Result := Result + EncodeDate(FDateRec.iYear, FDateRec.iMonth, FDateRec.iDay);
    End;
End;

(**

  This procedure tries to extract the value from the indexed string list item into the passed variable 
  reference. It delete is true it remove the item from the string list.

  @precon  iIndex is the index of the item from the string list to extract, iValue is a word variable 
           to place the converted item into and Delete determines whether the item is removed from 
           the string list.
  @postcon Tries to extract the value from the indexed string list item into the passed variable 
           reference. It delete is true it remove the item from the string list.

  @param   iIndex     as an Integer as a constant
  @param   iValue     as a Word as a reference
  @param   boolDelete as a Boolean as a constant

**)
Class Procedure TConvertDate.ProcessValue(Const iIndex : Integer; Var iValue : Word;
  Const boolDelete : Boolean);

Var
  iErrorCode : Integer;
    
Begin
  If iIndex > FDateParts.Count - 1 Then
    Exit;
  Val(FDateParts[iIndex], iValue, iErrorCode);
  If iErrorCode <> 0 Then
    Raise ESearchException.CreateFmt(strErrMsg, [FDate]);
  If boolDelete Then
    FDateParts.Delete(iIndex);
End;

(**

  This method deletes any occurrances of the day of the week from the string list.

  @precon  sl must be a valid instance.
  @postcon Any day of the weeks are removed from the string list.

**)
Class Procedure TConvertDate.RemoveDayOfWeek;

Var
  iIndex: Integer;

Begin
  For iIndex := FDateParts.Count - 1 DownTo 0 Do
    If TSearchStrUtils.IsKeyWord(FDateParts[iIndex], Days) Then
      FDateParts.Delete(iIndex);
End;

(**

  This method splits the date down into the parts and adds them to the string list.

  @precon  None.
  @postcon The date is split and added to the string list.

  @param   iTime as an Integer as a reference

**)
Class Procedure TConvertDate.SplitDate(Var iTime : Integer);

Var
  i: Integer;
  strToken: String;

Begin
  strToken := '';
  For i := 1 To Length(FDate) Do
    If CharInSet(FDate[i], Delimiters) Then
      Begin
        AddToken(FDateParts, strToken);
        If (CharInSet(FDate[i], [':'])) And (iTime = -1) Then
          iTime := FDateParts.Count - 1;
      End Else
        strToken := strToken + FDate[i];
  AddToken(FDateParts, strToken);
End;

End.
