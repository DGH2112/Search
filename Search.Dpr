(**

  This module defines the Search Win32 console application. This application
  is designed to provide regular expression search capabilities for files on
  disk.

  @Version 1.0
  @Author  David Hoyle
  @Date    31 Oct 2004

**)
Program Search;

{$APPTYPE CONSOLE}

{$R *.RES}

uses
  SysUtils, Windows, Classes;

Type
  (** This is a list of boolean on / off command line switches. **)
  TCommandLineSwitch = (
    clsShowHelp,         { /? or -? }
    clsSubDirectories,   { /s or -s }
    clsDebug,            { /d or -d }
    clsShowAttribs,      { /a or -a }
    clsPause,            { /p or -p }
    clsUseRegex,         { /r or -r }
    clsSummaryLevel,     { /1..9 or -1..9 }
    clsSupressZeros,     { /0 or -0 }
    clsDateRange,        { /d or -d }
    clsSizeRange,        { /z or -z }
    clsAttrRange         { /t ot -t }
  );

  (** This is a set of boolean command line switches. **)
  TCommandLineSwitches = Set of TCommandLineSwitch;

Var
  (** Define the applications command line switches. **)
  CommandLineSwitches : TCommandLineSwitches;
  (** The otal number of file found by the search. **)
  iFiles : Integer;
  (** The total number of directories found by the search. **)
  iDirectories : Integer;
  (** This is the upper limit of a file size search. **)
  iUSize : Integer;
  (** This is the lower limit of a file size search. **)
  iLSize  :Integer;
  (** This is the lower limit of a file date search. **)
  dtLDate : Double;
  (** This is the upper limit of a file date search. **)
  dtUDate : Double;
  (** The total size of al the files found in the search. **)
  iFileSize : Int64;
  (** Height of the console. **)
  iHeight : Integer;
  (** Width of the console. **)
  iWidth : Integer;
  (** This is the current number of lines outputted to the console. **)
  iLinesOutputted : Integer;
  (** This is a list of file attributes that are required in a search. **)
  iFileAttrs : Integer;
  (** This is a list of file type attributes that are required in a search. **)
  iTypeAttrs : Integer;
  (** This is a list of search parameters that are not part of the command line
      switches. **)
  slSearchParams : TStringList;
  (** This is the level of directory of summarisation required. **)
  iSummaryLevel : Integer;

(**

  This function returns true if the given word is in the supplied word list. It
  uses a binary search, so the word lists need to be sorted.

  @precon  strWord is the word to be searches for in the word list and
           strWordList is a static array of words in lowercase and alphabetical
           order.
  @postcon Returns true if the word is found in the list.

  @param   strWord     as a String
  @param   strWordList as an Array Of String
  @return  a Boolean

**)
function IsKeyWord(strWord : String; strWordList : Array Of String): Boolean;

Var
  l, m, r : Integer;
  str : String;

begin
  Result := False;
  str := LowerCase(strWord);
  l := 0;
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

  This function converts a freeform text string representing dates and times
  in standard formats in to a TDateTime value.

  @precon  strDate is the string to convert into a date.
  @postcon Returns a valid TDateTime value.

  @param   strDate as a String
  @return  a TDateTime

**)
Function ConvertDate(Const strDate : String) : TDateTime;

Type
  TDateRec = Record
    iDay, iMonth, iYear, iHour, iMinute, iSecond : Word;
  End;

Const
  strErrMsg = 'Can not convert the date "%s" to a valid TDateTime value.';
  Delimiters : Set Of Char = ['-', ' ', '\', '/', ':'];
  Days : Array[1..7] Of String = ('fri', 'mon', 'sat', 'sun', 'thu', 'tue', 'wed');
  Months : Array[1..24] Of String = (
    'apr', 'april',
    'aug', 'august',
    'feb', 'february',
    'dec', 'december',
    'jan', 'january',
    'jul', 'july',
    'jun', 'june',
    'mar', 'march',
    'may', 'may',
    'nov', 'november',
    'oct', 'october',
    'sep', 'september'
    );
  MonthIndexes : Array[1..24] Of Word = (
    4, 4,
    8, 8,
    2, 2,
    12, 12,
    1, 1,
    7, 7,
    6, 6,
    3, 3,
    5, 5,
    11, 11,
    10, 10,
    9, 9
  );

Var
  i : Integer;
  sl : TStringList;
  strToken : String;
  iTime : Integer;
  recDate : TDateRec;
  tmp : Word;
  iIndex0, iIndex1, iIndex2 : Integer;

  (**

    This procedure adds the token to the specified string list and clears the
    token.

    @precon  StringList is the string list to add the token too and strToken is
             the token to add to the list.
    @postcon Adds the token to the specified string list and clears the
             token.

    @param   StringList as a TStringList
    @param   strToken   as a String as a reference

  **)
  Procedure AddToken(StringList : TStringList; var strToken  : String);

  Begin
    If strToken <> '' Then
      Begin
        StringList.Add(strToken);
        strToken := '';
      End;
  End;

  (**

    This procedure tries to extract the value from the indexed string list
    item into the passed variable reference. It delete is true it remove the
    item from the string list.

    @precon  iIndex is the index of the item from the string list to extract,
             iValue is a word variable to place the converted item into and
             Delete determines whether the item is removed from the string list.
    @postcon Tries to extract the value from the indexed string list item into
             the passed variable reference. It delete is true it remove the
             item from the string list.

    @param   iIndex as an Integer
    @param   iValue as a Word as a reference
    @param   Delete as a Boolean

  **)
  Procedure ProcessValue(iIndex : Integer; var iValue : Word; Delete : Boolean);

  Begin
    If iIndex > sl.Count - 1 Then Exit;
    Val(sl[iIndex], iValue, i);
    If i <> 0 Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
    If Delete Then sl.Delete(iIndex);
  End;

  (**

    This procedure assigns string list indexes to the three index values
    according to the short date format and what information is supplied.

    @precon  None.
    @postcon Assigns string list indexes to the three index values
             according to the short date format and what information is
             supplied.

  **)
  Procedure AssignIndexes();

  Var
    slFormat : TStringList;
    str : String;
    j : Integer;

  Begin
    iIndex0 := 0; // Default Day / Month / Year
    iIndex1 := 1;
    iIndex2 := 2;
    slFormat := TstringList.Create;
    Try
      str := '';
      For j := 1 To Length(ShortDateFormat) Do
        If ShortDateFormat[j] In Delimiters Then
          AddToken(slFormat, str)
        Else
          str := str + ShortDateFormat[j];
      AddToken(slFormat, str);
      // Remove day of week
      For j := slFormat.Count - 1 DownTo 0 Do
        If (slFormat[j][1] In ['d', 'D']) And (Length(slFormat[j]) > 2) Then
          slFormat.Delete(j);
      For j := 0 To slFormat.Count - 1 Do
        Begin
          If slFormat[j][1] In ['d', 'D'] Then iIndex0 := j;
          If slFormat[j][1] In ['m', 'M'] Then iIndex1 := j;
          If slFormat[j][1] In ['y', 'Y'] Then iIndex2 := j;
        End;
    Finally
      slFormat.Free;
    End;
  End;

Begin
  Result := 0;
  sl := TStringList.Create;
  Try
    strToken := '';
    iTime := -1;
    For i := 1 To Length(strDate) Do
      If strDate[i] In Delimiters Then
        Begin
          AddToken(sl, strToken);
          If (strDate[i] = ':') And (iTime = -1) Then iTime := sl.Count - 1;
        End Else
          strToken := strToken + strDate[i];
    AddToken(sl, strToken);
    FillChar(recDate, SizeOf(recDate), 0);
    // Decode time
    If iTime > -1 Then
      Begin
        ProcessValue(iTime,recDate.iHour, True);
        ProcessValue(iTime,recDate.iMinute, True);
        ProcessValue(iTime,recDate.iSecond, True);
      End;
    // Remove day value if present
    For i := sl.Count - 1 DownTo 0 Do
      If IsKeyWord(sl[i], Days) Then
        sl.Delete(i);
    // Decode date
    Case sl.Count Of
      1 :
        Begin
          DecodeDate(Now, recDate.iYear, recDate.iMonth, tmp);
          ProcessValue(0, recDate.iDay, False); // Day only
        End;
      2, 3 : // Day and Month (Year)
        Begin
          DecodeDate(Now, recDate.iYear, tmp, tmp);
          AssignIndexes;
          ProcessValue(iIndex0, recDate.iDay, False); // Get day
          If IsKeyWord(sl[iIndex1], Months) Then
            Begin
              For i := Low(Months) To High(Months) Do
                If AnsiCompareText(Months[i], sl[iIndex1]) = 0 Then
                  Begin
                    recDate.iMonth := MonthIndexes[i];
                    Break;
                  End;
            End Else
              ProcessValue(iIndex1, recDate.iMonth, False); // Get Month
            If sl.Count = 3 Then
              Begin
                ProcessValue(iIndex2, recDate.iYear, False); // Get Year
                If recDate.iYear < 1900 Then Inc(recDate.iYear, 2000);
              End;
        End;
    Else
      If sl.Count <> 0 Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
    End;
    // Output result.
    With recDate Do
      Begin
        If Not (iHour In [0..23]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
        If Not (iMinute In [0..59]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
        If Not (iSecond In [0..59]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
        Result := EncodeTime(iHour, iMinute, iSecond, 0);
        If iYear * iMonth * iDay <> 0 Then
          Begin
            If Not (iDay In [1..31]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
            If Not (iMonth In [1..12]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
            Result := Result + EncodeDate(iYear, iMonth, iDay);
          End;
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This routine is used to output all the text to the screen. It monitors
  how many lines have been output and pauses after a full screen full.

  @precon  None.
  @postcon Used to output all the text to the screen. It monitors how many lines
           have been output and pauses after a full screen full.

  @param   strText as a String

**)
Procedure OutputLinesToConsole(strText : String = '');

Begin
  Writeln(strText);
  Inc(iLinesOutputted);
  If (clsPause In CommandLineSwitches) And (iLinesOutputted >= (iHeight - 1)) Then
    Begin
      Write('Press any <Enter> to continue...');
      Readln;
      iLinesOutputted := 0;
    End;
End;

(**

  This routine get and stores the current number of lines and columns
  in the console window.

  @precon  None.
  @postcon Gets the width and height of the console.

**)
Procedure GetConsoleInformation;

Var
  ConsoleInfo : _CONSOLE_SCREEN_BUFFER_INFO;

Begin
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), ConsoleInfo);
  iWidth := ConsoleInfo.dwSize.X;
  iHeight := ConsoleInfo.dwSize.Y;
End;

(**

  This routine extract the build number from the EXE resources for
  display in the app title.

  @precon  None.
  @postcon Extract the build number from the EXE resources for display in the
           app title.

  @param   iMajor  as an Integer as a reference
  @param   iMinor  as an Integer as a reference
  @param   iBugfix as an Integer as a reference
  @return  a String

**)
Function GetBuildNumber(var iMajor, iMinor, iBugfix : Integer) : String;

Const
  strBuild = '%d.%d.%d.%d';

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;

Begin
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      with VerValue^ do
      begin
        iMajor := dwFileVersionMS shr 16;
        iMinor := dwFileVersionMS and $FFFF;
        iBugfix := dwFileVersionLS shr 16;
        Result := Format(strBuild, [iMajor, iMinor, iBugfix, dwFileVersionLS and $FFFF]);
      end;
      FreeMem(VerInfo, VerInfoSize);
    End Else
      OutputLinesToConsole('This executable does not contain any version information.');
End;

(**

  Prints a Title for the application on the screen.

  @precon  None.
  @postcon Prints the title for the application on the screen.

**)
Procedure PrintTitle;

Const
  strTitle = 'Search %d.%d%s (Build %s) File Find and Summary Tool.';
  strBugFix = ' abcdefghijklmnopqrstuvwxyz';

Var
  iMajor, iMinor, iBugfix : Integer;
  strBuildNumber  : String;

Begin
  strBuildNumber := GetBuildNumber(iMajor, iMinor, iBugFix);
  OutputLinesToConsole(Format(strTitle, [iMajor, iMinor, strBugFix[iBugFix + 1],
    strBuildNumber]));
  OutputLinesToConsole(
    Format(
      'Written by David Hoyle (c) %s',
      [FormatDateTime('mmm/yyyy', FileDateToDateTime(FileAge(ParamStr(0))))]));
  OutputLinesToConsole;
End;

(**

  Prints the path and search pattern before each search

  @precon  None.
  @postcon Prints the path and search pattern before each search

  @param   strPath    as a String
  @param   strPattern as a String

**)
Procedure PrintHeader(strPath, strPattern : String);

Begin
  OutputLinesToConsole('Searching "' + strPath + '" for "' + strPattern + '".');
  iFiles := 0;
  iDirectories := 0;
  iFileSize := 0;
End;

(**

  Prints a footer after the search displaying stats on the search.

  @precon  None.
  @postcon Prints a footer after the search displaying stats on the search.

**)
Procedure PrintFooter;

Begin
  OutputLinesToConsole(Format('%1.0n bytes in %1.0n Files in %1.0n Directories.',
    [iFileSize + 0.1, iFiles + 0.1, iDirectories + 0.1]));
End;

(**

  This method prints on the console screen the command line search help
  information.

  @precon  None.
  @postcon Prints on the console screen the command line search help
           information.

**)
Procedure PrintHelp;

Begin
  OutputLinesToConsole('Syntax:');
  OutputLinesToConsole('  SearchRX searchparam1 [searchparam2...] [/A] [/S]');
  OutputLinesToConsole('    [/T RASHFDV] [/D "(DD/MM/YY HH:MM:SS-DD/MM/YY HH:MM:SS)"]');
  OutputLinesToConsole('    [/Z LowerByteSize-UpperByteSize]');
  OutputLinesToConsole('');
  OutputLinesToConsole('    /?     This help screen');
  OutputLinesToConsole('    /A     Show file and directory attributes');
  OutputLinesToConsole('    /P     Pause the results after each screen');
  OutputLinesToConsole('    /S     Recurse subdirectories');
  OutputLinesToConsole('    /1..9  Summarise subdirectories');
  OutputLinesToConsole('');
  OutputLinesToConsole('    /T     R = Read Only, A = Archive, S = System, H = Hidden, F = File,');
  OutputLinesToConsole('           D = Directory, and V=  Volume ID');
  OutputLinesToConsole('    /D     ( lower bounding date and time - upper bounding date and time )');
  OutputLinesToConsole('    /Z     ( lower bounding size in bytes - upper bounding size in bytes )');
  OutputLinesToConsole('');
  OutputLinesToConsole('NOTE: The date time input format is dependent on your local settings');
  OutputLinesToConsole('');;
End;

(**

  This method determines if a date range has been specified on the command line.

  @precon  None.
  @postcon Determines if a date range has been specified on the command line.

  @param   iSwitch as an Integer as a Reference

**)
Procedure GetDateRange(Var iSwitch : Integer);

Const
  strRangeException = 'The parameter "%s" does not contain a range delimiter (-).';

Var
  iPos : Integer;
  strTmp : String;

Begin
  Include(CommandLineSwitches, clsDateRange);
  Inc(iSwitch); // Next parameter is the size range of the search
  iPos := Pos('-', ParamStr(iSwitch));
  If iPos = 0 Then
    Raise Exception.CreateFmt(strRangeException, [ParamStr(iSwitch)]);
  strTmp := Copy(ParamStr(iSwitch), 1, iPos - 1);
  dtLDate := ConvertDate(strTmp);
  strTmp := Copy(ParamStr(iSwitch), iPos + 1, Length(ParamStr(iSwitch)) - iPos);
  dtUDate := ConvertDate(strTmp);
End;

(**

  This method determines of a size range has been specified on the command line.

  @precon  None.
  @postcon Determines of a size range has been specified on the command line.

  @param   iSwitch as an Integer as a Reference

**)
Procedure GetSizeRange(Var iSwitch : Integer);

Const
  strRangeException = 'The parameter "%s" does not contain a range delimiter (-).';
  strLowerRangeException = 'The lower range value of "%s" is not a valid integer.';
  strUpperRangeException = 'The upper range value of "%s" is not a valid integer.';

Var
  iPos, iCode : Integer;
  strTmp : String;

Begin
  Include(CommandLineSwitches, clsSizeRange);
  Inc(iSwitch); // Next parameter is the sieze range of the search
  iPos := Pos('-', ParamStr(iSwitch));
  If iPos = 0 Then
    Raise Exception.CreateFmt(strRangeException, [ParamStr(iSwitch)]);
  strTmp := Copy(ParamStr(iSwitch), 1, iPos - 1);
  Val(strTmp, iLSize, iCode);
  If iCode > 0 Then
    Raise Exception.CreateFmt(strLowerRangeException, [ParamStr(iSwitch)]);
  strTmp := Copy(ParamStr(iSwitch), iPos + 1, Length(ParamStr(iSwitch)) - iPos);
  Val(strTmp, iUSize, iCode);
  If iCode > 0 Then
    Raise Exception.CreateFmt(strUpperRangeException, [ParamStr(iSwitch)]);
End;

(**

  This method determines the attribute range from the command line.

  @precon  None.
  @postcon Determines the attribute range from the command line.

  @param   iSwitch as an Integer as a reference

**)
Procedure GetAttributes(Var iSwitch : Integer);

Const
  strNotAValidAttrList = '"%s" is not a valid attribute list.';

Var
  i : Integer;

Begin
  Include(CommandLineSwitches, clsAttrRange);
  Inc(iSwitch); // Next parameter is the size range of the search
  iFileAttrs := 0;
  iTypeAttrs := 0;
  For i := 1 To Length(ParamStr(iSwitch)) Do
    Case ParamStr(iSwitch)[i] Of
      'r', 'R' : iFileAttrs := iFileAttrs Or faReadOnly;
      'a', 'A' : iFileAttrs := iFileAttrs Or faArchive;
      's', 'S' : iFileAttrs := iFileAttrs Or faSysFile;
      'h', 'H' : iFileAttrs := iFileAttrs Or faHidden;
      'f', 'F' : iTypeAttrs := iTypeAttrs Or faAnyFile;
      'd', 'D' : iTypeAttrs := iTypeAttrs Or faDirectory;
      'v', 'V' : iTypeAttrs := iTypeAttrs Or faVolumeID;
    Else
      Raise Exception.CreateFmt(strNotAValidAttrList, [ParamStr(iSwitch)]);
    End;
  If iFileAttrs = 0 Then
    iFileAttrs := faArchive Or faReadOnly Or faHidden Or faSysFile Or
      faDirectory Or faVolumeID;
  If iTypeAttrs = 0 Then
    iTypeAttrs := faDirectory Or faVolumeID;
End;

(**

  This method determines the summary level required.

  @precon  None.
  @postcon Determines the summary level required.

  @param   iSwitch   as an Integer as a reference
  @param   iSwitches as an Integer

**)
Procedure GetSummaryLevel(Var iSwitch : Integer; iSwitches : Integer);

Const
  strSummaryLevelException = 'The summary level "%s" is invalid.';
  strSummaryAlreadySet = 'Then summary level is already set.';

Var
  iCode : Integer;

Begin
  Include(CommandLineSwitches, clsSummaryLevel);
  If iSummaryLevel > 0 Then
    Raise Exception.Create(strSummaryAlreadySet);
  Val(ParamStr(iSwitch)[iSwitches], iSummaryLevel, iCode);
  If iCode > 0 Then
    Raise Exception.CreateFmt(strSummaryLevelException, [ParamStr(iSwitch)]);
End;

(**

  This gets all the command lines switches and sets numerous flags.

  @precon  None.
  @postcon Gets all the command lines switches and sets numerous flags.

**)
Procedure GetCommandLineSwitches;

Const
  strInvalidCommandLineSwitch = 'Invalid command line switch "%s."';

Var
  iSwitch, iSwitches : Integer;

Begin
  iSummaryLevel := 0;
  CommandLineSwitches := [];
  iSwitch := 1;
  While iSwitch <= ParamCount Do
    Begin
      If ParamStr(iSwitch)[1] In ['-', '/'] Then
        Begin
          For iSwitches := 2 To Length(ParamStr(iSwitch)) Do
            Case ParamStr(iSwitch)[iSwitches] Of
              '?'     : Include(CommandLineSwitches, clsShowHelp);
              's', 'S': Include(CommandLineSwitches, clsSubDirectories);
              '!'     : Include(CommandLineSwitches, clsDebug);
              'a', 'A': Include(CommandLineSwitches, clsShowAttribs);
              'p', 'P': Include(CommandLineSwitches, clsPause);
              'r', 'R': Include(CommandLineSwitches, clsUseRegex);
              '1'..'9': GetSummaryLevel(iSwitch, iSwitches);
              '0'     : Include(CommandLineSwitches, clsSupressZeros);
              'd', 'D': GetDateRange(iSwitch);
              'z', 'Z': GetSizeRange(iSwitch);
              't', 'T': GetAttributes(iSwitch);
            Else
              Raise Exception.CreateFmt(strInValidCommandLineSwitch,
                [ParamStr(iSwitch)]);
            End;
        End Else
          slSearchParams.Add(
            Format('%s=%s', [
              ExtractFilePath(ParamStr(iSwitch)),
              ExtractFileName(ParamStr(iSwitch))
            ])
          );
      Inc(iSwitch); // Get the next switch
    End;
  If ParamCount = 0 Then
    Include(CommandLineSwitches, clsShowHelp);
End;

(**

  This method outputs the files attributes if they are requireed at the command
  line.

  @precon  None.
  @postcon Outputs the files attributes if they are requireed at the command
           line.

  @param   iAttr as an Integer
  @return  a String

**)
Function OutputAttributes(iAttr : Integer) : String;

Begin
  If clsShowAttribs In CommandLineSwitches Then
    Begin
      Result := '.......  ';
      If faReadOnly And iAttr > 0 Then Result[1] := 'R';
      If faArchive And iAttr > 0 Then Result[2] := 'A';
      If faSysFile And iAttr > 0 Then Result[3] := 'S';
      If faHidden And iAttr > 0 Then Result[4] := 'H';
      If faDirectory And iAttr > 0 Then
        Result[6] := 'D'
      Else If faVolumeID And iAttr > 0 Then
        Result[7] := 'V'
      Else
        Result[5] := 'F';
    End;
End;

(**

  This is the main recursive routine that searches the currently passed
  directory for files and then if subdirectories are required calls
  its self on those directories.

  @precon  None.
  @postcon Main recursive routine that searches the currently passed directory
           for files and then if subdirectories are required calls its self on
           those directories.

  @param   strPath    as a String
  @param   strPattern as a String
  @param   iLevel     as an Integer as a Reference
  @return  an Int64 

**)
Function SearchDirectory(strPath, strPattern : String; Var iLevel : Integer) : Int64;

Var
  iResult : Integer;
  recSearch : TSearchRec;
  boolDirPrinted : Boolean;
  strOutput : String;
  boolFound : Boolean;
  iDirFiles : Integer;

Begin
  Inc(iLevel);
  Result := 0;
  iDirFiles := 0;
  Inc(iDirectories);
  boolDirPrinted := False;
  (* Find Files *)
  boolFound := True;
  iResult := FindFirst(strPath + strPattern, faAnyFile, recSearch);
  While iResult = 0  Do
    Begin
      If clsAttrRange In CommandLineSwitches Then
        Begin
          boolFound := boolFound And (recSearch.Attr And iFileAttrs > 0);
          boolFound := boolFound And (recSearch.Attr And iTypeAttrs > 0);
        End;
      If clsSizeRange In CommandLineSwitches Then
        Begin
          boolFound := boolFound And (recSearch.Size >= iLSize);
          boolFound := boolFound And (recSearch.Size <= iUSize);
        End;
      If clsDateRange In CommandLineSwitches Then
        Begin
          boolFound := boolFound And (FileDateToDateTime(recSearch.Time) >= dtLDate);
          boolFound := boolFound And (FileDateToDateTime(recSearch.Time) <= dtUDate);
        End;
      If boolFound Then
        Begin
          Inc(iFiles);
          Inc(iFileSize, recSearch.Size);
          Inc(Result, recSearch.Size);
          If Not (clsSummaryLevel In CommandLineSwitches) Then
            Begin
              Inc(iDirFiles);
              If Not boolDirPrinted Then
                Begin
                  OutputLinesToConsole(strPath);
                  boolDirPrinted := True;
                End;
              strOutput := Format('  %s  %13.0n  ', [
                FormatDateTime('ddd dd/mmm/yyyy hh:mm:ss', FileDateToDateTime(recSearch.Time)),
                recSearch.Size + 0.1]);
              strOutput := strOutput + OutputAttributes(recSearch.Attr);
              OutputLinesToConsole(strOutput + recSearch.Name);
            End;
        End;
      iResult := FindNext(recSearch);
      boolFound := True;
    End;
  SysUtils.FindClose(recSearch);
  (* Search sub directories *)
  If clsSubDirectories In CommandLineSwitches Then
    Begin
      iResult := FindFirst(strPath + '*.*', faDirectory, recSearch);
      While (iResult = 0)  Do
        Begin
          If recSearch.Attr And faDirectory > 0 Then
            If (recSearch.Name <> '.') And (recSearch.Name <> '..') Then
              Result := Result  + SearchDirectory(
                strPath + recSearch.Name + '\', strPattern, iLevel);
          iResult := FindNext(recSearch);
        End;
      SysUtils.FindClose(recSearch);
    End;
  (* Output directory summary levels *)
  If clsSummaryLevel In CommandLineSwitches Then
    Begin
      If (iLevel > 0) And (iLevel <= iSummaryLevel) Then
        If Not ((Result = 0) And (clsSupressZeros In CommandLineSwitches)) Then
        OutputLinesToConsole(Format('  %13.0n%s%s', [
          Result + 0.1, StringOfChar(#32, 2 * iLevel), strPath]));
    End Else
      If iDirFiles > 0 Then
        OutputLinesToConsole;
  Dec(iLevel);
End;

(** ---------------------------------------------------------------------------

   Main Program body.

 -------------------------------------------------------------------------- **)

Const
  (** This is a constant to represent a parameter string error. **)
  strErrorInPathSearchParamString =
    'There was an error in the path=search param pairing. (%s)';

Var
  (** This is a loop iterator. **)
  i : Integer;
  (** A position indicator for the = in the path=search param string list. **)
  iPos : Integer;
  (** A  strings to represent the search path. **)
  strPath : String;
  (** A  strings to represent the search param. **)
  strSearch : String;
  (** This is the starting level for summarise. **)
  iLevel : Integer;

Begin
  iLevel := -1;
  PrintTitle;
  GetConsoleInformation;
  Try
    slSearchParams := TStringList.Create;
    Try
      iLinesOutputted := 0;
      GetCommandLineSwitches;
      If Not (clsShowHelp In CommandLineSwitches) Then
        Begin
          For i := 0 To slSearchParams.Count - 1 Do
            Begin
              iPos := Pos('=', slSearchParams[i]);
              If iPos = 0 Then
                Raise Exception.CreateFmt(strErrorInPathSearchParamString,
                  [slSearchParams[i]]);
              strPath := Copy(slSearchParams[i], 1, iPos - 1);
              If strPath = '' Then
                strPath := GetCurrentDir;
              If strPath[Length(strPath)] <> '\' Then
                strPath := strPath + '\';
              strSearch := Copy(slSearchParams[i], iPos + 1,
                Length(slSearchParams[i]) - iPos);
              PrintHeader(strPath, strSearch);
              SearchDirectory(strPath, strSearch, iLevel);
              PrintFooter;
            End;
        End Else
          PrintHelp;
    Finally
      slSearchParams.Free;
    End;
  Except
    On E : Exception Do
      Writeln('Exception: ' + E.Message);
  End;
  If clsDebug In CommandLineSwitches Then
    Readln;
End.
