(**

  This module defines the Search Win32 console application. This application
  is designed to provide regular expression search capabilities for files on
  disk.

  @Version 1.0
  @Author  David Hoyle
  @Date    06 Nov 2004

**)
Program Search;

{$APPTYPE CONSOLE}

{$R *.RES}

uses
  SysUtils,
  Windows,
  Classes,
  DGHLibrary50 in '..\..\LIBRARY\DGHLibrary50.pas';

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

  This routine is used to output all the text to the screen. It monitors
  how many lines have been output and pauses after a full screen full.

  @precon  None.
  @postcon Used to output all the text to the screen. It monitors how many lines
           have been output and pauses after a full screen full.

  @param   strText as a String

**)
Procedure OutputLinesToConsole(strText : String = '');

Begin
  Writeln(strText + StringOfChar(#32, iWidth - Length(strText) - 1));
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

  This method outputs the currently being searched path to the screen.

  @precon  None.
  @postcon Outputs the currently being searched path to the screen.

  @param   strPath as a String

**)
Procedure OutputCurrentSearchPath(strPath : String);

Const
  strLabel : String = 'Searching... ';
  iLength : Integer = 14;

Var
  C : CONSOLE_SCREEN_BUFFER_INFO;
  i, j : Integer;

Begin
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), C);
  While Length(strPath) > (iWidth - iLength) Do
    Begin
      If Pos('...', strPath) = 0 Then
        Begin
          i := PosOfNthChar('\', strPath, 1) + 1;
          j := PosOfNthChar('\', strPath, 2);
          If (i > 0) And (j > 0) Then
            strPath := StringReplace(strPath, Copy(strPath, i, j - i), '...', []);
        End Else
        Begin
          i := PosOfNthChar('\', strPath, 2);
          j := PosOFNthChar('\', strPath, 3);
          If (i > 0) And (j > 0) Then
            strPath := StringReplace(strPath, Copy(strPath, i, j - i), '', []);
        End;
    End;
  strPath := Format('%s%-*s', [strLabel, iWidth - iLength, strPath]);
  Write(strPath);
  SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), C.dwCursorPosition);
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

  @param   strPath as a String

**)
Procedure PrintFooter(strPath : String);

Const
  strMsg = '%15.0n bytes in %1.0n Files in %1.0n Directories and %15.0n bytes Free.';

Var
  i : Integer;

Begin
  i := 0;
  If Length(strPath) >= 2 Then
    If strPath[2] = ':' Then
      i := Integer(strPath[1]) - Integer('A') + 1;
  OutputLinesToConsole(Format(strMsg, [iFileSize + 0.1, iFiles + 0.1, iDirectories + 0.1,
    DiskFree(i) + 0.1]));
  OutputLinesToConsole;
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
  OutputLinesToConsole('    [/T RASHFDV] [/D "(DD[/MM[/YY [HH:MM[:SS]]]]-DD[/MM[/YY [HH:MM[:SS]]]])"]');
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

  @bug     This method will only work on size that are integers because of Val().    

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
  If slSearchParams.Count = 0 Then
    Raise Exception.Create('You need to specify at least one search criteria.');
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
  iSize : Int64;

Begin
  OutputCurrentSearchPath(strPath);
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
      With recSearch.FindData Do // Workaround for files larger than 2147483647
        iSize := Int64(nFileSizeHigh * MAXDWORD) + nFileSizeLow;
      If clsAttrRange In CommandLineSwitches Then
        Begin
          boolFound := boolFound And (recSearch.Attr And iFileAttrs > 0);
          boolFound := boolFound And (recSearch.Attr And iTypeAttrs > 0);
        End;
      If clsSizeRange In CommandLineSwitches Then
        Begin
          boolFound := boolFound And (iSize >= iLSize);
          boolFound := boolFound And (iSize <= iUSize);
        End;
      If clsDateRange In CommandLineSwitches Then
        Begin
          boolFound := boolFound And (FileDateToDateTime(recSearch.Time) >= dtLDate);
          boolFound := boolFound And (FileDateToDateTime(recSearch.Time) <= dtUDate);
        End;
      If boolFound Then
        Begin
          Inc(iFiles);
          Inc(iFileSize, iSize);
          Inc(Result, iSize);
          If Not (clsSummaryLevel In CommandLineSwitches) Then
            Begin
              Inc(iDirFiles);
              If Not boolDirPrinted Then
                Begin
                  OutputLinesToConsole(strPath);
                  boolDirPrinted := True;
                End;
              strOutput := Format('  %s  %15.0n  ', [
                FormatDateTime('ddd dd/mmm/yyyy hh:mm:ss', FileDateToDateTime(recSearch.Time)),
                iSize + 0.1]);
              strOutput := strOutput + OutputAttributes(recSearch.Attr);
              OutputLinesToConsole(strOutput + recSearch.Name);
            End;
        End;
      iResult := FindNext(recSearch);
      boolFound := True;
    End;
  SysUtils.FindClose(recSearch);
  If Not (clsSummaryLevel In CommandLineSwitches) Then
    If iDirFiles > 0 Then
      OutputLinesToConsole;
  (* Search sub directories *)
  If clsSubDirectories In CommandLineSwitches Then
    Begin
      iResult := FindFirst(strPath + '*.*', faDirectory, recSearch);
      While (iResult = 0)  Do
        Begin
          If recSearch.Attr And faDirectory > 0 Then
            If (recSearch.Name <> '.') And (recSearch.Name <> '..') Then
              Inc(Result, SearchDirectory(strPath + recSearch.Name + '\', strPattern,
                iLevel));
          iResult := FindNext(recSearch);
        End;
      SysUtils.FindClose(recSearch);
    End;
  (* Output directory summary levels *)
  If clsSummaryLevel In CommandLineSwitches Then
    Begin
      If (iLevel > -1) And (iLevel <= iSummaryLevel) Then
        If Not ((Result = 0) And (clsSupressZeros In CommandLineSwitches)) Then
        OutputLinesToConsole(Format('%15.0n%s%s', [
          Result + 0.1, StringOfChar(#32, 2 + 2 * iLevel), strPath]));
    End;
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
              If clsSummaryLevel In CommandLineSwitches Then
                OutputLinesToConsole;
              PrintFooter(strPath);
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
