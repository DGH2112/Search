Program SearchRX;

{$APPTYPE CONSOLE}

{$R *.RES}

Uses
  SysUtils, Windows,
  FileDirX in 'FILEDIRX.PAS',
  FileRegX in 'FILEREGX.PAS';


{ -------------------------------------------------------------------------

   Global Flags and Variables

  -------------------------------------------------------------------------- }

Var
  boolSearchSubDirectories : Boolean;
  boolDebug : Boolean;
  boolShowDate : Boolean;
  boolShowTime : Boolean;
  boolShowAttributes : Boolean;
  boolShowSize  : Boolean;
  boolPause : Boolean;
  strSearchPath : String;
  i : Integer;
  iFiles : Integer;
  iDirectories : Integer;
  FileAttrs : TdeAttrSet;
  TypeAttrs : TdeTypeSet;
  iUSize : Integer;
  iLSize  :Integer;
  iLDate : Double;
  iUDate : Double;
  iFileSize : Int64;
  iHeight : Integer;
  iWidth : Integer;
  iLinesOutputted : Integer;

{ -------------------------------------------------------------------------

   This routine is used to output all the text to the screen. It monitors
   how many lines have been output and pauses after a full screen full.

   OutputLinesToConsole(
     strText   // Text to output as a string
   );

  -------------------------------------------------------------------------- }

Procedure OutputLinesToConsole(strText : String = '');

Begin
  Writeln(strText);
  Inc(iLinesOutputted);
  If boolPause And (iLinesOutputted >= (iHeight - 1)) Then
    Begin
      Write('Press any <Enter> to continue...');
      Readln;
      iLinesOutputted := 0;
    End;
End;

{ -------------------------------------------------------------------------

   This routine get and stores the current number of lines and columns
   in the console window.

   GetConsoleInformation(
   );

  -------------------------------------------------------------------------- }

Procedure GetConsoleInformation;

Var
  ConsoleInfo : _CONSOLE_SCREEN_BUFFER_INFO;

Begin
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), ConsoleInfo);
  iWidth := ConsoleInfo.dwSize.X;
  iHeight := ConsoleInfo.dwSize.Y;
End;

{ -------------------------------------------------------------------------

   This routine extract the build number from the EXE resources for
   display in the app title.

   GetBuildNumber(
   );

  -------------------------------------------------------------------------- }

Function GetBuildNumber : String;

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;

Begin
  { Build Number }
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      with VerValue^ do
      begin
        Result := IntToStr(dwFileVersionMS shr 16) +
          '.' + IntToStr(dwFileVersionMS and $FFFF) +
          '.' + IntToStr(dwFileVersionLS shr 16) +
          '.' + IntToStr(dwFileVersionLS and $FFFF);
      end;
      FreeMem(VerInfo, VerInfoSize);
    End Else
      OutputLinesToConsole('This executable does not contain any version information.');
End;

{ -------------------------------------------------------------------------

   Prints a Title for the application on the screen.

   PrintTitle(
   );

  -------------------------------------------------------------------------- }

Procedure PrintTitle;

Begin
  OutputLinesToConsole('SearchRX 0.9 (Build ' + GetBuildNumber +
    ') Regular Expression Search Tool.');
  OutputLinesToConsole('Written by David Hoyle - Portions copyright of Julian Bucknell');
  OutputLinesToConsole;
End;

{ -------------------------------------------------------------------------

   Prints the path and search pattern before each search

   PrintHeader(
     strPath      // The path tot be searched as a string
     strPattern   // The pattern to be matched as a string
   );

  -------------------------------------------------------------------------- }

Procedure PrintHeader(strPath, strPattern : String);

Begin
  OutputLinesToConsole('Searching "' + strPath + '" for "' + strPattern + '".');
  iFiles := 0;
  iDirectories := 0;
  iFileSize := 0;
End;

{ -------------------------------------------------------------------------

   Prints a footer after the search displaying stats on the search.

   PrintFooter(
   );

  -------------------------------------------------------------------------- }

Procedure PrintFooter;

Begin
  OutputLinesToConsole(Format('%1.0n bytes in %d Files in %d Directories.',
    [iFileSize + 0.1, iFiles, iDirectories]));
End;

{ -------------------------------------------------------------------------

   This gets all the command lines switches and sets numerous flags.

   GetCommandLineSwitches(
   );

  -------------------------------------------------------------------------- }

Procedure GetCommandLineSwitches;

Var
  i, j : Integer;
  iStart, iMiddle, iEnd : Integer;

Begin
  (*
    If /? or no command print some help text
  *)
  If FindCmdLineSwitch('?', ['/'], True) Or (ParamCount = 0) Then
    Begin
      PrintTitle;
      OutputLinesToConsole('Syntax:');
      OutputLinesToConsole('  SearchRX [/A] [/D] [/L] [/S] [/T]');
      OutputLinesToConsole('    [/ATTR:RASHFDV] ["/DATE(DD/MM/YY HH:MM:SS-DD/MM/YY HH:MM:SS)"]');
      OutputLinesToConsole('    [/PATH=startdirectory] [/SIZE(lowsize-highsize)]');
      OutputLinesToConsole('');
      OutputLinesToConsole('    /?     This help screen');
      OutputLinesToConsole('    /A     Show file and directory attributes');
      OutputLinesToConsole('    /D     Show file and directory dates');
      OutputLinesToConsole('    /L     Show file and directory sizes');
      OutputLinesToConsole('    /P     Pause the results after each screen');
      OutputLinesToConsole('    /S     Recurse subdirectories');
      OutputLinesToConsole('    /T     Show file and directory times');
      OutputLinesToConsole('');
      OutputLinesToConsole('    /ATTR: R = Read Only, A = Archive, S = System, H = Hidden, F = File,');
      OutputLinesToConsole('           D = Directory, and V=  Volume ID');
      OutputLinesToConsole('    /DATE( local computer short date and time format )');
      OutputLinesToConsole('    /PATH= Relative or Direct path to a starting directory');
      OutputLinesToConsole('    /SIZE( lower bounding size in bytes - upper bounding size in bytes )');
      OutputLinesToConsole('');
      OutputLinesToConsole('NOTE: The date time input format is dependent on your local settings');
      Raise Exception.Create('');;
    End;
  (*
    get and set standard switches /S /Debug /D /T /P /L /S /A
  *)
  boolSearchSubDirectories := FindCmdLineSwitch('S', ['/'], True);
  boolDebug := FindCmdLineSwitch('Debug', ['/'], True);
  boolShowDate := FindCmdLineSwitch('D', ['/'], True);
  boolPause := FindCmdLineSwitch('P', ['/'], True);
  boolShowTime := FindCmdLineSwitch('T', ['/'], True);
  boolShowAttributes := FindCmdLineSwitch('A', ['/'], True);
  boolShowSize := FindCmdLineSwitch('L', ['/'], True);
  (*
    Find out if a starting path has been specified
  *)
  For i := 1 To ParamCount Do
    If LowerCase(Copy(ParamStr(i), 1, 6)) = '/path=' Then
      strSearchPath := Copy(ParamStr(i), 7, Length(ParamStr(i)) - 6);
  If strSearchPath = '' Then
    strSearchPath := GetCurrentDir;
  If strSearchPath[Length(strSearchpath)] <> '\' Then
    strSearchpath := ConCat(strSearchPath, '\');
  (*
    Find out if specific attributes have been asked for
  *)
  FileAttrs := [];
  TypeAttrs := [];
  For i := 1 To ParamCount Do
    If LowerCase(Copy(ParamStr(i), 1, 6)) = '/attr:' Then
      For j := 7 To Length(ParamStr(i)) Do
        Case ParamStr(i)[j] Of
          'r', 'R' : FileAttrs := FileAttrs + [deaReadOnly];
          'a', 'A' : FileAttrs := FileAttrs + [deaAltered];
          's', 'S' : FileAttrs := FileAttrs + [deaSystem];
          'h', 'H' : FileAttrs := FileAttrs + [deaHidden];
          'f', 'F' : TypeAttrs := TypeAttrs + [detFile];
          'd', 'D' : TypeAttrs := TypeAttrs + [detDirectory];
          'v', 'V' : TypeAttrs := TypeAttrs + [detVolumeID];
        End;
  If FileAttrs = [] Then
    FileAttrs := [deaNormal, deaAltered, deaReadOnly, deaHidden, deaSystem];
  If TypeAttrs = [] Then
    TypeAttrs := [detFile, detDirectory, detVolumeID];
  (*
    Find out if a starting and ending size has been specified
  *)
  iLSize := 0;
  iUSize := 2147483647;
  For i := 1 To ParamCount Do
    If LowerCase(Copy(ParamStr(i), 1, 6)) = '/size(' Then
      Begin
        iStart := Pos('(', ParamStr(i));
        If iStart <> 0 Then
          Begin
            iMiddle := Pos('-', ParamStr(i));
            If iMiddle <> 0 Then
              Begin
                iEnd := Pos(')', ParamStr(i));
                If iEnd <> 0 Then
                  Begin
                    iLSize := StrToInt(Copy(ParamStr(i), iStart + 1,
                      iMiddle - iStart - 1));
                    iUSize := StrToInt(Copy(ParamStr(i), iMiddle + 1,
                      iEnd - iMiddle - 1));
                  End Else
                    Raise Exception.Create('Could not find SIZE end parenthesis.');
              End Else
                Raise Exception.Create('Could not find SIZE range operator.');
          End Else
            Raise Exception.Create('Could not find SIZE start parenthesis.');
      End;
  (*
    Find out if a starting and ending Date / Time has been specified
  *)
  iLDate := 0;
  iUDate := 999999;
  For i := 1 To ParamCount Do
    If LowerCase(Copy(ParamStr(i), 1, 6)) = '/date(' Then
      Begin
        iStart := Pos('(', ParamStr(i));
        If iStart <> 0 Then
          Begin
            iMiddle := Pos('-', ParamStr(i));
            If iMiddle <> 0 Then
              Begin
                iEnd := Pos(')', ParamStr(i));
                If iEnd <> 0 Then
                  Begin
                    iLDate := strToDateTime(Copy(ParamStr(i), iStart + 1,
                      iMiddle - iStart - 1));
                    iUDate := strToDateTime(Copy(ParamStr(i), iMiddle + 1,
                      iEnd - iMiddle - 1));
                  End Else
                    Raise Exception.Create('Could not find DATE end parenthesis.');
              End Else
                Raise Exception.Create('Could not find DATE range operator.');
          End Else
            Raise Exception.Create('Could not find DATE start parenthesis.');
      End;
End;

{ -------------------------------------------------------------------------

   This is the main recursive routine that searches the currently passed
   directory for files and then if subdirectories are required calls
   its self on those dreictories.

   SearchDirectories(
     strPath      // The path as a string
     strPattern   // The RegEx attern to match as a string
   );

  -------------------------------------------------------------------------- }

Procedure SearchDirectory(strPath, strPattern : String);

Var
  iResult : Integer;
  recSearch : TdeSearchRec;
  boolDirPrinted : Boolean;
  strOutput : String;

Begin
  Inc(iDirectories);
  boolDirPrinted := False;
  (*
    Find Files
  *)
  iResult := FindFirstEx(strPath, strPattern, TypeAttrs, FileAttrs, recSearch);
  While iResult = 0 Do
    Begin
      If (recSearch.srSize >= iLSize) And (recSearch.srSize <= iUSize) And
        (FileDateToDateTime(recSearch.srTime) >= iLDate) And
        (FileDateToDateTime(recSearch.srTime) <= iUDate) Then
        Begin
          Inc(iFiles);
          Inc(iFileSize, recSearch.srSize);
          If Not boolDirPrinted Then
            Begin
              OutputLinesToConsole(strPath);
              boolDirPrinted := True;
            End;
          strOutput := '  ';
          If boolShowDate Then
            strOutput := strOutput + FormatDateTime('ddd dd/mmm/yyyy  ',
              FileDateToDateTime(recSearch.srTime));
          If boolShowTime Then
            strOutput := strOutput + FormatDateTime('hh:mm:ss  ',
              FileDateToDateTime(recSearch.srTime));
          If boolShowSize Then
            strOutput := strOutput + Format('%11.0n  ', [recSearch.srSize + 0.1]);
          If boolShowAttributes Then
            Begin
              If deaReadOnly In recSearch.srAttrs Then
                strOutput := strOutput + 'R'
              Else
                strOutput := strOutput + '.';
              If deaAltered In recSearch.srAttrs Then
                strOutput := strOutput + 'A'
              Else
                strOutput := strOutput + '.';
              If deaSystem In recSearch.srAttrs Then
                strOutput := strOutput + 'S'
              Else
                strOutput := strOutput + '.';
              If deaHidden In recSearch.srAttrs Then
                strOutput := strOutput + 'H'
              Else
                strOutput := strOutput + '.';
              If detFile = recSearch.srType Then
                strOutput := strOutput + 'F'
              Else
                strOutput := strOutput + '.';
              If detDirectory = recSearch.srType Then
                strOutput := strOutput + 'D'
              Else
                strOutput := strOutput + '.';
              If detVolumeID = recSearch.srType Then
                strOutput := strOutput + 'V  '
              Else
                strOutput := strOutput + '.  ';
            End;
          OutputLinesToConsole(strOutput + recSearch.srName);
        End;
      iResult := FindNextEx(recSearch);
    End;
  FindCloseEx(recSearch);
  (*
    Search sub directories
  *)
  If boolSearchSubDirectories Then
    Begin
      iResult := FindFirstEx(strPath, '*', [detDirectory],
        [deaNormal, deaAltered, deaReadOnly, deaHidden, deaSystem], recSearch);
      While iResult = 0 Do
        Begin
          If (recSearch.srName <> '.') And (recSearch.srName <> '..') Then
            SearchDirectory(strPath + recSearch.srName + '\', strPattern);
          iResult := FindNextEx(recSearch);
        End;
      FindCloseEx(recSearch);
    End;
End;

{ -------------------------------------------------------------------------

   Main Program body.

  -------------------------------------------------------------------------- }

Begin
  Try
    iLinesOutputted := 0;
    GetConsoleInformation;
    GetCommandLineSwitches;
    PrintTitle;
    For i := 1 To ParamCount Do
      If ParamStr(i)[1] <> '/' Then
        Begin
          PrintHeader(strSearchPath, ParamStr(i));
          SearchDirectory(strSearchPath, ParamStr(i));
          PrintFooter;
        End;
    If boolDebug Then
      Readln;
  Except
    On E : Exception Do
      Writeln(E.Message);
  End;
End.
