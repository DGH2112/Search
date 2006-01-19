(**

  This module defines the Search Win32 console application. This application
  is designed to provide regular expression search capabilities for files on
  disk.

  @Version 1.0
  @Author  David Hoyle
  @Date    19 Jan 2006

**)
Program Search;

{$APPTYPE CONSOLE}

{$R *.RES}

uses
  SysUtils,
  Windows,
  Classes,
  ACCCTRL,
  Contnrs,
  DGHLibrary in '..\..\library\DGHLibrary.pas';

Type
  (** This is a list of boolean on / off command line switches. **)
  TCommandLineSwitch = (
    clsShowHelp,         { /? or -? }
    clsSubDirectories,   { /s or -s }
    clsDebug,            { /!       }
    clsShowAttribs,      { /a or -a }
    clsPause,            { /p or -p }
    clsSummaryLevel,     { /1..9 or -1..9 }
    clsSupressZeros,     { /0 or -0 }
    clsDateRange,        { /d or -d }
    clsSizeRange,        { /z or -z }
    clsAttrRange,        { /t or -t }
    clsQuiet,            { /q or -q }
    clsOwner,            { /w or -w }
    clsOrderBy           { /o or -o }
  );

  (** This is a set of boolean command line switches. **)
  TCommandLineSwitches = Set of TCommandLineSwitch;
  
  (** A type for a pointer to a PSID structure **)
  PPSID = ^PSID;
  
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
  
  (** An enumerate to define the order for sorting the files. **)
  TOrderBy = (obNone, obName, obSize, obDate, obOwner, obAttribute);
  (** An enumberate to define whether the files should be ascending or
      descending **)
  TOrderDirection = (odAscending, odDescending);

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
  (** A variable for a collection of files. **)
  objFiles : TFiles;
  (** A type to indicate the order of file sorting **)
  OrderFilesBy : TOrderBy;
  (** A type to indicate the direction of sorting of files. **)
  OrderFilesDirection : TOrderDirection;
  
Const
  (** A constant to define that only files should be listed. **)
  iFileOnly = $0100;

  (**

     This method returns the security information for a specified file.

     @precon  See Win32 Help.
     @postcon Returns the security information for a specified file.

     @param   pObjectName          as a PAnsiChar
     @param   ObjectType           as a SE_OBJECT_TYPE
     @param   SecurityInfo         as a SECURITY_INFORMATION
     @param   ppsidOwner           as a PPSID
     @param   ppsidGroup           as a PPSID
     @param   ppDacl               as a PACL
     @param   ppSacl               as a PACL
     @param   ppSecurityDescriptor as a PSECURITY_DESCRIPTOR as a reference
     @return  a DWORD

   **)
   Function GetNamedSecurityInfo(pObjectName: PAnsiChar; ObjectType: SE_OBJECT_TYPE;
    SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PPSID; ppDacl,
    ppSacl: PACL; var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD; Stdcall;
         external 'advapi32.dll' name 'GetNamedSecurityInfoA';

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
  If clsQuiet In CommandLineSwitches Then Exit;
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), C);
  While Length(strPath) > (iWidth - iLength) Do
    Begin
      If Pos('...', strPath) = 0 Then
        Begin
          i := PosOfNthChar(strPath, '\', 1) + 1;
          j := PosOfNthChar(strPath, '\', 2);
          If (i > 0) And (j > 0) Then
            strPath := StringReplace(strPath, Copy(strPath, i, j - i), '...', []);
        End Else
        Begin
          i := PosOfNthChar(strPath, '\', 2);
          j := PosOFNthChar(strPath, '\', 3);
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
  dtDate : TDateTime;

Begin
  strBuildNumber := GetBuildNumber(iMajor, iMinor, iBugFix);
  OutputLinesToConsole(Format(strTitle, [iMajor, iMinor, strBugFix[iBugFix + 1],
    strBuildNumber]));
  FileAge(ParamStr(0), dtDate);
  OutputLinesToConsole(
    Format('Written by David Hoyle (c) %s', [FormatDateTime('mmm/yyyy', dtDate)]));
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
  strMsg1 = '  Found %15.0n bytes in %1.0n Files in %1.0n Directories.';
  strMsg2 = '  Total space %15.0n bytes, and %15.0n bytes Free.';

Var
  iFreeBytesAvailableToCaller,
  iTotalNumberOfBytes,
  iTotalNumberOfFreeBytes : Int64;

Begin
  GetDiskFreeSpaceEx(PChar(strPath), iFreeBytesAvailableToCaller,
    iTotalNumberOfBytes, @iTotalNumberOfFreeBytes);
  OutputLinesToConsole(Format(strMsg1, [iFileSize + 0.1, iFiles + 0.1,
    iDirectories + 0.1]));
  OutputLinesToConsole(Format(strMsg2, [iTotalNumberOfBytes + 0.1,
    iTotalNumberOfFreeBytes + 0.1]));
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
  OutputLinesToConsole('  Search searchparam1 [searchparam2...] [/A] [/S] [/Q] [/W]');
  OutputLinesToConsole('    [/T RASHFDV] [/D "(DD[/MM[/YY [HH:MM[:SS]]]]-DD[/MM[/YY [HH:MM[:SS]]]])"]');
  OutputLinesToConsole('    [/Z LowerByteSize-UpperByteSize] [/O NDAOS]');
  OutputLinesToConsole('');
  OutputLinesToConsole('    searchparam# = [drive:\path\]filter1[;filter2[;filter3[;...]]]');
  OutputLinesToConsole('    filter# can contain wildcards * and ? within the filename. ');
  OutputLinesToConsole('');
  OutputLinesToConsole('    /?     This help screen');
  OutputLinesToConsole('    /A     Show file and directory attributes');
  OutputLinesToConsole('    /P     Pause the results after each screen');
  OutputLinesToConsole('    /S     Recurse subdirectories');
  OutputLinesToConsole('    /1..9  Summarise subdirectories');
  OutputLinesToConsole('    /0     Hide empty Summarised subdirectories');
  OutputLinesToConsole('    /T     Show only files with certain attributes');
  OutputLinesToConsole('           R = Read Only, A = Archive, S = System, H = Hidden, F = File,');
  OutputLinesToConsole('           D = Directory, and V=  Volume ID');
  OutputLinesToConsole('    /D     ( lower bounding date and time - upper bounding date and time )');
  OutputLinesToConsole('    /Z     ( lower bounding size in bytes - upper bounding size in bytes )');
  OutputLinesToConsole('    /Q     Quiet mode');
  OutputLinesToConsole('    /W     Owner Information');
  OutputLinesToConsole('    /O     Order files ascending [+] and descending [-]');
  OutputLinesToConsole('           N = Name, A = Attribute, D = Date and Time, O = Owner, S = Size');
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
  If iSwitch > ParamCount Then
    Raise Exception.Create('Missing Attribute directive');
  iFileAttrs := 0;
  iTypeAttrs := 0;
  For i := 1 To Length(ParamStr(iSwitch)) Do
    Case ParamStr(iSwitch)[i] Of
      'r', 'R' : iFileAttrs := iFileAttrs Or faReadOnly;
      'a', 'A' : iFileAttrs := iFileAttrs Or faArchive;
      's', 'S' : iFileAttrs := iFileAttrs Or faSysFile;
      'h', 'H' : iFileAttrs := iFileAttrs Or faHidden;
      'f', 'F' : iTypeAttrs := iTypeAttrs Or iFileOnly;
      'd', 'D' : iTypeAttrs := iTypeAttrs Or faDirectory;
      'v', 'V' : iTypeAttrs := iTypeAttrs Or faVolumeID;
    Else
      Raise Exception.CreateFmt(strNotAValidAttrList, [ParamStr(iSwitch)]);
    End;
  If iTypeAttrs = 0 Then
    iTypeAttrs := faDirectory Or faVolumeID;
End;

(**

  This method gets the command line information for the ordering of the found
  files.

  @precon  None.
  @postcon Gets the command line information for the ordering of the found
           files.

  @param   iSwitch as an Integer as a reference

**)
Procedure GetOrderBy(var iSwitch : Integer);

Var
  strOrderBy : String;
  iIndex : Integer;

Begin
  Include(CommandLineSwitches, clsOrderBy);
  Inc(iSwitch); // Next parameter is the order by
  If iSwitch > ParamCount Then
    Raise Exception.Create('Missing Order By directive');
  strOrderBy := ParamStr(iSwitch);
  iIndex := 1;
  If Length(strOrderBy) < iIndex Then
    Raise Exception.Create('Missing Order By directive.');
  If strOrderBy[iIndex] = '-' Then
    Begin
      OrderFilesDirection := odDescending;
      Inc(iIndex);
    End;
  If Length(strOrderBy) < iIndex Then
    Raise Exception.Create('Missing Order By directive.');
  Case strOrderBy[iIndex] Of
    'n', 'N': OrderFilesBy := obName;
    'd', 'D': OrderFilesBy := obDate;
    's', 'S': OrderFilesBy := obSize;
    'a', 'A': OrderFilesBy := obAttribute;
    'o', 'O': OrderFilesBy := obOwner;
  Else
    Raise Exception.Create('Invalid Order By directive.')
  End
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
  OrderFilesBy := obNone;
  OrderFilesDirection := odAscending;
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
              '1'..'9': GetSummaryLevel(iSwitch, iSwitches);
              '0'     : Include(CommandLineSwitches, clsSupressZeros);
              'd', 'D': GetDateRange(iSwitch);
              'z', 'Z': GetSizeRange(iSwitch);
              't', 'T': GetAttributes(iSwitch);
              'q', 'Q': Include(CommandLineSwitches, clsQuiet);
              'w', 'W': Include(CommandLineSwitches, clsOwner);
              'o', 'O': GetOrderBy(iSwitch);
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
  If (slSearchParams.Count = 0) And Not (ClsShowHelp In CommandLineSwitches) Then
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
  Result := '.......';
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

(**

  This method returns the owner of the files from a SID.

  @precon  None.
  @postcon Returns the owner of the files from a SID.

  @param   SID as a PSID
  @return  a String

**)
Function LookupAccountBySID(SID : PSID) : String;

Var
  Name, RefDomain : String;
  NameSize, RefDomainSize : DWORD;
  Use : SID_NAME_USE;
  
Begin
  NameSize := 0;
  RefDomainSize := 0;
  LookupAccountSID(Nil, SID, Nil, NameSize, Nil, RefDomainSize, Use);
  SetLength(Name, NameSize);
  SetLength(RefDomain, RefDomainSize);
  LookupAccountSID(Nil, SID, PChar(Name), NameSize, PChar(RefDomain),
    RefDomainSize, Use);
  If Name = '' Then
    Name := '(Unknown)';
  Result := PChar(RefDomain) + '/' + PChar(Name);
End;

(**

  Outputs the owner of the filename.

  @precon  None.
  @postcon Outputs the owner of the file.

  @param   strFileName as a String
  @return  a String

**)
Function OutputOwner(strFileName : String) : String;

Var
  SD : PSecurityDescriptor;
  Owner : PSID;

Begin
  If GetNamedSecurityInfo(PChar(strFileName), SE_FILE_OBJECT,
    OWNER_SECURITY_INFORMATION, @Owner, Nil, Nil, Nil,
    Pointer(SD)) = ERROR_SUCCESS Then
    Begin
      Result := LookupAccountBySID(Owner);
      LocalFree(Cardinal(SD));
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
  @param   slPatterns  as a TStringList
  @param   iLevel     as an Integer as a Reference
  @return  an Int64

**)
Function SearchDirectory(strPath: String; slPatterns : TStringList;
  Var iLevel : Integer) : Int64;

Var
  iResult : Integer;
  recSearch : TSearchRec;
  boolDirPrinted : Boolean;
  strOutput : String;
  boolFound : Boolean;
  iDirFiles : Integer;
  iSize : Int64;
  iAttributes : Integer;
  i : Integer;
  iWidth : Integer;
  iPattern: Integer;

Begin
  OutputCurrentSearchPath(strPath);
  Inc(iLevel);
  Result := 0;
  iDirFiles := 0;
  Inc(iDirectories);
  boolDirPrinted := False;
  (* Find Files *)
  boolFound := True;
  objFiles := TFiles.Create;
  Try
    For iPattern := 0 To slPatterns.Count - 1 Do
      Begin
        iResult := FindFirst(strPath + slPatterns[iPattern], faAnyFile, recSearch);
        Try
          While iResult = 0  Do
            Begin
              With recSearch.FindData Do // Workaround for files larger than 2147483647
                Begin
                  iSize := Int64(nFileSizeHigh) * Int64(MAXDWORD);
                  iSize := iSize + nFileSizeLow;
                End;
              If clsAttrRange In CommandLineSwitches Then
                Begin
                  If faDirectory And recSearch.Attr > 0 Then
                    iAttributes := faDirectory
                  Else If faVolumeID And recSearch.Attr > 0 Then
                    iAttributes := faVolumeID
                  Else
                    iAttributes := iFileOnly;
                  boolFound := boolFound And (iAttributes And iTypeAttrs > 0);
                  boolFound := boolFound And ((recSearch.Attr And iFileAttrs > 0) Or
                    (iFileAttrs = 0));
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
                      objFiles.Add(FileDateToDateTime(recSearch.Time), iSize,
                        OutputAttributes(recSearch.Attr),
                        OutputOwner(strPath + recSearch.name), recSearch.Name)
                    End;
                End;
              iResult := FindNext(recSearch);
              boolFound := True;
            End;
        Finally
          SysUtils.FindClose(recSearch);
        End;
      End;
    If OrderFilesBy <> obNone Then
      objFiles.OrderBy(OrderFilesBy, OrderFilesDirection);
    For i := 0 To objFiles.Count - 1 Do
      Begin
        strOutput := Format('  %s  %15.0n  ', [
          FormatDateTime('ddd dd/mmm/yyyy hh:mm:ss', objFiles.FileInfo[i].Date),
          objFiles.FileInfo[i].Size + 0.1]);
        If clsShowAttribs In CommandLineSwitches Then
          strOutput := strOutput + Format('%s  ', [objFiles.FileInfo[i].Attr]);
        If clsOwner In CommandLineSwitches Then
          Begin
            iWidth := objFiles.OwnerWidth;
            strOutput := strOutput + Format('%-*s  ', [iWidth,
              objFiles.FileInfo[i].Owner]);
          End;
        OutputLinesToConsole(strOutput + objFiles.FileInfo[i].Name);
      End;
  Finally
    objFiles.Free;
  End;
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
              Inc(Result, SearchDirectory(strPath + recSearch.Name + '\',
                slPatterns, iLevel));
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
  (** A string list to hold the different search patterns. **)
  slPatterns : TStringList;
  (** A variable to iterate file filter patterns **)
  iPattern : Integer;

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
              strPath := slSearchParams.Names[i];
              If strPath = '' Then
                strPath := GetCurrentDir;
              If strPath[Length(strPath)] <> '\' Then
                strPath := strPath + '\';
              strSearch := slSearchParams.Values[slSearchParams.Names[i]];
              PrintHeader(strPath, strSearch);
              slPatterns := TStringList.Create;
              Try
                For iPattern := 1 To CharCount(';', strSearch) + 1 Do
                  slPatterns.Add(GetField(strSearch, ';', iPattern));
                SearchDirectory(strPath, slPatterns, iLevel);
              Finally
                slPatterns.Free;
              End;
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
