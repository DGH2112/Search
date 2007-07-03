(**

  This module defines the Search Win32 console application. This application
  is designed to provide regular expression search capabilities for files on
  disk.

  @Version 1.0
  @Author  David Hoyle
  @Date    03 Jul 2007

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
  DGHLibrary in '..\..\library\DGHLibrary.pas',
  FileHandling in 'Source\FileHandling.pas';

resourcestring
  (** An exception message for a missing colon in the order by definition. **)
  strColonExpectedInOrderBy = 'Colon expected in Order By definition.';
  (** An execption message for a missing criteria in the order by definition. **)
  strMissingOrderByDirective = 'Missing Order By definition.';
  (** An exception message for an invalid order by criteria. **)
  strInvalidOrderByDirective = 'Invalid Order By definition.';
  (** An exception message for a missing Attribute definition. **)
  strMissingAttribDirective = 'Missing Attribute directive';
  (** An exception message for a missing ] in an attribute definition. **)
  strCloseSquareExpectedInAttribDef = '"]" Expected in Attribute Definition';
  (** An exception message for a missing [ in an attribute definition. **)
  strOpenSquareExpectedInAttribDef = '"[" Expected in Attribute Definition.';
  (** An exception message for an invalid Attribute criteria definition. **)
  strNotAValidAttrList = '"%s" is not a valid attribute criteria.';
  (** An exception message for a missing [ in a Date Range. **)
  strOpenSquareExpectedInDate = '"[" Expected in Date Range.';
  (** An exception message for a missing date range separater. **)
  strMissingDateRangeSeparater = 'Missing Start Date Range Separater "-".';
  (** An exception message for a missing ] in the date range. **)
  strCloseSquareExpectedInDate = '"]" Expected in Date Range.';
  (** An exception message for a missing [ in a size Range. **)
  strOpenSquareExpectedInSize = '"[" Expected in Size Range.';
  (** An exception message for a missing size range separater. **)
  strMissingSizeRangeSeparater = 'Missing Start Size Range Separater "-".';
  (** An exception message for a missing ] in the size range. **)
  strCloseSquareExpectedInSize = '"]" Expected in Size Range.';
  (** An exception message for an invalid lower size range. **)
  strInvalidLowerSizeRange = 'Invalid Lower Size Range';
  (** An exception message for an invalid upper size range. **)
  strInvalidUpperSizeRange = 'Invalid Upper Size Range';
  (** An exception message for a missing colon in the date type definition. **)
  strColonExpectedInDateType = 'Colon expected in date Type definition.';
  (** An execption message for a missing criteria in the Date Type definition. **)
  strMissingDateTypeDirective = 'Missing Date Type definition.';
  (** An exception message for an invalid Date Type criteria. **)
  strInvalidDateTypeDirective = 'Invalid Date Type definition.';
  (** An exception message for a missing Search Text definition. **)
  strMissingSearchText = 'Missing Search Text';
  (** An exception message for a missing ] in an Search definition. **)
  strCloseSquareExpectedInSearchDef = '"]" Expected in Search Definition';
  (** An exception message for a missing [ in an Search definition. **)
  strOpenSquareExpectedInSearchDef = '"[" Expected in Search Definition.';
  (** An exception message for the exclusions file not found. **)
  strExclusionsNotFound = 'The filename "%s" was not found to process exclusions in the searches.';
  (** An exception message for the Owner Search Criteria being empty. **)
  strOwnerSearchIsEmpty = 'You must specify a valid Owner Search Criteria.';

(**

  This method increments the position of the index within the current command
  line switch and checks to see if the index is valid else raise the exception
  message provided.

  @precon  None.
  @postcon Checks the index to be valid else raises an exception.

  @param   iIndex          as an Integer as a reference
  @param   iSwitch         as an Integer as a reference
  @param   strExceptionMsg as a String

**)
procedure IncrementSwitchPosition(var iIndex, iSwitch: Integer;
  strExceptionMsg : String);

begin
  Inc(iIndex);
  If Length(ParamStr(iSwitch)) < iIndex then
    Raise Exception.Create(strExceptionMsg);
end;

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
    clsOrderBy,          { /o or -o }
    clsSearchIn,         { /i or -i }
    clsDateType,         { /e or -e }
    clsDisplayCriteria,  { /c or -c }
    clsExclusions        { /x or -x }
  );
  
  (** This is an enumerate to defines whether the owner should be searched. **)
  TOwnerSearch = (osNone, osEquals, osNotEquals);
  
  (** This is an enumerate to defines where the owner should be searched. **)
  TOwnerSearchPos = (ospExact, ospStart, ospMiddle, ospEnd);

  (** This is a set of boolean command line switches. **)
  TCommandLineSwitches = Set of TCommandLineSwitch;

  (** An enumerate to define the type of date to display and search on. **)
  TDateType = (dtCreation, dtLastAccess, dtLastWrite);

  (** A type for a pointer to a PSID structure **)
  PPSID = ^PSID;

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

Var
  (** Define the applications command line switches. **)
  CommandLineSwitches : TCommandLineSwitches;
  (** The otal number of file found by the search. **)
  iFiles : Integer;
  (** The total number of directories found by the search. **)
  iDirectories : Integer;
  (** This is the upper limit of a file size search. **)
  iUSize : Int64;
  (** This is the lower limit of a file size search. **)
  iLSize  :Int64;
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
  (** A string for which should be searched for inside text files. **)
  strSearchInText : String;
  (** A variable to hold the type of date to display and search for. **)
  DateType : TDateType;
  (** A file name which hold exclusions which need to be applied to the searches. **)
  strExlFileName : String;
  (** A string list of exclusions to be applied to the searches. **)
  slExclusions : TStringList;
  (** A string to hold the owner search criteria. **)
  strOwnerSearch : String;
  OwnerSearch : TOwnerSearch;
  OwnerSearchPos : TOwnerSearchPos;

Const
  (** A constant to define that only files should be listed. **)
  iFileOnly = $0100;

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
  If clsQuiet In CommandLineSwitches Then
    Writeln(strText)
  Else
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

  Prints a Title for the application on the screen.

  @precon  None.
  @postcon Prints the title for the application on the screen.

**)
Procedure PrintTitle;

Const
  strTitle = 'Search %d.%d%s (Build %s) File Find and Summary Tool.';

Begin
  OutputLinesToConsole(GetConsoleTitle(strTitle));
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
  strMsg1 = '  Found %1.0n bytes in %1.0n Files in %1.0n Directories.';
  strMsg2 = '  Total space %1.0n bytes, and %1.0n bytes Free.';

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
  OutputLinesToConsole('  Search searchparam1 {searchparam2}... {/A} {/S} {/Q} {/W}');
  OutputLinesToConsole('    {/T[RASHFDV]} {/D:[{DD{/MM{/YY {HH:MM{:SS}}}}}-{DD{/MM{/YY {HH:MM{:SS}}}}}]');
  OutputLinesToConsole('    {/Z[{LowerByteSize}-{UpperByteSize}]} {/O:NDAOS} {/E:CAW} {/I[text]}');
  OutputLinesToConsole('    {/X[filename]}');
  OutputLinesToConsole('');
  OutputLinesToConsole('  searchparam# = {drive:\path\}filter1{;filter2{;filter3{;...}}}');
  OutputLinesToConsole('');
  OutputLinesToConsole('  filter# can contain wildcards * and ? within the filename. ');
  OutputLinesToConsole('');
  OutputLinesToConsole('  { ... } denotes optional items.');
  OutputLinesToConsole('');
  OutputLinesToConsole('  /?             This help screen');
  OutputLinesToConsole('  /A             Show file and directory attributes');
  OutputLinesToConsole('  /P             Pause the results after each screen');
  OutputLinesToConsole('  /S             Recurse subdirectories');
  OutputLinesToConsole('  /1..9          Summarise subdirectories');
  OutputLinesToConsole('  /0             Hide empty Summarised subdirectories');
  OutputLinesToConsole('  /T[RASHFFDV]   Show only files with certain attributes');
  OutputLinesToConsole('                 R = Read Only, A = Archive, S = System, H = Hidden,');
  OutputLinesToConsole('                 F = File, D = Directory, and V=  Volume ID');
  OutputLinesToConsole('  /D[{l}-{u}]    l = lower bounding date and time,  u = upper bounding date and time');
  OutputLinesToConsole('  /Z[{l}-{u}]    l = lower bounding size in bytes, u = upper bounding size in bytes');
  OutputLinesToConsole('  /Q             Quiet mode');
  OutputLinesToConsole('  /W {[Owner]}   Owner Information');
  OutputLinesToConsole('  /O:NADOS       Order files ascending [+] and descending [-]');
  OutputLinesToConsole('                 N = Name, A = Attribute, D = Date and Time, O = Owner, S = Size');
  OutputLinesToConsole('  /E:CAW         Type of file date to display and search on:');
  OutputLinesToConsole('                 C = Creation, A = Last Access, W = Last Write (Default)');
  OutputLinesToConsole('  /I[text]       Search within files for text.');
  OutputLinesToConsole('  /C             Display a list of Criteria and Command Line Options.');
  OutputLinesToConsole('  /X[FileName]   A list of exclusions to apply to paths and filenames.');
  OutputLinesToConsole('');
  OutputLinesToConsole('  NOTE: The date time input format is dependent on your local settings');
  OutputLinesToConsole('');;
End;

(**

  This local function searches the current command line parameter for the
  given end token while collecting the date string.

  @precon  None.
  @postcon Searches the current command line parameter for the given end token
           while collecting the date string.

  @param   chEndToken      as a Char
  @param   strExceptionMsg as a String
  @param   iIndex          as an Integer as a reference
  @param   iSwitch         as an Integer as a reference
  @param   strDate         as a String as a reference

**)
Procedure GetRangeString(chEndToken : Char; strExceptionMsg : String; var iIndex,
  iSwitch : Integer; var strDate : String);

Begin
  strDate := '';
  While (Length(ParamStr(iSwitch)) >= iIndex) And
    (ParamStr(iSwitch)[iIndex] <> chEndToken) Do
    Begin
      IncrementSwitchPosition(iIndex, iSwitch, strExceptionMsg);
      If ParamStr(iSwitch)[iIndex] <> chEndToken Then
        strDate := strDate + ParamStr(iSwitch)[iIndex];
    End;
End;

(**

  This method determines if a date range has been specified on the command line.

  @precon  None.
  @postcon Determines if a date range has been specified on the command line.

  @param   iSwitch as an Integer as a Reference
  @param   iIndex as an Integer as a Reference

**)
Procedure GetDateRange(var iSwitch, iIndex : Integer);

Var
  strDate : String;

Begin
  Include(CommandLineSwitches, clsDateRange);
  IncrementSwitchPosition(iIndex, iSwitch, strOpenSquareExpectedInDate);
  If ParamStr(iSwitch)[iIndex] <> '[' Then
    Raise Exception.Create(strOpenSquareExpectedInDate);
  GetRangeString('-', strMissingDateRangeSeparater, iIndex, iSwitch, strDate);
  If strDate <> '' Then
    dtLDate := ConvertDate(strDate)
  Else
    dtUDate := ConvertDate('01/Jan/1900 00:00:01');
  GetRangeString(']', strCloseSquareExpectedInDate, iIndex, iSwitch, strDate);
  If strDate <> '' Then
    dtUDate := ConvertDate(strDate)
  Else
    dtUDate := ConvertDate('31/Dec/2099 23:59:59');
End;

(**

  This method determines of a size range has been specified on the command line.

  @precon  None.
  @postcon Determines of a size range has been specified on the command line.

  @param   iSwitch as an Integer as a Reference
  @param   iIndex  as an Integer as a Reference

**)
Procedure GetSizeRange(Var iSwitch, iIndex : Integer);

Var
  strSize : String;

  (**

    This method converts the size as a string into a Int64 in the passed
    variable.

    @precon  None.
    @postcon Converts the size as a string into a Int64 in the passed variable.

    @param   iSize           as an Int64 as a reference
    @param   strExceptionMsg as a String
    @param   iDefault        as an Int64

  **)
  Procedure GetSize(var iSize : Int64; strExceptionMsg : String;
    iDefault : Int64);

  Var
    iErrorCode: Integer;
    iFactor : Int64;

  Begin
    If strSize <> '' Then
      Begin
        Case strSize[Length(strSize)] Of
          'k', 'K': iFactor := $400;
          'm', 'M': iFactor := $100000;
          'g', 'G': iFactor := $40000000;
          't', 'T': iFactor := $10000000000;
        Else
          iFactor := 1;
        End;
        If strSize[Length(strSize)] In ['k', 'K', 'm', 'M', 'g', 'G', 't', 'T'] Then
          strSize := Copy(strSize, 1, Length(strSize) - 1);
        Val(strSize, iSize, iErrorCode);
        If iErrorCode > 0 Then
          Raise Exception.Create(strExceptionMsg);
        iSize := iSize * iFactor;
      End Else
        iSize := iDefault;
  End;

Begin
  Include(CommandLineSwitches, clsSizeRange);
  IncrementSwitchPosition(iIndex, iSwitch, strOpenSquareExpectedInSize);
  If ParamStr(iSwitch)[iIndex] <> '[' Then
    Raise Exception.Create(strOpenSquareExpectedInSize);
  GetRangeString('-', strMissingSizeRangeSeparater, iIndex, iSwitch, strSize);
  GetSize(iLSize, strInvalidLowerSizeRange, 0);
  GetRangeString(']', strCloseSquareExpectedInSize, iIndex, iSwitch, strSize);
  GetSize(iUSize, strInvalidUpperSizeRange, $F00000000000000);
End;

{$IFDEF VER180}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

(**

  This method determines the attribute range from the command line.

  @precon  None.
  @postcon Determines the attribute range from the command line.

  @param   iSwitch as an Integer as a reference
  @param   iIndex as an Integer as a reference

**)
Procedure GetAttributes(var iSwitch, iIndex : Integer);

Begin
  Include(CommandLineSwitches, clsAttrRange);
  IncrementSwitchPosition(iIndex, iSwitch, strOpenSquareExpectedInAttribDef);
  If ParamStr(iSwitch)[iIndex] <> '[' Then
    Raise Exception.Create(strOpenSquareExpectedInAttribDef);
  IncrementSwitchPosition(iIndex, iSwitch, strMissingAttribDirective);
  iFileAttrs := 0;
  iTypeAttrs := 0;
  While (iIndex <= Length(ParamStr(iSwitch))) And
    (ParamStr(iSwitch)[iIndex] <> ']') Do
    Begin
      Case ParamStr(iSwitch)[iIndex] Of
        'r', 'R' : iFileAttrs := iFileAttrs Or faReadOnly;
        'a', 'A' : iFileAttrs := iFileAttrs Or faArchive;
        's', 'S' : iFileAttrs := iFileAttrs Or faSysFile;
        'h', 'H' : iFileAttrs := iFileAttrs Or faHidden;
        'f', 'F' : iTypeAttrs := iTypeAttrs Or iFileOnly;
        'd', 'D' : iTypeAttrs := iTypeAttrs Or faDirectory;
        'v', 'V' : iTypeAttrs := iTypeAttrs Or faVolumeID;
      Else
        Raise Exception.CreateFmt(strNotAValidAttrList, [ParamStr(iSwitch)[iIndex]]);
      End;
      IncrementSwitchPosition(iIndex, iSwitch, strCloseSquareExpectedInAttribDef);
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
  @param   iIndex as an Integer as a reference

**)
Procedure GetOrderBy(var iSwitch, iIndex : Integer);

Begin
  Include(CommandLineSwitches, clsOrderBy);
  IncrementSwitchPosition(iIndex, iSwitch, strColonExpectedInOrderBy);
  If ParamStr(iSwitch)[iIndex] <> ':' Then
    Raise Exception.Create(strColonExpectedInOrderBy);
  IncrementSwitchPosition(iIndex, iSwitch, strMissingOrderByDirective);
  If ParamStr(iSwitch)[iIndex] = '-' Then
    Begin
      OrderFilesDirection := odDescending;
      IncrementSwitchPosition(iIndex, iSwitch, strMissingOrderByDirective);
    End;
  Case ParamStr(iSwitch)[iIndex] Of
    'n', 'N': OrderFilesBy := obName;
    'd', 'D': OrderFilesBy := obDate;
    's', 'S': OrderFilesBy := obSize;
    'a', 'A': OrderFilesBy := obAttribute;
    'o', 'O': OrderFilesBy := obOwner;
  Else
    Raise Exception.Create(strInvalidOrderByDirective);
  End
End;

(**

  This method gets the command line information for the date type of the found
  files.

  @precon  None.
  @postcon Gets the command line information for the date type of the found
           files.

  @param   iSwitch as an Integer as a reference
  @param   iIndex as an Integer as a reference

**)
Procedure GetDateType(var iSwitch, iIndex : Integer);

Begin
  Include(CommandLineSwitches, clsDateType);
  IncrementSwitchPosition(iIndex, iSwitch, strColonExpectedInDateType);
  If ParamStr(iSwitch)[iIndex] <> ':' Then
    Raise Exception.Create(strColonExpectedInDateType);
  IncrementSwitchPosition(iIndex, iSwitch, strMissingDateTypeDirective);
  Case ParamStr(iSwitch)[iIndex] Of
    'c', 'C': DateType := dtCreation;
    'a', 'A': DateType := dtLastAccess;
    'w', 'W': DateType := dtLastWrite;
  Else
    Raise Exception.Create(strInvalidDateTypeDirective);
  End
End;

(**

  This method gets the command line parameter for the search in text file for
  a text string option.

  @precon  None.
  @postcon Gets the command line parameter for the search in text file for
           a text string option.

  @param   iSwitch as an Integer as a reference
  @param   iIndex  as an Integer as a reference

**)
Procedure GetSearchInInfo(var iSwitch, iIndex : Integer);

Begin
  Include(CommandLineSwitches, clsSearchIn);
  IncrementSwitchPosition(iIndex, iSwitch, strOpenSquareExpectedInSearchDef);
  If ParamStr(iSwitch)[iIndex] <> '[' Then
    Raise Exception.Create(strOpenSquareExpectedInSearchDef);
  IncrementSwitchPosition(iIndex, iSwitch, strMissingSearchText);
  While (iIndex <= Length(ParamStr(iSwitch))) And
    (ParamStr(iSwitch)[iIndex] <> ']') Do
    Begin
      strSearchInText := strSearchInText + ParamStr(iSwitch)[iIndex];
      IncrementSwitchPosition(iIndex, iSwitch, strCloseSquareExpectedInSearchDef);
    End;
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

  This method extracts from the command line switch the filename to be used
  as the source of the exclusion list.

  @precon  None.
  @postcon Extracts from the command line switch the filename to be used
           as the source of the exclusion list.

  @param   iSwitch as an Integer as a reference
  @param   iIndex  as an Integer as a reference

**)
Procedure GetExclusions(var iSwitch, iIndex : Integer);

Begin
  Include(CommandLineSwitches, clsExclusions);
  IncrementSwitchPosition(iIndex, iSwitch, strOpenSquareExpectedInSearchDef);
  If ParamStr(iSwitch)[iIndex] <> '[' Then
    Raise Exception.Create(strOpenSquareExpectedInSearchDef);
  IncrementSwitchPosition(iIndex, iSwitch, strMissingSearchText);
  While (iIndex <= Length(ParamStr(iSwitch))) And
    (ParamStr(iSwitch)[iIndex] <> ']') Do
    Begin
      strExlFileName := strExlFileName + ParamStr(iSwitch)[iIndex];
      IncrementSwitchPosition(iIndex, iSwitch, strCloseSquareExpectedInSearchDef);
    End;
End;

(**

  This method displays the criteria and command line options used during the
  session.

  @precon  None.
  @postcon Displays the criteria and command line options used during the
           session.

**)
Procedure DisplayCriteria();

  (**

    This method outputs a single switch and its correpsonding message.

    @precon  None.
    @postcon Outputs a single switch and its correpsonding message.

    @param   strSwitch as a String
    @param   strText   as a String

  **)
  Procedure DisplayText(strSwitch, strText : String);

  Begin
    Writeln(Format('  %2s) %s', [strSwitch, strText]));
  End;

  (**

    This method tries to reduce the file size limit to a more manageable
    length for reading using K, M and G.

    @precon  None.
    @postcon Tries to reduce the size to a factor of 1024.

    @param   iSize as an Int64
    @return  a String

  **)
  Function ReduceSize(iSize : Int64): String;

  Const
    strSizes = 'KMGT';

  Var
    iReductions : Integer;
    strKMG: String;

  Begin
    iReductions := 0;
    If iSize > 0 Then
      While (iSize Mod 1024 = 0) And (iReductions < 4) Do
        Begin
          iSize := iSize Div 1024;
          Inc(iReductions);
        End;
    If iSize > 0 Then
      strKMG := strSizes[iReductions]
    Else
      strKMG := '';
    Result := Format('%1.0n%s', [Int64(iSize), strKMG]);
  End;

  (**

    This method output a text relating to the attribute options used in the
    search.

    @precon  None.
    @postcon Output a text relating to the attribute options used in the
             search.

  **)
  Procedure DisplayAttributes;

  Begin
    DisplayText('t', 'Searching for files with the following attibutes:');
    If iFileAttrs And faReadOnly > 0 Then
      OutputLinesToConsole('        Read Only');
    If iFileAttrs And faArchive > 0 Then
      OutputLinesToConsole('        Archive');
    If iFileAttrs And faSysFile > 0 Then
      OutputLinesToConsole('        System File');
    If iFileAttrs And faHidden > 0 Then
      OutputLinesToConsole('        Hidden');
    If iTypeAttrs And iFileOnly > 0 Then
      OutputLinesToConsole('        File');
    If iTypeAttrs And faDirectory > 0 Then
      OutputLinesToConsole('        Directory');
    If iTypeAttrs And faVolumeID > 0 Then
      OutputLinesToConsole('        Volume');
  End;

Begin
  Writeln('Search Criteria and Command Line Options Used in this Session.');
  If clsSubDirectories In CommandLineSwitches Then
    DisplayText('s', 'Recursing sub-directories.');
  If clsDebug In CommandLineSwitches Then
    DisplayText('!', 'DEBUG ReadLn; Pause after finish.');
  If clsShowAttribs In CommandLineSwitches Then
    DisplayText('a', 'Displaying File and Directory Attributes.');
  If clsPause In CommandLineSwitches Then
    DisplayText('p', 'Pause after a full console page of text.');
  If (clsSummaryLevel In CommandLineSwitches) And
    Not (clsSearchIn In CommandLineSwitches) Then
    DisplayText(IntToStr(iSummaryLevel), Format(
      'Summarising output to the %d level of detail from' +
      ' the specified starting point.', [iSummaryLevel]));
  If clsSupressZeros In CommandLineSwitches Then
    DisplayText('0', 'Supressing summary information on directoies with zero files.');
  If clsDateRange In CommandLineSwitches Then
    DisplayText('d', 'Display files with dates between ' +
      FormatDateTime('ddd dd/mmm/yyyy hh:mm:ss', dtLDate) + ' and ' +
      FormatDateTime('ddd dd/mmm/yyyy hh:mm:ss', dtUDate) + '.');
  If clsSizeRange In CommandLineSwitches Then
    DisplayText('d', Format('Display files with sizes between %s and %s bytes.',
      [ReduceSize(iLSize), ReduceSize(iUSize)]));
  If clsAttrRange In CommandLineSwitches Then
    DisplayAttributes;
  If clsQuiet In CommandLineSwitches Then
    DisplayText('q', 'Quiet mode - no progress updates.');
  If clsOwner In CommandLineSwitches Then
    DisplayText('w', 'Display the Owners of the files found.');
  If clsOrderBy In CommandLineSwitches Then
    Begin
      Case OrderFilesBy Of
        obName: DisplayText('o', 'Ordering files by Name.');
        obSize: DisplayText('o', 'Ordering files by Size.');
        obDate: DisplayText('o', 'Ordering files by Date.');
        obOwner: DisplayText('o', 'Ordering files by Owner.');
        obAttribute: DisplayText('o', 'Ordering files by their Attributes.');
      End;
      If OrderFilesDirection = odDescending Then
        DisplayText(' ', 'Ordering files in Descending Order.');
    End;
  If clsDateType In CommandLineSwitches Then
    Case DateType Of
      dtCreation:   DisplayText('e', 'Displaying file Creation Dates.');
      dtLastAccess: DisplayText('e', 'Displaying file Last Access Dates.');
      dtLastWrite:  DisplayText('e', 'Displaying file Last Write Dates.');
    End;
  If clsSearchIn In CommandLineSwitches Then
    DisplayText('i', Format('Search for the text "%s" within the files.',
      [strSearchInText]));
  If clsExclusions In CommandLineSwitches Then
    DisplayText('x', Format('Applying exclusions from the file "%s".',
      [strExlFileName]));
  Writeln;
End;

(**

  This method obtains the owner search information from the command line.

  @precon  None.
  @postcon Obtains the owner search information from the command line.

  @param   iSwitch as an Integer as a reference
  @param   iIndex  as an Integer as a reference

**)
Procedure GetOwnerSwitch(var iSwitch, iIndex : Integer);

Begin
  Include(CommandLineSwitches, clsOwner);
  OwnerSearch := osNone;
  OwnerSearchPos := ospExact;
  If Length(ParamStr(iSwitch)) = iIndex Then
    Exit;
  IncrementSwitchPosition(iIndex, iSwitch, strOpenSquareExpectedInSearchDef);
  If ParamStr(iSwitch)[iIndex] = '[' Then
    Begin
      IncrementSwitchPosition(iIndex, iSwitch, strMissingSearchText);
      While (iIndex <= Length(ParamStr(iSwitch))) And
        (ParamStr(iSwitch)[iIndex] <> ']') Do
        Begin
          strOwnerSearch := strOwnerSearch + ParamStr(iSwitch)[iIndex];
          IncrementSwitchPosition(iIndex, iSwitch, strCloseSquareExpectedInSearchDef);
        End;
      OwnerSearch := osEquals;
      If Length(strOwnerSearch) > 0 Then
        Begin
          If strOwnerSearch[1] = '!' Then
            Begin
              OwnerSearch := osNotEquals;
              Delete(strOwnerSearch, 1, 1);
            End;
          If strOwnerSearch[1] = '*' Then
            Begin
              OwnerSearchPos := ospStart;
              Delete(strOwnerSearch, 1, 1);
            End;
          If strOwnerSearch[Length(strOwnerSearch)] = '*' Then
            Begin
              If OwnerSearchPos = ospStart Then
                OwnerSearchPos := ospMiddle
              Else
                OwnerSearchPos := ospEnd;
              Delete(strOwnerSearch, Length(strOwnerSearch), 1);
            End;
        End;
      If Length(strOwnerSearch) = 0 Then
        Raise Exception.Create(strOwnerSearchIsEmpty);
    End;
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
  iSwitch, iIndex : Integer;

Begin
  iSummaryLevel := 0;
  CommandLineSwitches := [];
  iSwitch := 1;
  OrderFilesBy := obNone;
  OrderFilesDirection := odAscending;
  DateType := dtLastWrite;
  While iSwitch <= ParamCount Do
    Begin
      If ParamStr(iSwitch)[1] In ['-', '/'] Then
        Begin
          iIndex := 2;
          While iIndex <= Length(ParamStr(iSwitch)) Do
            Begin
              Case ParamStr(iSwitch)[iIndex] Of
                '?'     : Include(CommandLineSwitches, clsShowHelp);
                's', 'S': Include(CommandLineSwitches, clsSubDirectories);
                '!'     : Include(CommandLineSwitches, clsDebug);
                'a', 'A': Include(CommandLineSwitches, clsShowAttribs);
                'p', 'P': Include(CommandLineSwitches, clsPause);
                '1'..'9': GetSummaryLevel(iSwitch, iIndex);
                '0'     : Include(CommandLineSwitches, clsSupressZeros);
                'd', 'D': GetDateRange(iSwitch, iIndex);
                'z', 'Z': GetSizeRange(iSwitch, iIndex);
                't', 'T': GetAttributes(iSwitch, iIndex);
                'q', 'Q': Include(CommandLineSwitches, clsQuiet);
                'w', 'W': GetOwnerSwitch(iSwitch, iIndex);
                'o', 'O': GetOrderBy(iSwitch, iIndex);
                'i', 'I': GetSearchInInfo(iSwitch, iIndex);
                'e', 'E': GetDateType(iSwitch, iIndex);
                'c', 'C': Include(CommandLineSwitches, clsDisplayCriteria);
                'x', 'X': GetExclusions(iSwitch, iIndex);
              Else
                Raise Exception.CreateFmt(strInValidCommandLineSwitch,
                  [ParamStr(iSwitch)]);
              End;
              Inc(iIndex);
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
  If clsDisplayCriteria In CommandLineSwitches Then
    DisplayCriteria;
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
  strName, strRefDomain : String;
  iNameSize, iRefDomainSize : DWORD;
  Use : SID_NAME_USE;

Begin
  iNameSize := 0;
  iRefDomainSize := 0;
  LookupAccountSID(Nil, SID, Nil, iNameSize, Nil, iRefDomainSize, Use);
  SetLength(strName, iNameSize);
  SetLength(strRefDomain, iRefDomainSize);
  LookupAccountSID(Nil, SID, PChar(strName), iNameSize, PChar(strRefDomain),
    iRefDomainSize, Use);
  If strName = '' Then
    strName := '(Unknown)';
  Result := PChar(strRefDomain) + '/' + PChar(strName);
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
  Result := '';
  If GetNamedSecurityInfo(PChar(strFileName), SE_FILE_OBJECT,
    OWNER_SECURITY_INFORMATION, @Owner, Nil, Nil, Nil,
    Pointer(SD)) = ERROR_SUCCESS Then
    Begin
      Result := LookupAccountBySID(Owner);
      LocalFree(Cardinal(SD));
    End;
End;

(**

  This method adds the file to the collection and increments the various counters.

  @precon  None.
  @postcon Adds the file to the collection and increments the various counters.

  @param   iSize     as an Int64
  @param   recSearch as a TSearchRec
  @param   iDirFiles as an Integer as a reference
  @param   Result    as an Int64 as a reference
  @param   strPath   as a String
  @param   strOwner  as a String

**)
procedure CheckFiles(iSize: Int64; recSearch: TSearchRec; var iDirFiles: Integer;
  var Result: Int64; strPath, strOwner: string);

Var
  dtDate, dtDateLocal : TFileTime;
  iTime : Integer;
  boolAdded: Boolean;

begin
  if not (clsSummaryLevel in CommandLineSwitches) then
    begin
      Inc(iDirFiles);
      Case DateType Of
        dtCreation: dtDate := recSearch.FindData.ftCreationTime;
        dtLastAccess: dtDate := recSearch.FindData.ftLastAccessTime;
        dtLastWrite: dtDate := recSearch.FindData.ftLastWriteTime;
      End;
      FileTimeToLocalFileTime(dtDate, dtDateLocal);
      iTime := 0;
      FileTimeToDosDateTime(dtDateLocal, LongRec(iTime).Hi, LongRec(iTime).Lo);
      boolAdded := objFiles.Add(FileDateToDateTime(iTime), iSize,
        OutputAttributes(recSearch.Attr), strOwner,
        strPath + recSearch.Name, strSearchInText);
    end Else
      boolAdded := True;
  If boolAdded Then
    Begin
      Inc(iFiles);
      Inc(iFileSize, iSize);
      Inc(Result, iSize);
    End;
end;

(**

  This method checks the currently found file information against the Date Range.

  @precon  None.
  @postcon Returns boolFound as true if the file date is within the date range.

  @param   recSearch as a TSearchRec
  @param   boolFound as a Boolean as a reference

**)
procedure CheckDateRange(recSearch: TSearchRec; var boolFound: Boolean);
begin
  if clsDateRange in CommandLineSwitches then
  begin
    boolFound := boolFound and (FileDateToDateTime(recSearch.Time) >= dtLDate);
    boolFound := boolFound and (FileDateToDateTime(recSearch.Time) <= dtUDate);
  end;
end;

(**

  This method checks the currently found file against the size range.

  @precon  None.
  @postcon Returns boolFound as true if the file is in the range.

  @param   iSize     as an Int64
  @param   boolFound as a Boolean as a reference

**)
procedure CheckSizeRange(iSize: Int64; var boolFound: Boolean);
begin
  if clsSizeRange in CommandLineSwitches then
  begin
    boolFound := boolFound and (iSize >= iLSize);
    boolFound := boolFound and (iSize <= iUSize);
  end;
end;

(**

  This method checks the currently found file against the file attributes.

  @precon  None.
  @postcon Returns boolFound as true is the file has the attributes.

  @param   recSearch as a TSearchRec
  @param   boolFound as a Boolean as a reference

**)
procedure CheckFileAttributes(recSearch: TSearchRec; var boolFound: Boolean);
var
  iAttributes: Integer;
begin
  if clsAttrRange in CommandLineSwitches then
  begin
    if faDirectory and recSearch.Attr > 0 then
      iAttributes := faDirectory
    else if faVolumeID and recSearch.Attr > 0 then
      iAttributes := faVolumeID
    else
      iAttributes := iFileOnly;
    boolFound := boolFound and (iAttributes and iTypeAttrs > 0);
    boolFound := boolFound and ((recSearch.Attr and iFileAttrs > 0) or (iFileAttrs = 0));
  end;
end;

(**

  This method provides a workaround for identifying the size of large files.

  @precon  None.
  @postcon Provides a workaround for identifying the size of large files.

  @param   iSize     as an Int64 as a reference
  @param   recSearch as a TSearchRec

**)
procedure WorkaroundLargeFiles(var iSize: Int64; recSearch: TSearchRec);

begin
  with recSearch.FindData do
  // Workaround for files larger than 2147483647
  begin
    iSize := Int64(nFileSizeHigh) * Int64(MAXDWORD);
    iSize := iSize + nFileSizeLow;
  end;
end;

(**

  This method outputs the files that have met the criteria to the console.

  @precon  None.
  @postcon Outputs the files that have met the criteria to the console.

  @param   strPath        as a string
  @param   boolDirPrinted as a Boolean

**)
procedure OutputFilesToConsole(strPath: string; boolDirPrinted: Boolean);
var
  i: Integer;
  iOwnerWidth: Integer;
  strOutput: string;
  iLine : Integer;
  boolGREP: Boolean;

begin
  For i := 0 to objFiles.Count - 1 Do
    begin
      If Pos('D', objFiles.FileInfo[i].Attr) = 0 Then
        strOutput := Format('  %s  %15.0n  ', [
          FormatDateTime('ddd dd/mmm/yyyy hh:mm:ss', objFiles.FileInfo[i].Date),
          objFiles.FileInfo[i].Size + 0.1])
      Else
        strOutput := Format('  %s  %15s  ', [
          FormatDateTime('ddd dd/mmm/yyyy hh:mm:ss', objFiles.FileInfo[i].Date),
          '<DIR>']);
      if clsShowAttribs in CommandLineSwitches then
        strOutput := strOutput + Format('%s  ', [objFiles.FileInfo[i].Attr]);
      if clsOwner in CommandLineSwitches then
      begin
        iOwnerWidth := objFiles.OwnerWidth;
        strOutput := strOutput + Format('%-*s  ', [iOwnerWidth, objFiles.FileInfo[i].Owner]);
      end;
      if not boolDirPrinted then
      begin
        OutputLinesToConsole(strPath);
        boolDirPrinted := True;
      end;
      boolGREP := (clsSearchIn In CommandLineSwitches) And
        (objFiles.FileInfo[i].GREPLines > 0);
      If Not (clsSearchIn In CommandLineSwitches) Or boolGREP Then
        OutputLinesToConsole(strOutput + ExtractFileName(objFiles.FileInfo[i].FileName));
      If boolGREP Then
        For iLine := 0 To objFiles.FileInfo[i].GREPLines - 1 Do
          OutputLinesToConsole(objFiles.FileInfo[i].GREPLine[iLine]);
    end;
  If objFiles.Count > 0 Then
    OutputLinesToConsole;
end;

(**

  This method is a forward declaration for the SearchDirectory method so that it
  can be used recursively.

  @precon  None.
  @postcon None.

  @param   strPath    as a String
  @param   slPatterns as a TStringList
  @param   iLevel     as an Integer as a reference
  @return  an Int64     

**)
Function SearchDirectory(strPath: String; slPatterns : TStringList;
  Var iLevel : Integer) : Int64; Forward;

(**

  This method recurses the directories searching for additional files.

  @precon  None.
  @postcon Recurses the directories searching for additional files.

  @param   strPath    as a string
  @param   iLevel     as an Integer as a reference
  @param   Result     as an Int64 as a reference
  @param   iResult    as an Integer
  @param   slPatterns as a TStringList
  @param   recSearch  as a TSearchRec

**)
procedure RecurseDirectories(strPath: string; var iLevel: Integer; var Result: Int64; iResult: Integer; slPatterns: TStringList; recSearch: TSearchRec);
begin
  begin
    iResult := FindFirst(strPath + '*.*', faDirectory, recSearch);
    while (iResult = 0) do
    begin
      if recSearch.Attr and faDirectory > 0 then
        if (recSearch.Name <> '.') and (recSearch.Name <> '..') then
          Inc(Result, SearchDirectory(strPath + recSearch.Name + '\', slPatterns, iLevel));
      iResult := FindNext(recSearch);
    end;
    SysUtils.FindClose(recSearch);
  end;
end;

(**

  This method check to see if the path and file name should be excluded from the
  search.

  @precon  recSearch must be a valid TSearchRec structure.
  @postcon Sets or maintains boolFound as True if the file should NOT be excluded
           else sets boolFound to False.

  @param   strPath   as a String
  @param   recSearch as a TSearchRec
  @param   boolFound as a Boolean as a reference

**)
Procedure CheckExclusions(strPath : String; recSearch : TSearchRec;
  var boolFound : Boolean);

Var
  strFileName : String;
  j: Integer;

Begin
  strFileName := LowerCase(strPath + recSearch.Name);
  For j := 0 To slExclusions.Count - 1 Do
    boolFound := boolFound And (Pos(slExclusions[j], strFileName) = 0);
End;

(**

  This method check the owner of the file against the owner search criteria.

  @precon  None.
  @postcon Check the owner of the file against the owner search criteria.

  @param   strOwner  as a String
  @param   boolFound as a Boolean as a reference

**)
Procedure CheckOwner(strOwner : String; var boolFound : Boolean);

Var
  i : Integer;
  bool : Boolean;

Begin
  i := Length(strOwnerSearch);
  Case OwnerSearchPos Of
    ospExact : bool := AnsiCompareText(strOwner, strOwnerSearch) = 0;
    ospStart : bool := LowerCase(strOwnerSearch) = Copy(Lowercase(strOwnerSearch), 1, i);
    ospMiddle: bool := Pos(LowerCase(strOwnerSearch), LowerCase(strOwner)) > 0;
    ospEnd   : bool := LowerCase(strOwnerSearch) = Copy(Lowercase(strOwnerSearch),
      Length(strOwner) - i, i);
  Else
    bool := True;
  End;
  If OwnerSearch = osNotEquals  Then
    bool := Not bool;
  boolFound := boolFound And bool
End;

(**

  This method search the current directory for files which match all the
  specified search patterns.

  @precon  None.
  @postcon Search the current directory for files which match all the
           specified search patterns.

  @param   slPatterns as a TStringList
  @param   iResult    as an Integer as a reference
  @param   boolFound  as a Boolean
  @param   iDirFiles  as an Integer
  @param   Result     as an Int64 as a reference
  @param   strPath    as a string
  @param   recSearch  as a TSearchRec as a reference

**)
procedure SearchForPatterns(slPatterns: TStringList; var iResult: Integer; boolFound: Boolean; iDirFiles: Integer; var Result: Int64; strPath: string; var recSearch: TSearchRec);
var
  iPattern: Integer;
  iSize: Int64;
  strOwner : String;

begin
  for iPattern := 0 to slPatterns.Count - 1 do
  begin
    iResult := FindFirst(strPath + slPatterns[iPattern], faAnyFile, recSearch);
    try
      while iResult = 0 do
      begin
        WorkaroundLargeFiles(iSize, recSearch);
        CheckFileAttributes(recSearch, boolFound);
        CheckSizeRange(iSize, boolFound);
        CheckDateRange(recSearch, boolFound);
        CheckExclusions(strPath, recSearch, boolFound);
        If clsOwner In CommandLineSwitches Then
          Begin
            strOwner := OutputOwner(strPath + recSearch.Name);
            CheckOwner(strOwner, boolFound);
          End;
        if boolFound then
          CheckFiles(iSize, recSearch, iDirFiles, Result, strPath, strOwner);
        iResult := FindNext(recSearch);
        boolFound := True;
      end;
    finally
      SysUtils.FindClose(recSearch);
    end;
  end;
end;

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
  boolFound : Boolean;
  iDirFiles : Integer;

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
    SearchForPatterns(slPatterns, iResult, boolFound, iDirFiles, Result, strPath,
      recSearch);
    If OrderFilesBy <> obNone Then
      objFiles.OrderBy(OrderFilesBy, OrderFilesDirection);
    OutputFilesToConsole(strPath, boolDirPrinted);
  Finally
    objFiles.Free;
  End;
  If Not (clsSummaryLevel In CommandLineSwitches) Then
    If iDirFiles > 0 Then
      OutputLinesToConsole;
  (* Search sub directories *)
  If clsSubDirectories In CommandLineSwitches Then
    RecurseDirectories(strPath, iLevel, Result, iResult, slPatterns, recSearch);
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
      slExclusions := TstringList.Create;
      Try
        If Not (clsExclusions In CommandLineSwitches) Or
          ((clsExclusions In CommandLineSwitches) And FileExists(strExlFileName)) Then
          Begin
            If clsExclusions In CommandLineSwitches Then
              slExclusions.LoadFromFile(strExlFileName);
            slExclusions.Text := LowerCase(slExclusions.Text);
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
          End Else
            Raise Exception.CreateFmt(strExclusionsNotFound, [strExlFileName]);
      Finally
        slExclusions.Free;
      End;
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
