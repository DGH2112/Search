(**

  This module defines the Search Win32 console application. This application
  is designed to provide regular expression search capabilities for files on
  disk.

  @Version 1.0
  @Author  David Hoyle
  @Date    06 Jan 2008

**)
Program Search;

{$APPTYPE CONSOLE}

{$R *.RES}

uses
  ExceptionLog,
  SysUtils,
  Windows,
  Classes,
  ACCCTRL,
  Contnrs,
  Graphics,
  ActiveX,
  DGHLibrary in '..\..\library\DGHLibrary.pas',
  FileHandling in 'Source\FileHandling.pas',
  MSXML2_TLB in '..\..\LIBRARY\MSXML2_TLB.pas',
  checkforupdates in '..\..\LIBRARY\checkforupdates.pas';

Type
  (** A custom Exception for any exceptions raised by incorrect information
      in the search criteria. **)
  ESearchException = Class(Exception);

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
  strCloseSquareExpectedInGREPSearchDef = '"]" Expected in GREP Search Definition';
  (** An exception message for a missing [ in an Search definition. **)
  strOpenSquareExpectedInGREPSearchDef = '"[" Expected in GREP Search Definition.';
  (** An exception message for a missing ] in an Search definition. **)
  strCloseSquareExpectedInExclSearchDef = '"]" Expected in Exclusion Search Definition';
  (** An exception message for a missing [ in an Search definition. **)
  strOpenSquareExpectedInExclSearchDef = '"[" Expected in Exclusion Search Definition.';
  (** An exception message for a missing ] in an Search definition. **)
  strCloseSquareExpectedInOwnerSearchDef = '"]" Expected in Owner Search Definition';
  (** An exception message for a missing [ in an Search definition. **)
  strOpenSquareExpectedInOwnerSearchDef = '"[" Expected in Owner Search Definition.';
  (** An exception message for the exclusions file not found. **)
  strExclusionsNotFound = 'The filename "%s" was not found to process exclusions ' +
    'in the searches.';
  (** An exception message for the Owner Search Criteria being empty. **)
  strOwnerSearchIsEmpty = 'You must specify a valid Owner Search Criteria.';
  (** A resource string to define the Exception output format. **)
  strException = 'Exception: %s';
  (** A resource string to define the URL to check for updates. **)
  strWebSiteURL = 'http://www.hoyld.freeserve.co.uk/HoylD.xml|' +
    'C:\Documents and Settings\HoylD\My Documents\Web Page\HoylD.xml';
  (** A resource string to define the Application ID for updates. **)
  strAppID = 'Search';
  (** This is a constant to represent a parameter string error. **)
  strErrorInPathSearchParamString = 'There was an error in the path=search ' +
    'param pairing. (%s)';
  (** A resource string to define the for of the date outputs. **)
  strDateFormat = 'ddd dd/mmm/yyyy hh:mm:ss';
  (** An exception message to prompt for a criteria **)
  strNeedToSpecifyCriteria = 'You need to specify at least one search criteria.';
  (** An exception message to notify of an invalid command line parameter. **)
  strInvalidCommandLineSwitch = 'Invalid command line switch "%s" in parameter "%s."';
  (** A help message to define the use of the u switch. **)
  strForceACheckForUpdate = 'Force a check for updates to the software from ' +
  'the Internet.';
  (** A help message to define the use of the x switch. **)
  strApplyingExclusions = 'Applying exclusions from the file "%s".';
  (** A resource string to display the text being searched for. **)
  strSearchForText = 'Search for the text "%s" within the files.';
  (** A resource string to display that Last Write Dates are being shown. **)
  strDisplayingFileLastWriteDates = 'Displaying file Last Write Dates.';
  (** A resource string to display that Last Access Dates are being shown. **)
  strDisplayingFileLastAccessDates = 'Displaying file Last Access Dates.';
  (** S resource string to define that file creation dates are being shown. **)
  strDisplayingFileCreationDates = 'Displaying file Creation Dates.';
  (** A resource string to define that files are being ordered in decsending order **)
  strOrderingFilesInDescOrder = 'Descending Order';
  (** A resource string to define that the file are being ordered by their attributes. **)
  strOrderingFilesByTheirAttrs = 'Ordering files by their Attributes';
  (** A resource string to define ordering files by Owner. **)
  strOrderingFilesByOwner = 'Ordering files by Owner';
  (** A resource string to define ordering files by Date. **)
  strOrderingFilesByDate = 'Ordering files by Date';
  (** A resource string to define ordering files by Size. **)
  strOrderingFilesBySize = 'Ordering files by Size';
  (** A resource string to define ordering files by Name. **)
  strOrderingFilesByName = 'Ordering files by Name';
  (** A resource string to define that file owners are being displayed. **)
  strDisplayTheOwners = 'Display the Owners of the files found';
  (** A resource string to define that no progress should be displayed. **)
  strQuietMode = 'Quiet mode - no progress updates.';
  (** A resource string to define size filter by size. **)
  strDisplayFilesWithSIzes = 'Display files with sizes between %s and %s byt' +
  'es.';
  (** A resource string to define size filter by date. **)
  strDisplayFilesWithDates = 'Display files with dates between %s and %s.';
  (** A resource string to define the suppression of summary information = 0 **)
  strSupressingSummary = 'Supressing summary information on directories with' +
  ' zero files.';
  (** A resource strig to define summary output to a specific depth of information. **)
  strSummarisingOutput = 'Summarising output to the %d level of detail from ' +
  'the specified starting point.';
  (** A resource string to define the displaying of attributes. **)
  strDisplayingFileAttibs = 'Displaying File and Directory Attributes.';
  (** A resource string to fine the debug pause. **)
  strDEBUGReadLnPause = 'Pause after finish.';
  (** A resource string for recursing sub directories. **)
  strRecursingSubdirectories = 'Recursing sub-directories.';
  (** A resource string to notify of output of command line options. **)
  strSearchCriteria = 'Search Criteria and Command Line Options Used in this' +
  ' Session.';
  (** A resource string to denote search for file attributes. **)
  strSearchingForFiles = 'Searching for files with the following attibutes:';
  (** A resource string to denote searching for readonly files. **)
  strReadOnly = '        Read Only';
  (** A resource string to denote searching for archive files. **)
  strArchive = '        Archive';
  (** A resource string to denote searching for System files. **)
  strSystemFile = '        System File';
  (** A resource string to denote searching for Hidden files. **)
  strHidden = '        Hidden';
  (** A resource string to denote searching for files. **)
  strFile = '        File';
  (** A resource string to denote searching for directories. **)
  strDirectory = '        Directory';
  (** A resource string to denote searching for volume labels. **)
  strVolume = '        Volume';
  (** A resource string for formatting information. **)
  strSummaryLevelException = 'The summary level "%s" is invalid.';
  (** A resource string for formatting information. **)
  strSummaryAlreadySet = 'Then summary level is already set.';
  (** A resource string for formatting information. **)
  strMsg1 = '  Found %1.0n bytes in %1.0n Files in %1.0n Directories.';
  (** A resource string for formatting information. **)
  strMsg2 = '  Total space %1.0n bytes, and %1.0n bytes Free.';
  (** A resource string for formatting information. **)
  strTitle = 'Search %d.%d%s (Build %s) File Find and Summary Tool.';
  (** A resource string for formatting information. **)
  strPressEnterToFinish = 'Press <Enter> to finish.';
  (** A resource string for formatting information. **)
  strSummaryFormat = '%15.0n%s%s';
  (** A resource string for formatting information. **)
  strOwnerFormat = '%-*s  ';
  (** A resource string for formatting information. **)
  strAttrFormat = '%s  ';
  (** A resource string for formatting information. **)
  strDirFormat = '  %s  %15s  ';
  (** A resource string for formatting information. **)
  strFileFormat = '  %s  %15.0n  ';
  strOwnerAtTheEnd = ' at the end';
  strOwnerInTheMiddle = ' in the middle';
  strOwnerAtTheStart = ' at the start';
  strOwnerExact = ' exactly';
  (** A resource string for formatting information. **)
  strOwnerMatching = 'matching "%s"';
  strOwnerNOT = 'NOT ';
  strOwnerSearchingFor = ' Searching for Owners ';
  (** A resource string for formatting information. **)
  strReduceSizeFormat = '%1.0n%s';
  (** A resource string for formatting information. **)
  strDisplayTextSwitchFormat = '  %2s';
  (** A resource string for formatting information. **)
  strDisplatTextInfoFormat = ') %s';
  (** A resource string for formatting information. **)
  strSearchingForHeader = 'Searching "%s" for "%s".';
  (** A resource string for formatting information. **)
  strOutputCurrPathFormat = '%s%-*s';
  (** A resource string for formatting information. **)
  strSearchLabel = 'Searching: (D:%d,F:%d) ';
  (** A resource string for the console help information. **)
  strHelp0010 = 'Syntax:';
  (** A resource string for the console help information. **)
  strHelp0020 = 'Search searchparam1 {searchparam2}... {/A} {/S} {/Q} {/W}';
  (** A resource string for the console help information. **)
  strHelp0030 = '{/T[RASHFDV]} {/D:[{DD{/MM{/YY {HH:MM{:SS}}}}}-{DD{/MM{' +
  '/YY {HH:MM{:SS}}}}}]';
  (** A resource string for the console help information. **)
  strHelp0040 = '{/Z[{LowerByteSize}-{UpperByteSize}]} {/O:NDAOS} {/E:CA' +
  'W} {/I[text]}';
  (** A resource string for the console help information. **)
  strHelp0050 = '{/X[filename]}';
  (** A resource string for the console help information. **)
  strHelp0060 = 'searchparam# = {drive:\path\}filter1{;filter2{;filter3{;.' +
  '..}}}';
  (** A resource string for the console help information. **)
  strHelp0070 = 'filter#';
  (** A resource string for the console help information. **)
  strHelp0075 = 'can contain wildcards * and ? within the filename. ';
  (** A resource string for the console help information. **)
  strHelp0080 = '{ ... } denotes optional items.';
  (** A resource string for the console help information. **)
  strHelp0090 = '/?';
  (** A resource string for the console help information. **)
  strHelp0095 = 'This help screen';
  (** A resource string for the console help information. **)
  strHelp0100 = '/A';
  (** A resource string for the console help information. **)
  strHelp0105 = 'Show file and directory attributes';
  (** A resource string for the console help information. **)
  strHelp0110 = '/S';
  (** A resource string for the console help information. **)
  strHelp0115 = 'Recurse subdirectories';
  (** A resource string for the console help information. **)
  strHelp0120 = '/1..9';
  (** A resource string for the console help information. **)
  strHelp0125 = 'Summarise subdirectories';
  (** A resource string for the console help information. **)
  strHelp0130 = '/0';
  (** A resource string for the console help information. **)
  strHelp0135 = 'Hide empty Summarised subdirectories';
  (** A resource string for the console help information. **)
  strHelp0140 = '/T[RASHFFDV]';
  (** A resource string for the console help information. **)
  strHelp0145 = 'Show only files with certain attributes';
  (** A resource string for the console help information. **)
  strHelp0150 = 'R = Read Only, A = Archive, S = System, H = Hidden,';
  (** A resource string for the console help information. **)
  strHelp0160 = 'F = File, D = Directory, and V = Volume ID';
  (** A resource string for the console help information. **)
  strHelp0170 = '/D[{l}-{u}]';
  (** A resource string for the console help information. **)
  strHelp0175 = 'Searches for files between dates (l) and (u).';
  (** A resource string for the console help information. **)
  strHelp0177 = 'l = lower bounding date and time, u = upper bounding date and time';
  (** A resource string for the console help information. **)
  strHelp0180 = '/Z[{l}-{u}]';
  (** A resource string for the console help information. **)
  strHelp0185 = 'Searches for files between sizes (l) and (u).';
  (** A resource string for the console help information. **)
  strHelp0187 = 'l = lower bounding size in bytes, u = upper bounding size in bytes';
  (** A resource string for the console help information. **)
  strHelp0190 = '/Q';
  (** A resource string for the console help information. **)
  strHelp0195 = 'Quiet mode';
  (** A resource string for the console help information. **)
  strHelp0200 = '/W {[Owner]}';
  (** A resource string for the console help information. **)
  strHelp0205 = 'Owner Information. The owner search can be a wildcard search with an *.';
  (** A resource string for the console help information. **)
  strHelp0207 = 'Searches beginning with ! force a negated criteria.';
  (** A resource string for the console help information. **)
  strHelp0210 = '/O:NADOS';
  (** A resource string for the console help information. **)
  strHelp0215 = 'Order files ascending [+] and descending [-]';
  (** A resource string for the console help information. **)
  strHelp0220 = 'N = Name, A = Attribute, D = Date and Time, O = Owner, S = Size';
  (** A resource string for the console help information. **)
  strHelp0230 = '/E:CAW';
  (** A resource string for the console help information. **)
  strHelp0235 = 'Type of file date to display and search on:';
  (** A resource string for the console help information. **)
  strHelp0240 = 'C = Creation, A = Last Access, W = Last Write (Default)';
  (** A resource string for the console help information. **)
  strHelp0250 = '/I[text]';
  (** A resource string for the console help information. **)
  strHelp0255 = 'Search within files for text.';
  (** A resource string for the console help information. **)
  strHelp0260 = '/C';
  (** A resource string for the console help information. **)
  strHelp0265 = 'Display a list of Criteria and Command Line Options.';
  (** A resource string for the console help information. **)
  strHelp0270 = '/X[FileName]';
  (** A resource string for the console help information. **)
  strHelp0275 = 'A list of exclusions to apply to paths and filenames.';
  (** A resource string for the console help information. **)
  strHelp0280 = 'NOTE: The date time input format is dependent on your loc' +
  'al settings';

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
    Raise ESearchException.Create(strExceptionMsg);
end;

Type
  (** This is a list of boolean on / off command line switches. **)
  TCommandLineSwitch = (
    clsShowHelp,         { /? or -? or /h or -h }
    clsSubDirectories,   { /s or -s }
    clsDebug,            { /!       }
    clsShowAttribs,      { /a or -a }
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
    clsExclusions,       { /x or -x }
    clsUpdate            { /u or -u }
  );

  (** This is an enumerate to defines whether the owner should be searched. **)
  TOwnerSearch = (osEquals, osNotEquals);

  (** This is an enumerate to defines where the owner should be searched. **)
  TOwnerSearchPos = (ospNone, ospExact, ospStart, ospMiddle, ospEnd);

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
  (** An enumerate to determine if the search is positive or negated **)
  OwnerSearch : TOwnerSearch;
  (** An enumerate to determine the position of the search **)
  OwnerSearchPos : TOwnerSearchPos;
  (** A handle to the standard console output. **)
  FStd : THandle;
  (** A handle to the error console output. **)
  FErr : THandle;
  (** A string to store the loading and saving settings filename in. **)
  strRootKey : String;

Const
  (** A constant to define that only files should be listed. **)
  iFileOnly = $0100;

(**

  This method builds the root key INI filename for the loading and saving of
  settings.

  @precon  None.
  @postcon Builds the root key INI filename for the loading and saving of
           settings.

**)
procedure BuildRootKey;

var
  i: Cardinal;
  strUserName: string;
  strComputerName: string;

begin
  i := 1024;
  SetLength(strUserName, i);
  GetUserName(@strUserName[1], i);
  Win32Check(LongBool(i));
  SetLength(strUserName, i - 1);
  i := 1024;
  SetLength(strComputerName, i);
  GetComputerName(@strComputerName[1], i);
  Win32Check(LongBool(i));
  SetLength(strComputerName, i);
  strRootKey := ExtractFilePath(ParamStr(0)) + Format('Search Settings for %s on %s.INI', [strUserName, strComputerName]);
end;

(**

  This routine get and stores the current number of lines and columns
  in the console window.

  @precon  None.
  @postcon Gets the width and height of the console.

**)
Procedure GetConsoleInformation;

Var
  ConsoleInfo : TConsoleScreenBufferInfo;

Begin
  FStd := GetStdHandle(STD_OUTPUT_HANDLE);
  FErr := GetStdHandle(STD_ERROR_HANDLE);
  GetConsoleScreenBufferInfo(FStd, ConsoleInfo);
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

Var
  C : CONSOLE_SCREEN_BUFFER_INFO;
  i, j : Integer;
  iLength : Integer;

Begin
  If clsQuiet In CommandLineSwitches Then Exit;
  GetConsoleScreenBufferInfo(FStd, C);
  iLength := Length(Format(strSearchLabel, [iDirectories, iFiles]));
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
  strPath := Format(strOutputCurrPathFormat, [Format(strSearchLabel,
    [iDirectories, iFiles]), iWidth - iLength, strPath]);
  OutputToConsoleLn(FStd, strPath, clMaroon, clNone, False);
  SetConsoleCursorPosition(FStd, C.dwCursorPosition);
End;


(**

  Prints a Title for the application on the screen.

  @precon  None.
  @postcon Prints the title for the application on the screen.

**)
Procedure PrintTitle;

Begin
  OutputToConsoleLn(FStd, GetConsoleTitle(strTitle));
  OutputToConsoleLn(FStd);
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
  OutputToConsoleLn(FStd, Format(strSearchingForHeader, [strPath, strPattern]),
    clYellow);
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

Var
  iFreeBytesAvailableToCaller,
  iTotalNumberOfBytes,
  iTotalNumberOfFreeBytes : Int64;

Begin
  GetDiskFreeSpaceEx(PChar(strPath), iFreeBytesAvailableToCaller,
    iTotalNumberOfBytes, @iTotalNumberOfFreeBytes);
  OutputtoConsole(FStd, StringOfChar(#32, iWidth - 1), clNone, clNone, False);
  OutputToConsoleLn(FStd, Format(strMsg1, [iFileSize + 0.1, iFiles + 0.1,
    iDirectories + 0.1]), clWhite);
  OutputToConsoleLn(FStd, Format(strMsg2, [iTotalNumberOfBytes + 0.1,
    iTotalNumberOfFreeBytes + 0.1]), clWhite);
End;

(**

  This method prints on the console screen the command line search help
  information.

  @precon  None.
  @postcon Prints on the console screen the command line search help
           information.

**)
Procedure PrintHelp;

  (**

    This function formats the output information with indents and padding to a
    specific width.

    @precon  None.
    @postcon Formats the output information with indents and padding to a
             specific width.

    @param   strText as a String
    @param   iIndent as an Integer
    @param   iWidth  as an Integer
    @return  a String

  **)
  Function Indent(strText : String; iIndent, iWidth : Integer) : String;

  Begin
    Result := StringOfChar(#32, iIndent) + Format('%-*s', [iWidth, strText]);
  End;

Const
  iWidth = 14;
  iTextIndent = 16;

Begin
  OutputToConsoleLn(FStd, strHelp0010, clWhite);
  OutputToConsoleLn(FStd, Indent(strHelp0020, 2, 0), clLime);
  OutputToConsoleLn(FStd, Indent(strHelp0030, 4, 0), clLime);
  OutputToConsoleLn(FStd, Indent(strHelp0040, 4, 0), clLime);
  OutputToConsoleLn(FStd, Indent(strHelp0050, 4, 0), clLime);
  OutputToConsoleLn(FStd);
  OutputToConsoleLn(FStd, Indent(strHelp0060, 2, 0), clLime);
  OutputToConsoleLn(FStd);
  OutputToConsole(FStd, Indent(strHelp0070, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0075);
  OutputToConsoleLn(FStd);
  OutputToConsoleLn(FStd, Indent(strHelp0080, iTextIndent, 0));
  OutputToConsoleLn(FStd);
  OutputToConsole(FStd, Indent(strHelp0090, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0095);
  OutputToConsole(FStd, Indent(strHelp0100, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0105);
  OutputToConsole(FStd, Indent(strHelp0110, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0115);
  OutputToConsole(FStd, Indent(strHelp0120, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0125);
  OutputToConsole(FStd, Indent(strHelp0130, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0135);
  OutputToConsole(FStd, Indent(strHelp0140, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0145);
  OutputToConsoleLn(FStd, Indent(strHelp0150, iTextIndent + 2, 0), clLime);
  OutputToConsoleLn(FStd, Indent(strHelp0160, iTextIndent + 2, 0), clLime);
  OutputToConsole(FStd, Indent(strHelp0170, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0175);
  OutputToConsoleLn(FStd, Indent(strHelp0177, iTextIndent + 2, 0), clLime);
  OutputToConsole(FStd, Indent(strHelp0180, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0185);
  OutputToConsoleLn(FStd, Indent(strHelp0187, iTextIndent + 2, 0), clLime);
  OutputToConsole(FStd, Indent(strHelp0190, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0195);
  OutputToConsole(FStd, Indent(strHelp0200, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0205);
  OutputToConsoleLn(FStd, Indent(strHelp0207, iTextIndent, 0));
  OutputToConsole(FStd, Indent(strHelp0210, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0215);
  OutputToConsoleLn(FStd, Indent(strHelp0220, iTextIndent + 2, 0), clLime);
  OutputToConsole(FStd, Indent(strHelp0230, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0235);
  OutputToConsoleLn(FStd, Indent(strHelp0240, iTextIndent + 2, 0), clLime);
  OutputToConsole(FStd, Indent(strHelp0250, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0255);
  OutputToConsole(FStd, Indent(strHelp0260, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0265);
  OutputToConsole(FStd, Indent(strHelp0270, 2, iWidth), clRed);
  OutputToConsoleLn(FStd, strHelp0275);
  OutputToConsoleLn(FStd);
  OutputToConsoleLn(FStd, strHelp0280, clYellow);
  OutputToConsoleLn(FStd);
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
  wH, wM, wS, wMS : Word;

Begin
  Include(CommandLineSwitches, clsDateRange);
  IncrementSwitchPosition(iIndex, iSwitch, strOpenSquareExpectedInDate);
  If ParamStr(iSwitch)[iIndex] <> '[' Then
    Raise ESearchException.Create(strOpenSquareExpectedInDate);
  GetRangeString('-', strMissingDateRangeSeparater, iIndex, iSwitch, strDate);
  If strDate <> '' Then
    Begin
      dtLDate := ConvertDate(strDate);
      DecodeTime(dtLDate, wH, wM, wS, wMS);
      If (wH = 0) And (wM = 0) And (wS = 0) And (wMS = 0) Then
        dtLDate := dtLDate + EncodeTime(0, 0, 0, 0);
    End Else
      dtUDate := ConvertDate('01/Jan/1900 00:00:01');
  GetRangeString(']', strCloseSquareExpectedInDate, iIndex, iSwitch, strDate);
  If strDate <> '' Then
    Begin
      dtUDate := ConvertDate(strDate);
      DecodeTime(dtUDate, wH, wM, wS, wMS);
      If (wH = 0) And (wM = 0) And (wS = 0) And (wMS = 0) Then
        dtUDate := dtUDate + EncodeTime(23, 59, 59, 999);
    End Else
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
          Raise ESearchException.Create(strExceptionMsg);
        iSize := iSize * iFactor;
      End Else
        iSize := iDefault;
  End;

Begin
  Include(CommandLineSwitches, clsSizeRange);
  IncrementSwitchPosition(iIndex, iSwitch, strOpenSquareExpectedInSize);
  If ParamStr(iSwitch)[iIndex] <> '[' Then
    Raise ESearchException.Create(strOpenSquareExpectedInSize);
  GetRangeString('-', strMissingSizeRangeSeparater, iIndex, iSwitch, strSize);
  GetSize(iLSize, strInvalidLowerSizeRange, 0);
  GetRangeString(']', strCloseSquareExpectedInSize, iIndex, iSwitch, strSize);
  GetSize(iUSize, strInvalidUpperSizeRange, $F00000000000000);
End;

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
    Raise ESearchException.Create(strOpenSquareExpectedInAttribDef);
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
        Raise ESearchException.CreateFmt(strNotAValidAttrList, [ParamStr(iSwitch)[iIndex]]);
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
    Raise ESearchException.Create(strColonExpectedInOrderBy);
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
    Raise ESearchException.Create(strInvalidOrderByDirective);
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
    Raise ESearchException.Create(strColonExpectedInDateType);
  IncrementSwitchPosition(iIndex, iSwitch, strMissingDateTypeDirective);
  Case ParamStr(iSwitch)[iIndex] Of
    'c', 'C': DateType := dtCreation;
    'a', 'A': DateType := dtLastAccess;
    'w', 'W': DateType := dtLastWrite;
  Else
    Raise ESearchException.Create(strInvalidDateTypeDirective);
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
  IncrementSwitchPosition(iIndex, iSwitch, strOpenSquareExpectedInGREPSearchDef);
  If ParamStr(iSwitch)[iIndex] <> '[' Then
    Raise ESearchException.Create(strOpenSquareExpectedInGREPSearchDef);
  IncrementSwitchPosition(iIndex, iSwitch, strMissingSearchText);
  While (iIndex <= Length(ParamStr(iSwitch))) And
    (ParamStr(iSwitch)[iIndex] <> ']') Do
    Begin
      strSearchInText := strSearchInText + ParamStr(iSwitch)[iIndex];
      IncrementSwitchPosition(iIndex, iSwitch, strCloseSquareExpectedInGREPSearchDef);
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

Var
  iCode : Integer;

Begin
  Include(CommandLineSwitches, clsSummaryLevel);
  If iSummaryLevel > 0 Then
    Raise ESearchException.Create(strSummaryAlreadySet);
  Val(ParamStr(iSwitch)[iSwitches], iSummaryLevel, iCode);
  If iCode > 0 Then
    Raise ESearchException.CreateFmt(strSummaryLevelException, [ParamStr(iSwitch)]);
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
  IncrementSwitchPosition(iIndex, iSwitch, strOpenSquareExpectedInExclSearchDef);
  If ParamStr(iSwitch)[iIndex] <> '[' Then
    Raise ESearchException.Create(strOpenSquareExpectedInExclSearchDef);
  IncrementSwitchPosition(iIndex, iSwitch, strMissingSearchText);
  While (iIndex <= Length(ParamStr(iSwitch))) And
    (ParamStr(iSwitch)[iIndex] <> ']') Do
    Begin
      strExlFileName := strExlFileName + ParamStr(iSwitch)[iIndex];
      IncrementSwitchPosition(iIndex, iSwitch, strCloseSquareExpectedInExclSearchDef);
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

    @param   ASwitch            as a TCommandLineSwitch
    @param   strSwitch          as a String
    @param   strText            as a String
    @param   boolCarriageReturn as a Boolean
    @return  a Boolean

  **)
  Function DisplayText(ASwitch : TCommandLineSwitch; strSwitch,
    strText : String; boolCarriageReturn : Boolean = True) : Boolean;

  Begin
    Result := ASwitch In CommandLineSwitches;
    If Result Then
      Begin
        OutputToConsole(FStd, Format(strDisplayTextSwitchFormat, [strSwitch]), clRed);
        OutputToConsole(FStd, Format(strDisplatTextInfoFormat, [strText]));
        If boolCarriageReturn Then
          OutputToConsoleLn(FStd);
      End;
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
    Result := Format(strReduceSizeFormat, [Int64(iSize) + 0.1, strKMG]);
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
    If DisplayText(clsAttrRange, 't', strSearchingForFiles) Then
      Begin
        If iFileAttrs And faReadOnly > 0 Then
          OutputToConsoleLn(FStd, strReadOnly, clLime);
        If iFileAttrs And faArchive > 0 Then
          OutputToConsoleLn(Fstd, strArchive, clLime);
        If iFileAttrs And faSysFile > 0 Then
          OutputToConsoleLn(FStd, strSystemFile, clLime);
        If iFileAttrs And faHidden > 0 Then
          OutputToConsoleLn(FStd, strHidden, clLime);
        If iTypeAttrs And iFileOnly > 0 Then
          OutputToConsoleLn(FStd, strFile, clLime);
        If iTypeAttrs And faDirectory > 0 Then
          OutputToConsoleLn(FStd, strDirectory, clLime);
        If iTypeAttrs And faVolumeID > 0 Then
          OutputToConsoleLn(FStd, strVolume, clLime);
      End;
  End;

Begin
  OutputToConsoleLn(FStd, strSearchCriteria, clWhite);
  DisplayText(clsDisplayCriteria, 'c', 'Displaying criteria used for this search.');
  DisplayText(clsSubDirectories, 's', strRecursingSubdirectories);
  DisplayText(clsDebug, '!', strDEBUGReadLnPause);
  DisplayText(clsShowAttribs, 'a', strDisplayingFileAttibs);
  If Not (clsSearchIn In CommandLineSwitches) Then
    DisplayText(clsSummaryLevel, IntToStr(iSummaryLevel),
      Format(strSummarisingOutput, [iSummaryLevel]));
  DisplayText(clsSupressZeros, '0', strSupressingSummary);
  DisplayText(clsDateRange, 'd', Format(strDisplayFilesWithDates,
    [FormatDateTime(strDateFormat, dtLDate),
    FormatDateTime(strDateFormat, dtUDate)]));
  DisplayText(clsSizeRange, 'd', Format(strDisplayFilesWithSIzes,
    [ReduceSize(iLSize), ReduceSize(iUSize)]));
  DisplayAttributes;
  DisplayText(clsQuiet, 'q', strQuietMode);
  If DisplayText(clsOwner, 'w', strDisplayTheOwners, False) Then
    Begin
      If OwnerSearchPos In [ospExact..ospEnd] Then
        Begin
          OutputToConsole(FStd, strOwnerSearchingFor);
          If OwnerSearch In [osNotEquals] Then
            OutputToConsole(FStd, strOwnerNOT, clRed);
          OutputToConsole(FStd, Format(strOwnerMatching, [strOwnerSearch]));
          Case OwnerSearchPos Of
            ospExact :OutputToConsole(FStd, strOwnerExact);
            ospStart :OutputToConsole(FStd, strOwnerAtTheStart);
            ospMiddle:OutputToConsole(FStd, strOwnerInTheMiddle);
            ospEnd   :OutputToConsole(FStd, strOwnerAtTheEnd);
          End;
        End;
      OutputToConsoleLn(FStd, '.');
    End;
  If clsOrderBy In CommandLineSwitches Then
    Begin
      Case OrderFilesBy Of
        obName: DisplayText(clsOrderBy, 'o', strOrderingFilesByName, False);
        obSize: DisplayText(clsOrderBy, 'o', strOrderingFilesBySize, False);
        obDate: DisplayText(clsOrderBy, 'o', strOrderingFilesByDate, False);
        obOwner: DisplayText(clsOrderBy, 'o', strOrderingFilesByOwner, False);
        obAttribute: DisplayText(clsOrderBy, 'o', strOrderingFilesByTheirAttrs, False);
      End;
      If OrderFilesDirection = odDescending Then
        Begin
          OutputToConsole(FStd, ' (');
          OutputToConsole(FStd, strOrderingFilesInDescOrder, clYellow);
          OutputToConsole(FStd, ')');
        End;
      OutputToConsoleLn(FStd, '.');
    End;
  Case DateType Of
    dtCreation:   DisplayText(clsDateType, 'e', strDisplayingFileCreationDates);
    dtLastAccess: DisplayText(clsDateType, 'e', strDisplayingFileLastAccessDates);
    dtLastWrite:  DisplayText(clsDateType, 'e', strDisplayingFileLastWriteDates);
  End;
  DisplayText(clsSearchIn, 'i', Format(strSearchForText, [strSearchInText]));
  DisplayText(clsExclusions, 'x', Format(strApplyingExclusions, [strExlFileName]));
  DisplayText(clsUpdate, 'u', strForceACheckForUpdate);
  OutputToConsoleLn(FStd);
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
  OwnerSearch := osEquals;
  OwnerSearchPos := ospNone;
  If Length(ParamStr(iSwitch)) = iIndex Then
    Exit;
  IncrementSwitchPosition(iIndex, iSwitch, strOpenSquareExpectedInOwnerSearchDef);
  If ParamStr(iSwitch)[iIndex] = '[' Then
    Begin
      OwnerSearchPos := ospExact;
      IncrementSwitchPosition(iIndex, iSwitch, strMissingSearchText);
      While (iIndex <= Length(ParamStr(iSwitch))) And
        (ParamStr(iSwitch)[iIndex] <> ']') Do
        Begin
          strOwnerSearch := strOwnerSearch + ParamStr(iSwitch)[iIndex];
          IncrementSwitchPosition(iIndex, iSwitch, strCloseSquareExpectedInOwnerSearchDef);
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
              OwnerSearchPos := ospEnd;
              Delete(strOwnerSearch, 1, 1);
            End;
          If strOwnerSearch[Length(strOwnerSearch)] = '*' Then
            Begin
              If OwnerSearchPos = ospEnd Then
                OwnerSearchPos := ospMiddle
              Else
                OwnerSearchPos := ospStart;
              Delete(strOwnerSearch, Length(strOwnerSearch), 1);
            End;
        End;
      If Length(strOwnerSearch) = 0 Then
        Raise ESearchException.Create(strOwnerSearchIsEmpty);
    End Else
      Dec(iIndex);
End;

(**

  This gets all the command lines switches and sets numerous flags.

  @precon  None.
  @postcon Gets all the command lines switches and sets numerous flags.

**)
Procedure GetCommandLineSwitches;

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
                'h', 'H': Include(CommandLineSwitches, clsShowHelp);
                's', 'S': Include(CommandLineSwitches, clsSubDirectories);
                '!'     : Include(CommandLineSwitches, clsDebug);
                'a', 'A': Include(CommandLineSwitches, clsShowAttribs);
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
                'u', 'U': Include(CommandLineSwitches, clsUpdate);
              Else
                Raise ESearchException.CreateFmt(strInValidCommandLineSwitch,
                  [ParamStr(iSwitch)[iIndex], ParamStr(iSwitch)]);
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
    Raise ESearchException.Create(strNeedToSpecifyCriteria);
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
        strPath + recSearch.Name, strSearchInText, recSearch.Attr);
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
        strOutput := Format(strFileFormat, [
          FormatDateTime(strDateFormat, objFiles.FileInfo[i].Date),
          objFiles.FileInfo[i].Size + 0.1])
      Else
        strOutput := Format(strDirFormat, [
          FormatDateTime(strDateFormat, objFiles.FileInfo[i].Date),
          '<DIR>']);
      if clsShowAttribs in CommandLineSwitches then
        strOutput := strOutput + Format(strAttrFormat, [objFiles.FileInfo[i].Attr]);
      if clsOwner in CommandLineSwitches then
      begin
        iOwnerWidth := objFiles.OwnerWidth;
        strOutput := strOutput + Format(strOwnerFormat, [iOwnerWidth, objFiles.FileInfo[i].Owner]);
      end;
      if not boolDirPrinted then
      begin
        OutputtoConsole(FStd, StringOfChar(#32, iWidth - 1), clNone, clNone, False);
        OutputToConsoleLn(FStd, strPath, clWhite);
        boolDirPrinted := True;
      end;
      boolGREP := (clsSearchIn In CommandLineSwitches) And
        (objFiles.FileInfo[i].GREPLines > 0);
      If Not (clsSearchIn In CommandLineSwitches) Or boolGREP Then
        OutputToConsoleLn(FStd, strOutput + ExtractFileName(objFiles.FileInfo[i].FileName));
      If boolGREP Then
        For iLine := 0 To objFiles.FileInfo[i].GREPLines - 1 Do
          Begin
            OutputToConsole(FStd,
              Format('  %10.0n: ', [objFiles.FileInfo[i].GREPLine[iLine] + 0.1]), clMaroon);
            OutputToConsoleLn(FStd, objFiles.FileInfo[i].GREPText[iLine], clLime, clNone, False);
          End;
    end;
  If objFiles.Count > 0 Then
    OutputToConsoleLn(FStd);
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
    iResult := FindFirst(strPath + '*.*', faAnyFile Or faHidden Or faSysFile,
      recSearch);
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
    ospStart : bool := AnsiCompareText(strOwnerSearch, Copy(strOwner, 1, i)) = 0;
    ospMiddle: bool := Pos(LowerCase(strOwnerSearch), LowerCase(strOwner)) > 0;
    ospEnd   : bool := AnsiCompareText(strOwnerSearch, Copy(strOwner,
      Length(strOwner) - i + 1, i)) = 0;
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
  strOutput : String;

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
      OutputToConsoleLn(FStd);
  (* Search sub directories *)
  If clsSubDirectories In CommandLineSwitches Then
    RecurseDirectories(strPath, iLevel, Result, iResult, slPatterns, recSearch);
  (* Output directory summary levels *)
  If clsSummaryLevel In CommandLineSwitches Then
    Begin
      If (iLevel > -1) And (iLevel <= iSummaryLevel) Then
        If Not ((Result = 0) And (clsSupressZeros In CommandLineSwitches)) Then
          Begin
            strOutput := Format(strSummaryFormat, [Result + 0.1,
              StringOfChar(#32, 2 + 2 * iLevel), strPath]);
            OutputToConsoleLn(FStd, strOutput +
              StringOfChar(#32, iWidth - 1 - Length(strOutput)));
          End;
    End;
  Dec(iLevel);
End;

(** ---------------------------------------------------------------------------

   Main Program body.

 -------------------------------------------------------------------------- **)

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
  BuildRootKey;
  iLevel := -1;
  GetConsoleInformation;
  Try
    PrintTitle;
    CoInitialize(Nil);
    slSearchParams := TStringList.Create;
    Try
      GetCommandLineSwitches;
      TCheckForupdates.Execute(strWebSiteURL, strAppID, strRootKey,
        clsUPdate In CommandLineSwitches);
      slExclusions := TStringList.Create;
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
                      Raise ESearchException.CreateFmt(strErrorInPathSearchParamString,
                        [slSearchParams[i]]);
                    strPath := slSearchParams.Names[i];
                    If strPath = '' Then
                      strPath := GetCurrentDir;
                    If strPath[Length(strPath)] <> '\' Then
                    // Coded like this to stop EurekaLog detecting a Memory Leak, was
                    // strPath := strPath + '\';
                      strPath := Format('%s\', [strPath]);
                    strSearch := slSearchParams.Names[i];
                    // Coded like this to stop EurekaLog detecting a Memory Leak, was
                    // strSearch := slSearchParams.Values[strSearch];
                    strSearch := Format('%s', [slSearchParams.Values[strSearch]]);
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
                      OutputToConsoleLn(FStd);
                    PrintFooter(strPath);
                  End;
              End Else
                PrintHelp;
          End Else
            Raise ESearchException.CreateFmt(strExclusionsNotFound, [strExlFileName]);
      Finally
        slExclusions.Free;
      End;
    Finally
      slSearchParams.Free;
    End;
  Except
    On E : ESearchException Do
      OutputToConsoleLn(FErr, Format(strException, [E.Message]), clRed);
  End;
  If clsDebug In CommandLineSwitches Then
    Begin
      OutputToConsoleLn(FStd);
      OutputToConsole(FStd, strPressEnterToFinish);
      Readln;
    End;
End.
