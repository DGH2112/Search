(**

  This module defines the Search Win32 console application. This application
  is designed to provide regular expression search capabilities for files on
  disk.

  @Version 1.0
  @Author  David Hoyle
  @Date    02 May 2010

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
  ZipForge,
  Graphics,
  ActiveX,
  IniFiles,
  Math,
  DGHLibrary in '..\..\library\DGHLibrary.pas',
  FileHandling in 'Source\FileHandling.pas',
  MSXML2_TLB in '..\..\LIBRARY\MSXML2_TLB.pas',
  checkforupdates in '..\..\LIBRARY\checkforupdates.pas',
  ApplicationFunctions in 'Source\ApplicationFunctions.pas';

Type
  (** This class defines the working that searches the directories and files
      for the information that matches the criteria. **)
  TSearch = Class
  Private
    (** The total number of file found by the search. **)
    FFiles : Integer;
    (** The total number of directories found by the search. **)
    FDirectories : Integer;
    (** This is the upper limit of a file size search. **)
    FUSize : Int64;
    (** This is the lower limit of a file size search. **)
    FLSize  :Int64;
    (** This is the lower limit of a file date search. **)
    FLDate : Double;
    (** This is the upper limit of a file date search. **)
    FUDate : Double;
    (** The total size of al the files found in the search. **)
    FFileSize : Int64;
    (** Height of the console. **)
    FHeight : Integer;
    (** Width of the console. **)
    FWidth : Integer;
    (** This is a list of file attributes that are required in a search. **)
    FFileAttrs : Integer;
    (** This is a list of file type attributes that are required in a search. **)
    FTypeAttrs : Integer;
    (** This is a list of search parameters that are not part of the command line
        switches. **)
    FSearchParams : TStringList;
    (** This is the level of directory of summarisation required. **)
    FSummaryLevel : Integer;
    (** A type to indicate the order of file sorting **)
    FOrderFilesBy : TOrderBy;
    (** A type to indicate the direction of sorting of files. **)
    FOrderFilesDirection : TOrderDirection;
    (** A string for which should be searched for inside text files. **)
    FSearchInText : String;
    (** A variable to hold the type of date to display and search for. **)
    FDateType : TDateType;
    (** A file name which hold exclusions which need to be applied to the searches. **)
    FExlFileName : String;
    (** A string list of exclusions to be applied to the searches. **)
    FExclusions : TStringList;
    (** A string to hold the owner search criteria. **)
    FOwnerSearch : String;
    (** An enumerate to determine if the search is positive or negated **)
    FOwnerSearchOps : TOwnerSearch;
    (** An enumerate to determine the position of the search **)
    FOwnerSearchPos : TOwnerSearchPos;
    (** A handle to the standard console output. **)
    FStd : THandle;
    (** A handle to the error console output. **)
    FErr : THandle;
    (** A string to store the loading and saving settings filename in. **)
    FSizeFormat : TSizeFormat;
    FRootKey : String;
    FParams : TStringList;
    FLevel : Integer;
    FSearchPathColour: TColor;
    FTitleColour: TColor;
    FHeaderColour: TColor;
    FFooterColour: TColor;
    FHelpHeaderColour: TColor;
    FHelpInfoColour: TColor;
    FHelpTextColour: TColor;
    FHelpSwitchColour: TColor;
    FHelpFootNoteColour: TColor;
    FFoundSearchPathColour: TColor;
    FFileInfoColour: TColor;
    FGREPLineNumbersColour: TColor;
    FGREPLineOutputColour: TColor;
    FSummaryOutputColour: TColor;
    FExceptionColour : TColor;
    FZipFileColour : TColor;
  Protected
    procedure GetConsoleInformation;
    procedure OutputCurrentSearchPath(strPath: String);
    procedure PrintTitle;
    procedure PrintHeader(strPath, strPattern: String);
    procedure PrintFooter(strPath: String);
    procedure PrintHelp;
    procedure DisplayCriteria;
    procedure GetCommandLineSwitches;
    function LookupAccountBySID(SID: PSID): String;
    function OutputOwner(strFileName: String): String;
    Function CheckFiles(recSearch: TSearchRec;
      var iDirFiles: Integer; strPath, strOwner: string;
      FilesCollection : TFiles) : Int64;
    Function CheckZipFiles(ZipArchive : TZipForge; ZFAI: TZFArchiveItem;
      var iDirFiles: Integer; strPath, strOwner: string;
      FilesCollection : TFiles) : Int64;
    procedure WorkaroundLargeFiles(var iSize: Int64; recSearch: TSearchRec);
    procedure OutputFilesToConsole(strPath: string; boolDirPrinted: Boolean;
      FilesCollection : TFiles);
    Function RecurseDirectories(strPath: string; var iLevel: Integer;
      slPatterns: TStringList) : Int64;
    Function SearchForPatterns(slPatterns: TStringList; iDirFiles: Integer;
      strPath: string; FilesCollection : TFiles) : Int64;
    Function SearchForPatternsInZip(strFileName : String; slPatterns: TStringList;
      iDirFiles: Integer; strPath: string; FilesCollection : TFiles) : Int64;
    function SearchDirectory(strPath: String; slPatterns: TStringList;
      var iLevel: Integer): Int64;
    function SearchZip(strFileName: String; slPatterns: TStringList;
      var iLevel: Integer): Int64;
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure FilesExceptionHandler(strException : String);
    Procedure ExceptionProc(strMsg : String);
    Function FormatSize(iSize : Int64) : String;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Run;
    (**
      This property returns the Exception colour to outside the class.
      @precon  None.
      @postcon Returns the Exception colour to outside the class.
      @return  a TColor
    **)
    Property ExceptionColour : TColor Read FExceptionColour;
  End;

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

Resourcestring
  (** An exception message for the exclusions file not found. **)
  strExclusionsNotFound = 'The filename "%s" was not found to process exclusions ' +
    'in the searches.';
  (** A resource string to define the Exception output format. **)
  strException = 'ESearchException: %s';
  (** A resource string to note an exception searching files. **)
  strFilesException = 'Exception search files: %s';
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
  (** A help message for searching within ZIP files **)
  strSearchInZips = 'Search within ZIP files.';
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
  (** A resource string to define size format output in kilobytes **)
  strSizeFormatKiloBytes = 'Sizes formatted in KiloBytes';
  (** A resource string to define size format output in megabytes **)
  strSizeFormatMegaBytes = 'Sizes formatted in MegaBytes';
  (** A resource string to define size format output in gigabytes **)
  strSizeFormatGigaBytes = 'Sizes formatted in GigaBytes';
  (** A resource string to define size format output in terabytes **)
  strSizeFormatTeraBytes = 'Sizes formatted in TeraBytes';
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
  strMsg1 = '  Found %s bytes in %1.0n Files in %1.0n Directories.';
  (** A resource string for formatting information. **)
  strMsg2 = '  Total space %s bytes, and %s bytes free.';
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
  strFileFormat = '  %s  %15s  ';
  (** A resource string for matching owners at the end of the string. **)
  strOwnerAtTheEnd = ' at the end';
  (** A resource string for matching owners in the middle of the string. **)
  strOwnerInTheMiddle = ' in the middle';
  (** A resource string for matching owners at the start of the string. **)
  strOwnerAtTheStart = ' at the start';
  (** A resource string for an exact pattern match for an owner. **)
  strOwnerExact = ' exactly';
  (** A resource string for formatting information. **)
  strOwnerMatching = 'matching "%s"';
  (** A resource string for a negative pattern match. **)
  strOwnerNOT = 'NOT ';
  (** A resource string for owner searches. **)
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
  strHelp0050 = '{/X[filename]} {/@[Drive:\Path\FileName.INI]}';
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
  (** A resource string for the console help information **)
  strHelp0188 = 'Sizes can be postfixed with k, m, g or t for kilo, mega, giga and terabytes';
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
  (** A resource string for the console help information. **)
  strHelp0276 = '/@[FileName]';
  (** A resource string for the console help information. **)
  strHelp0277 = 'Allows you to specify an alternate INI file.';
  (** A resource string for the console help information. **)
  strHelp0281 = '/P';
  (** A resource string for the console help information. **)
  strHelp0282 = 'Enables the searching within ZIP archives.';
  (** A resource string for the console help information **)
  strHelp0290 = '/F:KMGT';
  (** A resource string for the console help information **)
  strHelp0291 = 'Changes the output size format to Kilo, Mega, Giga or Terabytes.';

(**

  This routine get and stores the current number of lines and columns
  in the console window.

  @precon  None.
  @postcon Gets the width and height of the console.

**)
Procedure TSearch.GetConsoleInformation;

Var
  ConsoleInfo : TConsoleScreenBufferInfo;

Begin
  FStd := GetStdHandle(STD_OUTPUT_HANDLE);
  FErr := GetStdHandle(STD_ERROR_HANDLE);
  GetConsoleScreenBufferInfo(FStd, ConsoleInfo);
  FWidth := ConsoleInfo.dwSize.X - 1;
  FHeight := ConsoleInfo.dwSize.Y;
End;

(**

  This method outputs the currently being searched path to the screen.

  @precon  None.
  @postcon Outputs the currently being searched path to the screen.

  @param   strPath as a String

**)
Procedure TSearch.OutputCurrentSearchPath(strPath : String);

Var
  i, j : Integer;
  iLength : Integer;

Begin
  If clsQuiet In CommandLineSwitches Then
    Exit;
  iLength := Length(Format(strSearchLabel, [FDirectories, FFiles]));
  While Length(strPath) > (FWidth - iLength) Do
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
    [FDirectories, FFiles]), FWidth - iLength, strPath]);
  If CheckConsoleMode(FStd) Then
    OutputToConsole(FStd, strPath, FSearchPathColour, clNone, False);
End;


(**

  Prints a Title for the application on the screen.

  @precon  None.
  @postcon Prints the title for the application on the screen.

**)
Procedure TSearch.PrintTitle;

Begin
  OutputToConsoleLn(FStd, GetConsoleTitle(strTitle), FTitleColour);
  OutputToConsoleLn(FStd);
End;

(**

  Prints the path and search pattern before each search

  @precon  None.
  @postcon Prints the path and search pattern before each search

  @param   strPath    as a String
  @param   strPattern as a String

**)
Procedure TSearch.PrintHeader(strPath, strPattern : String);

Begin
  OutputToConsoleLn(FStd, Format(strSearchingForHeader, [strPath, strPattern]),
    FHeaderColour);
  FFiles := 0;
  FDirectories := 0;
  FFileSize := 0;
End;

(**

  Prints a footer after the search displaying stats on the search.

  @precon  None.
  @postcon Prints a footer after the search displaying stats on the search.

  @param   strPath as a String

**)
Procedure TSearch.PrintFooter(strPath : String);

Var
  iFreeBytesAvailableToCaller,
  iTotalNumberOfBytes,
  iTotalNumberOfFreeBytes : Int64;

Begin
  GetDiskFreeSpaceEx(PChar(strPath), iFreeBytesAvailableToCaller,
    iTotalNumberOfBytes, @iTotalNumberOfFreeBytes);
  If CheckConsoleMode(FStd) Then
    OutputtoConsole(FStd, StringOfChar(#32, FWidth - 1), clNone, clNone, False);
  OutputToConsoleLn(FStd, Format(strMsg1, [Trim(FormatSize(FFileSize)),
    FFiles + 0.1, FDirectories + 0.1]), FFooterColour);
  OutputToConsoleLn(FStd, Format(strMsg2, [Trim(FormatSize(iTotalNumberOfBytes)),
    Trim(FormatSize(iTotalNumberOfFreeBytes))]), FFooterColour);
End;

(**

  This method prints on the console screen the command line search help
  information.

  @precon  None.
  @postcon Prints on the console screen the command line search help
           information.

**)
Procedure TSearch.PrintHelp;

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
  OutputToConsoleLn(FStd, strHelp0010, FHelpHeaderColour);
  OutputToConsoleLn(FStd, Indent(strHelp0020, 2, 0), FHelpInfoColour);
  OutputToConsoleLn(FStd, Indent(strHelp0030, 4, 0), FHelpInfoColour);
  OutputToConsoleLn(FStd, Indent(strHelp0040, 4, 0), FHelpInfoColour);
  OutputToConsoleLn(FStd, Indent(strHelp0050, 4, 0), FHelpInfoColour);
  OutputToConsoleLn(FStd);
  OutputToConsoleLn(FStd, Indent(strHelp0060, 2, 0), FHelpInfoColour);
  OutputToConsoleLn(FStd);
  OutputToConsole(FStd, Indent(strHelp0070, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0075, FHelpTextColour);
  OutputToConsoleLn(FStd);
  OutputToConsoleLn(FStd, Indent(strHelp0080, iTextIndent, 0), FHelpTextColour);
  OutputToConsoleLn(FStd);
  OutputToConsole(FStd, Indent(strHelp0090, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0095, FHelpTextColour);
  OutputToConsole(FStd, Indent(strHelp0100, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0105, FHelpTextColour);
  OutputToConsole(FStd, Indent(strHelp0110, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0115, FHelpTextColour);
  OutputToConsole(FStd, Indent(strHelp0120, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0125, FHelpTextColour);
  OutputToConsole(FStd, Indent(strHelp0130, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0135, FHelpTextColour);
  OutputToConsole(FStd, Indent(strHelp0140, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0145, FHelpTextColour);
  OutputToConsoleLn(FStd, Indent(strHelp0150, iTextIndent + 2, 0), FHelpInfoColour);
  OutputToConsoleLn(FStd, Indent(strHelp0160, iTextIndent + 2, 0), FHelpInfoColour);
  OutputToConsole(FStd, Indent(strHelp0170, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0175, FHelpTextColour);
  OutputToConsoleLn(FStd, Indent(strHelp0177, iTextIndent + 2, 0), FHelpInfoColour);
  OutputToConsole(FStd, Indent(strHelp0180, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0185, FHelpTextColour);
  OutputToConsoleLn(FStd, Indent(strHelp0187, iTextIndent + 2, 0), FHelpInfoColour);
  OutputToConsoleLn(FStd, Indent(strHelp0188, iTextIndent + 2, 0), FHelpInfoColour);
  OutputToConsole(FStd, Indent(strHelp0190, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0195, FHelpTextColour);
  OutputToConsole(FStd, Indent(strHelp0200, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0205, FHelpTextColour);
  OutputToConsoleLn(FStd, Indent(strHelp0207, iTextIndent, 0));
  OutputToConsole(FStd, Indent(strHelp0210, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0215, FHelpTextColour);
  OutputToConsoleLn(FStd, Indent(strHelp0220, iTextIndent + 2, 0), FHelpInfoColour);
  OutputToConsole(FStd, Indent(strHelp0230, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0235, FHelpTextColour);
  OutputToConsoleLn(FStd, Indent(strHelp0240, iTextIndent + 2, 0), FHelpInfoColour);
  OutputToConsole(FStd, Indent(strHelp0250, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0255, FHelpTextColour);
  OutputToConsole(FStd, Indent(strHelp0260, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0265, FHelpTextColour);
  OutputToConsole(FStd, Indent(strHelp0270, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0275, FHelpTextColour);
  OutputToConsole(FStd, Indent(strHelp0276, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0277, FHelpTextColour);
  OutputToConsole(FStd, Indent(strHelp0281, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0282, FHelpTextColour);
  OutputToConsole(FStd, Indent(strHelp0290, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0291, FHelpTextColour);
  OutputToConsoleLn(FStd);
  OutputToConsoleLn(FStd, strHelp0280, FHelpFootNoteColour);
  OutputToConsoleLn(FStd);
End;

(**

  This is the destructor method for the TSearch class.

  @precon  None.
  @postcon Frees the SearchParams and Exclusions string lists.

**)
destructor TSearch.Destroy;
begin
  SaveSettings;
  FExclusions.Free;
  FSearchParams.Free;
  FParams.Free;
  Inherited Destroy;
end;

(**

  This method displays the criteria and command line options used during the
  session.

  @precon  None.
  @postcon Displays the criteria and command line options used during the
           session.

**)
Procedure TSearch.DisplayCriteria();

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
        OutputToConsole(FStd, Format(strDisplayTextSwitchFormat, [strSwitch]), FHelpSwitchColour);
        OutputToConsole(FStd, Format(strDisplatTextInfoFormat, [strText]), FHelpTextColour);
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
        If FFileAttrs And faReadOnly > 0 Then
          OutputToConsoleLn(FStd, strReadOnly, FHelpInfoColour);
        If FFileAttrs And faArchive > 0 Then
          OutputToConsoleLn(Fstd, strArchive, FHelpInfoColour);
        If FFileAttrs And faSysFile > 0 Then
          OutputToConsoleLn(FStd, strSystemFile, FHelpInfoColour);
        If FFileAttrs And faHidden > 0 Then
          OutputToConsoleLn(FStd, strHidden, FHelpInfoColour);
        If FTypeAttrs And iFileOnly > 0 Then
          OutputToConsoleLn(FStd, strFile, FHelpInfoColour);
        If FTypeAttrs And faDirectory > 0 Then
          OutputToConsoleLn(FStd, strDirectory, FHelpInfoColour);
        If FTypeAttrs And faVolumeID > 0 Then
          OutputToConsoleLn(FStd, strVolume, FHelpInfoColour);
      End;
  End;

Begin
  OutputToConsoleLn(FStd, strSearchCriteria, FHelpHeaderColour);
  DisplayText(clsDisplayCriteria, 'c', 'Displaying criteria used for this search.');
  DisplayText(clsSubDirectories, 's', strRecursingSubdirectories);
  DisplayText(clsDebug, '!', strDEBUGReadLnPause);
  DisplayText(clsShowAttribs, 'a', strDisplayingFileAttibs);
  If Not (clsSearchIn In CommandLineSwitches) Then
    DisplayText(clsSummaryLevel, IntToStr(FSummaryLevel),
      Format(strSummarisingOutput, [FSummaryLevel]));
  DisplayText(clsSupressZeros, '0', strSupressingSummary);
  DisplayText(clsDateRange, 'd', Format(strDisplayFilesWithDates,
    [FormatDateTime(strDateFormat, FLDate),
    FormatDateTime(strDateFormat, FUDate)]));
  DisplayText(clsSizeRange, 'd', Format(strDisplayFilesWithSIzes,
    [ReduceSize(FLSize), ReduceSize(FUSize)]));
  DisplayAttributes;
  DisplayText(clsQuiet, 'q', strQuietMode);
  If DisplayText(clsOwner, 'w', strDisplayTheOwners, False) Then
    Begin
      If FOwnerSearchPos In [ospExact..ospEnd] Then
        Begin
          OutputToConsole(FStd, strOwnerSearchingFor, FHelpTextColour);
          If FOwnerSearchOps In [osNotEquals] Then
            OutputToConsole(FStd, strOwnerNOT, FHelpSwitchColour);
          OutputToConsole(FStd, Format(strOwnerMatching, [FOwnerSearch]), FHelpTextColour);
          Case FOwnerSearchPos Of
            ospExact :OutputToConsole(FStd, strOwnerExact, FHelpTextColour);
            ospStart :OutputToConsole(FStd, strOwnerAtTheStart, FHelpTextColour);
            ospMiddle:OutputToConsole(FStd, strOwnerInTheMiddle, FHelpTextColour);
            ospEnd   :OutputToConsole(FStd, strOwnerAtTheEnd, FHelpTextColour);
          End;
        End;
      OutputToConsoleLn(FStd, '.', FHelpTextColour);
    End;
  If clsOrderBy In CommandLineSwitches Then
    Begin
      Case FOrderFilesBy Of
        obName: DisplayText(clsOrderBy, 'o', strOrderingFilesByName, False);
        obSize: DisplayText(clsOrderBy, 'o', strOrderingFilesBySize, False);
        obDate: DisplayText(clsOrderBy, 'o', strOrderingFilesByDate, False);
        obOwner: DisplayText(clsOrderBy, 'o', strOrderingFilesByOwner, False);
        obAttribute: DisplayText(clsOrderBy, 'o', strOrderingFilesByTheirAttrs, False);
      End;
      If FOrderFilesDirection = odDescending Then
        Begin
          OutputToConsole(FStd, ' (', FHelpTextColour);
          OutputToConsole(FStd, strOrderingFilesInDescOrder, FHelpFootNoteColour);
          OutputToConsole(FStd, ')', FHelpTextColour);
        End;
      OutputToConsoleLn(FStd, '.', FHelpTextColour);
    End;
  Case FDateType Of
    dtCreation:   DisplayText(clsDateType, 'e', strDisplayingFileCreationDates);
    dtLastAccess: DisplayText(clsDateType, 'a', strDisplayingFileLastAccessDates);
    dtLastWrite:  DisplayText(clsDateType, 'w', strDisplayingFileLastWriteDates);
  End;
  DisplayText(clsSearchIn, 'i', Format(strSearchForText, [FSearchInText]));
  DisplayText(clsExclusions, 'x', Format(strApplyingExclusions, [FExlFileName]));
  DisplayText(clsUpdate, 'u', strForceACheckForUpdate);
  DisplayText(clsUpdate, 'p', strSearchInZips);
  Case FSizeFormat Of
    sfKilobytes: DisplayText(clsSizeOutput, 'p', strSizeFormatKilobytes);
    sfMegaBytes: DisplayText(clsSizeOutput, 'p', strSizeFormatMegabytes);
    sfGigaBytes: DisplayText(clsSizeOutput, 'p', strSizeFormatGigabytes);
    sfTeraBytes: DisplayText(clsSizeOutput, 'p', strSizeFormatTerabytes);
  End;
  DisplayText(clsUpdate, 'f', strForceACheckForUpdate);
  OutputToConsoleLn(FStd);
End;

(**

  This is a exception handler for the BuildRootKey method.

  @precon  None.
  @postcon Raise the exception for termination.

  @param   strMsg as a String

**)
procedure TSearch.ExceptionProc(strMsg: String);
begin
  Raise Exception.Create(strMsg);
end;

(**

  This method outputs exception from the TFiles class to the console.

  @precon  None.
  @postcon Outputs exception from the TFiles class to the console.

  @param   strException as a String

**)
procedure TSearch.FilesExceptionHandler(strException: String);
begin
  OutputToConsoleLn(FStd, Format(strFilesException, [strException]),
    FExceptionColour);
end;

(**

  This method returns the file size formatted to that defined on the command
  line.

  @precon  None.
  @postcon Returns the file size formatted to that defined on the command
           line.

  @param   iSize as an Int64
  @return  a String

**)
function TSearch.FormatSize(iSize: Int64): String;

Var
  dblSize : Double;

begin
  dblSize := iSize;
  Case FSizeFormat Of
    sfKilobytes: Result := Format('%13.0nK',
      [RoundTo(dblSize / 1024.0, 0)]);
    sfMegaBytes: Result := Format('%10.0nM',
      [RoundTo(dblSize / (1024.0 * 1024.0), 0)]);
    sfGigaBytes: Result := Format('%7.0nG',
      [RoundTo(dblSize / (1024.0 * 1024.0 * 1024.0), 0)]);
    sfTeraBytes: Result := Format('%4.0nT',
      [RoundTo(dblSize / (1024.0 * 1024.0 * 1024.0 * 1024.0), 0)]);
  Else
    Result := Format('%15.0n', [dblSize]);
  End;
end;

(**

  This gets all the command lines switches and sets numerous flags.

  @precon  None.
  @postcon Gets all the command lines switches and sets numerous flags.

**)
Procedure TSearch.GetCommandLineSwitches;

Var
  iSwitch, iIndex : Integer;

Begin
  FSummaryLevel := 0;
  CommandLineSwitches := [];
  iSwitch := 0;
  FOrderFilesBy := obNone;
  FOrderFilesDirection := odAscending;
  FDateType := dtLastWrite;
  While iSwitch <= FParams.Count - 1 Do
    Begin
      If CharInSet(FParams[iSwitch][1], ['-', '/']) Then
        Begin
          iIndex := 2;
          While iIndex <= Length(FParams[iSwitch]) Do
            Begin
              Case FParams[iSwitch][iIndex] Of
                '?'     : Include(CommandLineSwitches, clsShowHelp);
                'h', 'H': Include(CommandLineSwitches, clsShowHelp);
                's', 'S': Include(CommandLineSwitches, clsSubDirectories);
                '!'     : Include(CommandLineSwitches, clsDebug);
                'a', 'A': Include(CommandLineSwitches, clsShowAttribs);
                '1'..'9': GetSummaryLevel(FParams, iSwitch, iIndex, FSummaryLevel);
                '0'     : Include(CommandLineSwitches, clsSupressZeros);
                'd', 'D': GetDateRange(FParams, iSwitch, iIndex, FLDate, FUDate);
                'z', 'Z': GetSizeRange(FParams, iSwitch, iIndex, FLSize, FUSize);
                't', 'T': GetAttributes(FParams, iSwitch, iIndex, FFileAttrs,
                            FTypeAttrs);
                'q', 'Q': Include(CommandLineSwitches, clsQuiet);
                'w', 'W': GetOwnerSwitch(FParams, iSwitch, iIndex, FOwnerSearchOps,
                            FOwnerSearchPos, FOwnerSearch);
                'o', 'O': GetOrderBy(FParams, iSwitch, iIndex, FOrderFilesDirection,
                            FOrderFilesBy);
                'i', 'I': GetSearchInInfo(FParams, iSwitch, iIndex, FSearchInText);
                'e', 'E': GetDateType(FParams, iSwitch, iIndex, FDateType);
                'c', 'C': Include(CommandLineSwitches, clsDisplayCriteria);
                'x', 'X': GetExclusions(FParams, iSwitch, iIndex, FExlFileName);
                'u', 'U': Include(CommandLineSwitches, clsUpdate);
                'p', 'P': Include(CommandLineSwitches, clsSearchZip);
                'f', 'F': GetSizeFormat(FParams, iSwitch, iIndex, FSizeFormat);
              Else
                Raise ESearchException.CreateFmt(strInValidCommandLineSwitch,
                  [FParams[iSwitch][iIndex], FParams[iSwitch]]);
              End;
              Inc(iIndex);
            End;
        End Else
          FSearchParams.Add(
            Format('%s=%s', [
              ExtractFilePath(FParams[iSwitch]),
              ExtractFileName(FParams[iSwitch])
            ])
          );
      Inc(iSwitch); // Get the next switch
    End;
  If FParams.Count = 0 Then
    Include(CommandLineSwitches, clsShowHelp);
  If (FSearchParams.Count = 0) And Not (ClsShowHelp In CommandLineSwitches) Then
    If FParams.Count = 0 Then
      Include(CommandLineSwitches, clsShowHelp)
    Else
      Raise ESearchException.Create(strNeedToSpecifyCriteria);
  If clsDisplayCriteria In CommandLineSwitches Then
    DisplayCriteria;
End;

(**

  This method loads the colour settings from the INI file.

  @precon  None.
  @postcon Loads the colour settings from the INI file.

**)
procedure TSearch.LoadSettings;
begin
  With TIniFile.Create(FRootKey) Do
    Try
      FSearchPathColour := StringToColor(ReadString('Colours', 'SearchPath', 'clMaroon'));
      FTitleColour := StringToColor(ReadString('Colours', 'Title', 'clNone'));
      FHeaderColour := StringToColor(ReadString('Colours', 'Header', 'clYellow'));
      FFooterColour := StringToColor(ReadString('Colours', 'Footer', 'clWhite'));
      FHelpHeaderColour := StringToColor(ReadString('Colours', 'HelpHeader', 'clWhite'));
      FHelpInfoColour := StringToColor(ReadString('Colours', 'HelpInfo', 'clLime'));
      FHelpTextColour := StringToColor(ReadString('Colours', 'HelpText', 'clNone'));
      FHelpSwitchColour := StringToColor(ReadString('Colours', 'HelpSwitch', 'clRed'));
      FHelpFootNoteColour := StringToColor(ReadString('Colours', 'HelpFoorNote', 'clYellow'));
      FFoundSearchPathColour := StringToColor(ReadString('Colours', 'FoundSearchPath', 'clWhite'));
      FFileInfoColour := StringToColor(ReadString('Colours', 'FileInfo', 'clNone'));
      FGREPLineNumbersColour := StringToColor(ReadString('Colours', 'GREPLineNumbers', 'clRed'));
      FGREPLineOutputColour := StringToColor(ReadString('Colours', 'GREPLineOutput', 'clLime'));
      FSummaryOutputColour := StringToColor(ReadString('Colours', 'SummaryOutput', 'clNone'));
      FExceptionColour := StringToColor(ReadString('Colours', 'Exception', 'clRed'));
      FZipFileColour := StringToColor(ReadString('Colours', 'ZipFile', 'clFuchsia'));
    Finally
      Free;
    End;
end;

(**

  This method returns the owner of the files from a SID.

  @precon  None.
  @postcon Returns the owner of the files from a SID.

  @param   SID as a PSID
  @return  a String

**)
Function TSearch.LookupAccountBySID(SID : PSID) : String;

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
  SetLength(strName, Length(strName) - 1);
  SetLength(strRefDomain, Length(strRefDomain) - 1);
  Result := strRefDomain + '/' + strName;
End;

(**

  Outputs the owner of the filename.

  @precon  None.
  @postcon Outputs the owner of the file.

  @param   strFileName as a String
  @return  a String

**)
Function TSearch.OutputOwner(strFileName : String) : String;

Var
  SD : PSecurityDescriptor;
  Owner : PSID;

Begin
  Result := '';
  If GetNamedSecurityInfo(PAnsiChar(AnsiString(strFileName)), SE_FILE_OBJECT,
    OWNER_SECURITY_INFORMATION, @Owner, Nil, Nil, Nil,
    Pointer(SD)) = ERROR_SUCCESS Then
    Begin
      Result := LookupAccountBySID(Owner);
      LocalFree(Cardinal(SD));
    End;
End;

(**

  This method adds the file to the collection and increments the various
  counters.

  @precon  None.
  @postcon Adds the file to the collection and increments the various counters.

  @param   recSearch       as a TSearchRec
  @param   iDirFiles       as an Integer as a reference
  @param   strPath         as a string
  @param   strOwner        as a string
  @param   FilesCollection as a TFiles
  @return  an Int64

**)
Function TSearch.CheckFiles(recSearch: TSearchRec; var iDirFiles: Integer;
  strPath, strOwner: string; FilesCollection : TFiles) : Int64;

  (**

    This function returns the text of the text file IF GREP searching is
    required.

    @precon  None.
    @postcon Returns the text of the text file IF GREP searching is
             required else returns a null string

    @return  a String

  **)
  Function GetFileText : String;

  Var
    sl : TStringList;

  Begin
    Result := '';
    If clsSearchIn In CommandLineSwitches Then
      Begin
        sl := TStringList.Create;
        Try
          sl.LoadFromFile(strPath + recSearch.Name);
          Result := sl.Text;
        Finally
          sl.Free;
        End;
      End;
  End;

Var
  dtDate, dtDateLocal : TFileTime;
  iTime : Integer;
  boolAdded: Boolean;

begin
  Result := 0;
  if not (clsSummaryLevel in CommandLineSwitches) then
    begin
      Inc(iDirFiles);
      Case FDateType Of
        dtCreation: dtDate := recSearch.FindData.ftCreationTime;
        dtLastAccess: dtDate := recSearch.FindData.ftLastAccessTime;
        dtLastWrite: dtDate := recSearch.FindData.ftLastWriteTime;
      End;
      FileTimeToLocalFileTime(dtDate, dtDateLocal);
      iTime := 0;
      FileTimeToDosDateTime(dtDateLocal, LongRec(iTime).Hi, LongRec(iTime).Lo);
      boolAdded := FilesCollection.Add(FileDateToDateTime(iTime), recSearch.Size,
        OutputAttributes(recSearch.Attr), strOwner, recSearch.Name,
        FSearchInText, GetFileText, recSearch.Attr);
    end Else
      boolAdded := True;
  If boolAdded Then
    Begin
      Inc(FFiles);
      Inc(FFileSize, recSearch.Size);
      Inc(Result, recSearch.Size);
    End;
end;

(**

  This method adds the zip file to the collection and increments the various
  counters.

  @precon  None.
  @postcon Adds the zip file to the collection and increments the various
           counters.

  @param   ZipArchive      as a TZipForge
  @param   ZFAI            as a TZFArchiveItem
  @param   iDirFiles       as an Integer as a reference
  @param   strPath         as a string
  @param   strOwner        as a string
  @param   FilesCollection as a TFiles
  @return  an Int64

**)
function TSearch.CheckZipFiles(ZipArchive : TZipForge; ZFAI: TZFArchiveItem;
  var iDirFiles: Integer; strPath, strOwner: string; FilesCollection : TFiles): Int64;

  (**

    This function returns the text of the zip file IF GREP searching is
    required.

    @precon  None.
    @postcon Returns the text of the zip file IF GREP searching is
             required else returns a null string

    @return  a String

  **)
  Function GetZipFileText : String;

  Begin
    Result := '';
    If clsSearchIn In CommandLineSwitches Then
      ZipArchive.ExtractToString(ZFAI.StoredPath + ZFAI.FileName, Result);
  End;

var
  iTime : Integer;
  boolAdded: Boolean;

begin
  Result := 0;
  if not (clsSummaryLevel in CommandLineSwitches) then
    begin
      Inc(iDirFiles);
      LongRec(iTime).Lo := ZFAI.LastModFileTime;
      LongRec(iTime).Hi := ZFAI.LastModFileDate;
      boolAdded := FilesCollection.Add(FileDateToDateTime(iTime),
        ZFAI.UncompressedSize, OutputAttributes(ZFAI.ExternalFileAttributes),
        strOwner, ZFAI.StoredPath + ZFAI.FileName, FSearchInText,
        GetZIPFileText, ZFAI.ExternalFileAttributes);
    end Else
      boolAdded := True;
  If boolAdded Then
    Begin
      Inc(FFiles);
      Inc(FFileSize, ZFAI.UncompressedSize);
      Inc(Result, ZFAI.UncompressedSize);
    End;
end;

(**

  This is the constructor method for the TSearch class.

  @precon  None.
  @postcon Initialises the SearchParams and Exclusions stringlists.

**)
constructor TSearch.Create;
begin
  FExceptionColour := clRed;
  FSearchParams := TStringList.Create;
  FExclusions := TStringList.Create;
  FParams := TStringList.Create;
  CoInitialize(Nil);
  FLevel := -1;
  SetRoundMode(rmUp);
end;

(**

  This method provides a workaround for identifying the size of large files.

  @precon  None.
  @postcon Provides a workaround for identifying the size of large files.

  @param   iSize     as an Int64 as a reference
  @param   recSearch as a TSearchRec

**)
procedure TSearch.WorkaroundLargeFiles(var iSize: Int64; recSearch: TSearchRec);

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

  @param   strPath         as a string
  @param   boolDirPrinted  as a Boolean
  @param   FilesCollection as a TFiles

**)
procedure TSearch.OutputFilesToConsole(strPath: string; boolDirPrinted: Boolean;
  FilesCollection : TFiles);
var
  i: Integer;
  iOwnerWidth: Integer;
  strOutput: string;
  iLine : Integer;
  boolGREP: Boolean;
  iZipPos: Integer;

begin
  For i := 0 to FilesCollection.Count - 1 Do
    begin
      If Pos('D', FilesCollection.FileInfo[i].Attr) = 0 Then
        strOutput := Format(strFileFormat, [
          FormatDateTime(strDateFormat, FilesCollection.FileInfo[i].Date),
          FormatSize(FilesCollection.FileInfo[i].Size)])
      Else
        strOutput := Format(strDirFormat, [
          FormatDateTime(strDateFormat, FilesCollection.FileInfo[i].Date),
          '<DIR>']);
      if clsShowAttribs in CommandLineSwitches then
        strOutput := strOutput + Format(strAttrFormat, [FilesCollection.FileInfo[i].Attr]);
      if clsOwner in CommandLineSwitches then
      begin
        iOwnerWidth := FilesCollection.OwnerWidth;
        strOutput := strOutput + Format(strOwnerFormat, [iOwnerWidth,
          FilesCollection.FileInfo[i].Owner]);
      end;
      if not boolDirPrinted then
      begin
        If CheckConsoleMode(FStd) Then
          OutputtoConsole(FStd, StringOfChar(#32, FWidth), clNone, clNone, False);
        If Not Like('*.zip\*', strPath) Then
          OutputToConsoleLn(FStd, strPath, FFoundSearchPathColour)
        Else
          Begin
            iZipPos := Pos('.zip', LowerCase(strPath));
            OutputtoConsole(FStd, ExtractFilePath(Copy(strPath, 1, iZipPos + 3)), FFoundSearchPathColour);
            OutputtoConsole(FStd, ExtractFileName(Copy(strPath, 1, iZipPos + 3)), FZipFileColour);
            OutputtoConsoleLn(FStd, Copy(strPath, iZipPos + 4, Length(strPath) - iZipPos - 4), FFoundSearchPathColour);
          End;
        boolDirPrinted := True;
      end;
      boolGREP := (clsSearchIn In CommandLineSwitches) And
        (FilesCollection.FileInfo[i].GREPLines > 0);
      If Not (clsSearchIn In CommandLineSwitches) Or boolGREP Then
        OutputToConsoleLn(FStd, strOutput +
          FilesCollection.FileInfo[i].FileName, FFileInfoColour);
      If boolGREP Then
        For iLine := 0 To FilesCollection.FileInfo[i].GREPLines - 1 Do
          Begin
            OutputToConsole(FStd,
              Format('  %10.0n: ', [FilesCollection.FileInfo[i].GREPLine[iLine] + 0.1]),
                FGREPLineNumbersColour);
            OutputToConsoleLn(FStd, FilesCollection.FileInfo[i].GREPText[iLine],
              FGREPLineOutputColour, clNone, False);
          End;
    end;
  If FilesCollection.Count > 0 Then
    OutputToConsoleLn(FStd);
end;

(**

  This method recurses the directories searching for additional files.

  @precon  None.
  @postcon Recurses the directories searching for additional files.

  @param   strPath    as a string
  @param   iLevel     as an Integer as a reference
  @param   slPatterns as a TStringList
  @return  an Int64

**)
Function TSearch.RecurseDirectories(strPath: string; var iLevel: Integer;
  slPatterns: TStringList) : Int64;

Var
  recSearch : TSearchRec;
  iResult: Integer;

begin
  Result := 0;
  iResult := FindFirst(strPath + '*.*', faAnyFile Or faHidden Or faSysFile,
    recSearch);
  While (iResult = 0) Do
    Begin
      If recSearch.Attr And faDirectory > 0 Then
        If (recSearch.Name <> '.') And (recSearch.Name <> '..') Then
          Inc(Result, SearchDirectory(strPath + recSearch.Name + '\', slPatterns,
            iLevel));
      If clsSearchZip In CommandLineSwitches Then
        If Like('*.zip', recSearch.Name) Then
          Inc(Result, SearchZip(strPath + recSearch.Name, slPatterns, iLevel));
      iResult := FindNext(recSearch);
    End;
  SysUtils.FindClose(recSearch);
end;

(**

  This method starts the processing of the files and directories inline with
  the criteria that are specified on the command line.

  @precon  None.
  @postcon Outputs to the console the the information on found files.

**)
procedure TSearch.Run;

Var
  iPos : Integer;
  strPath : String;
  strSearch : String;
  FPatterns : TStringList;
  i : Integer;
  iPattern : Integer;
  
begin
  GetConsoleInformation;
  FRootKey := BuildRootKey(FParams, FilesExceptionHandler);
  LoadSettings;
  PrintTitle;
  GetCommandLineSwitches;
  TCheckForupdates.Execute(strAppID, FRootKey, clsUPdate In CommandLineSwitches);
  If Not (clsExclusions In CommandLineSwitches) Or
    ((clsExclusions In CommandLineSwitches) And FileExists(FExlFileName)) Then
    Begin
      If clsExclusions In CommandLineSwitches Then
        FExclusions.LoadFromFile(FExlFileName);
      FExclusions.Text := LowerCase(FExclusions.Text);
      If Not (clsShowHelp In CommandLineSwitches) Then
        Begin
          For i := 0 To FSearchParams.Count - 1 Do
            Begin
              iPos := Pos('=', FSearchParams[i]);
              If iPos = 0 Then
                Raise ESearchException.CreateFmt(
                  strErrorInPathSearchParamString, [FSearchParams[i]]);
              strPath := FSearchParams.Names[i];
              If strPath = '' Then
                strPath := GetCurrentDir;
              If strPath[Length(strPath)] <> '\' Then
                strPath := strPath + '\';
              strSearch := FSearchParams.Names[i];
              strSearch := FSearchParams.Values[strSearch];
              PrintHeader(strPath, strSearch);
              FPatterns := TStringList.Create;
              Try
                For iPattern := 1 To CharCount(';', strSearch) + 1 Do
                  FPatterns.Add(GetField(strSearch, ';', iPattern));
                SearchDirectory(strPath, FPatterns, FLevel);
              Finally
                FPatterns.Free;
              End;
              If clsSummaryLevel In CommandLineSwitches Then
                OutputToConsoleLn(FStd);
              PrintFooter(strPath);
            End;
        End Else
          PrintHelp;
    End Else
      Raise ESearchException.CreateFmt(strExclusionsNotFound, [FExlFileName]);
end;

(**

  This method search the current directory for files which match all the
  specified search patterns.

  @precon  None.
  @postcon Search the current directory for files which match all the specified
           search patterns.

  @param   slPatterns      as a TStringList
  @param   iDirFiles       as an Integer
  @param   strPath         as a string
  @param   FilesCollection as a TFiles
  @return  an Int64

**)
Function TSearch.SearchForPatterns(slPatterns: TStringList; iDirFiles: Integer;
  strPath: string; FilesCollection : TFiles) : Int64;

var
  iPattern: Integer;
  strOwner : String;
  recSearch : TSearchRec;
  iResult: Integer;
  boolFound : Boolean;
  ST: TSystemTime;
  dtDate: TDateTime;

begin
  Result := 0;
  For iPattern := 0 to slPatterns.Count - 1 Do
    Begin
      iResult := FindFirst(strPath + slPatterns[iPattern], faAnyFile, recSearch);
      Try
        While iResult = 0 Do
          Begin
            boolFound := True;
            CheckFileAttributes(recSearch.Attr, FFileAttrs, FTypeAttrs, boolFound);
            CheckSizeRange(recSearch.Size, FLSize, FUSize, boolFound);
            Case FDateType Of
              dtCreation:
                Begin
                  FileTimeToSystemTime(recSearch.FindData.ftCreationTime, ST);
                  dtDate := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) +
                    EncodeTime(ST.wHour, ST.wMinute, ST.wSecond, ST.wMilliseconds);
                  CheckDateRange(DateTimeToFileDate(dtDate), FLDate, FUDate, boolFound);
                End;
              dtLastAccess:
                Begin
                  FileTimeToSystemTime(recSearch.FindData.ftLastAccessTime, ST);
                  dtDate := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) +
                    EncodeTime(ST.wHour, ST.wMinute, ST.wSecond, ST.wMilliseconds);
                  CheckDateRange(DateTimeToFileDate(dtDate), FLDate, FUDate, boolFound);
                End
            Else
              CheckDateRange(recSearch.Time, FLDate, FUDate, boolFound);
            End;
            CheckExclusions(strPath, recSearch.Name, boolFound, FExclusions);
            If clsOwner In CommandLineSwitches Then
              Begin
                strOwner := OutputOwner(strPath + recSearch.Name);
                CheckOwner(strOwner, FOwnerSearch, FOwnerSearchPos, FOwnerSearchOps,
                  boolFound);
              End;
            If boolFound Then
              Inc(Result, CheckFiles(recSearch, iDirFiles, strPath, strOwner,
                FilesCollection));
            iResult := FindNext(recSearch);
            boolFound := True;
          End;
      Finally
        SysUtils.FindClose(recSearch);
      End;
    End;
end;

(**

  This method search the zip file for files which match all the specified search
  patterns.

  @precon  None.
  @postcon Search the zip file for files which match all the specified search
           patterns.

  @param   strFileName     as a String
  @param   slPatterns      as a TStringList
  @param   iDirFiles       as an Integer
  @param   strPath         as a string
  @param   FilesCollection as a TFiles
  @return  an Int64

**)
function TSearch.SearchForPatternsInZip(strFileName: String;
  slPatterns: TStringList; iDirFiles: Integer; strPath: string;
  FilesCollection : TFiles): Int64;

Var
  Z : TZipForge;
  iPattern : Integer;
  boolResult : Boolean;
  boolFound: Boolean;
  ZFAI : TZFArchiveItem;
  iSize: Int64;
  iDateTime : Integer;
  strOwner: String;

begin
  Result := 0;
  Z := TZipForge.Create(Nil);
  Try
    Z.FileName := strFileName;
    Z.OpenArchive;
    Try
      For iPattern := 0 to slPatterns.Count - 1 Do
        Begin
          boolResult := Z.FindFirst(slPatterns[iPattern], ZFAI);
          While boolResult Do
            Begin
              OutputCurrentSearchPath(strFileName + '\' + ZFAI.StoredPath);
              boolFound := True;
              iSize := ZFAI.UncompressedSize;
              CheckFileAttributes(ZFAI.ExternalFileAttributes, FFileAttrs,
                FTypeAttrs, boolFound);
              CheckSizeRange(iSize, FLSize, FUSize, boolFound);
              // Zip files only contain the last write date of a file.
              LongRec(iDateTime).Lo := ZFAI.LastModFileTime;
              LongRec(iDateTime).Hi := ZFAI.LastModFileDate;
              CheckDateRange(iDateTime, FLDate, FUDate, boolFound);
              CheckExclusions(strPath, ZFAI.StoredPath + ZFAI.FileName, boolFound, FExclusions);
              If clsOwner In CommandLineSwitches Then
                Begin
                  strOwner := OutputOwner(strFileName);
                  CheckOwner(strOwner, FOwnerSearch, FOwnerSearchPos,
                    FOwnerSearchOps, boolFound);
                End;
              If boolFound Then
                Inc(Result, CheckZipFiles(Z, ZFAI, iDirFiles, strPath, strOwner,
                  FilesCollection));
              boolResult := Z.FindNext(ZFAI);
              boolFound := True;
            End;
        End;
    Finally
      Z.CloseArchive;
    End;
  Finally
    Z.Free;
  End;
end;

(**

  This method searches the named zip file for files.

  @precon  None.
  @postcon Method searches the named zip file for files.

  @param   strFileName as a String
  @param   slPatterns  as a TStringList
  @param   iLevel      as an Integer as a reference
  @return  an Int64

**)
function TSearch.SearchZip(strFileName: String; slPatterns: TStringList;
  var iLevel: Integer): Int64;

Var
  boolDirPrinted: Boolean;
  iDirFiles: Integer;
  FilesCollection : TFiles;
  PathCollections: TObjectList;
  iPath: Integer;
  PathCollection: TFiles;
  iFile: Integer;
  strPath: String;
  Files: TFiles;

begin
  OutputCurrentSearchPath(strFileName);
  Inc(iLevel);
  Result := 0;
  iDirFiles := 0;
  Inc(FDirectories);
  boolDirPrinted := False;
  (* Find Files in Zip *)
  FilesCollection := TFiles.Create(FilesExceptionHandler);
  Try
    Inc(Result, SearchForPatternsInZip(strFileName, slPatterns, iDirFiles,
      strFileName, FilesCollection));
    // Break down files into individual paths
    FilesCollection.OrderBy(obName, odAscending);
    PathCollections := TObjectList.Create(True);
    Try
      strPath := '';
      Files := Nil;
      For iFile := 0 To FilesCollection.Count - 1 Do
        Begin
          If (strPath <> ExtractFilePath(FilesCollection.FileInfo[iFile].FileName)) Or
            (Files = Nil) Then
            Begin
              Files := TFiles.Create(FilesExceptionHandler);
              PathCollections.Add(Files);
              strPath := ExtractFilePath(FilesCollection.FileInfo[iFile].FileName);
              Files.Path := strPath;
            End;
          If Files <> Nil Then
            Files.Add(FilesCollection.FileInfo[iFile].Clone);
        End;
      For iPath := 0 To PathCollections.Count - 1 Do
        Begin
          PathCollection := PathCollections[iPath] As TFiles;
          If FOrderFilesBy <> obNone Then
            PathCollection.OrderBy(FOrderFilesBy, FOrderFilesDirection);
          OutputFilesToConsole(strFileName + '\' + PathCollection.Path,
            boolDirPrinted, PathCollection);
        End;
    Finally
      PathCollections.Free;
    End;
  Finally
    FilesCollection.Free;
  End;
  If Not (clsSummaryLevel In CommandLineSwitches) Then
    If iDirFiles > 0 Then
      OutputToConsoleLn(FStd);
  Dec(iLevel);
end;

(**

  This method saves the colour settings to the INI file.

  @precon  None.
  @postcon Saves the colour settings to the INI file.

**)
procedure TSearch.SaveSettings;
begin
  With TIniFile.Create(FRootKey) Do
    Try
      WriteString('Colours', 'SearchPath', ColorToString(FSearchPathColour));
      WriteString('Colours', 'Title', ColorToString(FTitleColour));
      WriteString('Colours', 'Header', ColorToString(FHeaderColour));
      WriteString('Colours', 'Footer', ColorToString(FFooterColour));
      WriteString('Colours', 'HelpHeader', ColorToString(FHelpHeaderColour));
      WriteString('Colours', 'HelpInfo', ColorToString(FHelpInfoColour));
      WriteString('Colours', 'HelpText', ColorToString(FHelpTextColour));
      WriteString('Colours', 'HelpSwitch', ColorToString(FHelpSwitchColour));
      WriteString('Colours', 'HelpFootNote', ColorToString(FHelpFootNoteColour));
      WriteString('Colours', 'FoundSearchPath', ColorToString(FFoundSearchPathColour));
      WriteString('Colours', 'FileInfo', ColorToString(FFileInfoColour));
      WriteString('Colours', 'GREPLineNumbers', ColorToString(FGREPLineNumbersColour));
      WriteString('Colours', 'GREPLineOutput', ColorToString(FGREPLineOutputColour));
      WriteString('Colours', 'SummaryOutput', ColorToString(FSummaryOutputColour));
      WriteString('Colours', 'Exception', ColorToString(FExceptionColour));
      WriteString('Colours', 'ZipFile', ColorToString(FZipFileColour));
    Finally
      Free;
    End;
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
Function TSearch.SearchDirectory(strPath: String; slPatterns : TStringList;
  Var iLevel : Integer) : Int64;

Var
  boolDirPrinted : Boolean;
  iDirFiles : Integer;
  strOutput : String;
  FilesCollection : TFiles;

Begin
  OutputCurrentSearchPath(strPath);
  Inc(iLevel);
  Result := 0;
  iDirFiles := 0;
  Inc(FDirectories);
  boolDirPrinted := False;
  (* Find Files *)
  FilesCollection := TFiles.Create(FilesExceptionHandler);
  Try
    Inc(Result, SearchForPatterns(slPatterns, iDirFiles, strPath, FilesCollection));
    If FOrderFilesBy <> obNone Then
      FilesCollection.OrderBy(FOrderFilesBy, FOrderFilesDirection);
    OutputFilesToConsole(strPath, boolDirPrinted, FilesCollection);
  Finally
    FilesCollection.Free;
  End;
  If Not (clsSummaryLevel In CommandLineSwitches) Then
    If iDirFiles > 0 Then
      OutputToConsoleLn(FStd);
  (* Search sub directories *)
  If clsSubDirectories In CommandLineSwitches Then
    Inc(Result, RecurseDirectories(strPath, iLevel, slPatterns));
  (* Output directory summary levels *)
  If clsSummaryLevel In CommandLineSwitches Then
    Begin
      If (iLevel > -1) And (iLevel <= FSummaryLevel) Then
        If Not ((Result = 0) And (clsSupressZeros In CommandLineSwitches)) Then
          Begin
            strOutput := Format(strSummaryFormat, [Result + 0.1,
              StringOfChar(#32, 2 + 2 * iLevel), strPath]);
            OutputToConsoleLn(FStd, strOutput +
              StringOfChar(#32, FWidth - 1 - Length(strOutput)), FSummaryOutputColour);
          End;
    End;
  Dec(iLevel);
End;

(** ---------------------------------------------------------------------------

   Main Program body.

 -------------------------------------------------------------------------- **)

Var
  (** A variable to hold whether an exception has occurred. **)
  boolException : Boolean;

Begin
  {$IFDEF EURALOG}
  SetEurekaLogState(Not IsDebuggerPresent);
  {$ENDIF}
  ReportMemoryLeaksOnShutdown := IsDebuggerPresent;
  boolException := False;
  With TSearch.Create Do
    Try
      Try
        Run;
      Except
        On E : ESearchException Do
          Begin
            OutputToConsoleLn(FErr, Format(strException, [E.Message]),
              ExceptionColour);
            boolException := True;
          End;
      End;
      If clsDebug In CommandLineSwitches Then
        Begin
          OutputToConsoleLn(FStd);
          OutputToConsole(FStd, strPressEnterToFinish);
          Readln;
        End;
    Finally
      Free;
    End;
  If boolException Then
    Halt(1);
End.
