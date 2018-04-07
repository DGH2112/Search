(**

  This module contains the main code used nby SEARCH to, yes, search for files.

  @Author  David Hoyle
  @Version 1.0
  @Date    07 Apr 2018

**)
Unit SearchEngine;

Interface

Uses
  SysUtils,
  Classes,
  Windows,
  ZipForge,
  Graphics,
  FileHandling, 
  Search.Functions;

Type
  (** This class defines the working that searches the directories and files
   for the information that matches the criteria. **)
  TSearch = Class
  Strict Private
    FFiles: Integer;       (** The total number of file found by the search. **)
    FDirectories: Integer; (** The total number of directories found by the search. **)
    FUSize: Int64;         (** This is the upper limit of a file size search. **)
    FLSize: Int64;         (** This is the lower limit of a file size search. **)
    FLDate: Double;        (** This is the lower limit of a file date search. **)
    FUDate: Double;        (** This is the upper limit of a file date search. **)
    FFileSize: Int64;      (** The total size of al the files found in the search. **)
    FHeight: Integer;      (** Height of the console. **)
    FWidth: Integer;       (** Width of the console. **)
    FFileAttrs: Integer;   (** This is a list of file attributes that are required in a search. **)
    FTypeAttrs: Integer;   (** This is a list of file type attributes that are required in a search. **)
    FSearchParams: TStringList;  (** This is a list of search parameters that are not part of the command line
                                     switches. **)
    FSummaryLevel: Integer;  (** This is the level of directory of summarisation required. **)
    FOrderFilesBy: TOrderBy;  (** A type to indicate the order of file sorting **)
    FOrderFilesDirection: TOrderDirection; (** A type to indicate the direction of sorting of files. **)
    FRegExSearch: String; (** A string for which should be searched for inside text files. **)
    FRegExSurroundingLines : Integer;
    (** A variable to hold the type of date to display and search for. **)
    FDateType: TDateType;
    (** A file name which hold exclusions which need to be applied to the searches. **)
    FExlFileName: String;
    (** A string list of exclusions to be applied to the searches. **)
    FExclusions: TStringList;
    (** A string to hold the owner search criteria. **)
    FOwnerSearch: String;
    (** An enumerate to determine if the search is positive or negated **)
    FOwnerSearchOps: TOwnerSearch;
    (** An enumerate to determine the position of the search **)
    FOwnerSearchPos: TOwnerSearchPos;
    (** A handle to the standard console output. **)
    FStd: THandle;
    (** A handle to the error console output. **)
    FErr: THandle;
    (** A string to store the loading and saving settings filename in. **)
    FSizeFormat: TSizeFormat;
    FRootKey: String;
    FParams: TStringList;
    FLevel: Integer;
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
    FRegExLineNumbersColour: TColor;
    FRegExLineOutputColour: TColor;
    FRegExFindOutputFGColour: TColor;
    FRegExFindOutputBGColour: TColor;
    FSummaryOutputColour: TColor;
    FExceptionColour: TColor;
    FZipFileColour: TColor;
    FErrorLog : TStringList;
    FUNCPath : Boolean;
    Procedure OutputDateTimeAndSize(FilesCollection: TFiles; Var strOutput: String;
      i: Integer);
    Procedure OutputFileAttributes(i: Integer; FilesCollection: TFiles;
      Var strOutput: String);
    Procedure OutputFileOwner(FilesCollection: TFiles; Var strOutput: String; i: Integer);
    Procedure OutputRegExInformation(strPath : String; boolRegEx: Boolean; FileInfo: TFile);
    Procedure OutputDirectoryOrZIPFile(Var boolDirPrinted: Boolean; strPath: String);
  Strict Protected
    Procedure GetConsoleInformation;
    Procedure OutputCurrentSearchPath(strPath: String);
    Procedure PrintTitle;
    Procedure PrintHeader(strPath, strPattern: String);
    Procedure PrintFooter(strPath: String);
    Procedure PrintHelp;
    Procedure DisplayCriteria;
    Procedure GetCommandLineSwitches;
    Function  LookupAccountBySID(SID: PSID): String;
    Function  OutputOwner(strFileName: String): String;
    Function  CheckFiles(recSearch: TSearchRec; Var iDirFiles: Integer;
      strPath, strOwner: String; FilesCollection: TFiles): Int64;
    Function  CheckZipFiles(ZipArchive: TZipForge; ZFAI: TZFArchiveItem;
      Var iDirFiles: Integer; strPath, strOwner: String; FilesCollection: TFiles): Int64;
    Procedure WorkaroundLargeFiles(Var iSize: Int64; recSearch: TSearchRec);
    Procedure OutputFilesToConsole(strPath: String; boolDirPrinted: Boolean;
      FilesCollection: TFiles);
    Function  RecurseDirectories(strPath: String; Var iLevel: Integer;
      slPatterns: TStringList): Int64;
    Function  SearchForPatterns(slPatterns: TStringList; iDirFiles: Integer;
      strPath: String; FilesCollection: TFiles): Int64;
    Function  SearchForPatternsInZip(strFileName: String; slPatterns: TStringList;
      iDirFiles: Integer; strPath: String; FilesCollection: TFiles): Int64;
    Function  SearchDirectory(strPath: String; slPatterns: TStringList;
      Var iLevel: Integer): Int64;
    Function  SearchZip(strFileName: String; slPatterns: TStringList;
      Var iLevel: Integer): Int64;
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure FilesExceptionHandler(strException: String);
    Procedure ExceptionProc(strMsg: String);
    Function  FormatSize(iSize: Int64): String;
    Procedure AddErrorToLog(strErrMsg, strFileName : String);
    Procedure PrintErrorLog;
    Procedure ProcessZipFileFailure(Sender : TObject; FileName : String;
      Operation : TZFProcessOperation; NativeError : Integer; ErrorCode : Integer;
      ErrorMessage : String; var Action : TZFAction);
    Procedure ZipDiskFull(Sender : TObject; VolumeNumber : Integer;
      VolumeFileName : String; var Cancel : Boolean);
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
    Property ExceptionColour: TColor Read FExceptionColour;
    (**
      This property returns the error handle for console output.
      @precon  None.
      @postcon Returns the error handle for console output.
      @return  a THandle
    **)
    Property ErrorHnd : THandle Read FErr;
    (**
      This property returns the standard handle for console output.
      @precon  None.
      @postcon Returns the standard handle for console output.
      @return  a THandle
    **)
    Property StdHnd : THandle Read FStd;
  End;

  (** A type for a pointer to a PSID structure **)
  PPSID = ^PSID;

  (** This is a helper class for the TStrings class. **)
  TStringsHelper = Class Helper For TStrings
    Procedure SafeLoadFromFile(strFileName : String);
  End;

Implementation

Uses
  Contnrs,
  ACCCTRL,
  ActiveX,
  IniFiles,
  Math,
  System.RegularExpressions;

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
  SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PPSID;
  ppDacl, ppSacl: PACL; Var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD; Stdcall;
External 'advapi32.dll' Name 'GetNamedSecurityInfoA';

Resourcestring
  (** An exception message for the exclusions file not found. **)
  strExclusionsNotFound = 'The filename "%s" was not found to process exclusions ' +
    'in the searches.';
  (** A resource string to note an exception searching files. **)
  strFilesException = 'Exception search files: %s';
  (** This is a constant to represent a parameter string error. **)
  strErrorInPathSearchParamString = 'There was an error in the path=search ' +
    'param pairing. (%s)';
  (** A resource string to define the for of the date outputs. **)
  strDateFormat = 'ddd dd/mmm/yyyy hh:mm:ss';
  (** A resource string to define the for of the date outputs. **)
  strDateFormatCSV = 'dd/mmm/yyyy hh:mm:ss';
  (** An exception message to prompt for a criteria **)
  strNeedToSpecifyCriteria = 'You need to specify at least one search criteria.';
  (** An exception message to notify of an invalid command line parameter. **)
  strInvalidCommandLineSwitch = 'Invalid command line switch "%s" in parameter "%s."';
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
  strDisplayFilesWithSIzes = 'Display files with sizes between %s and %s byt' + 'es.';
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
  strMsg2 = '  Total space %s bytes and %s bytes free.';
  (** A resource string for formatting information. **)
  strSummaryFormat = '%s%s%s';
  (** A resource string for formatting information. **)
  strOwnerFormat = '%-*s  ';
  (** A resource string for formatting information. **)
  strOwnerFormatCSV = '%s,';
  (** A resource string for formatting information. **)
  strAttrFormat = '%s  ';
  (** A resource string for formatting information. **)
  strAttrFormatCSV = '%s,';
  (** A resource string for formatting information. **)
  strDirFormat = '  %s  %15s  ';
  (** A resource string for formatting information. **)
  strFileFormat = '  %s  %s  ';
  (** A resource string for formatting information. **)
  strFileFormatCSV = '%s,%d,';
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
  strSearchLabel = 'Searching: (D:%1.0n,F:%1.0n) ';
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
  strHelp0060 = 'searchparam# = {drive:\path\}filter1{;filter2{;filter3{;.' + '..}}}';
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
  strHelp0140 = '/T[RASHFDV]';
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
  strHelp0188 =
    'Sizes can be postfixed with k, m, g or t for kilo, mega, giga and terabytes';
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
  (** A resource string for the console help information **)
  strHelp0292 = '/V';
  (** A resource string for the console help information **)
  strHelp0293 = 'Output the found information in CSV format.';

(**

  This routine get and stores the current number of lines and columns
  in the console window.

  @precon  None.
  @postcon Gets the width and height of the console.

**)
Procedure TSearch.GetConsoleInformation;

Var
  ConsoleInfo: TConsoleScreenBufferInfo;

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
Procedure TSearch.OutputCurrentSearchPath(strPath: String);

Var
  i, j: Integer;
  iLength: Integer;

Begin
  If (clsQuiet In CommandLineSwitches) Or (clsOutputAsCSV In CommandLineSwitches) Then
    Exit;
  iLength := Length(Format(strSearchLabel, [Int(FDirectories), Int(FFiles)]));
  While Length(strPath) > (FWidth - iLength) Do
    Begin
      If Pos('...', strPath) = 0 Then
        Begin
          i := PosOfNthChar(strPath, '\', 1 + Integer(FUNCPath) * 2) + 1;
          j := PosOfNthChar(strPath, '\', 2 + Integer(FUNCPath) * 2);
          If (i > 0) And (j > 0) Then
            strPath := StringReplace(strPath, Copy(strPath, i, j - i), '...', [])
          Else
            strPath := Copy(strPath, Length(strPath) - (FWidth - iLength),
              FWidth - iLength);
        End
      Else
        Begin
          i := PosOfNthChar(strPath, '\', 2 + Integer(FUNCPath) * 2);
          j := PosOfNthChar(strPath, '\', 3 + Integer(FUNCPath) * 2);
          If (i > 0) And (j > 0) Then
            strPath := StringReplace(strPath, Copy(strPath, i, j - i), '', [])
          Else
            strPath := Copy(strPath, Length(strPath) - (FWidth - iLength),
              FWidth - iLength);
        End;
    End;
  strPath := Format(strOutputCurrPathFormat,
    [Format(strSearchLabel, [Int(FDirectories), Int(FFiles)]), FWidth - iLength,
    strPath]);
  If CheckConsoleMode(FStd) Then
    OutputToConsole(FStd, strPath, FSearchPathColour, clNone, False);
End;

(**

  Prints a Title for the application on the screen.

  @precon  None.
  @postcon Prints the title for the application on the screen.

**)
Procedure TSearch.PrintTitle;

Var
  strOutput: String;

Begin
  If Not (clsOutputAsCSV In CommandLineSwitches) Then
    Begin
      OutputToConsoleLn(FStd, GetConsoleTitle, FTitleColour);
      OutputToConsoleLn(FStd);
      If clsDisplayCriteria In CommandLineSwitches Then
        DisplayCriteria;
    End
  Else
    Begin
      strOutput := 'Date and Time,Size';
      If clsShowAttribs In CommandLineSwitches Then
        strOutput := strOutput + ',Attrbutes';
      If clsOwner In CommandLineSwitches Then
        strOutput := strOutput + ',Owner';
      strOutput := strOutput + ',Filename';
      OutputToConsoleLn(FStd, strOutput, FTitleColour);
    End;
End;

(**

  This method captures a zip file process failure and logs an error message.

  @note    Should never get called.

  @precon  None.
  @postcon Captures a zip file process failure and logs an error message.

  @param   Sender       as a TObject
  @param   FileName     as a String
  @param   Operation    as a TZFProcessOperation
  @param   NativeError  as an Integer
  @param   ErrorCode    as an Integer
  @param   ErrorMessage as a String
  @param   Action       as a TZFAction as a reference

**)
Procedure TSearch.ProcessZipFileFailure(Sender: TObject; FileName: String;
  Operation: TZFProcessOperation; NativeError, ErrorCode: Integer;
  ErrorMessage: String; Var Action: TZFAction);

Begin
  AddErrorToLog(ErrorMessage, (Sender As TZipForge).FileName + '\' + FileName);
End;

(**

  Prints the path and search pattern before each search

  @precon  None.
  @postcon Prints the path and search pattern before each search

  @param   strPath    as a String
  @param   strPattern as a String

**)
Procedure TSearch.PrintHeader(strPath, strPattern: String);

Begin
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
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
Procedure TSearch.PrintFooter(strPath: String);

Var
  iFreeBytesAvailableToCaller, iTotalNumberOfBytes, iTotalNumberOfFreeBytes: Int64;

Begin
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    Begin
      GetDiskFreeSpaceEx(PChar(strPath), iFreeBytesAvailableToCaller,
        iTotalNumberOfBytes, @iTotalNumberOfFreeBytes);
      If CheckConsoleMode(FStd) Then
        OutputToConsole(FStd, StringOfChar(#32, FWidth - 1), clNone, clNone, False);
      OutputToConsoleLn(FStd, Format(strMsg1, [Trim(FormatSize(FFileSize)), FFiles + 0.1,
          FDirectories + 0.1]), FFooterColour);
      OutputToConsoleLn(FStd, Format(strMsg2, [Trim(FormatSize(iTotalNumberOfBytes)),
          Trim(FormatSize(iTotalNumberOfFreeBytes))]), FFooterColour);
    End;
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
  Function Indent(strText: String; iIndent, iWidth: Integer): String;

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
  OutputToConsole(FStd, Indent(strHelp0292, 2, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStd, strHelp0293, FHelpTextColour);
  OutputToConsoleLn(FStd);
  OutputToConsoleLn(FStd, strHelp0280, FHelpFootNoteColour);
  OutputToConsoleLn(FStd);
End;

(**

  This is the destructor method for the TSearch class.

  @precon  None.
  @postcon Frees the SearchParams and Exclusions string lists.

**)
Destructor TSearch.Destroy;

Begin
  SaveSettings;
  FErrorLog.Free;
  FExclusions.Free;
  FSearchParams.Free;
  FParams.Free;
  Inherited Destroy;
End;

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
  Function DisplayText(ASwitch: TCommandLineSwitch; strSwitch, strText: String;
    boolCarriageReturn: Boolean = True): Boolean;

  Begin
    Result := ASwitch In CommandLineSwitches;
    If Result Then
      Begin
        OutputToConsole(FStd, Format(strDisplayTextSwitchFormat, [strSwitch]),
          FHelpSwitchColour);
        OutputToConsole(FStd, Format(strDisplatTextInfoFormat, [strText]),
          FHelpTextColour);
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
  Function ReduceSize(iSize: Int64): String;

  Const
    strSizes = 'KMGT';

  Var
    iReductions: Integer;
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
          OutputToConsoleLn(FStd, strArchive, FHelpInfoColour);
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
  If Not(clsRegExSearch In CommandLineSwitches) Then
    DisplayText(clsSummaryLevel, IntToStr(FSummaryLevel),
      Format(strSummarisingOutput, [FSummaryLevel]));
  DisplayText(clsSupressZeros, '0', strSupressingSummary);
  DisplayText(clsDateRange, 'd', Format(strDisplayFilesWithDates,
      [FormatDateTime(strDateFormat, FLDate), FormatDateTime(strDateFormat, FUDate)]));
  DisplayText(clsSizeRange, 'd', Format(strDisplayFilesWithSIzes, [ReduceSize(FLSize),
      ReduceSize(FUSize)]));
  DisplayAttributes;
  DisplayText(clsQuiet, 'q', strQuietMode);
  If DisplayText(clsOwner, 'w', strDisplayTheOwners, False) Then
    Begin
      If FOwnerSearchPos In [ospExact .. ospEnd] Then
        Begin
          OutputToConsole(FStd, strOwnerSearchingFor, FHelpTextColour);
          If FOwnerSearchOps In [osNotEquals] Then
            OutputToConsole(FStd, strOwnerNOT, FHelpSwitchColour);
          OutputToConsole(FStd, Format(strOwnerMatching, [FOwnerSearch]),
            FHelpTextColour);
          Case FOwnerSearchPos Of
            ospExact:
              OutputToConsole(FStd, strOwnerExact, FHelpTextColour);
            ospStart:
              OutputToConsole(FStd, strOwnerAtTheStart, FHelpTextColour);
            ospMiddle:
              OutputToConsole(FStd, strOwnerInTheMiddle, FHelpTextColour);
            ospEnd:
              OutputToConsole(FStd, strOwnerAtTheEnd, FHelpTextColour);
          End;
        End;
      OutputToConsoleLn(FStd, '.', FHelpTextColour);
    End;
  If clsOrderBy In CommandLineSwitches Then
    Begin
      Case FOrderFilesBy Of
        obName:
          DisplayText(clsOrderBy, 'o', strOrderingFilesByName, False);
        obSize:
          DisplayText(clsOrderBy, 'o', strOrderingFilesBySize, False);
        obDate:
          DisplayText(clsOrderBy, 'o', strOrderingFilesByDate, False);
        obOwner:
          DisplayText(clsOrderBy, 'o', strOrderingFilesByOwner, False);
        obAttribute:
          DisplayText(clsOrderBy, 'o', strOrderingFilesByTheirAttrs, False);
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
    dtCreation:
      DisplayText(clsDateType, 'e', strDisplayingFileCreationDates);
    dtLastAccess:
      DisplayText(clsDateType, 'a', strDisplayingFileLastAccessDates);
    dtLastWrite:
      DisplayText(clsDateType, 'w', strDisplayingFileLastWriteDates);
  End;
  DisplayText(clsRegExSearch, 'i', Format(strSearchForText, [FRegExSearch]));
  DisplayText(clsExclusions, 'x', Format(strApplyingExclusions, [FExlFileName]));
  DisplayText(clsSearchZip, 'p', strSearchInZips);
  Case FSizeFormat Of
    sfKilobytes:
      DisplayText(clsSizeOutput, 'f', strSizeFormatKiloBytes);
    sfMegaBytes:
      DisplayText(clsSizeOutput, 'f', strSizeFormatMegaBytes);
    sfGigaBytes:
      DisplayText(clsSizeOutput, 'f', strSizeFormatGigaBytes);
    sfTeraBytes:
      DisplayText(clsSizeOutput, 'f', strSizeFormatTeraBytes);
  End;
  OutputToConsoleLn(FStd);
End;

(**

  This method outputs any error messages at the end of the search process.

  @precon  None.
  @postcon Outputs any error messages at the end of the search process.

**)
Procedure TSearch.PrintErrorLog;

Var
  i : Integer;
  
Begin
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    If FErrorLog.Count > 0 Then
      Begin
        OutputToConsoleLn(FStd);
        OutputToConsoleLn(FStd, 'The following errors were encountered during the search:',
          FExceptionColour);
        For i := 0 To FErrorLog.Count - 1 Do
          OutputToConsoleLn(FStd, #32#32 + FErrorLog[i]);
      End;
End;

(**

  This is a exception handler for the BuildRootKey method.

  @precon  None.
  @postcon Raise the exception for termination.

  @param   strMsg as a String

**)
Procedure TSearch.ExceptionProc(strMsg: String);

Begin
  Raise Exception.Create(strMsg);
End;

(**

  This method outputs exception from the TFiles class to the console.

  @precon  None.
  @postcon Outputs exception from the TFiles class to the console.

  @param   strException as a String

**)
Procedure TSearch.FilesExceptionHandler(strException: String);
Begin
  OutputToConsoleLn(FStd, Format(strFilesException, [strException]), FExceptionColour);
End;

(**

  This method returns the file size formatted to that defined on the command
  line.

  @precon  None.
  @postcon Returns the file size formatted to that defined on the command
           line.

  @param   iSize as an Int64
  @return  a String

**)
Function TSearch.FormatSize(iSize: Int64): String;

Var
  dblSize: Double;

Begin
  dblSize := iSize;
  Case FSizeFormat Of
    sfKilobytes:
      Result := Format('%13.0nK', [RoundTo(dblSize / 1024.0, 0)]);
    sfMegaBytes:
      Result := Format('%10.0nM', [RoundTo(dblSize / (1024.0 * 1024.0), 0)]);
    sfGigaBytes:
      Result := Format('%7.0nG', [RoundTo(dblSize / (1024.0 * 1024.0 * 1024.0), 0)]);
    sfTeraBytes:
      Result := Format('%4.0nT', [RoundTo(dblSize / (1024.0 * 1024.0 * 1024.0 * 1024.0),
          0)]);
    Else
      Result := Format('%15.0n', [dblSize]);
    End;
End;

(**

  This gets all the command lines switches and sets numerous flags.

  @precon  None.
  @postcon Gets all the command lines switches and sets numerous flags.

**)
Procedure TSearch.GetCommandLineSwitches;

Var
  iSwitch, iIndex: Integer;

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
                '?':      Include(CommandLineSwitches, clsShowHelp);
                'h', 'H': Include(CommandLineSwitches, clsShowHelp);
                's', 'S': Include(CommandLineSwitches, clsSubDirectories);
                '!':      Include(CommandLineSwitches, clsDebug);
                'a', 'A': Include(CommandLineSwitches, clsShowAttribs);
                '1'..'9': GetSummaryLevel(FParams, iSwitch, iIndex, FSummaryLevel);
                '0':      Include(CommandLineSwitches, clsSupressZeros);
                'd', 'D': GetDateRange(FParams, iSwitch, iIndex, FLDate, FUDate);
                'z', 'Z': GetSizeRange(FParams, iSwitch, iIndex, FLSize, FUSize);
                't', 'T': GetAttributes(FParams, iSwitch, iIndex, FFileAttrs, FTypeAttrs);
                'q', 'Q': Include(CommandLineSwitches, clsQuiet);
                'w', 'W': GetOwnerSwitch(FParams, iSwitch, iIndex, FOwnerSearchOps,
                    FOwnerSearchPos, FOwnerSearch);
                'o', 'O': GetOrderBy(FParams, iSwitch, iIndex, FOrderFilesDirection, FOrderFilesBy);
                'i', 'I': GetSearchInInfo(FParams, iSwitch, iIndex, FRegExSearch,
                  FRegExSurroundingLines);
                'e', 'E': GetDateType(FParams, iSwitch, iIndex, FDateType);
                'c', 'C': Include(CommandLineSwitches, clsDisplayCriteria);
                'x', 'X': GetExclusions(FParams, iSwitch, iIndex, FExlFileName);
                'p', 'P': Include(CommandLineSwitches, clsSearchZip);
                'f', 'F': GetSizeFormat(FParams, iSwitch, iIndex, FSizeFormat);
                'v', 'V': Include(CommandLineSwitches, clsOutputAsCSV);
                Else
                  Raise ESearchException.CreateFmt(strInvalidCommandLineSwitch,
                    [FParams[iSwitch][iIndex], FParams[iSwitch]]);
                End;
              Inc(iIndex);
            End;
        End
      Else
        FSearchParams.Add(Format('%s=%s', [ExtractFilePath(FParams[iSwitch]),
            ExtractFileName(FParams[iSwitch])]));
      Inc(iSwitch); // Get the next switch
    End;
  If FParams.Count = 0 Then
    Include(CommandLineSwitches, clsShowHelp);
  If (FSearchParams.Count = 0) And Not(clsShowHelp In CommandLineSwitches) Then
    If FParams.Count = 0 Then
      Include(CommandLineSwitches, clsShowHelp)
    Else
      Raise ESearchException.Create(strNeedToSpecifyCriteria);
End;

(**

  This method loads the colour settings from the INI file.

  @precon  None.
  @postcon Loads the colour settings from the INI file.

**)
Procedure TSearch.LoadSettings;
Begin
  With TIniFile.Create(FRootKey) Do
    Try
      FSearchPathColour := StringToColor(ReadString('Colours', 'SearchPath', 'clMaroon'));
      FTitleColour := StringToColor(ReadString('Colours', 'Title', 'clWhite'));
      FHeaderColour := StringToColor(ReadString('Colours', 'Header', 'clYellow'));
      FFooterColour := StringToColor(ReadString('Colours', 'Footer', 'clWhite'));
      FHelpHeaderColour := StringToColor(ReadString('Colours', 'HelpHeader', 'clWhite'));
      FHelpInfoColour := StringToColor(ReadString('Colours', 'HelpInfo', 'clLime'));
      FHelpTextColour := StringToColor(ReadString('Colours', 'HelpText', 'clNone'));
      FHelpSwitchColour := StringToColor(ReadString('Colours', 'HelpSwitch', 'clRed'));
      FHelpFootNoteColour := StringToColor(ReadString('Colours', 'HelpFoorNote', 'clYellow'));
      FFoundSearchPathColour := StringToColor(ReadString('Colours', 'FoundSearchPath', 'clWhite'));
      FFileInfoColour := StringToColor(ReadString('Colours', 'FileInfo', 'clNone'));
      FRegExLineNumbersColour := StringToColor(ReadString('Colours', 'RegExLineNumbers', 'clRed'));
      FRegExLineOutputColour := StringToColor(ReadString('Colours', 'RegExLineOutput', 'clLime'));
      FRegExFindOutputFGColour := StringToColor(ReadString('Colours', 'RegExFindOutputFG', 'clRed'));
      FRegExFindOutputBGColour := StringToColor(ReadString('Colours', 'RegExFindOutputBG', 'clYellow'));
      FSummaryOutputColour := StringToColor(ReadString('Colours', 'SummaryOutput', 'clNone'));
      FExceptionColour := StringToColor(ReadString('Colours', 'Exception', 'clRed'));
      FZipFileColour := StringToColor(ReadString('Colours', 'ZipFile', 'clFuchsia'));
    Finally
      Free;
    End;
End;

(**

  This method returns the owner of the files from a SID.

  @precon  None.
  @postcon Returns the owner of the files from a SID.

  @param   SID as a PSID
  @return  a String

**)
Function TSearch.LookupAccountBySID(SID: PSID): String;

Var
  strName, strRefDomain: String;
  iNameSize, iRefDomainSize: DWORD;
  Use: SID_NAME_USE;

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
Function TSearch.OutputOwner(strFileName: String): String;

Var
  SD: PSecurityDescriptor;
  Owner: PSID;

Begin
  Result := '';
  If GetNamedSecurityInfo(PAnsiChar(AnsiString(strFileName)), SE_FILE_OBJECT,
    OWNER_SECURITY_INFORMATION, @Owner, Nil, Nil, Nil, Pointer(SD)) = ERROR_SUCCESS Then
    Begin
      Result := LookupAccountBySID(Owner);
      LocalFree(Cardinal(SD));
    End;
End;

(**

  This method adds an error messages to the error message log.

  @precon  None.
  @postcon Adds an error messages to the error message log.

  @param   strErrMsg   as a String
  @param   strFileName as a String

**)
Procedure TSearch.AddErrorToLog(strErrMsg, strFileName: String);

Begin
  FErrorLog.Add(Format('%s (%s)', [strErrMsg, strFileName]))
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
Function TSearch.CheckFiles(recSearch: TSearchRec; Var iDirFiles: Integer;
  strPath, strOwner: String; FilesCollection: TFiles): Int64;

  (**

    This function returns the text of the text file IF RegEx searching is
    required.

    @precon  None.
    @postcon Returns the text of the text file IF RegEx searching is
             required else returns a null string

    @return  a String

  **)
  Function GetFileText: String;

  Var
    sl: TStringList;

  Begin
    Result := '';
    If clsRegExSearch In CommandLineSwitches Then
      Begin
        sl := TStringList.Create;
        Try
          sl.SafeLoadFromFile(strPath + recSearch.Name);
          Result := sl.Text;
        Finally
          sl.Free;
        End;
      End;
  End;

Var
  dtDate, dtDateLocal: TFileTime;
  iTime: Integer;
  boolAdded: Boolean;
  dtFileDate: TDateTime;

Begin
  Result := 0;
  If Not(clsSummaryLevel In CommandLineSwitches) Then
    Begin
      Inc(iDirFiles);
      Case FDateType Of
        dtCreation:
          dtDate := recSearch.FindData.ftCreationTime;
        dtLastAccess:
          dtDate := recSearch.FindData.ftLastAccessTime;
        dtLastWrite:
          dtDate := recSearch.FindData.ftLastWriteTime;
      End;
      FileTimeToLocalFileTime(dtDate, dtDateLocal);
      iTime := 0;
      If Not FileTimeToDosDateTime(dtDateLocal, LongRec(iTime).Hi,
        LongRec(iTime).Lo) Then
        iTime := 0;
      dtFileDate := SafeFileDateToDateTime(iTime, strPath + recSearch.Name,
        AddErrorToLog);
      boolAdded := FilesCollection.Add(dtFileDate, recSearch.Size,
        OutputAttributes(recSearch.Attr), strOwner, recSearch.Name, GetFileText, recSearch.Attr);
    End
  Else
    boolAdded := True;
  If boolAdded Then
    Begin
      Inc(FFiles);
      Inc(FFileSize, recSearch.Size);
      Inc(Result, recSearch.Size);
    End;
End;

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
Function TSearch.CheckZipFiles(ZipArchive: TZipForge; ZFAI: TZFArchiveItem;
  Var iDirFiles: Integer; strPath, strOwner: String; FilesCollection: TFiles): Int64;

  (**

    This function returns the text of the zip file IF RegEx searching is
    required.

    @precon  None.
    @postcon Returns the text of the zip file IF RegEx searching is
             required else returns a null string

    @return  a String

  **)
  Function GetZipFileText: String;

  Begin
    Result := '';
    If clsRegExSearch In CommandLineSwitches Then
      ZipArchive.ExtractToString(ZFAI.StoredPath + ZFAI.FileName, Result);
  End;

Var
  iTime: Integer;
  boolAdded: Boolean;

Begin
  Result := 0;
  If Not(clsSummaryLevel In CommandLineSwitches) Then
    Begin
      Inc(iDirFiles);
      LongRec(iTime).Lo := ZFAI.LastModFileTime;
      LongRec(iTime).Hi := ZFAI.LastModFileDate;
      boolAdded := FilesCollection.Add(FileDateToDateTime(iTime), ZFAI.UncompressedSize,
        OutputAttributes(ZFAI.ExternalFileAttributes), strOwner,
        ZFAI.StoredPath + ZFAI.FileName, GetZipFileText, ZFAI.ExternalFileAttributes);
    End
  Else
    boolAdded := True;
  If boolAdded Then
    Begin
      Inc(FFiles);
      Inc(FFileSize, ZFAI.UncompressedSize);
      Inc(Result, ZFAI.UncompressedSize);
    End;
End;

(**

  This is the constructor method for the TSearch class.

  @precon  None.
  @postcon Initialises the SearchParams and Exclusions stringlists.

**)
Constructor TSearch.Create;

Begin
  FExceptionColour := clRed;
  FSearchParams := TStringList.Create;
  FExclusions := TStringList.Create;
  FParams := TStringList.Create;
  FErrorLog := TStringList.Create;
  CoInitialize(Nil);
  FLevel := -1;
  SetRoundMode(rmUp);
End;

(**

  This method provides a workaround for identifying the size of large files.

  @precon  None.
  @postcon Provides a workaround for identifying the size of large files.

  @param   iSize     as an Int64 as a reference
  @param   recSearch as a TSearchRec

**)
Procedure TSearch.WorkaroundLargeFiles(Var iSize: Int64; recSearch: TSearchRec);

Begin
  With recSearch.FindData Do
    // Workaround for files larger than 2147483647
    Begin
      iSize := Int64(nFileSizeHigh) * Int64(MAXDWORD);
      iSize := iSize + nFileSizeLow;
    End;
End;

(**

  This method logs an errror message if the zip code reports a disk full error.

  @precon  None.
  @postcon Logs an errror message if the zip code reports a disk full error.

  @param   Sender         as a TObject
  @param   VolumeNumber   as an Integer
  @param   VolumeFileName as a String
  @param   Cancel         as a Boolean as a reference

**)
Procedure TSearch.ZipDiskFull(Sender: TObject; VolumeNumber: Integer;
  VolumeFileName: String; Var Cancel: Boolean);

Begin
  AddErrorToLog(Format('Disk volume %d full [%s]',
    [VolumeNumber, VolumeFileName]), (Sender As TZipForge).FileName);
  Cancel := True;
End;

(**

  This method outputs the files that have met the criteria to the console.

  @precon  None.
  @postcon Outputs the files that have met the criteria to the console.

  @param   strPath         as a string
  @param   boolDirPrinted  as a Boolean
  @param   FilesCollection as a TFiles

**)
Procedure TSearch.OutputFilesToConsole(strPath: String; boolDirPrinted: Boolean;
  FilesCollection: TFiles);

Var
  iFile: Integer;
  strOutput: String;
  boolRegEx: Boolean;

Begin
  For iFile := 0 To FilesCollection.Count - 1 Do
    Begin
      OutputDateTimeAndSize(FilesCollection, strOutput, iFile);
      OutputFileAttributes(iFile, FilesCollection, strOutput);
      OutputFileOwner(FilesCollection, strOutput, iFile);
      If Not(clsOutputAsCSV In CommandLineSwitches) Then
        OutputDirectoryOrZIPFile(boolDirPrinted, strPath);
      boolRegEx := (clsRegExSearch In CommandLineSwitches) And
        (FilesCollection.FileInfo[iFile].RegExMatches.Count > 0);
      If Not(clsRegExSearch In CommandLineSwitches) Or boolRegEx Then
        If Not(clsOutputAsCSV In CommandLineSwitches) Then
          OutputToConsoleLn(FStd, strOutput + FilesCollection.FileInfo[iFile].FileName,
            FFileInfoColour)
        Else
          OutputToConsoleLn(FStd,
            strOutput + strPath + FilesCollection.FileInfo[iFile].FileName, FFileInfoColour);
      OutputRegExInformation(strPath, boolRegEx, FilesCollection.FileInfo[iFile]);
    End;
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    If FilesCollection.Count > 0 Then
      OutputToConsoleLn(FStd);
End;

(**

  This method recurses the directories searching for additional files.

  @precon  None.
  @postcon Recurses the directories searching for additional files.

  @param   strPath    as a string
  @param   iLevel     as an Integer as a reference
  @param   slPatterns as a TStringList
  @return  an Int64

**)
Function TSearch.RecurseDirectories(strPath: String; Var iLevel: Integer;
  slPatterns: TStringList): Int64;

Var
  recSearch: TSearchRec;
  iResult: Integer;

Begin
  Result := 0;
  iResult := FindFirst(strPath + '*.*', faAnyFile Or faHidden Or faSysFile, recSearch);
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
End;

(**

  This method starts the processing of the files and directories inline with
  the criteria that are specified on the command line.

  @precon  None.
  @postcon Outputs to the console the the information on found files.

**)
Procedure TSearch.Run;

Var
  iPos: Integer;
  strPath: String;
  strSearch: String;
  FPatterns: TStringList;
  i: Integer;
  iPattern: Integer;

Begin
  GetConsoleInformation;
  FRootKey := BuildRootKey(FParams, FilesExceptionHandler);
  LoadSettings;
  GetCommandLineSwitches;
  PrintTitle;
  If Not(clsExclusions In CommandLineSwitches) Or
    ((clsExclusions In CommandLineSwitches) And FileExists(FExlFileName)) Then
    Begin
      If clsExclusions In CommandLineSwitches Then
        FExclusions.LoadFromFile(FExlFileName);
      FExclusions.Text := LowerCase(FExclusions.Text);
      If Not(clsShowHelp In CommandLineSwitches) Then
        Begin
          For i := 0 To FSearchParams.Count - 1 Do
            Begin
              iPos := Pos('=', FSearchParams[i]);
              If iPos = 0 Then
                Raise ESearchException.CreateFmt(strErrorInPathSearchParamString,
                  [FSearchParams[i]]);
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
                FUNCPath := Copy(strPath, 1, 2) = '\\';
                SearchDirectory(strPath, FPatterns, FLevel);
              Finally
                FPatterns.Free;
              End;
              If clsSummaryLevel In CommandLineSwitches Then
                OutputToConsoleLn(FStd);
              PrintFooter(strPath);
              PrintErrorLog;
            End;
        End
      Else
        PrintHelp;
    End
  Else
    Raise ESearchException.CreateFmt(strExclusionsNotFound, [FExlFileName]);
End;

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
  strPath: String; FilesCollection: TFiles): Int64;

Var
  iPattern: Integer;
  strOwner: String;
  recSearch: TSearchRec;
  iResult: Integer;
  boolFound: Boolean;
  ST: TSystemTime;
  dtDate: TDateTime;

Begin
  Result := 0;
  For iPattern := 0 To slPatterns.Count - 1 Do
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
                  dtDate := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) + EncodeTime
                    (ST.wHour, ST.wMinute, ST.wSecond, ST.wMilliseconds);
                  CheckDateRange(DateTimeToFileDate(dtDate), FLDate, FUDate, boolFound,
                    strPath + recSearch.Name, AddErrorToLog);
                End;
              dtLastAccess:
                Begin
                  FileTimeToSystemTime(recSearch.FindData.ftLastAccessTime, ST);
                  dtDate := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) + EncodeTime
                    (ST.wHour, ST.wMinute, ST.wSecond, ST.wMilliseconds);
                  CheckDateRange(DateTimeToFileDate(dtDate), FLDate, FUDate, boolFound,
                    strPath + recSearch.Name, AddErrorToLog);
                End
              Else
                CheckDateRange(recSearch.Time, FLDate, FUDate, boolFound,
                  strPath + recSearch.Name, AddErrorToLog);
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
End;

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
Function TSearch.SearchForPatternsInZip(strFileName: String; slPatterns: TStringList;
  iDirFiles: Integer; strPath: String; FilesCollection: TFiles): Int64;

Var
  Z: TZipForge;
  iPattern: Integer;
  boolResult: Boolean;
  boolFound: Boolean;
  ZFAI: TZFArchiveItem;
  iSize: Int64;
  iDateTime: Integer;
  strOwner: String;

Begin
  Result := 0;
  Z := TZipForge.Create(Nil);
  Try
    Z.FileName := strFileName;
    Z.OnProcessFileFailure := ProcessZipFileFailure;
    Z.OnDiskFull := ZipDiskFull;
    Try
      If Z.IsValidArchiveFile Then
        Begin
          Z.OpenArchive;
          Try
            For iPattern := 0 To slPatterns.Count - 1 Do
              Begin
                boolResult := Z.FindFirst(slPatterns[iPattern], ZFAI);
                While boolResult And Not ZFAI.Encrypted Do
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
                    CheckDateRange(iDateTime, FLDate, FUDate, boolFound,
                      strFileName + '\' + ZFAI.StoredPath + ZFAI.FileName, AddErrorToLog);
                    CheckExclusions(strPath, ZFAI.StoredPath + ZFAI.FileName, boolFound,
                      FExclusions);
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
        End;
    Except
      On E : Exception Do
        AddErrorToLog(E.Message, strFileName);
    End;
  Finally
    Z.Free;
  End;
End;

(**

  This method searches the named zip file for files.

  @precon  None.
  @postcon Method searches the named zip file for files.

  @param   strFileName as a String
  @param   slPatterns  as a TStringList
  @param   iLevel      as an Integer as a reference
  @return  an Int64

**)
Function TSearch.SearchZip(strFileName: String; slPatterns: TStringList;
  Var iLevel: Integer): Int64;

Var
  boolDirPrinted: Boolean;
  iDirFiles: Integer;
  FilesCollection: TFiles;
  PathCollections: TObjectList;
  iPath: Integer;
  PathCollection: TFiles;
  iFile: Integer;
  strPath: String;
  Files: TFiles;

Begin
  OutputCurrentSearchPath(strFileName);
  Inc(iLevel);
  Result := 0;
  iDirFiles := 0;
  Inc(FDirectories);
  boolDirPrinted := False;
  (* Find Files in Zip *)
  FilesCollection := TFiles.Create(FilesExceptionHandler, FRegExSearch);
  Try
    Inc(Result, SearchForPatternsInZip(strFileName, slPatterns, iDirFiles, strFileName,
        FilesCollection));
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
              Files := TFiles.Create(FilesExceptionHandler, FRegExSearch);
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
          OutputFilesToConsole(strFileName + '\' + PathCollection.Path, boolDirPrinted,
            PathCollection);
        End;
    Finally
      PathCollections.Free;
    End;
  Finally
    FilesCollection.Free;
  End;
  If Not(clsSummaryLevel In CommandLineSwitches) Then
    If iDirFiles > 0 Then
      OutputToConsoleLn(FStd);
  Dec(iLevel);
End;

(**

  This method saves the colour settings to the INI file.

  @precon  None.
  @postcon Saves the colour settings to the INI file.

**)
Procedure TSearch.SaveSettings;
Begin
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
      WriteString('Colours', 'RegExLineNumbers', ColorToString(FRegExLineNumbersColour));
      WriteString('Colours', 'RegExLineOutput', ColorToString(FRegExLineOutputColour));
      WriteString('Colours', 'RegExFindOutputFG', ColorToString(FRegExFindOutputFGColour));
      WriteString('Colours', 'RegExFindOutputBG', ColorToString(FRegExFindOutputBGColour));
      WriteString('Colours', 'SummaryOutput', ColorToString(FSummaryOutputColour));
      WriteString('Colours', 'Exception', ColorToString(FExceptionColour));
      WriteString('Colours', 'ZipFile', ColorToString(FZipFileColour));
    Finally
      Free;
    End;
End;

(**                                                  999

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
Function TSearch.SearchDirectory(strPath: String; slPatterns: TStringList;
  Var iLevel: Integer): Int64;

Var
  boolDirPrinted: Boolean;
  iDirFiles: Integer;
  strOutput: String;
  FilesCollection: TFiles;

Begin
  OutputCurrentSearchPath(strPath);
  Inc(iLevel);
  Result := 0;
  iDirFiles := 0;
  Inc(FDirectories);
  boolDirPrinted := False;
  (* Find Files *)
  FilesCollection := TFiles.Create(FilesExceptionHandler, FRegExSearch);
  Try
    Inc(Result, SearchForPatterns(slPatterns, iDirFiles, strPath, FilesCollection));
    If FOrderFilesBy <> obNone Then
      FilesCollection.OrderBy(FOrderFilesBy, FOrderFilesDirection);
    OutputFilesToConsole(strPath, boolDirPrinted, FilesCollection);
  Finally
    FilesCollection.Free;
  End;
  If Not(clsSummaryLevel In CommandLineSwitches) Then
    If iDirFiles > 0 Then
      OutputToConsoleLn(FStd);
  (* Search sub directories *)
  If clsSubDirectories In CommandLineSwitches Then
    Inc(Result, RecurseDirectories(strPath, iLevel, slPatterns));
  (* Output directory summary levels *)
  If clsSummaryLevel In CommandLineSwitches Then
    Begin
      If (iLevel > -1) And (iLevel <= FSummaryLevel) Then
        If Not((Result = 0) And (clsSupressZeros In CommandLineSwitches)) Then
          Begin
            strOutput := Format(strSummaryFormat, [FormatSize(Result),
              StringOfChar(#32, 2 + 2 * iLevel), strPath]);
            OutputToConsoleLn(FStd, strOutput + StringOfChar(#32,
                FWidth - 1 - Length(strOutput)), FSummaryOutputColour);
          End;
    End;
  Dec(iLevel);
End;

(**

  This method outputs the file date and time and its size to the passed output
  string.

  @precon  FilesCollection must be a valid instance.
  @postcon Outputs the file date and time and its size to the passed output
           string.

  @param   FilesCollection as a TFiles
  @param   strOutput       as a String as a reference
  @param   i               as an Integer

**)
Procedure TSearch.OutputDateTimeAndSize(FilesCollection: TFiles; Var strOutput: String;
  i: Integer);

Begin
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    Begin
      If Pos('D', FilesCollection.FileInfo[i].Attr) = 0 Then
        strOutput := Format(strFileFormat, [FormatDateTime(strDateFormat,
            FilesCollection.FileInfo[i].Date),
          FormatSize(FilesCollection.FileInfo[i].Size)])
      Else
        strOutput := Format(strDirFormat, [FormatDateTime(strDateFormat,
            FilesCollection.FileInfo[i].Date), '<DIR>']);
    End
  Else
    Begin
      If Pos('D', FilesCollection.FileInfo[i].Attr) = 0 Then
        strOutput := Format(strFileFormatCSV, [FormatDateTime(strDateFormatCSV,
            FilesCollection.FileInfo[i].Date), FilesCollection.FileInfo[i].Size])
      Else
        strOutput := Format(strFileFormatCSV, [FormatDateTime(strDateFormatCSV,
            FilesCollection.FileInfo[i].Date), 0]);
    End;
End;

(**

  This method outputs the files attributes to the passed output string.

  @precon  FileCollection must be a valid instance.
  @postcon Outputs the files attributes to the passed output string.

  @param   i               as an Integer
  @param   FilesCollection as a TFiles
  @param   strOutput       as a String as a reference

**)
Procedure TSearch.OutputFileAttributes(i: Integer; FilesCollection: TFiles;
  Var strOutput: String);

Begin
  If clsShowAttribs In CommandLineSwitches Then
    If Not(clsOutputAsCSV In CommandLineSwitches) Then
      strOutput := strOutput + Format(strAttrFormat, [FilesCollection.FileInfo[i].Attr])
    Else
      strOutput := strOutput + Format(strAttrFormatCSV, [FilesCollection.FileInfo[i].Attr]);
End;

(**

  This method outputs the file owner.

  @precon  FilesCollection must be a valid instance.
  @postcon Outputs the file owner.

  @param   FilesCollection as a TFiles
  @param   strOutput       as a String as a reference
  @param   i               as an Integer

**)
Procedure TSearch.OutputFileOwner(FilesCollection: TFiles; Var strOutput: String;
  i: Integer);

Var
  iOwnerWidth: Integer;

Begin
  If clsOwner In CommandLineSwitches Then
    If Not(clsOutputAsCSV In CommandLineSwitches) Then
      Begin
        iOwnerWidth := FilesCollection.OwnerWidth;
        strOutput := strOutput + Format(strOwnerFormat, [iOwnerWidth,
          FilesCollection.FileInfo[i].Owner]);
      End
    Else
      Begin
        strOutput := strOutput + Format(strOwnerFormatCSV,
          [FilesCollection.FileInfo[i].Owner]);
      End;
End;

(**

  This method outputs the files RegEx information for a file.

  @precon  FilesCollection must be a valid instance.
  @postcon Outputs the files RegEx information for a file.

  @param   strPath   as a String
  @param   boolRegEx as a Boolean
  @param   FileInfo  as a TFile

**)
Procedure TSearch.OutputRegExInformation(strPath : String; boolRegEx: Boolean; FileInfo: TFile);

Var
  iMatch: Integer;
  M: TRegExMatches;
  iGroup: Integer;
  iStart: Integer;
  S : String;
  iLine: Integer;
  slText : TStringList;
  strLine: String;
  strFileName: String;
  REZip: TRegEx;
  MZip : TMatch;
  Z: TZipForge;
  ZFAI: TZFArchiveItem;
  strText: String;

Begin
  If boolRegEx Then
    Begin
      slText := TStringList.Create;
      Try
        REZip := TRegEx.Create('\.zip\\', [roCompiled, roIgnoreCase, roSingleLine]);
        //: @bug Cannot open from a ZIP file.
        strFileName := strPath + FileInfo.FileName;
        MZip := REZip.Match(strPath);
        If Not MZip.Success Then
          slText.SafeLoadFromFile(strFileName)
        Else
          Begin
            Z := TZipForge.Create(Nil);
            Try
              Z.FileName := Copy(strFileName, 1, MZip.Index + MZip.Length - 2);
              If Z.IsValidArchiveFile Then
                Begin
                  Z.OpenArchive;
                  Try
                    strFileName := Copy(strFileName, Succ(MZip.Index + MZip.Length - 1),
                      Length(strFileName));
                    Z.ExtractToString(strFileName, strText);
                    slText.Text := strText;
                  Finally
                    Z.CloseArchive;
                  End;
                End;
            Finally
              Z.Free;
            End;
          End;
        For iMatch := 0 To FileInfo.RegExMatches.Count - 1 Do
          Begin
            M := FileInfo.RegExMatches[iMatch];
            iStart := 1;
            For iLine := Max(1, M.LineNum - FRegExSurroundingLines) To M.LineNum - 1 Do
              Begin
                OutputToConsole(FStd, Format('  %10.0n: ', [iLine + 0.1]), FRegExLineNumbersColour);
                OutputToConsoleLn(FStd, slText[iLine - 1], FRegExLineOutputColour, clNone);
              End;
            OutputToConsole(FStd, Format('  %10.0n: ', [M.LineNum + 0.1]), FRegExLineNumbersColour);
            strLine := slText[M.LineNum - 1];
            For iGroup := 0 To M.Count - 1 Do
              Begin
                S := Copy(strLine, 1, M.Group[iGroup].FIndex - iStart);
                OutputToConsole(FStd, S, FRegExLineOutputColour, clNone);
                OutputToConsole(FStd, Copy(strLine, M.Group[iGroup].FIndex - iStart + 1,
                  M.Group[iGroup].FLength), FRegExFindOutputFGColour, FRegExFindOutputBGColour);
                Delete(strLine, 1, M.Group[iGroup].FIndex + M.Group[iGroup].FLength - iStart);
                iStart := M.Group[iGroup].FIndex + M.Group[iGroup].FLength;
              End;
            OutputToConsoleLn(FStd, strLine, FRegExLineOutputColour, clNone);
            For iLine := Min(slText.Count, M.LineNum + 1) To (M.LineNum + FRegExSurroundingLines) Do
              Begin
                OutputToConsole(FStd, Format('  %10.0n: ', [iLine + 0.1]), FRegExLineNumbersColour);
                OutputToConsoleLn(FStd, slText[iLine - 1], FRegExLineOutputColour, clNone);
              End;
            If FRegExSurroundingLines > 0 Then
              OutputToConsoleLn(FStd);
          End;
      Finally
        slText.Free;
      End;
    End;
End;

(**

  This method outputs the name of the directory or zip from which files are being found.

  @precon  None.
  @postcon Outputs the name of the directory or zip from which files are being found.

  @param   boolDirPrinted as a Boolean as a reference
  @param   strPath        as a String

**)
Procedure TSearch.OutputDirectoryOrZIPFile(Var boolDirPrinted: Boolean; strPath: String);

Var
  iZipPos: Integer;

Begin
  // Output Dir / Zip file
  If Not boolDirPrinted Then
    Begin
      If CheckConsoleMode(FStd) Then
        OutputToConsole(FStd, StringOfChar(' ', FWidth), clNone, clNone, False);
      If Not Like('*.zip\*', strPath) Then
        OutputToConsoleLn(FStd, strPath, FFoundSearchPathColour)
      Else
        Begin
          iZipPos := Pos('.zip', LowerCase(strPath));
          OutputToConsole(FStd, ExtractFilePath(Copy(strPath, 1, iZipPos + 3)),
            FFoundSearchPathColour);
          OutputToConsole(FStd, ExtractFileName(Copy(strPath, 1, iZipPos + 3)),
            FZipFileColour);
          OutputToConsoleLn(FStd, Copy(strPath, iZipPos + 4,
              Length(strPath) - iZipPos - 4), FFoundSearchPathColour);
        End;
      boolDirPrinted := True;
    End;
End;

{ TStringsHelper }

(**

  This method provide the RegEx functionality of SEARCH with a safe mechanism to load
  information from files without any share violations.

  @precon  None.
  @postcon Opens the named file safely.

  @param   strFileName as a String

**)
Procedure TStringsHelper.SafeLoadFromFile(strFileName: String);

Var
  Stream: TStream;

Begin
  Stream := TFileStream.Create(strFileName, fmOpenRead Or fmShareDenyNone);
  Try
    LoadFromStream(Stream);
  Finally
    Stream.Free;
  End;
End;

End.
