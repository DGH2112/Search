(**

  This module contains the main code used nby SEARCH to, yes, search for files.

  @Author  David Hoyle
  @Version 1.0
  @Date    08 Apr 2018

  @todo    Add number of GREP matches to output (only IF GREP enabled).
  @todo    Update Attributes to more than RASH.
  @todo    Add switch for RegEx filename matching.
  @todo    Check that GREP will work with multi-line matches.
  @todo    Allow a switch to change a default colour.
  
  @bug     Fix the AV bug in asking for extra lines around GREP searches.

**)
Unit SearchEngine;

Interface

Uses
  SysUtils,
  Classes,
  Windows,
  ZipForge,
  Graphics,
  Search.Functions,
  Search.Types,
  Search.Interfaces;

Type
  (** This class defines the working that searches the directories and files
   for the information that matches the criteria. **)
  TSearch = Class(TInterfacedObject, ISearchEngine)
  Strict Private
    (** The total number of file found by the search. **)
    FFiles: Integer;
    (** The total number of directories found by the search. **)
    FDirectories: Integer;
    (** This is the upper limit of a file size search. **)
    FUSize: Int64;
     (** This is the lower limit of a file size search. **)
    FLSize: Int64;
    (** This is the lower limit of a file date search. **)
    FLDate: Double;
    (** This is the upper limit of a file date search. **)
    FUDate: Double;
    (** The total size of al the files found in the search. **)
    FFileSize: Int64;
    (** Height of the console. **)
    FHeight: Integer;
    (** Width of the console. **)
    FWidth: Integer;
    (** This is a list of file attributes that are required in a search. **)
    FFileAttrs: Integer;
    (** This is a list of file type attributes that are required in a search. **)
    FTypeAttrs: Integer;
    (** This is a list of search parameters that are not part of the command line switches. **)
    FSearchParams: TStringList;
    (** This is the level of directory of summarisation required. **)
    FSummaryLevel: Integer;
    (** A type to indicate the order of file sorting **)
    FOrderFilesBy: TOrderBy;
    (** A type to indicate the direction of sorting of files. **)
    FOrderFilesDirection: TOrderDirection;
    (** A string for which should be searched for inside text files. **)
    FRegExSearch: String;
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
    FStdHnd: THandle;
    (** A handle to the error console output. **)
    FErrHnd: THandle;
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
  Strict Protected
    Procedure OutputDateTimeAndSize(Const FilesCollection: ISearchFiles; Var strOutput: String;
      Const i: Integer);
    Procedure OutputFileAttributes(Const i: Integer; Const FilesCollection: ISearchFiles;
      Var strOutput: String);
    Procedure OutputFileOwner(Const FilesCollection: ISearchFiles; Var strOutput: String;
      Const i: Integer);
    Procedure OutputRegExInformation(Const strPath : String; Const boolRegEx: Boolean;
      Const FileInfo: ISearchFile);
    Procedure OutputDirectoryOrZIPFile(Var boolDirPrinted: Boolean; Const strPath: String);
    Procedure GetConsoleInformation;
    Procedure OutputCurrentSearchPath(Const strPath: String);
    Procedure PrintTitle;
    Procedure PrintHeader(Const strPath, strPattern: String);
    Procedure PrintFooter(Const strPath: String);
    Procedure PrintHelp;
    Procedure DisplayCriteria;
    Procedure GetCommandLineSwitches;
    Function  LookupAccountBySID(Const SID: PSID): String;
    Function  OutputOwner(Const strFileName: String): String;
    Function  CheckFiles(Const recSearch: TSearchRec; Var iDirFiles: Integer;
      Const strPath, strOwner: String; Const FilesCollection: ISearchFiles): Int64;
    Function  CheckZipFiles(Const ZipArchive: TZipForge; Const ZFAI: TZFArchiveItem;
      Var iDirFiles: Integer; Const strOwner: String;
      Const FilesCollection: ISearchFiles): Int64;
    Procedure WorkaroundLargeFiles(Var iSize: Int64; Const recSearch: TSearchRec);
    Procedure OutputFilesToConsole(Const strPath: String; Var boolDirPrinted: Boolean;
      Const FilesCollection: ISearchFiles);
    Function  RecurseDirectories(Const strPath: String; Var iLevel: Integer;
      Const slPatterns: TStringList): Int64;
    Function  SearchForPatterns(Const slPatterns: TStringList; Const iDirFiles: Integer;
      Const strPath: String; Const FilesCollection: ISearchFiles): Int64;
    Function  SearchForPatternsInZip(Const strFileName: String; Const slPatterns: TStringList;
      Const iDirFiles: Integer; Const strPath: String; Const FilesCollection: ISearchFiles): Int64;
    Function  SearchDirectory(Const strPath: String; Const slPatterns: TStringList;
      Var iLevel: Integer): Int64;
    Function  SearchZip(Const strFileName: String; Const slPatterns: TStringList;
      Var iLevel: Integer): Int64;
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure FilesExceptionHandler(Const strException: String);
    Procedure ExceptionProc(Const strMsg: String);
    Function  FormatSize(Const iSize: Int64): String;
    Procedure AddErrorToLog(Const strErrMsg, strFileName : String);
    Procedure PrintErrorLog;
    Procedure ProcessZipFileFailure(Sender : TObject; FileName : String;
      Operation : TZFProcessOperation; NativeError : Integer; ErrorCode : Integer;
      ErrorMessage : String; var Action : TZFAction);
    Procedure ZipDiskFull(Sender : TObject; VolumeNumber : Integer;
      VolumeFileName : String; var Cancel : Boolean);
    Function  GetExceptionColour : TColor;
    Function  GetStdHnd : THandle;
    Function  GetErrHnd : THandle;
    Procedure Run;
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

  (** A type for a pointer to a PSID structure **)
  PPSID = ^PSID;

  (** This is a helper class for the TStrings class. **)
  TStringsHelper = Class Helper For TStrings
    Procedure SafeLoadFromFile(Const strFileName : String);
  End;

Implementation

Uses
  Contnrs,
  ACCCTRL,
  ActiveX,
  IniFiles,
  Math,
  System.RegularExpressions,
  Search.RegExMatches,
  Search.FilesCls;

Const
  (** An ini section for the console colours. **)
  strColoursINISection = 'Colours';
  (** An ini key for the search path colour **)
  strSearchPathKey = 'SearchPath';
  (** An ini key for the Title colour **)
  strTitleKey = 'Title';
  (** An ini key for the Header colour **)
  strHeaderKey = 'Header';
  (** An ini key for the Footer colour **)
  strFooterKey = 'Footer';
  (** An ini key for the Help Header colour **)
  strHelpHeaderKey = 'HelpHeader';
  (** An ini key for the Help Info colour **)
  strHelpInfoKey = 'HelpInfo';
  (** An ini key for the Help Text colour **)
  strHelpTextKey = 'HelpText';
  (** An ini key for the Help Switch colour **)
  strHelpSwitchKey = 'HelpSwitch';
  (** An ini key for the Help Foot Note colour **)
  strHelpFootNoteKey = 'HelpFootNote';
  (** An ini key for the Found Search Path colour **)
  strFoundSearchPathKey = 'FoundSearchPath';
  (** An ini key for the File infosearch path colour **)
  strFileInfoKey = 'FileInfo';
  (** An ini key for the Regaular Expression Line Numbers colour **)
  strRegExLineNumbersKey = 'RegExLineNumbers';
  (** An ini key for the Regular Expression Line Output colour **)
  strRegExLineOutputKey = 'RegExLineOutput';
  (** An ini key for the Regular Expression Find Output Foreground colour **)
  strRegExFindOutputFGKey = 'RegExFindOutputFG';
  (** An ini key for the Regular Expression Find Output Background colour **)
  strRegExFindOutputBGKey = 'RegExFindOutputBG';
  (** An ini key for the Summary Output colour **)
  strSummaryOutputKey = 'SummaryOutput';
  (** An ini key for the Exception colour **)
  strExceptionKey = 'Exception';
  (** An ini key for the Zip File colour **)
  strZipFileKey = 'ZipFile';
  (** An output format for all dates. **)
  strOutputDateFmt = 'ddd dd/mmm/yyyy hh:mm:ss';

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

(**

  This method provide the RegEx functionality of SEARCH with a safe mechanism to load information from
  files without any share violations.

  @precon  None.
  @postcon Opens the named file safely.

  @param   strFileName as a String as a constant

**)
Procedure TStringsHelper.SafeLoadFromFile(Const strFileName: String);

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

(**
  This method adds an error messages to the error message log.

  @precon  None.
  @postcon Adds an error messages to the error message log.

  @param   strErrMsg   as a String as a constant
  @param   strFileName as a String as a constant

**)
Procedure TSearch.AddErrorToLog(Const strErrMsg, strFileName: String);

Begin
  FErrorLog.Add(Format('%s (%s)', [strErrMsg, strFileName]))
End;

(**

  This method adds the file to the collection and increments the various counters.

  @precon  None.
  @postcon Adds the file to the collection and increments the various counters.

  @param   recSearch       as a TSearchRec as a constant
  @param   iDirFiles       as an Integer as a reference
  @param   strPath         as a String as a constant
  @param   strOwner        as a String as a constant
  @param   FilesCollection as an ISearchFiles as a constant
  @return  an Int64

**)
Function TSearch.CheckFiles(Const recSearch: TSearchRec; Var iDirFiles: Integer;
  Const strPath, strOwner: String; Const FilesCollection: ISearchFiles): Int64;

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
        dtCreation:   dtDate := recSearch.FindData.ftCreationTime;
        dtLastAccess: dtDate := recSearch.FindData.ftLastAccessTime;
        dtLastWrite:  dtDate := recSearch.FindData.ftLastWriteTime;
      End;
      FileTimeToLocalFileTime(dtDate, dtDateLocal);
      iTime := 0;
      If Not FileTimeToDosDateTime(dtDateLocal, LongRec(iTime).Hi,
        LongRec(iTime).Lo) Then
        iTime := 0;
      dtFileDate := SafeFileDateToDateTime(iTime, strPath + recSearch.Name, AddErrorToLog);
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

  This method adds the zip file to the collection and increments the various counters.

  @precon  None.
  @postcon Adds the zip file to the collection and increments the various counters.

  @param   ZipArchive      as a TZipForge as a constant
  @param   ZFAI            as a TZFArchiveItem as a constant
  @param   iDirFiles       as an Integer as a reference
  @param   strOwner        as a String as a constant
  @param   FilesCollection as an ISearchFiles as a constant
  @return  an Int64

**)
Function TSearch.CheckZipFiles(Const ZipArchive: TZipForge; Const ZFAI: TZFArchiveItem;
  Var iDirFiles: Integer; Const strOwner: String; Const FilesCollection: ISearchFiles): Int64;

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
Procedure TSearch.DisplayCriteria;

  (**

    This method outputs a single switch and its correpsonding message.

    @precon  None.
    @postcon Outputs a single switch and its correpsonding message.

    @param   ASwitch            as a TCommandLineSwitch as a constant
    @param   strSwitch          as a String as a constant
    @param   strText            as a String as a constant
    @param   boolCarriageReturn as a Boolean as a constant
    @return  a Boolean

  **)
  Function DisplayText(Const ASwitch: TCommandLineSwitch; Const strSwitch, strText: String;
    Const boolCarriageReturn: Boolean = True): Boolean;

  Begin
    Result := ASwitch In CommandLineSwitches;
    If Result Then
      Begin
        OutputToConsole(FStdHnd, Format('  %2s', [strSwitch]), FHelpSwitchColour);
        OutputToConsole(FStdHnd, Format(') %s', [strText]), FHelpTextColour);
        If boolCarriageReturn Then
          OutputToConsoleLn(FStdHnd);
      End;
  End;

  (**

    This method tries to reduce the file size limit to a more manageable length for reading using K, M
    and G.

    @precon  None.
    @postcon Tries to reduce the size to a factor of 1024.

    @param   iSize as an Int64 as a constant
    @return  a String

  **)
  Function ReduceSize(Const iSize: Int64): String;

  Const
    strSizes = 'KMGT';
    iKiloByte = 1024;

  Var
    iReductions: Integer;
    strKMG: String;
    iLSize: Int64;

  Begin
    iLSize := iSize;
    iReductions := 0;
    If iLSize > 0 Then
      While (iLSize Mod iKiloByte = 0) And (iReductions < 4) Do
        Begin
          iLSize := iLSize Div iKiloByte;
          Inc(iReductions);
        End;
    If iLSize > 0 Then
      strKMG := strSizes[iReductions]
    Else
      strKMG := '';
    Result := Format('%1.0n%s', [Int(Int64(iLSize)), strKMG]);
  End;

  (**

    This method output a text relating to the attribute options used in the
    search.

    @precon  None.
    @postcon Output a text relating to the attribute options used in the
             search.

  **)
  Procedure DisplayAttributes;

  ResourceString
    strSearchingForFiles = 'Searching for files with the following attibutes:';
    strReadOnly =   '        Read Only';
    strArchive =    '        Archive';
    strSystemFile = '        System File';
    strHidden =     '        Hidden';
    strFile =       '        File';
    strDirectory =  '        Directory';
    strVolume =     '        Volume';

  Begin
    If DisplayText(clsAttrRange, 't', strSearchingForFiles) Then
      Begin
        If FFileAttrs And faReadOnly > 0 Then
          OutputToConsoleLn(FStdHnd, strReadOnly, FHelpInfoColour);
        If FFileAttrs And faArchive > 0 Then
          OutputToConsoleLn(FStdHnd, strArchive, FHelpInfoColour);
        If FFileAttrs And faSysFile > 0 Then
          OutputToConsoleLn(FStdHnd, strSystemFile, FHelpInfoColour);
        If FFileAttrs And faHidden > 0 Then
          OutputToConsoleLn(FStdHnd, strHidden, FHelpInfoColour);
        If FTypeAttrs And iFileOnly > 0 Then
          OutputToConsoleLn(FStdHnd, strFile, FHelpInfoColour);
        If FTypeAttrs And faDirectory > 0 Then
          OutputToConsoleLn(FStdHnd, strDirectory, FHelpInfoColour);
        If FTypeAttrs And faVolumeID > 0 Then
          OutputToConsoleLn(FStdHnd, strVolume, FHelpInfoColour);
      End;
  End;

ResourceString
  strSearchCriteria = 'Search Criteria and Command Line Options Used in this Session.';
  strDisplayingCriteria = 'Displaying criteria used for this search.';
  strRecursingSubdirectories = 'Recursing sub-directories.';
  strDEBUGReadLnPause = 'Pause after finish.';
  strDisplayingFileAttibs = 'Displaying File and Directory Attributes.';
  strSummarisingOutput = 'Summarising output to the %d level of detail from ' +
    'the specified starting point.';
  strSupressingSummary = 'Supressing summary information on directories with zero files.';
  strDisplayFilesWithDates = 'Display files with dates between %s and %s.';
  strDisplayFilesWithSIzes = 'Display files with sizes between %s and %s byt' + 'es.';
  strQuietMode = 'Quiet mode - no progress updates.';
  strDisplayTheOwners = 'Display the Owners of the files found';
  strSizeFormatTeraBytes = 'Sizes formatted in TeraBytes';
  strSizeFormatGigaBytes = 'Sizes formatted in GigaBytes';
  strSizeFormatMegaBytes = 'Sizes formatted in MegaBytes';
  strSizeFormatKiloBytes = 'Sizes formatted in KiloBytes';
  strOrderingFilesByName = 'Ordering files by Name';
  strOrderingFilesBySize = 'Ordering files by Size';
  strOrderingFilesByDate = 'Ordering files by Date';
  strOrderingFilesByOwner = 'Ordering files by Owner';
  strOrderingFilesByTheirAttrs = 'Ordering files by their Attributes';
  strOrderingFilesInDescOrder = 'Descending Order';
  strDisplayingFileCreationDates = 'Displaying file Creation Dates.';
  strDisplayingFileLastAccessDates = 'Displaying file Last Access Dates.';
  strDisplayingFileLastWriteDates = 'Displaying file Last Write Dates.';
  strOwnerSearchingFor = ' Searching for Owners ';
  strOwnerNOT = 'NOT ';
  strOwnerMatching = 'matching "%s"';
  strOwnerExact = ' exactly';
  strOwnerAtTheStart = ' at the start';
  strOwnerInTheMiddle = ' in the middle';
  strOwnerAtTheEnd = ' at the end';
  strSearchInZips = 'Search within ZIP files.';
  strApplyingExclusions = 'Applying exclusions from the file "%s".';
  strSearchForText = 'Search for the text "%s" within the files.';

Begin
  OutputToConsoleLn(FStdHnd, strSearchCriteria, FHelpHeaderColour);
  DisplayText(clsDisplayCriteria, 'c', strDisplayingCriteria);
  DisplayText(clsSubDirectories, 's', strRecursingSubdirectories);
  DisplayText(clsDebug, '!', strDEBUGReadLnPause);
  DisplayText(clsShowAttribs, 'a', strDisplayingFileAttibs);
  If Not(clsRegExSearch In CommandLineSwitches) Then
    DisplayText(clsSummaryLevel, IntToStr(FSummaryLevel),
      Format(strSummarisingOutput, [FSummaryLevel]));
  DisplayText(clsSupressZeros, '0', strSupressingSummary);
  DisplayText(clsDateRange, 'd', Format(strDisplayFilesWithDates, [
    FormatDateTime(strOutputDateFmt, FLDate),
    FormatDateTime(strOutputDateFmt, FUDate)]));
  DisplayText(clsSizeRange, 'd', Format(strDisplayFilesWithSIzes, [ReduceSize(FLSize),
      ReduceSize(FUSize)]));
  DisplayAttributes;
  DisplayText(clsQuiet, 'q', strQuietMode);
  If DisplayText(clsOwner, 'w', strDisplayTheOwners, False) Then
    Begin
      If FOwnerSearchPos In [ospExact .. ospEnd] Then
        Begin
          OutputToConsole(FStdHnd, strOwnerSearchingFor, FHelpTextColour);
          If FOwnerSearchOps In [osNotEquals] Then
            OutputToConsole(FStdHnd, strOwnerNOT, FHelpSwitchColour);
          OutputToConsole(FStdHnd, Format(strOwnerMatching, [FOwnerSearch]),
            FHelpTextColour);
          Case FOwnerSearchPos Of
            ospExact:  OutputToConsole(FStdHnd, strOwnerExact, FHelpTextColour);
            ospStart:  OutputToConsole(FStdHnd, strOwnerAtTheStart, FHelpTextColour);
            ospMiddle: OutputToConsole(FStdHnd, strOwnerInTheMiddle, FHelpTextColour);
            ospEnd:    OutputToConsole(FStdHnd, strOwnerAtTheEnd, FHelpTextColour);
          End;
        End;
      OutputToConsoleLn(FStdHnd, '.', FHelpTextColour);
    End;
  If clsOrderBy In CommandLineSwitches Then
    Begin
      Case FOrderFilesBy Of
        obName:      DisplayText(clsOrderBy, 'o', strOrderingFilesByName, False);
        obSize:      DisplayText(clsOrderBy, 'o', strOrderingFilesBySize, False);
        obDate:      DisplayText(clsOrderBy, 'o', strOrderingFilesByDate, False);
        obOwner:     DisplayText(clsOrderBy, 'o', strOrderingFilesByOwner, False);
        obAttribute: DisplayText(clsOrderBy, 'o', strOrderingFilesByTheirAttrs, False);
      End;
      If FOrderFilesDirection = odDescending Then
        Begin
          OutputToConsole(FStdHnd, ' (', FHelpTextColour);
          OutputToConsole(FStdHnd, strOrderingFilesInDescOrder, FHelpFootNoteColour);
          OutputToConsole(FStdHnd, ')', FHelpTextColour);
        End;
      OutputToConsoleLn(FStdHnd, '.', FHelpTextColour);
    End;
  Case FDateType Of
    dtCreation:      DisplayText(clsDateType, 'e', strDisplayingFileCreationDates);
    dtLastAccess:    DisplayText(clsDateType, 'a', strDisplayingFileLastAccessDates);
    dtLastWrite:     DisplayText(clsDateType, 'w', strDisplayingFileLastWriteDates);
  End;
  DisplayText(clsRegExSearch, 'i', Format(strSearchForText, [FRegExSearch]));
  DisplayText(clsExclusions, 'x', Format(strApplyingExclusions, [FExlFileName]));
  DisplayText(clsSearchZip, 'p', strSearchInZips);
  Case FSizeFormat Of
    sfKilobytes:     DisplayText(clsSizeOutput, 'f', strSizeFormatKiloBytes);
    sfMegaBytes:     DisplayText(clsSizeOutput, 'f', strSizeFormatMegaBytes);
    sfGigaBytes:     DisplayText(clsSizeOutput, 'f', strSizeFormatGigaBytes);
    sfTeraBytes:     DisplayText(clsSizeOutput, 'f', strSizeFormatTeraBytes);
  End;
  OutputToConsoleLn(FStdHnd);
End;

(**

  This is a exception handler for the BuildRootKey method.

  @precon  None.
  @postcon Raise the exception for termination.

  @param   strMsg as a String as a constant

**)
Procedure TSearch.ExceptionProc(Const strMsg: String);

Begin
  Raise Exception.Create(strMsg);
End;

(**

  This method outputs exception from the TFiles class to the console.

  @precon  None.
  @postcon Outputs exception from the TFiles class to the console.

  @param   strException as a String as a constant

**)
Procedure TSearch.FilesExceptionHandler(Const strException: String);

ResourceString
  strFilesException = 'Exception search files: %s';

Begin
  OutputToConsoleLn(FStdHnd, Format(strFilesException, [strException]), FExceptionColour);
End;

(**

  This method returns the file size formatted to that defined on the command line.

  @precon  None.
  @postcon Returns the file size formatted to that defined on the command line.

  @param   iSize as an Int64 as a constant
  @return  a String

**)
Function TSearch.FormatSize(Const iSize: Int64): String;

Const
  dblKileByte  = 1024.0;
  dblMegaByte  = dblKileByte * dblKileByte;
  dblGigaByte  = dblKileByte * dblKileByte * dblKileByte;
  dblTerraByte = dblKileByte * dblKileByte * dblKileByte * dblKileByte;
  strKikoByteFmt = '%13.0nK';
  strMegaByteFmt = '%10.0nM';
  strGigaByteFmt = '%7.0nG';
  strTerraByteFmt = '%4.0nT';

Var
  dblSize: Double;

Begin
  dblSize := iSize;
  Case FSizeFormat Of
    sfKilobytes: Result := Format(strKikoByteFmt, [RoundTo(dblSize / dblKileByte, 0)]);
    sfMegaBytes: Result := Format(strMegaByteFmt, [RoundTo(dblSize / dblMegaByte, 0)]);
    sfGigaBytes: Result := Format(strGigaByteFmt, [RoundTo(dblSize / dblGigaByte, 0)]);
    sfTeraBytes: Result := Format(strTerraByteFmt, [RoundTo(dblSize / dblTerraByte, 0)]);
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

ResourceString
  strInvalidCommandLineSwitch = 'Invalid command line switch "%s" in parameter "%s."';
  strNeedToSpecifyCriteria = 'You need to specify at least one search criteria.';

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

  This routine get and stores the current number of lines and columns
  in the console window.

  @precon  None.
  @postcon Gets the width and height of the console.

**)
Procedure TSearch.GetConsoleInformation;

Var
  ConsoleInfo: TConsoleScreenBufferInfo;

Begin
  FStdHnd := GetStdHandle(STD_OUTPUT_HANDLE);
  FErrHnd := GetStdHandle(STD_ERROR_HANDLE);
  GetConsoleScreenBufferInfo(FStdHnd, ConsoleInfo);
  FWidth := ConsoleInfo.dwSize.X - 1;
  FHeight := ConsoleInfo.dwSize.Y;
End;

(**

  This is a getter method for the ErrHnd property.

  @precon  None.
  @postcon Returns the console error output handle.

  @return  a THandle

**)
Function TSearch.GetErrHnd: THandle;

Begin
  Result := FErrHnd;
End;

(**

  This is a getter method for the ExceptionColour property.

  @precon  None.
  @postcon Returns the console colour to be used for errors.

  @return  a TColor

**)
Function TSearch.GetExceptionColour: TColor;

Begin
  Result := FExceptionColour;
End;

(**

  This is a getter method for the StdHnd property.

  @precon  None.
  @postcon Returns the console standard console output handle.

  @return  a THandle

**)
Function TSearch.GetStdHnd: THandle;

Begin
  Result := FStdHnd;
End;

(**

  This method loads the colour settings from the INI file.

  @precon  None.
  @postcon Loads the colour settings from the INI file.

**)
Procedure TSearch.LoadSettings;

Const
  strDefaultSearchPath = 'clMaroon';
  strDefaultTitle = 'clWhite';
  strDefaultHeader = 'clYellow';
  strDefaultFooter = 'clWhite';
  strDefaultHelpFooter = 'clWhite';
  strDefaultHelpInfo = 'clLime';
  strDefaultHelpText = 'clNone';
  strDefaultHelpSwitch = 'clRed';
  strDefaultHelpFootNote = 'clYellow';
  strDefaultFoumdSearchPath = 'clWhite';
  strDefaultFileInfo = 'clNone';
  strDefaultRegExLineNum = 'clRed';
  strDefaultRegExLineOutput = 'clLime';
  strDefaultRegExFindoutputFG = 'clRed';
  strDefaultRegExFindoutputBG = 'clYellow';
  strDefaultSummaryOutput = 'clNone';
  strDefaultException = 'clRed';
  strDefaultZipFile = 'clFuchsia';

Var
  iniFile: TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(FRootKey);
  Try
    FSearchPathColour := StringToColor(iniFile.ReadString(strColoursINISection, strSearchPathKey, strDefaultSearchPath));
    FTitleColour := StringToColor(iniFile.ReadString(strColoursINISection, strTitleKey, strDefaultTitle));
    FHeaderColour := StringToColor(iniFile.ReadString(strColoursINISection, strHeaderKey, strDefaultHeader));
    FFooterColour := StringToColor(iniFile.ReadString(strColoursINISection, strFooterKey, strDefaultFooter));
    FHelpHeaderColour := StringToColor(iniFile.ReadString(strColoursINISection, strHelpHeaderKey, strDefaultHelpFooter));
    FHelpInfoColour := StringToColor(iniFile.ReadString(strColoursINISection, strHelpInfoKey, strDefaultHelpInfo));
    FHelpTextColour := StringToColor(iniFile.ReadString(strColoursINISection, strHelpTextKey, strDefaultHelpText));
    FHelpSwitchColour := StringToColor(iniFile.ReadString(strColoursINISection, strHelpSwitchKey, strDefaultHelpSwitch));
    FHelpFootNoteColour := StringToColor(iniFile.ReadString(strColoursINISection, strHelpFootNoteKey, strDefaultHelpFootNote));
    FFoundSearchPathColour := StringToColor(iniFile.ReadString(strColoursINISection, strFoundSearchPathKey, strDefaultFoumdSearchPath));
    FFileInfoColour := StringToColor(iniFile.ReadString(strColoursINISection, strFileInfoKey, strDefaultFileInfo));
    FRegExLineNumbersColour := StringToColor(iniFile.ReadString(strColoursINISection, strRegExLineNumbersKey, strDefaultRegExLineNum));
    FRegExLineOutputColour := StringToColor(iniFile.ReadString(strColoursINISection, strRegExLineOutputKey, strDefaultRegExLineOutput));
    FRegExFindOutputFGColour := StringToColor(iniFile.ReadString(strColoursINISection, strRegExFindOutputFGKey, strDefaultRegExFindoutputFG));
    FRegExFindOutputBGColour := StringToColor(iniFile.ReadString(strColoursINISection, strRegExFindOutputBGKey, strDefaultRegExFindoutputBG));
    FSummaryOutputColour := StringToColor(iniFile.ReadString(strColoursINISection, strSummaryOutputKey, strDefaultSummaryOutput));
    FExceptionColour := StringToColor(iniFile.ReadString(strColoursINISection, strExceptionKey, strDefaultException));
    FZipFileColour := StringToColor(iniFile.ReadString(strColoursINISection, strZipFileKey, strDefaultZipFile));
  Finally
    iniFile.Free;
  End;
End;

(**

  This method returns the owner of the files from a SID.

  @precon  None.
  @postcon Returns the owner of the files from a SID.

  @param   SID as a PSID as a constant
  @return  a String

**)
Function TSearch.LookupAccountBySID(Const SID: PSID): String;

Const
  strUnknownOwner = '(Unknown)';

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
    strName := strUnknownOwner;
  SetLength(strName, Length(strName) - 1);
  SetLength(strRefDomain, Length(strRefDomain) - 1);
  Result := strRefDomain + '/' + strName;
End;

(**

  This method outputs the currently being searched path to the screen.

  @precon  None.
  @postcon Outputs the currently being searched path to the screen.

  @param   strPath as a String as a constant

**)
Procedure TSearch.OutputCurrentSearchPath(Const strPath: String);

ResourceString
  strSearchLabel = 'Searching: (D:%1.0n,F:%1.0n) ';
  
Var
  i, j: Integer;
  iLength: Integer;
  strLPath: String;

Begin
  strLPath := strPath;
  If (clsQuiet In CommandLineSwitches) Or (clsOutputAsCSV In CommandLineSwitches) Then
    Exit;
  iLength := Length(Format(strSearchLabel, [Int(FDirectories), Int(FFiles)]));
  While Length(strLPath) > (FWidth - iLength) Do
    Begin
      If Pos('...', strLPath) = 0 Then
        Begin
          i := PosOfNthChar(strLPath, '\', 1 + Integer(FUNCPath) * 2) + 1;
          j := PosOfNthChar(strLPath, '\', 2 + Integer(FUNCPath) * 2);
          If (i > 0) And (j > 0) Then
            strLPath := StringReplace(strLPath, Copy(strLPath, i, j - i), '...', [])
          Else
            strLPath := Copy(strLPath, Length(strLPath) - (FWidth - iLength),
              FWidth - iLength);
        End
      Else
        Begin
          i := PosOfNthChar(strLPath, '\', 2 + Integer(FUNCPath) * 2);
          j := PosOfNthChar(strLPath, '\', 3 + Integer(FUNCPath) * 2);
          If (i > 0) And (j > 0) Then
            strLPath := StringReplace(strLPath, Copy(strLPath, i, j - i), '', [])
          Else
            strLPath := Copy(strLPath, Length(strLPath) - (FWidth - iLength),
              FWidth - iLength);
        End;
    End;
  strLPath := Format('%s%-*s',
    [Format(strSearchLabel, [Int(FDirectories), Int(FFiles)]), FWidth - iLength,
    strLPath]);
  If CheckConsoleMode(FStdHnd) Then
    OutputToConsole(FStdHnd, strLPath, FSearchPathColour, clNone, False);
End;

(**

  This method outputs the file date and time and its size to the passed output string.

  @precon  FilesCollection must be a valid instance.
  @postcon Outputs the file date and time and its size to the passed output string.

  @param   FilesCollection as an ISearchFiles as a constant
  @param   strOutput       as a String as a reference
  @param   i               as an Integer as a constant

**)
Procedure TSearch.OutputDateTimeAndSize(Const FilesCollection: ISearchFiles; Var strOutput: String;
  Const i: Integer);

Const
  strDIR = '<DIR>';

Begin
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    Begin
      If Pos('D', FilesCollection.FileInfo[i].Attr) = 0 Then
        strOutput := Format('  %s  %s  ', [FormatDateTime(strOutputDateFmt,
            FilesCollection.FileInfo[i].Date),
          FormatSize(FilesCollection.FileInfo[i].Size)])
      Else
        strOutput := Format('  %s  %15s  ', [FormatDateTime(strOutputDateFmt,
            FilesCollection.FileInfo[i].Date), strDIR]);
    End
  Else
    Begin
      If Pos('D', FilesCollection.FileInfo[i].Attr) = 0 Then
        strOutput := Format('%s,%d,', [FormatDateTime(strOutputDateFmt,
            FilesCollection.FileInfo[i].Date), FilesCollection.FileInfo[i].Size])
      Else
        strOutput := Format('%s,%d,', [FormatDateTime(strOutputDateFmt,
            FilesCollection.FileInfo[i].Date), 0]);
    End;
End;

(**

  This method outputs the name of the directory or zip from which files are being found.

  @precon  None.
  @postcon Outputs the name of the directory or zip from which files are being found.

  @param   boolDirPrinted as a Boolean as a reference
  @param   strPath        as a String as a constant

**)
Procedure TSearch.OutputDirectoryOrZIPFile(Var boolDirPrinted: Boolean; Const strPath: String);

Const
  strZipExt = '.zip';
  strZipPattern = '*.zip\*';

Var
  iZipPos: Integer;

Begin
  // Output Dir / Zip file
  If Not boolDirPrinted Then
    Begin
      If CheckConsoleMode(FStdHnd) Then
        OutputToConsole(FStdHnd, StringOfChar(' ', FWidth), clNone, clNone, False);
      If Not Like(strZipPattern, strPath) Then
        OutputToConsoleLn(FStdHnd, strPath, FFoundSearchPathColour)
      Else
        Begin
          iZipPos := Pos(strZipExt, LowerCase(strPath));
          OutputToConsole(FStdHnd, ExtractFilePath(Copy(strPath, 1, iZipPos + 3)),
            FFoundSearchPathColour);
          OutputToConsole(FStdHnd, ExtractFileName(Copy(strPath, 1, iZipPos + 3)),
            FZipFileColour);
          OutputToConsoleLn(FStdHnd, Copy(strPath, iZipPos + 4,
              Length(strPath) - iZipPos - 4), FFoundSearchPathColour);
        End;
      boolDirPrinted := True;
    End;
End;

(**

  This method outputs the files attributes to the passed output string.

  @precon  FileCollection must be a valid instance.
  @postcon Outputs the files attributes to the passed output string.

  @param   i               as an Integer as a constant
  @param   FilesCollection as an ISearchFiles as a constant
  @param   strOutput       as a String as a reference

**)
Procedure TSearch.OutputFileAttributes(Const i: Integer; Const FilesCollection: ISearchFiles;
  Var strOutput: String);

Begin
  If clsShowAttribs In CommandLineSwitches Then
    If Not(clsOutputAsCSV In CommandLineSwitches) Then
      strOutput := strOutput + Format('%s  ', [FilesCollection.FileInfo[i].Attr])
    Else
      strOutput := strOutput + Format('%s,', [FilesCollection.FileInfo[i].Attr]);
End;

(**

  This method outputs the file owner.

  @precon  FilesCollection must be a valid instance.
  @postcon Outputs the file owner.

  @param   FilesCollection as an ISearchFiles as a constant
  @param   strOutput       as a String as a reference
  @param   i               as an Integer as a constant

**)
Procedure TSearch.OutputFileOwner(Const FilesCollection: ISearchFiles; Var strOutput: String;
  Const i: Integer);

Var
  iOwnerWidth: Integer;

Begin
  If clsOwner In CommandLineSwitches Then
    If Not(clsOutputAsCSV In CommandLineSwitches) Then
      Begin
        iOwnerWidth := FilesCollection.OwnerWidth;
        strOutput := strOutput + Format('%-*s  ', [iOwnerWidth, FilesCollection.FileInfo[i].Owner]);
      End
    Else
      Begin
        strOutput := strOutput + Format('%s,', [FilesCollection.FileInfo[i].Owner]);
      End;
End;

(**

  This method outputs the files that have met the criteria to the console.

  @precon  None.
  @postcon Outputs the files that have met the criteria to the console.

  @param   strPath         as a String as a constant
  @param   boolDirPrinted  as a Boolean as a reference
  @param   FilesCollection as an ISearchFiles as a constant

**)
Procedure TSearch.OutputFilesToConsole(Const strPath: String; Var boolDirPrinted: Boolean;
  Const FilesCollection: ISearchFiles);

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
          OutputToConsoleLn(FStdHnd, strOutput + FilesCollection.FileInfo[iFile].FileName,
            FFileInfoColour)
        Else
          OutputToConsoleLn(FStdHnd,
            strOutput + strPath + FilesCollection.FileInfo[iFile].FileName, FFileInfoColour);
      OutputRegExInformation(strPath, boolRegEx, FilesCollection.FileInfo[iFile]);
    End;
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    If FilesCollection.Count > 0 Then
      OutputToConsoleLn(FStdHnd);
End;

(**

  Outputs the owner of the filename.

  @precon  None.
  @postcon Outputs the owner of the file.

  @param   strFileName as a String as a constant
  @return  a String

**)
Function TSearch.OutputOwner(Const strFileName: String): String;

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

  This method outputs the files RegEx information for a file.

  @precon  FilesCollection must be a valid instance.
  @postcon Outputs the files RegEx information for a file.

  @param   strPath   as a String as a constant
  @param   boolRegEx as a Boolean as a constant
  @param   FileInfo  as an ISearchFile as a constant

**)
Procedure TSearch.OutputRegExInformation(Const strPath : String; Const boolRegEx: Boolean;
  Const FileInfo: ISearchFile);

Const
  strZipRegExPattern = '\.zip\\';

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
  strText: String;

Begin
  If boolRegEx Then
    Begin
      slText := TStringList.Create;
      Try
        REZip := TRegEx.Create(strZipRegExPattern, [roCompiled, roIgnoreCase, roSingleLine]);
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
                OutputToConsole(FStdHnd, Format('  %10.0n: ', [Int(iLine)]), FRegExLineNumbersColour);
                OutputToConsoleLn(FStdHnd, slText[iLine - 1], FRegExLineOutputColour, clNone);
              End;
            OutputToConsole(FStdHnd, Format('  %10.0n: ', [Int(M.LineNum)]), FRegExLineNumbersColour);
            strLine := slText[M.LineNum - 1];
            For iGroup := 0 To M.Count - 1 Do
              Begin
                S := Copy(strLine, 1, M.Group[iGroup].FIndex - iStart);
                OutputToConsole(FStdHnd, S, FRegExLineOutputColour, clNone);
                OutputToConsole(FStdHnd, Copy(strLine, M.Group[iGroup].FIndex - iStart + 1,
                  M.Group[iGroup].FLength), FRegExFindOutputFGColour, FRegExFindOutputBGColour);
                Delete(strLine, 1, M.Group[iGroup].FIndex + M.Group[iGroup].FLength - iStart);
                iStart := M.Group[iGroup].FIndex + M.Group[iGroup].FLength;
              End;
            OutputToConsoleLn(FStdHnd, strLine, FRegExLineOutputColour, clNone);
            For iLine := Min(slText.Count, M.LineNum + 1) To (M.LineNum + FRegExSurroundingLines) Do
              Begin
                OutputToConsole(FStdHnd, Format('  %10.0n: ', [Int(iLine)]), FRegExLineNumbersColour);
                OutputToConsoleLn(FStdHnd, slText[iLine - 1], FRegExLineOutputColour, clNone);
              End;
            If FRegExSurroundingLines > 0 Then
              OutputToConsoleLn(FStdHnd);
          End;
      Finally
        slText.Free;
      End;
    End;
End;

(**

  This method outputs any error messages at the end of the search process.

  @precon  None.
  @postcon Outputs any error messages at the end of the search process.

**)
Procedure TSearch.PrintErrorLog;

ResourceString
  strErrorsWereEncountered = 'The following errors were encountered during the search:';

Var
  i : Integer;

Begin
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    If FErrorLog.Count > 0 Then
      Begin
        OutputToConsoleLn(FStdHnd);
        OutputToConsoleLn(FStdHnd, strErrorsWereEncountered,
          FExceptionColour);
        For i := 0 To FErrorLog.Count - 1 Do
          OutputToConsoleLn(FStdHnd, #32#32 + FErrorLog[i]);
      End;
End;

(**

  Prints a footer after the search displaying stats on the search.

  @precon  None.
  @postcon Prints a footer after the search displaying stats on the search.

  @param   strPath as a String as a constant

**)
Procedure TSearch.PrintFooter(Const strPath: String);

ResourceString
  strMsg1 = '  Found %s bytes in %1.0n Files in %1.0n Directories.';
  strMsg2 = '  Total space %s bytes and %s bytes free.';

Var
  iFreeBytesAvailableToCaller, iTotalNumberOfBytes, iTotalNumberOfFreeBytes: Int64;

Begin
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    Begin
      GetDiskFreeSpaceEx(PChar(strPath), iFreeBytesAvailableToCaller,
        iTotalNumberOfBytes, @iTotalNumberOfFreeBytes);
      If CheckConsoleMode(FStdHnd) Then
        OutputToConsole(FStdHnd, StringOfChar(#32, FWidth - 1), clNone, clNone, False);
      OutputToConsoleLn(FStdHnd, Format(strMsg1, [Trim(FormatSize(FFileSize)), Int(FFiles),
          Int(FDirectories)]), FFooterColour);
      OutputToConsoleLn(FStdHnd, Format(strMsg2, [Trim(FormatSize(iTotalNumberOfBytes)),
          Trim(FormatSize(iTotalNumberOfFreeBytes))]), FFooterColour);
    End;
End;

(**

  Prints the path and search pattern before each search

  @precon  None.
  @postcon Prints the path and search pattern before each search

  @param   strPath    as a String as a constant
  @param   strPattern as a String as a constant

**)
Procedure TSearch.PrintHeader(Const strPath, strPattern: String);

ResourceString
  strSearchingForHeader = 'Searching "%s" for "%s".';

Begin
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    OutputToConsoleLn(FStdHnd, Format(strSearchingForHeader, [strPath, strPattern]),
      FHeaderColour);
  FFiles := 0;
  FDirectories := 0;
  FFileSize := 0;
End;

(**

  This method prints on the console screen the command line search help
  information.

  @precon  None.
  @postcon Prints on the console screen the command line search help
           information.

**)
Procedure TSearch.PrintHelp;

ResourceString
  strHelp0010 = 'Syntax:';
  strHelp0020 = 'Search searchparam1 {searchparam2}... {/A} {/S} {/Q} {/W}';
  strHelp0030 = '{/T[RASHFDV]} {/D:[{DD{/MM{/YY {HH:MM{:SS}}}}}-{DD{/MM{/YY {HH:MM{:SS}}}}}]';
  strHelp0040 = '{/Z[{LowerByteSize}-{UpperByteSize}]} {/O:NDAOS} {/E:CAW} {/I[text]}';
  strHelp0050 = '{/X[filename]} {/@[Drive:\Path\FileName.INI]}';
  strHelp0060 = 'searchparam# = {drive:\path\}filter1{;filter2{;filter3{;.' + '..}}}';
  strHelp0070 = 'filter#';
  strHelp0075 = 'can contain wildcards * and ? within the filename. ';
  strHelp0080 = '{ ... } denotes optional items.';

  strHelp0090 = '/?';
  strHelp0095 = 'This help screen';

  strHelp0100 = '/A';
  strHelp0105 = 'Show file and directory attributes';

  strHelp0110 = '/S';
  strHelp0115 = 'Recurse subdirectories';

  strHelp0120 = '/1..9';
  strHelp0125 = 'Summarise subdirectories';

  strHelp0130 = '/0';
  strHelp0135 = 'Hide empty Summarised subdirectories';

  strHelp0140 = '/T[RASHFDV]';
  strHelp0145 = 'Show only files with certain attributes';
  strHelp0150 = 'R = Read Only, A = Archive, S = System, H = Hidden,';
  strHelp0160 = 'F = File, D = Directory, and V = Volume ID';

  strHelp0170 = '/D[{l}-{u}]';
  strHelp0175 = 'Searches for files between dates (l) and (u).';
  strHelp0177 = 'l = lower bounding date and time, u = upper bounding date and time';

  strHelp0180 = '/Z[{l}-{u}]';
  strHelp0185 = 'Searches for files between sizes (l) and (u).';
  strHelp0187 = 'l = lower bounding size in bytes, u = upper bounding size in bytes';
  strHelp0188 = 'Sizes can be postfixed with k, m, g or t for kilo, mega, giga and terabytes';

  strHelp0190 = '/Q';
  strHelp0195 = 'Quiet mode';

  strHelp0200 = '/W {[Owner]}';
  strHelp0205 = 'Owner Information. The owner search can be a wildcard search with an *.';
  strHelp0207 = 'Searches beginning with ! force a negated criteria.';

  strHelp0210 = '/O:NADOS';
  strHelp0215 = 'Order files ascending [+] and descending [-]';
  strHelp0220 = 'N = Name, A = Attribute, D = Date and Time, O = Owner, S = Size';

  strHelp0230 = '/E:CAW';
  strHelp0235 = 'Type of file date to display and search on:';
  strHelp0240 = 'C = Creation, A = Last Access, W = Last Write (Default)';

  strHelp0250 = '/I[text]';
  strHelp0255 = 'Search within files for text.';

  strHelp0260 = '/C';
  strHelp0265 = 'Display a list of Criteria and Command Line Options.';

  strHelp0270 = '/X[FileName]';
  strHelp0275 = 'A list of exclusions to apply to paths and filenames.';
  strHelp0280 = 'NOTE: The date time input format is dependent on your local settings';

  strHelp0276 = '/@[FileName]';
  strHelp0277 = 'Allows you to specify an alternate INI file.';

  strHelp0281 = '/P';
  strHelp0282 = 'Enables the searching within ZIP archives.';

  strHelp0290 = '/F:KMGT';
  strHelp0291 = 'Changes the output size format to Kilo, Mega, Giga or Terabytes.';

  strHelp0292 = '/V';
  strHelp0293 = 'Output the found information in CSV format.';

  (**

    This function formats the output information with indents and padding to a specific width.

    @precon  None.
    @postcon Formats the output information with indents and padding to a specific width.

    @param   strText as a String as a constant
    @param   iIndent as an Integer as a constant
    @param   iWidth  as an Integer as a constant
    @return  a String

  **)
  Function Indent(Const strText: String; Const iIndent, iWidth: Integer): String;

  Begin
    Result := StringOfChar(#32, iIndent) + Format('%-*s', [iWidth, strText]);
  End;

Const
  iWidth = 14;
  iTextIndent = 16;
  iMainIndent = 2;
  iMinorIndent = 4;

Begin
  OutputToConsoleLn(FStdHnd, strHelp0010, FHelpHeaderColour);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0020, iMainIndent, 0), FHelpInfoColour);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0030, iMinorIndent, 0), FHelpInfoColour);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0040, iMinorIndent, 0), FHelpInfoColour);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0050, iMinorIndent, 0), FHelpInfoColour);
  OutputToConsoleLn(FStdHnd);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0060, iMainIndent, 0), FHelpInfoColour);
  OutputToConsoleLn(FStdHnd);
  OutputToConsole(FStdHnd, Indent(strHelp0070, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0075, FHelpTextColour);
  OutputToConsoleLn(FStdHnd);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0080, iTextIndent, 0), FHelpTextColour);
  OutputToConsoleLn(FStdHnd);
  OutputToConsole(FStdHnd, Indent(strHelp0090, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0095, FHelpTextColour);
  OutputToConsole(FStdHnd, Indent(strHelp0100, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0105, FHelpTextColour);
  OutputToConsole(FStdHnd, Indent(strHelp0110, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0115, FHelpTextColour);
  OutputToConsole(FStdHnd, Indent(strHelp0120, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0125, FHelpTextColour);
  OutputToConsole(FStdHnd, Indent(strHelp0130, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0135, FHelpTextColour);
  OutputToConsole(FStdHnd, Indent(strHelp0140, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0145, FHelpTextColour);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0150, iTextIndent + iMainIndent, 0), FHelpInfoColour);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0160, iTextIndent + iMainIndent, 0), FHelpInfoColour);
  OutputToConsole(FStdHnd, Indent(strHelp0170, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0175, FHelpTextColour);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0177, iTextIndent + iMainIndent, 0), FHelpInfoColour);
  OutputToConsole(FStdHnd, Indent(strHelp0180, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0185, FHelpTextColour);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0187, iTextIndent + iMainIndent, 0), FHelpInfoColour);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0188, iTextIndent + iMainIndent, 0), FHelpInfoColour);
  OutputToConsole(FStdHnd, Indent(strHelp0190, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0195, FHelpTextColour);
  OutputToConsole(FStdHnd, Indent(strHelp0200, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0205, FHelpTextColour);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0207, iTextIndent, 0));
  OutputToConsole(FStdHnd, Indent(strHelp0210, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0215, FHelpTextColour);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0220, iTextIndent + iMainIndent, 0), FHelpInfoColour);
  OutputToConsole(FStdHnd, Indent(strHelp0230, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0235, FHelpTextColour);
  OutputToConsoleLn(FStdHnd, Indent(strHelp0240, iTextIndent + iMainIndent, 0), FHelpInfoColour);
  OutputToConsole(FStdHnd, Indent(strHelp0250, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0255, FHelpTextColour);
  OutputToConsole(FStdHnd, Indent(strHelp0260, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0265, FHelpTextColour);
  OutputToConsole(FStdHnd, Indent(strHelp0270, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0275, FHelpTextColour);
  OutputToConsole(FStdHnd, Indent(strHelp0276, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0277, FHelpTextColour);
  OutputToConsole(FStdHnd, Indent(strHelp0281, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0282, FHelpTextColour);
  OutputToConsole(FStdHnd, Indent(strHelp0290, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0291, FHelpTextColour);
  OutputToConsole(FStdHnd, Indent(strHelp0292, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strHelp0293, FHelpTextColour);
  OutputToConsoleLn(FStdHnd);
  OutputToConsoleLn(FStdHnd, strHelp0280, FHelpFootNoteColour);
  OutputToConsoleLn(FStdHnd);
End;

(**

  Prints a Title for the application on the screen.

  @precon  None.
  @postcon Prints the title for the application on the screen.

**)
Procedure TSearch.PrintTitle;

ResourceString
  strDateAndTimeSize = 'Date and Time,Size';
  strAttrbutes = ',Attrbutes';
  strOwner = ',Owner';
  strFilename = ',Filename';

Var
  strOutput: String;

Begin
  If Not (clsOutputAsCSV In CommandLineSwitches) Then
    Begin
      OutputToConsoleLn(FStdHnd, GetConsoleTitle, FTitleColour);
      OutputToConsoleLn(FStdHnd);
      If clsDisplayCriteria In CommandLineSwitches Then
        DisplayCriteria;
    End
  Else
    Begin
      strOutput := strDateAndTimeSize;
      If clsShowAttribs In CommandLineSwitches Then
        strOutput := strOutput + strAttrbutes;
      If clsOwner In CommandLineSwitches Then
        strOutput := strOutput + strOwner;
      strOutput := strOutput + strFilename;
      OutputToConsoleLn(FStdHnd, strOutput, FTitleColour);
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

  This method recurses the directories searching for additional files.

  @precon  None.
  @postcon Recurses the directories searching for additional files.

  @param   strPath    as a String as a constant
  @param   iLevel     as an Integer as a reference
  @param   slPatterns as a TStringList as a constant
  @return  an Int64

**)
Function TSearch.RecurseDirectories(Const strPath: String; Var iLevel: Integer;
  Const slPatterns: TStringList): Int64;

Const
  strZipExt = '*.zip';

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
        If Like(strZipExt, recSearch.Name) Then
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

ResourceString
  strErrorInPathSearchParamString = 'There was an error in the path=search param pairing. (%s)';
  strExclusionsNotFound = 'The filename "%s" was not found to process exclusions in the searches.';

Var
  iPos: Integer;
  strPath: String;
  strSearch: String;
  FPatterns: TStringList;
  i: Integer;
  iPattern: Integer;

Begin
  GetConsoleInformation;
  FRootKey := BuildRootKey(FParams);
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
                OutputToConsoleLn(FStdHnd);
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

  This method saves the colour settings to the INI file.

  @precon  None.
  @postcon Saves the colour settings to the INI file.

**)
Procedure TSearch.SaveSettings;

Var
  iniFile: TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(FRootKey);
  Try
    iniFile.WriteString(strColoursINISection, strSearchPathKey, ColorToString(FSearchPathColour));
    iniFile.WriteString(strColoursINISection, strTitleKey, ColorToString(FTitleColour));
    iniFile.WriteString(strColoursINISection, strHeaderKey, ColorToString(FHeaderColour));
    iniFile.WriteString(strColoursINISection, strFooterKey, ColorToString(FFooterColour));
    iniFile.WriteString(strColoursINISection, strHelpHeaderKey, ColorToString(FHelpHeaderColour));
    iniFile.WriteString(strColoursINISection, strHelpInfoKey, ColorToString(FHelpInfoColour));
    iniFile.WriteString(strColoursINISection, strHelpTextKey, ColorToString(FHelpTextColour));
    iniFile.WriteString(strColoursINISection, strHelpSwitchKey, ColorToString(FHelpSwitchColour));
    iniFile.WriteString(strColoursINISection, strHelpFootNoteKey, ColorToString(FHelpFootNoteColour));
    iniFile.WriteString(strColoursINISection, strFoundSearchPathKey, ColorToString(FFoundSearchPathColour));
    iniFile.WriteString(strColoursINISection, strFileInfoKey, ColorToString(FFileInfoColour));
    iniFile.WriteString(strColoursINISection, strRegExLineNumbersKey, ColorToString(FRegExLineNumbersColour));
    iniFile.WriteString(strColoursINISection, strRegExLineOutputKey, ColorToString(FRegExLineOutputColour));
    iniFile.WriteString(strColoursINISection, strRegExFindOutputFGKey, ColorToString(FRegExFindOutputFGColour));
    iniFile.WriteString(strColoursINISection, strRegExFindOutputBGKey, ColorToString(FRegExFindOutputBGColour));
    iniFile.WriteString(strColoursINISection, strSummaryOutputKey, ColorToString(FSummaryOutputColour));
    iniFile.WriteString(strColoursINISection, strExceptionKey, ColorToString(FExceptionColour));
    iniFile.WriteString(strColoursINISection, strZipFileKey, ColorToString(FZipFileColour));
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
End;

(**

  999 This is the main recursive routine that searches the currently passed directory for files and then
  if subdirectories are required calls its self on those directories.

  @precon  None.
  @postcon Main recursive routine that searches the currently passed directory for files and then if
           subdirectories are required calls its self on those directories.

  @param   strPath    as a String as a constant
  @param   slPatterns as a TStringList as a constant
  @param   iLevel     as an Integer as a reference
  @return  an Int64

**)
Function TSearch.SearchDirectory(Const strPath: String; Const slPatterns: TStringList;
  Var iLevel: Integer): Int64;

Var
  boolDirPrinted: Boolean;
  iDirFiles: Integer;
  strOutput: String;
  FilesCollection: ISearchFiles;

Begin
  OutputCurrentSearchPath(strPath);
  Inc(iLevel);
  Result := 0;
  iDirFiles := 0;
  Inc(FDirectories);
  boolDirPrinted := False;
  (* Find Files *)
  FilesCollection := TSearchFiles.Create(FilesExceptionHandler, FRegExSearch);
  Try
    Inc(Result, SearchForPatterns(slPatterns, iDirFiles, strPath, FilesCollection));
    If FOrderFilesBy <> obNone Then
      FilesCollection.OrderBy(FOrderFilesBy, FOrderFilesDirection);
    OutputFilesToConsole(strPath, boolDirPrinted, FilesCollection);
  Finally
    FilesCollection := Nil;
  End;
  If Not(clsSummaryLevel In CommandLineSwitches) Then
    If iDirFiles > 0 Then
      OutputToConsoleLn(FStdHnd);
  (* Search sub directories *)
  If clsSubDirectories In CommandLineSwitches Then
    Inc(Result, RecurseDirectories(strPath, iLevel, slPatterns));
  (* Output directory summary levels *)
  If clsSummaryLevel In CommandLineSwitches Then
    Begin
      If (iLevel > -1) And (iLevel <= FSummaryLevel) Then
        If Not((Result = 0) And (clsSupressZeros In CommandLineSwitches)) Then
          Begin
            strOutput := Format('%s%s%s', [FormatSize(Result),
              StringOfChar(#32, 2 + 2 * iLevel), strPath]);
            OutputToConsoleLn(FStdHnd, strOutput + StringOfChar(#32,
                FWidth - 1 - Length(strOutput)), FSummaryOutputColour);
          End;
    End;
  Dec(iLevel);
End;

(**

  This method search the current directory for files which match all the specified search patterns.

  @precon  None.
  @postcon Search the current directory for files which match all the specified search patterns.

  @param   slPatterns      as a TStringList as a constant
  @param   iDirFiles       as an Integer as a constant
  @param   strPath         as a String as a constant
  @param   FilesCollection as an ISearchFiles as a constant
  @return  an Int64

**)
Function TSearch.SearchForPatterns(Const slPatterns: TStringList; Const iDirFiles: Integer;
  Const strPath: String; Const FilesCollection: ISearchFiles): Int64;

Var
  iPattern: Integer;
  strOwner: String;
  recSearch: TSearchRec;
  iResult: Integer;
  boolFound: Boolean;
  ST: TSystemTime;
  dtDate: TDateTime;
  iLDirFiles: Integer;

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
            iLDirFiles := iDirFiles; //: @bug Is this a bug which does not capture increments?
            If boolFound Then
              Inc(Result, CheckFiles(recSearch, iLDirFiles, strPath, strOwner, FilesCollection));
            iResult := FindNext(recSearch);
            boolFound := True;
          End;
      Finally
        SysUtils.FindClose(recSearch);
      End;
    End;
End;

(**

  This method search the zip file for files which match all the specified search patterns.

  @precon  None.
  @postcon Search the zip file for files which match all the specified search patterns.

  @param   strFileName     as a String as a constant
  @param   slPatterns      as a TStringList as a constant
  @param   iDirFiles       as an Integer as a constant
  @param   strPath         as a String as a constant
  @param   FilesCollection as an ISearchFiles as a constant
  @return  an Int64

**)
Function TSearch.SearchForPatternsInZip(Const strFileName: String; Const slPatterns: TStringList;
  Const iDirFiles: Integer; Const strPath: String; Const FilesCollection: ISearchFiles): Int64;

Var
  Z: TZipForge;
  iPattern: Integer;
  boolResult: Boolean;
  boolFound: Boolean;
  ZFAI: TZFArchiveItem;
  iSize: Int64;
  iDateTime: Integer;
  strOwner: String;
  iLDirFiles: Integer;

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
                    iLDirFiles := iDirFiles;
                    If boolFound Then
                      Inc(Result, CheckZipFiles(Z, ZFAI, iLDirFiles, strOwner, FilesCollection));
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

  @param   strFileName as a String as a constant
  @param   slPatterns  as a TStringList as a constant
  @param   iLevel      as an Integer as a reference
  @return  an Int64

**)
Function TSearch.SearchZip(Const strFileName: String; Const slPatterns: TStringList;
  Var iLevel: Integer): Int64;

Var
  boolDirPrinted: Boolean;
  iDirFiles: Integer;
  FilesCollection: ISearchFiles;
  PathCollections: TInterfaceList;
  iPath: Integer;
  PathCollection: ISearchFiles;
  iFile: Integer;
  strPath: String;
  Files: ISearchFiles;

Begin
  OutputCurrentSearchPath(strFileName);
  Inc(iLevel);
  Result := 0;
  iDirFiles := 0;
  Inc(FDirectories);
  boolDirPrinted := False;
  (* Find Files in Zip *)
  FilesCollection := TSearchFiles.Create(FilesExceptionHandler, FRegExSearch);
  Try
    Inc(Result, SearchForPatternsInZip(strFileName, slPatterns, iDirFiles, strFileName,
        FilesCollection));
    // Break down files into individual paths
    FilesCollection.OrderBy(obName, odAscending);
    PathCollections := TInterfaceList.Create;
    Try
      strPath := '';
      Files := Nil;
      For iFile := 0 To FilesCollection.Count - 1 Do
        Begin
          If (strPath <> ExtractFilePath(FilesCollection.FileInfo[iFile].FileName)) Or
            (Files = Nil) Then
            Begin
              Files := TSearchFiles.Create(FilesExceptionHandler, FRegExSearch);
              PathCollections.Add(Files);
              strPath := ExtractFilePath(FilesCollection.FileInfo[iFile].FileName);
              Files.Path := strPath;
            End;
          If Files <> Nil Then
            Files.Add(FilesCollection.FileInfo[iFile].Clone);
        End;
      For iPath := 0 To PathCollections.Count - 1 Do
        Begin
          PathCollection := PathCollections[iPath] As ISearchFiles;
          If FOrderFilesBy <> obNone Then
            PathCollection.OrderBy(FOrderFilesBy, FOrderFilesDirection);
          OutputFilesToConsole(strFileName + '\' + PathCollection.Path, boolDirPrinted, PathCollection);
        End;
    Finally
      PathCollections.Free;
    End;
  Finally
    FilesCollection := Nil;
  End;
  If Not(clsSummaryLevel In CommandLineSwitches) Then
    If iDirFiles > 0 Then
      OutputToConsoleLn(FStdHnd);
  Dec(iLevel);
End;

(**

  This method provides a workaround for identifying the size of large files.

  @precon  None.
  @postcon Provides a workaround for identifying the size of large files.

  @param   iSize     as an Int64 as a reference
  @param   recSearch as a TSearchRec as a constant

**)
Procedure TSearch.WorkaroundLargeFiles(Var iSize: Int64; Const recSearch: TSearchRec);

Begin
  // Workaround for files larger than 2,147,483,647 bytes
  iSize := Int64(recSearch.FindData.nFileSizeHigh) * Int64(MAXDWORD);
  iSize := iSize + recSearch.FindData.nFileSizeLow;
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

ResourceString
  strDiskVolumeFull = 'Disk volume %d full [%s]';

Begin
  AddErrorToLog(Format(strDiskVolumeFull,
    [VolumeNumber, VolumeFileName]), (Sender As TZipForge).FileName);
  Cancel := True;
End;

//=================================================================================================

End.
