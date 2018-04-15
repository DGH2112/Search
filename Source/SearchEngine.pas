(**

  This module contains the main code used nby SEARCH to, yes, search for files.

  @Author  David Hoyle
  @Version 1.0
  @Date    15 Apr 2018

  @todo    Add switch for RegEx filename matching
  @todo    Check that GREP will work with multi-line matches
  @todo    Allow a switch to change a default colour
  
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
    FFileAttrs: TSearchFileAttrs;
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
    FGrepCount : Integer;
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
    FWarningColour: TColor;
    FExceptionColour: TColor;
    FZipFileColour: TColor;
    FErrorLog : TStringList;
    FUNCPath : Boolean;
  Strict Protected
    Procedure OutputDateTime(Const SearchFile: ISearchFile; Var strOutput: String);
    Procedure OutputSize(Const SearchFile: ISearchFile; Const boolHasCompressed : Boolean;
      Var strOutput: String);
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
    Function  CheckFiles(Const recSearch: TSearchRec; Const setAttrs : TSearchFileAttrs;
      Var iDirFiles: Integer; Const strPath, strOwner: String;
      Const FilesCollection: ISearchFiles): Int64;
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
    Procedure PrintHelpSyntax;
    Procedure PrintHelpHelp;
    Procedure PrintHelpShowAttributes;
    Procedure PrintHelpRecurseSubdirectories;
    Procedure PrintHelpSummarise;
    Procedure PrintHelpHideEmptySummarise;
    Procedure PrintHelpFilterByAttributes;
    Procedure PrintHelpFilterByDate;
    Procedure PrintHelpFilterBySize;
    Procedure PrintHelpQuietMode;
    Procedure PrintHelpShowOwner;
    Procedure PrintHelpOrderBy;
    Procedure PrintHelpDateType;
    Procedure PrintHelpGrepSearch;
    Procedure PrintHelpDisplayCriteria;
    Procedure PrintHelpExclusions;
    Procedure PrintHelpSearchWithinZips;
    Procedure PrintHelpFileOutputSize;
    Procedure PrintHelpOutputInCSV;
    Procedure PrintHelpFooter;
    // ISearchEngine;
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
  (** An ini key for the Warning colour **)
  strWarningKey = 'Warning';
  (** An ini key for the Exception colour **)
  strExceptionKey = 'Exception';
  (** An ini key for the Zip File colour **)
  strZipFileKey = 'ZipFile';
  (** An output format for all dates. **)
  strOutputDateFmt = 'ddd dd/mmm/yyyy hh:mm:ss';
  (** This is the width of the help information. **)
  iWidth = 14;
  (** This is the indent for the main helptext **)
  iTextIndent = 16;
  (** This is the indent for the main help items. **)
  iMainIndent = 2;
  (** This is the indent for the minor help items. **)
  iMinorIndent = 4;

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
  @param   setAttrs        as a TSearchFileAttrs as a constant
  @param   iDirFiles       as an Integer as a reference
  @param   strPath         as a String as a constant
  @param   strOwner        as a String as a constant
  @param   FilesCollection as an ISearchFiles as a constant
  @return  an Int64

**)
Function TSearch.CheckFiles(Const recSearch: TSearchRec; Const setAttrs : TSearchFileAttrs;
  Var iDirFiles: Integer; Const strPath, strOwner: String; Const FilesCollection: ISearchFiles): Int64;

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
  iCompressedSize: Int64;
  iFileSizeHigh: DWORD;
  iFileSizeLow: DWORD;

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
      If Not FileTimeToDosDateTime(dtDateLocal, LongRec(iTime).Hi, LongRec(iTime).Lo) Then
        iTime := 0;
      iCompressedSize := 0;
      If [sfaFile, sfaCompressed] <= setAttrs Then
        Begin
          iFileSizeLow := GetCompressedFileSize(PChar(strPath + recSearch.Name), @iFileSizeHigh);
          iCompressedSize := Int64(iFileSizeHigh) * Int64(MAXDWORD);
          iCompressedSize := iCompressedSize + iFileSizeLow;
        End;
      boolAdded := FilesCollection.Add(TSearchFileRec.Create(recSearch.TimeStamp, recSearch.Size, 
        iCompressedSize, setAttrs, strOwner, recSearch.Name), GetFileText, FGREPCount);
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
      boolAdded := FilesCollection.Add(TSearchFileRec.Create(FileDateToDateTime(iTime),
        ZFAI.UncompressedSize, ZFAI.CompressedSize, FileAttrsToAttrsSet(ZFAI.ExternalFileAttributes),
        strOwner, ZFAI.StoredPath + ZFAI.FileName), GetZipFileText, FGREPCount);
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
  FGrepCount := 0;
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
    iReductionLoopLimit = 4;

  Var
    iReductions: Integer;
    strKMG: String;
    iLSize: Int64;

  Begin
    iLSize := iSize;
    iReductions := 0;
    If iLSize > 0 Then
      While (iLSize Mod iKiloByte = 0) And (iReductions < iReductionLoopLimit) Do
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
    strNormal =     '        Normal';
    strDevice =     '        Device';
    strTemporary =  '        Temporary';
    strSparse =     '        Sparse';
    strReparse =    '        Reparse';
    strCompressed = '        Compressed';
    strOffline =    '        Offline';
    strIndexed =    '        Indexed';
    strEncrypted =  '        Encrypted';
    strVirtual =    '        Virtual';

  Begin
    If DisplayText(clsAttrRange, 't', strSearchingForFiles) Then
      Begin
        If sfaReadOnly in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strReadOnly, FHelpInfoColour);
        If sfaArchive in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strArchive, FHelpInfoColour);
        If sfaSystem in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strSystemFile, FHelpInfoColour);
        If sfaHidden in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strHidden, FHelpInfoColour);
        If sfaFile in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strFile, FHelpInfoColour);
        If sfaDirectory in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strDirectory, FHelpInfoColour);
        If sfaVolume in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strVolume, FHelpInfoColour);
        If sfaNormal in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strNormal, FHelpInfoColour);
        If sfaDevice in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strDevice, FHelpInfoColour);
        If sfaTemporary in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strTemporary, FHelpInfoColour);
        If sfaSparse in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strSparse, FHelpInfoColour);
        If sfaReparse in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strReparse, FHelpInfoColour);
        If sfaCompressed in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strCompressed, FHelpInfoColour);
        If sfaOffLine in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strOffLine, FHelpInfoColour);
        If sfaIndexed in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strIndexed, FHelpInfoColour);
        If sfaEncrypted in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strEncrypted, FHelpInfoColour);
        If sfaVirtual in FFileAttrs Then
          OutputToConsoleLn(FStdHnd, strVirtual, FHelpInfoColour);
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
  strNoSearchPattern = 'No search pattern provided! Searching for all files...';

Const
  iSwitchLetterStart = 2;

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
          iIndex := iSwitchLetterStart;
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
                't', 'T': GetAttributes(FParams, iSwitch, iIndex, FFileAttrs);
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
      Begin
        OutputToConsoleLn(FStdHnd, strNoSearchPattern,
          FWarningColour);
        OutputToConsoleLn(FStdHnd);
        FSearchParams.AddPair(GetCurrentDir, '*.*');
      End;
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
  strDefaultWarning = 'clYellow';
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
    FWarningColour := StringToColor(iniFile.ReadString(strColoursINISection, strWarningKey, strDefaultWarning));
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

  @param   SearchFile as an ISearchFile as a constant
  @param   strOutput  as a String as a reference

**)
Procedure TSearch.OutputDateTime(Const SearchFile: ISearchFile; Var strOutput: String);

Begin
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    Begin
      If Not (sfaDirectory In SearchFile.Attributes) Then
        strOutput := strOutput + Format('  %s', [FormatDateTime(strOutputDateFmt, SearchFile.Date)])
      Else
        strOutput := strOutput + Format('  %s', [FormatDateTime(strOutputDateFmt, SearchFile.Date)]);
    End
  Else
    Begin
      If Not (sfaDirectory In SearchFile.Attributes) Then
        strOutput := strOutput + Format('%s,', [FormatDateTime(strOutputDateFmt, SearchFile.Date)])
      Else
        strOutput := strOutput + Format('%s,', [FormatDateTime(strOutputDateFmt, SearchFile.Date)]);
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
      strOutput := strOutput + Format('%s  ', [OutputAttributes(FilesCollection.FileInfo[i].Attributes)])
    Else
      strOutput := strOutput + Format('%s,', [OutputAttributes(FilesCollection.FileInfo[i].Attributes)]);
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
      strOutput := '';
      OutputDateTime(FilesCollection.FileInfo[iFile], strOutput);
      OutputSize(FilesCollection.FileInfo[iFile], FilesCollection.HasCompressed, strOutput);
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
            For iLine := Min(slText.Count, M.LineNum + 1) To Min(slText.Count, M.LineNum + FRegExSurroundingLines) Do
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

  This method outputs the size and optionally the cmopressed size of the files to the given output
  string.

  @precon  SearchFile must be a valid instance.
  @postcon The size and compressed size of the file is output to the geivne output string.

  @param   SearchFile        as an ISearchFile as a constant
  @param   boolHasCompressed as a Boolean as a constant
  @param   strOutput         as a String as a reference

**)
Procedure TSearch.OutputSize(Const SearchFile: ISearchFile; Const boolHasCompressed : Boolean;
  Var strOutput: String);

Const
  strDIR = '<DIR>';
  iWidthOfCompressed = 17;

Begin
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    Begin
      If Not (sfaDirectory In SearchFile.Attributes) Then
        Begin
          strOutput := strOutput + Format('  %s  ', [FormatSize(SearchFile.Size)]);
          If boolHasCompressed Then
            If sfaCompressed In SearchFile.Attributes Then
              strOutput := strOutput + Format('[%s]  ', [FormatSize(SearchFile.CompressedSize)])
            Else
              strOutput := strOutput + Format('%s  ', [StringOfChar(#32, iWidthOfCompressed)]);
        End Else
        Begin
          strOutput := strOutput + Format('  %15s  ', [strDIR]);
          If boolHasCompressed Then
            strOutput := strOutput + Format('%s  ', [StringOfChar(#32, iWidthOfCompressed)]);
        End;
    End
  Else
    Begin
      If Not (sfaDirectory In SearchFile.Attributes) Then
        Begin
          strOutput := strOutput + Format('%d,', [SearchFile.Size]);
          If sfaCompressed In SearchFile.Attributes Then
            strOutput := strOutput + Format('%d,', [SearchFile.CompressedSize])
            Else
              strOutput := strOutput + ',';
        End Else
        Begin
          strOutput := strOutput + Format('%d,', [0]);
          If boolHasCompressed Then
            strOutput := strOutput + ',';
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
  strGREPResults = 'Found %1.0n RegEx Matches, ';
  strSearchResults = 'Found %s bytes in %1.0n Files in %1.0n Directories.';
  strMsg2 = '  Total space %s bytes and %s bytes free.';

Var
  iFreeBytesAvailableToCaller, iTotalNumberOfBytes, iTotalNumberOfFreeBytes: Int64;

Begin
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    Begin
      GetDiskFreeSpaceEx(PChar(strPath), iFreeBytesAvailableToCaller, iTotalNumberOfBytes,
        @iTotalNumberOfFreeBytes);
      If CheckConsoleMode(FStdHnd) Then
        OutputToConsole(FStdHnd, StringOfChar(#32, FWidth - 1), clNone, clNone, False);
      OutputToConsole(FStdHnd, #32#32);
      If clsRegExSearch In CommandLineSwitches Then
        OutputToConsole(FStdHnd, Format(strGREPResults, [Int(FGREPCount)]), FFooterColour);
      OutputToConsoleLn(FStdHnd, Format(strSearchResults, [Trim(FormatSize(FFileSize)), Int(FFiles),
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

Begin
  PrintHelpSyntax;
  PrintHelpHelp;
  PrintHelpShowAttributes;
  PrintHelpRecurseSubdirectories;
  PrintHelpSummarise;
  PrintHelpHideEmptySummarise;
  PrintHelpFilterByAttributes;
  PrintHelpFilterByDate;
  PrintHelpFilterBySize;
  PrintHelpQuietMode;
  PrintHelpShowOwner;
  PrintHelpOrderBy;
  PrintHelpDateType;
  PrintHelpGrepSearch;
  PrintHelpDisplayCriteria;
  PrintHelpExclusions;
  PrintHelpSearchWithinZips;
  PrintHelpFileOutputSize;
  PrintHelpOutputInCSV;
  PrintHelpFooter;
End;

(**

  This method outputs the command line switches for changing the type of date displayed and filtered for
  to the console.

  @precon  None.
  @postcon The command line switches for changing the type of date display and filterd for is output to
           the console.

**)
Procedure TSearch.PrintHelpDateType;

ResourceString
  strSwitch = '/E:CAW';
  strDescription = 'Type of file date to display and search on:';
  strOptions = 'C = Creation, A = Last Access, W = Last Write (Default)';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
  OutputToConsoleLn(FStdHnd, Indent(strOptions, iTextIndent + iMainIndent, 0), FHelpInfoColour);
End;

(**

  This method outputs the command line switches for displaying the search criteria to the console.

  @precon  None.
  @postcon The command line switches for displaying the search criteria are output to the console.

**)
Procedure TSearch.PrintHelpDisplayCriteria;

ResourceString
  strSwitch = '/C';
  strDescription = 'Display a list of Criteria and Command Line Options.';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
End;

(**

  This method outputs the command line switches for exclusing files from the search to the console.

  @precon  None.
  @postcon The command line switches for excluding files from the search are output to the console.

**)
Procedure TSearch.PrintHelpExclusions;

ResourceString
  strSwitch = '/X[FileName]';
  strDescription = 'A list of exclusions to apply to paths and filenames.';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
End;

(**

  This method prints the command line switch for setting the file output size format to the console.

  @precon  None.
  @postcon The command line switch for setting the file output size is output to the console.

**)
Procedure TSearch.PrintHelpFileOutputSize;

ResourceString
  strSwitch = '/F:KMGT';
  strDescription = 'Changes the output size format to [K]ilo, [M]ega, [G]iga or [T]erabytes.';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
End;

(**

  This method prints the command line switch for filtering files by their attributes to the console.

  @precon  None.
  @postcon The command line switch for filtering files by their attributes is output to the console.

**)
Procedure TSearch.PrintHelpFilterByAttributes;

ResourceString
  strSwitch = '/T[RASHFDV]';
  strDescription = 'Show only files with certain attributes';
  strOptions1 = 'R = Read Only, A = Archive,    S = System,    H = Hidden,';
  strOptions2 = 'F = File,      D = Directory,  V = Volume ID,';
  strOptions3 = 'd = Device,    N = Normal,     T = Temporary, s = Sparse,';
  strOptions4 = 'r = Reparse,   C = Compressed, O = Offline,   I = Not Indexed,';
  strOptions5 = 'E = Encrypted, v = Virtual';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
  OutputToConsoleLn(FStdHnd, Indent(strOptions1, iTextIndent + iMainIndent, 0), FHelpInfoColour);
  OutputToConsoleLn(FStdHnd, Indent(strOptions2, iTextIndent + iMainIndent, 0), FHelpInfoColour);
  OutputToConsoleLn(FStdHnd, Indent(strOptions3, iTextIndent + iMainIndent, 0), FHelpInfoColour);
  OutputToConsoleLn(FStdHnd, Indent(strOptions4, iTextIndent + iMainIndent, 0), FHelpInfoColour);
  OutputToConsoleLn(FStdHnd, Indent(strOptions5, iTextIndent + iMainIndent, 0), FHelpInfoColour);
End;

(**

  This method prints the command line switch for filtering files by their dates to the console.

  @precon  None.
  @postcon The command line switch for filtering files by their dates is output to the console.

**)
Procedure TSearch.PrintHelpFilterByDate;

ResourceString
  strSwitch = '/D[{l}-{u}]';
  strDescription = 'Searches for files between dates (l) and (u).';
  strOptions = 'l = lower bounding date and time, u = upper bounding date and time';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
  OutputToConsoleLn(FStdHnd, Indent(strOptions, iTextIndent + iMainIndent, 0), FHelpInfoColour);
End;

(**

  This method prints the command line switch for filtering files by their size to the console.

  @precon  None.
  @postcon The command line switch for filtering files by their size is output to the console.

**)
Procedure TSearch.PrintHelpFilterBySize;

ResourceString
  strSwitch = '/Z[{l}-{u}]';
  strDescription = 'Searches for files between sizes (l) and (u).';
  strOptions = 'l = lower bounding size in bytes, u = upper bounding size in bytes';
  strFootNote = 'Sizes can be postfixed with k, m, g or t for kilo, mega, giga and terabytes';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
  OutputToConsoleLn(FStdHnd, Indent(strOptions, iTextIndent + iMainIndent, 0), FHelpInfoColour);
  OutputToConsoleLn(FStdHnd, Indent(strFootNote, iTextIndent + iMainIndent, 0), FHelpInfoColour);
End;

(**

  This method prints the help footer.

  @precon  None.
  @postcon The help footer is printed to the console.

**)
Procedure TSearch.PrintHelpFooter;

ResourceString
  strFooterText = 'NOTE: The date time input format is dependent on your local settings';

Begin
  OutputToConsoleLn(FStdHnd, strFooterText, FHelpFootNoteColour);
  OutputToConsoleLn(FStdHnd);
End;

(**

  This method prints the command line switch for search for text within files to the console.

  @precon  None.
  @postcon The command line switch for search for text within files is output to the console.

**)
Procedure TSearch.PrintHelpGrepSearch;

ResourceString
  strSwitch = '/I[text]';
  strDescription = 'Search within files for text using Perl regular expressions.';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
End;

(**

  This method prints the command line switch for displaying help to the console.

  @precon  None.
  @postcon The command line switch for displaying help is output to the console.

**)
Procedure TSearch.PrintHelpHelp;

ResourceString
  strSwitch = '/?';
  strDescription = 'This help screen';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
End;

(**

  This method prints the command line switch for hiding empty summaries to the console.

  @precon  None.
  @postcon The command line switch for hiding empty summaries is output to the console.

**)
Procedure TSearch.PrintHelpHideEmptySummarise;

ResourceString
  strSwitch = '/0';
  strDescription = 'Hide empty Summarised subdirectories';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
End;

(**

  This method prints the command line switch for ordering files to the console.

  @precon  None.
  @postcon The command line switch for ordering files is output to the console.

**)
Procedure TSearch.PrintHelpOrderBy;

ResourceString
  strSwitch = '/O:NADOS';
  strDescription = 'Order files ascending [+] and descending [-]';
  strOptions = 'N = Name, D = Date and Time, O = Owner, S = Size';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
  OutputToConsoleLn(FStdHnd, Indent(strOptions, iTextIndent + iMainIndent, 0), FHelpInfoColour);
End;

(**

  This method prints the command line switch for outputting the search in CSV format.

  @precon  None.
  @postcon The command line switch for outputting the search in CSV format is output to the console.

**)
Procedure TSearch.PrintHelpOutputInCSV;

ResourceString
  strSwitch = '/V';
  strDescription = 'Output the found information in CSV format.';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
  OutputToConsoleLn(FStdHnd);
End;

(**

  This method prints the command line switch for outputting in quiet mode to the console.

  @precon  None.
  @postcon The command line switch for outputting in quiet mode is output to the console.

**)
Procedure TSearch.PrintHelpQuietMode;

ResourceString
  strSwitch = '/Q';
  strDescription = 'Quiet mode';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
End;

(**

  This method prints the command line switch for recursing subdirectories to the console.

  @precon  None.
  @postcon The command line switch for recursing subdirectories is output to the console.

**)
Procedure TSearch.PrintHelpRecurseSubdirectories;

ResourceString
  strSwitch = '/S';
  strDescription = 'Recurse subdirectories';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
End;

(**

  This method print to the console the help information for enabling searching within zip files.

  @precon  None.
  @postcon The inform ation for searching within ZIP files is output to the console.

**)
Procedure TSearch.PrintHelpSearchWithinZips;

ResourceString
  strSwitch = '/P';
  strDescription = 'Enables the searching within ZIP archives.';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
End;

(**

  This method prints the command line switch for showing attributes to the console.

  @precon  None.
  @postcon The command line switch for showing attributes is output to the console.

**)
Procedure TSearch.PrintHelpShowAttributes;

ResourceString
  strSwitch = '/A';
  strDescription = 'Show file and directory attributes';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
End;

(**

  This method prints the command line switch for searching for file withn an owner to the console.

  @precon  None.
  @postcon The command line switch for searching for file withn an owner is output to the console.

**)
Procedure TSearch.PrintHelpShowOwner;

ResourceString
  strSwitch = '/W {[Owner]}';
  strDescription = 'Owner Information. The owner search can be a wildcard search with an *.';
  strOptions = 'Searches beginning with ! force a negated criteria.';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
  OutputToConsoleLn(FStdHnd, Indent(strOptions, iTextIndent, 0));
End;

(**

  This method prints the command line switch for summarising results at directory level to the console.

  @precon  None.
  @postcon The command line switch for summarising results at directory level is output to the console.

**)
Procedure TSearch.PrintHelpSummarise;

ResourceString
  strSwitch = '/1..9';
  strDescription = 'Summarise subdirectories';

Begin
  OutputToConsole(FStdHnd, Indent(strSwitch, iMainIndent, iWidth), FHelpSwitchColour);
  OutputToConsoleLn(FStdHnd, strDescription, FHelpTextColour);
End;

(**

  This method prints the Syntax of the command line options to the console.

  @precon  None.
  @postcon The syntax of the command line otions are printed to the console.

**)
Procedure TSearch.PrintHelpSyntax;

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

Const
  iUNCPrefixLen = 2;

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
  PrintTitle;
  GetCommandLineSwitches;
  If Not (clsOutputAsCSV In CommandLineSwitches) Then
    If clsDisplayCriteria In CommandLineSwitches Then
      DisplayCriteria;
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
                FUNCPath := Copy(strPath, 1, iUNCPrefixLen) = '\\';
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
    iniFile.WriteString(strColoursINISection, strWarningKey, ColorToString(FWarningColour));
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

Const
  iSummaryIndent = 2;
  iSummaryPadding = 2;

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
              StringOfChar(#32, iSummaryPadding + iSummaryIndent * iLevel), strPath]);
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
  setAttributes : TSearchFileAttrs;

Begin
  Result := 0;
  For iPattern := 0 To slPatterns.Count - 1 Do
    Begin
      iResult := FindFirst(strPath + slPatterns[iPattern], faAnyFile, recSearch);
      Try
        While iResult = 0 Do
          Begin
            boolFound := True;
            setAttributes := FileAttrsToAttrsSet(GetFileAttributes(PChar(strPath + recSearch.Name)));
            CheckFileAttributes(FFileAttrs, setAttributes, boolFound);
            CheckSizeRange(recSearch.Size, FLSize, FUSize, boolFound);
            Case FDateType Of
              dtCreation:
                Begin
                  FileTimeToSystemTime(recSearch.FindData.ftCreationTime, ST);
                  dtDate := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) + EncodeTime
                    (ST.wHour, ST.wMinute, ST.wSecond, ST.wMilliseconds);
                  CheckDateRange(dtDate, FLDate, FUDate, boolFound);
                End;
              dtLastAccess:
                Begin
                  FileTimeToSystemTime(recSearch.FindData.ftLastAccessTime, ST);
                  dtDate := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) + EncodeTime
                    (ST.wHour, ST.wMinute, ST.wSecond, ST.wMilliseconds);
                  CheckDateRange(dtDate, FLDate, FUDate, boolFound);
                End
              Else
                CheckDateRange(recSearch.TimeStamp, FLDate, FUDate, boolFound);
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
              Inc(Result, CheckFiles(recSearch, setAttributes, iLDirFiles, strPath,
                strOwner, FilesCollection));
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
                    CheckFileAttributes(FileAttrsToAttrsSet(ZFAI.ExternalFileAttributes), FFileAttrs,
                      boolFound);
                    CheckSizeRange(iSize, FLSize, FUSize, boolFound);
                    // Zip files only contain the last write date of a file.
                    LongRec(iDateTime).Lo := ZFAI.LastModFileTime;
                    LongRec(iDateTime).Hi := ZFAI.LastModFileDate;
                    CheckDateRange(FileDateToDateTime(iDateTime), FLDate, FUDate, boolFound);
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
      On E : Exception Do //: @todo Add event handler to negate this!
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
