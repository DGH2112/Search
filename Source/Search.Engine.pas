(**

  This module contains the main code used nby SEARCH to, yes, search for files.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Jun 2018

**)
Unit Search.Engine;

Interface

Uses
  SysUtils,
  Classes,
  Windows,
  ZipForge,
  Graphics,
  Search.Functions,
  Search.Types,
  Search.Interfaces, 
  System.RegularExpressions, 
  Search.Options;

Type 
  (** This class defines the working that searches the directories and files
   for the information that matches the criteria. **)
  TSearch = Class(TInterfacedObject, ISearchEngine)
  Strict Private
    //: @todo Add the ability to show how many files in a total number of file.
    FFoundFiles: Integer;       // The total number of file found by the search
    FTotalFiles: Integer;       // The total number of files searched
    FDirectories: Integer;      // The total number of directories found by the search
    FFileSize: Int64;           // The total size of al the files found in the search
    FSearchParams: TStringList; // This is a list of search parameters
    FOwnerRegEx : TRegEx;       // A regular expression record to matchnig owner names
    FRootKey: String;
    FParams: TStringList;
    FLevel: Integer;
    FGrepCount : Integer;
    FErrorLog : TStringList;
    FUNCPath : Boolean;
    FColourSettings : TStringList;
    FConsole : ISearchConsole;
    FOptions : TSearchOptions;
  Strict Protected
    Procedure OutputDateTime(Const SearchFile: ISearchFile; Var strOutput: String);
    Procedure OutputSize(Const SearchFile: ISearchFile; Const boolHasCompressed : Boolean;
      Var strOutput: String);
    Procedure OutputFileAttributes(Const i: Integer; Const FilesCollection: ISearchFiles;
      Var strOutput: String);
    Procedure OutputFileOwner(Const SearchFile: ISearchFile; Const iOwnerWidth : Integer;
      Var strOutput: String);
    Procedure OutputRegExInformation(Const strPath : String; Const boolRegEx: Boolean;
      Const FileInfo: ISearchFile);
    Procedure OutputDirectoryOrZIPFile(Var boolDirPrinted: Boolean; Const strPath: String);
    Procedure OutputCurrentSearchPath(Const strPath: String);
    Procedure PrintTitle;
    Procedure PrintHeader(Const strPath, strPattern: String);
    Procedure PrintFooter(Const strPath: String);
    Procedure GetCommandLineSwitches;
    Function  LookupAccountBySID(Const SID: PSID): String;
    Function  OutputOwner(Const strFileName: String): String;
    Function  CheckFiles(Const recSearch: TSearchRec; Const setAttrs : TSearchFileAttrs;
      Const strPath, strOwner: String; Const FilesCollection: ISearchFiles): Int64;
    Function  CheckZipFiles(Const ZipArchive: TZipForge; Const ZFAI: TZFArchiveItem;
      Var iDirFiles: Integer; Const strOwner: String;
      Const FilesCollection: ISearchFiles): Int64;
    Procedure OutputFilesToConsole(Const strPath: String; Var boolDirPrinted: Boolean;
      Const FilesCollection: ISearchFiles);
    Function  RecurseDirectories(Const strPath: String; Var iLevel: Integer;
      Const slPatterns: TStringList): Int64;
    Function  SearchForPatterns(Const slPatterns: TStringList; Const strPath: String;
      Const FilesCollection: ISearchFiles): Int64;
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
    Procedure ProcessZipFileFailure(Sender : TObject; FileName : String;
      Operation : TZFProcessOperation; NativeError : Integer; ErrorCode : Integer;
      ErrorMessage : String; var Action : TZFAction);
    Procedure ZipDiskFull(Sender : TObject; VolumeNumber : Integer;
      VolumeFileName : String; var Cancel : Boolean);
    Procedure OutputConfiguredColours;
    Procedure ConfigureColour;
    Function  CountFiles(const strPath: String): Integer;
    Procedure SafeLoadFromFile(Const slText : TStringList; Const strFileName: String);
    Procedure PrintErrorLog;
    // ISearchEngine;
    Procedure Run;
    Function  GetSearchConsole : ISearchConsole;
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

  (** A type for a pointer to a PSID structure **)
  PPSID = ^PSID;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  System.Contnrs,
  WinApi.ACCCTRL,
  WinApi.ActiveX,
  System.IniFiles,
  System.Math,
  Search.RegExMatches,
  Search.FilesCls,
  System.TypInfo, 
  Search.Constants, 
  Search.StrUtils, 
  System.RegularExpressionsCore, 
  Search.Help, 
  Search.Console, 
  Search.DisplayCriteria;

Const
  (** An ini section for the console colours. **)
  strColoursINISection = 'Colours';
  (** An ini key for the None colour - this should NOT be changed from clNone. **)
  strNoneKey = 'None';
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
  strSuccessKey = 'Success';
  (** An ini key for the Warning colour **)
  strWarningKey = 'Warning';
  (** An ini key for the Exception colour **)
  strExceptionKey = 'Exception';
  (** An ini key for the Zip File colour **)
  strZipFileKey = 'ZipFile';
  (** An ini key for the Input colour **)
  strInputKey = 'Input';

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
Function GetNamedSecurityInfo(pObjectName: PAnsiChar; ObjectType: SE_OBJECT_TYPE; //FI:C102
  SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PPSID;
  ppDacl, ppSacl: PACL; Var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD; Stdcall;
  External 'advapi32.dll' Name 'GetNamedSecurityInfoA';

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
  @param   strPath         as a String as a constant
  @param   strOwner        as a String as a constant
  @param   FilesCollection as an ISearchFiles as a constant
  @return  an Int64

**)
Function TSearch.CheckFiles(Const recSearch: TSearchRec; Const setAttrs : TSearchFileAttrs;
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
          SafeLoadFromFile(sl, strPath + recSearch.Name);
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
  If Not (clsSummaryLevel In CommandLineSwitches) Then
    Begin
      Case FOptions.FDateType Of
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
      Inc(FFoundFiles);
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
      Inc(FFoundFiles);
      Inc(FFileSize, ZFAI.UncompressedSize);
      Inc(Result, ZFAI.UncompressedSize);
    End;
End;

(**

  This method configures a colour for a specific output as provided on the command line.

  @precon  None.
  @postcon The specified output colour us updated.

**)
Procedure TSearch.ConfigureColour;

ResourceString
  strInvalidUseColourName = 'The output colour name "%s" is not valid. Use the command line switch /' + 
    'L so see valid colour names';
  strInvalidUseColourValue = 'The output colour value "%s" is not valid. Use the command line switch /' + 
    'L so see valid colour values';
  strColoursUpdated1 = 'Colour name "';
  strcoloursUpdated2 = '" updated to colour value "';
  strColoursUpdated3 = '"!';

Var
  iColour: TSearchColour;
  iColourItem : TSearchColourList;
  iColourName : TSearchColour;
  iColourValue : TSearchColourList;

Begin
  iColourName := scNone;
  For iColour := Low(TSearchColour) To High(TSearchColour) Do
    If CompareText(strSearchColour[iColour], FColourSettings.Names[0]) = 0 Then
      Begin
        iColourName := iColour;
        Break;
      End;
  If iColourName = scNone Then
    Raise ESearchException.CreateFmt(strInvalidUseColourName, [FColourSettings.Names[0]]);
  iColourValue := sclNone;
  For iColourItem := Low(TSearchColourList) To High(TSearchColourList) Do
    If CompareText(strSearchColourList[iColourItem], FColourSettings.ValueFromIndex[0]) = 0 Then
      Begin
        iColourValue := iColourItem;
        Break;
      End;
  If iColourValue = sclNone Then
    Raise ESearchException.CreateFmt(strInvalidUseColourValue, [FColourSettings.ValueFromIndex[0]]);
  FConsole.OutputToConsole(coStd, strColoursUpdated1, scSuccess);
  FConsole.OutputToConsole(coStd, FColourSettings.Names[0], iColourName);
  FConsole.Colour[iColourName] := SearchColourList[iColourValue];
  FConsole.OutputToConsole(coStd, strcoloursUpdated2, scSuccess);
  FConsole.OutputToConsole(coStd, FColourSettings.ValueFromIndex[0], iColourName);
  FConsole.OutputToConsoleLn(coStd, strColoursUpdated3, scSuccess);
  FConsole.OutputToConsoleLn(coStd);
End;

(**

  This method counts the number of files in the given directory.

  @precon  None.
  @postcon Returns the number of files in the given directory.

  @param   strPath as a String as a constant
  @return  an Integer

**)
Function TSearch.CountFiles(Const strPath: String): Integer;

Var
  recSearch: TSearchRec;
  iResult: Integer;
  iFileOnly: Integer;

Begin
  Result := 0;
  iResult := FindFirst(strPath + '*.*', faAnyFile, recSearch);
  Try
    While iResult = 0 Do
      Begin
        If recSearch.Attr And faDirectory = 0 Then
          Inc(Result);
        iResult := FindNext(recSearch);
      End;
  Finally
    SysUtils.FindClose(recSearch);
  End;
End;

(**

  This is the constructor method for the TSearch class.

  @precon  None.
  @postcon Initialises the SearchParams and Exclusions stringlists.

**)
Constructor TSearch.Create;

Begin
  FConsole := TSearchConsole.Create;
  FSearchParams := TStringList.Create;
  FOptions.FExclusions := TStringList.Create;
  FParams := TStringList.Create;
  FErrorLog := TStringList.Create;
  FColourSettings := TStringList.Create;
  CoInitialize(Nil);
  FLevel := -1;
  FGrepCount := 0;
  SetRoundMode(rmUp);
  FFoundFiles := 0;
  FTotalFiles := 0;
  FDirectories := 0;
  FFileSize := 0;
End;

(**

  This is the destructor method for the TSearch class.

  @precon  None.
  @postcon Frees the SearchParams and Exclusions string lists.

**)
Destructor TSearch.Destroy;

Begin
  SaveSettings;
  FColourSettings.Free;
  FErrorLog.Free;
  FOptions.FExclusions.Free;
  FSearchParams.Free;
  FParams.Free;
  Inherited Destroy;
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
  FConsole.OutputToConsoleLn(coStd, Format(strFilesException, [strException]), scException);
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
  Case FOptions.FSizeFormat Of
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
  strInvalidCommandLineSwitch = 'Invalid command line switch "%s" in parameter "%s." ' + 
    'Type SEARCH /? for more help!';
  strNeedToSpecifyCriteria = 'You need to specify at least one search criteria. ' +
    'Type SEARCH /? for more help!';

Const
  iSwitchLetterStart = 2;

Var
  iSwitch, iIndex: Integer;

Begin
  FOptions.FSummaryLevel := 0;
  CommandLineSwitches := [];
  iSwitch := 0;
  FOptions.FOrderFilesBy := obNone;
  FOptions.FOrderFilesDirection := odAscending;
  FOptions.FDateType := dtLastWrite;
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
                '1'..'9': GetSummaryLevel(FParams, iSwitch, iIndex, FOptions.FSummaryLevel);
                '0':      Include(CommandLineSwitches, clsSupressZeros);
                'd', 'D': GetDateRange(FParams, iSwitch, iIndex, FOptions.FLDate, FOptions.FUDate);
                'z', 'Z': GetSizeRange(FParams, iSwitch, iIndex, FOptions.FLSize, FOptions.FUSize);
                't', 'T': GetAttributes(FParams, iSwitch, iIndex, FOptions.FFileAttrs);
                'q', 'Q': Include(CommandLineSwitches, clsQuiet);
                'w', 'W': FOptions.FOwnerSearch := GetOwnerSwitch(FParams, iSwitch, iIndex,
                            FOptions.FOwnerRegExSearch);
                'o', 'O': GetOrderBy(FParams, iSwitch, iIndex, FOptions.FOrderFilesDirection,
                            FOptions.FOrderFilesBy);
                'i', 'I': GetSearchInInfo(FParams, iSwitch, iIndex, FOptions.FRegExSearch,
                            FOptions.FRegExSurroundingLines);
                'e', 'E': GetDateType(FParams, iSwitch, iIndex, FOptions.FDateType);
                'c', 'C': Include(CommandLineSwitches, clsDisplayCriteria);
                'x', 'X': GetExclusions(FParams, iSwitch, iIndex, FOptions.FExlFileName);
                'p', 'P': Include(CommandLineSwitches, clsSearchZip);
                'f', 'F': GetSizeFormat(FParams, iSwitch, iIndex, FOptions.FSizeFormat);
                'v', 'V': Include(CommandLineSwitches, clsOutputAsCSV);
                'l', 'L': GetColoursSwitch(FParams, iSwitch, iIndex, FColourSettings);
              Else
                Raise ESearchException.CreateFmt(strInvalidCommandLineSwitch, [FParams[iSwitch][iIndex],
                  FParams[iSwitch]]);
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
  If FParams.Count = 0 Then
    Include(CommandLineSwitches, clsShowHelp);
  If (FSearchParams.Count = 0) And Not([clsShowHelp, clsColours] * CommandLineSwitches <> []) Then
    If FParams.Count > 0 Then
      Raise ESearchException.Create(strNeedToSpecifyCriteria);
End;

(**

  This is a getter method for the Console property.

  @precon  None.
  @postcon Returns a reference to the ISearchConsole interface for outputting colour text to the console.

  @return  an ISearchConsole

**)
Function TSearch.GetSearchConsole: ISearchConsole;

Begin
  Result := FConsole;
End;

(**

  This method loads the colour settings from the INI file.

  @precon  None.
  @postcon Loads the colour settings from the INI file.

**)
Procedure TSearch.LoadSettings;

Const
  strDefaultNone = 'clNone';
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
  strDefaultSuccess = 'clLime';
  strDefaultWarning = 'clYellow';
  strDefaultException = 'clRed';
  strDefaultZipFile = 'clFuchsia';
  strDefaultInput = 'clFuchsia';

Var
  iniFile: TMemIniFile;

Begin
  iniFile := TMemIniFile.Create(FRootKey);
  Try
    FConsole.Colour[scNone] := StringToColor(iniFile.ReadString(strColoursINISection, strNoneKey, strDefaultNone));
    FConsole.Colour[scSearchPath] := StringToColor(iniFile.ReadString(strColoursINISection, strSearchPathKey, strDefaultSearchPath));
    FConsole.Colour[scTitle] := StringToColor(iniFile.ReadString(strColoursINISection, strTitleKey, strDefaultTitle));
    FConsole.Colour[scHeader] := StringToColor(iniFile.ReadString(strColoursINISection, strHeaderKey, strDefaultHeader));
    FConsole.Colour[scFooter] := StringToColor(iniFile.ReadString(strColoursINISection, strFooterKey, strDefaultFooter));
    FConsole.Colour[scHelpHeader] := StringToColor(iniFile.ReadString(strColoursINISection, strHelpHeaderKey, strDefaultHelpFooter));
    FConsole.Colour[scHelpInfo] := StringToColor(iniFile.ReadString(strColoursINISection, strHelpInfoKey, strDefaultHelpInfo));
    FConsole.Colour[scHelpText] := StringToColor(iniFile.ReadString(strColoursINISection, strHelpTextKey, strDefaultHelpText));
    FConsole.Colour[scHelpSwitch] := StringToColor(iniFile.ReadString(strColoursINISection, strHelpSwitchKey, strDefaultHelpSwitch));
    FConsole.Colour[scHelpFootNote] := StringToColor(iniFile.ReadString(strColoursINISection, strHelpFootNoteKey, strDefaultHelpFootNote));
    FConsole.Colour[scFoundSearchPath] := StringToColor(iniFile.ReadString(strColoursINISection, strFoundSearchPathKey, strDefaultFoumdSearchPath));
    FConsole.Colour[scFileInfo] := StringToColor(iniFile.ReadString(strColoursINISection, strFileInfoKey, strDefaultFileInfo));
    FConsole.Colour[scRegExLineNumbers] := StringToColor(iniFile.ReadString(strColoursINISection, strRegExLineNumbersKey, strDefaultRegExLineNum));
    FConsole.Colour[scRegExLineOutput] := StringToColor(iniFile.ReadString(strColoursINISection, strRegExLineOutputKey, strDefaultRegExLineOutput));
    FConsole.Colour[scRegExFindOutputFG] := StringToColor(iniFile.ReadString(strColoursINISection, strRegExFindOutputFGKey, strDefaultRegExFindoutputFG));
    FConsole.Colour[scRegExFindOutputBG] := StringToColor(iniFile.ReadString(strColoursINISection, strRegExFindOutputBGKey, strDefaultRegExFindoutputBG));
    FConsole.Colour[scSummaryOutput] := StringToColor(iniFile.ReadString(strColoursINISection, strSummaryOutputKey, strDefaultSummaryOutput));
    FConsole.Colour[scSuccess] := StringToColor(iniFile.ReadString(strColoursINISection, strSuccessKey, strDefaultSuccess));
    FConsole.Colour[scWarning] := StringToColor(iniFile.ReadString(strColoursINISection, strWarningKey, strDefaultWarning));
    FConsole.Colour[scException] := StringToColor(iniFile.ReadString(strColoursINISection, strExceptionKey, strDefaultException));
    FConsole.Colour[scZipFile] := StringToColor(iniFile.ReadString(strColoursINISection, strZipFileKey, strDefaultZipFile));
    FConsole.Colour[scInput] := StringToColor(iniFile.ReadString(strColoursINISection, strInputKey, strDefaultInput));
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

  This method outputs the current colour configuration and the list of available colours.

  @precon  None.
  @postcon Outputs the current colour configuration and the list of available colours.

**)
Procedure TSearch.OutputConfiguredColours;

ResourceString
  strCurrentlyConfigColours = 'Currently configured colours are:';
  strAvailableColours = 'Available Colours:';
  strSampleText = 'Sample text...';

Const
  strColours = '  %20s = ';
  iColourPrefixLen = 2;

Var
  iColour: TSearchColour;
  iAvailableColour : TSearchColourList;
  strValue : String;

Begin
  FConsole.OutputToConsoleLn(coStd, strCurrentlyConfigColours, scTitle);
  For iColour := Low(TSearchColour) To High(TSearchColour) Do
    Begin
      strValue := GetEnumName(TypeInfo(TSearchColour), Ord(iColour));
      Delete(strValue, 1, iColourPrefixLen);
      FConsole.OutputToConsole(coStd, Format(strColours, [strValue]));
      strValue := ColorToString(FConsole.Colour[iColour]);
      Delete(strValue, 1, iColourPrefixLen);
      FConsole.OutputToConsoleLn(coStd, strValue, iColour);
    End;
  FConsole.OutputToConsoleLn(coStd);
  FConsole.OutputToConsoleLn(coStd, strAvailableColours, scTitle);
  For iAvailableColour := Low(TSearchColourList) To High(TSearchColourList) Do
    Begin
      FConsole.OutputToConsole(coStd, Format(strColours, [strSearchColourList[iAvailableColour]]));
      FConsole.OutputToConsoleLn(coStd, strSampleText, SearchColourList[iAvailableColour], clNone, True);
    End;
  FConsole.OutputToConsoleLn(coStd);
End;

(**

  This method outputs the currently being searched path to the screen.

  @precon  None.
  @postcon Outputs the currently being searched path to the screen.

  @param   strPath as a String as a constant

**)
Procedure TSearch.OutputCurrentSearchPath(Const strPath: String);

ResourceString
  strSearchLabel = 'Searching: (D:%1.0n,F:%1.0n/%1.0n) ';
  
Const
  iUNCLen = 2;
  iFirstSlash = 1;
  iSecondSlash = 2;
  iThirdSlash = 3;

Var
  i, j: Integer;
  iLength: Integer;
  strLPath: String;

Begin
  strLPath := strPath;
  If (clsQuiet In CommandLineSwitches) Or (clsOutputAsCSV In CommandLineSwitches) Then
    Exit;
  iLength := Length(Format(strSearchLabel, [Int(FDirectories), Int(FFoundFiles), Int(FTotalFiles)]));
  While Length(strLPath) > (FConsole.Width - iLength) Do
    Begin
      If Pos('...', strLPath) = 0 Then
        Begin
          i := TSearchStrUtils.PosOfNthChar(strLPath, '\', iFirstSlash + Integer(FUNCPath) * iUNCLen) + 1;
          j := TSearchStrUtils.PosOfNthChar(strLPath, '\', iSecondSlash + Integer(FUNCPath) * iUNCLen);
          If (i > 0) And (j > 0) Then
            strLPath := StringReplace(strLPath, Copy(strLPath, i, j - i), '...', [])
          Else
            strLPath := Copy(strLPath, Length(strLPath) - (FConsole.Width - iLength),
              FConsole.Width - iLength);
        End Else
        Begin
          i := TSearchStrUtils.PosOfNthChar(strLPath, '\', iSecondSlash + Integer(FUNCPath) * iUNCLen);
          j := TSearchStrUtils.PosOfNthChar(strLPath, '\', iThirdSlash + Integer(FUNCPath) * iUNCLen);
          If (i > 0) And (j > 0) Then
            strLPath := StringReplace(strLPath, Copy(strLPath, i, j - i), '', [])
          Else
            strLPath := Copy(strLPath, Length(strLPath) - (FConsole.Width - iLength),
              FConsole.Width - iLength);
        End;
    End;
  strLPath := Format('%s%-*s',
    [Format(strSearchLabel, [Int(FDirectories), Int(FFoundFiles), Int(FTotalFiles)]),
      FConsole.Width - iLength, strLPath]);
  If FConsole.CheckConsoleMode(coStd) Then
    FConsole.OutputToConsole(coStd, strLPath, scSearchPath, scNone, False);
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
    strOutput := strOutput + Format('  %s', [FormatDateTime(strOutputDateFmt, SearchFile.Date)])
  Else
    strOutput := strOutput + Format('%s,', [FormatDateTime(strOutputDateFmt, SearchFile.Date)]);
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
  strZipFileName: String;

Begin
  // Output Dir / Zip file
  If Not boolDirPrinted Then
    Begin
      If FConsole.CheckConsoleMode(coStd) Then
        FConsole.OutputToConsole(coStd, StringOfChar(' ', FConsole.Width), scNone, scNone, False);
      If Not TSearchStrUtils.Like(strZipPattern, strPath) Then
        FConsole.OutputToConsoleLn(coStd, strPath, scFoundSearchPath)
      Else
        Begin
          iZipPos := Pos(strZipExt, LowerCase(strPath));
          strZipFileName := Copy(strPath, 1, iZipPos + Length(strZipExt) - 1);
          FConsole.OutputToConsole(coStd, ExtractFilePath(strZipFileName), scFoundSearchPath);
          FConsole.OutputToConsole(coStd, ExtractFileName(strZipFileName), scZipFile);
          FConsole.OutputToConsoleLn(coStd, Copy(strPath, iZipPos + Length(strZipExt),
            Length(strPath) - iZipPos - Length(strZipExt)), scFoundSearchPath);
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

  @param   SearchFile  as an ISearchFile as a constant
  @param   iOwnerWidth as an Integer as a constant
  @param   strOutput   as a String as a reference

**)
Procedure TSearch.OutputFileOwner(Const SearchFile: ISearchFile; Const iOwnerWidth : Integer;
  Var strOutput: String);

Begin
  If clsOwner In CommandLineSwitches Then
    If Not(clsOutputAsCSV In CommandLineSwitches) Then
      strOutput := strOutput + Format('%-*s  ', [iOwnerWidth, SearchFile.Owner])
    Else
      strOutput := strOutput + Format('%s,', [SearchFile.Owner]);
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
      OutputFileOwner(FilesCollection.FileInfo[iFile], FilesCollection.OwnerWidth, strOutput);
      If Not(clsOutputAsCSV In CommandLineSwitches) Then
        OutputDirectoryOrZIPFile(boolDirPrinted, strPath);
      boolRegEx := (clsRegExSearch In CommandLineSwitches) And
        (FilesCollection.FileInfo[iFile].RegExMatches.Count > 0);
      If Not(clsRegExSearch In CommandLineSwitches) Or boolRegEx Then
        If Not(clsOutputAsCSV In CommandLineSwitches) Then
          FConsole.OutputToConsoleLn(coStd, strOutput + FilesCollection.FileInfo[iFile].FileName,
            scFileInfo)
        Else
          FConsole.OutputToConsoleLn(coStd,
            strOutput + strPath + FilesCollection.FileInfo[iFile].FileName, scFileInfo);
      OutputRegExInformation(strPath, boolRegEx, FilesCollection.FileInfo[iFile]);
    End;
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    If FilesCollection.Count > 0 Then
      FConsole.OutputToConsoleLn(coStd);
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
        strFileName := strPath + ExtractFileName(FileInfo.FileName);
        MZip := REZip.Match(strPath);
        If Not MZip.Success Then
          SafeLoadFromFile(slText, strFileName)
        Else
          Begin
            Z := TZipForge.Create(Nil);
            Try
              Z.FileName := Copy(strFileName, 1, MZip.Index + (MZip.Length - 1) - 1);
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
            For iLine := Max(1, M.LineNum - FOptions.FRegExSurroundingLines) To M.LineNum - 1 Do
              Begin
                FConsole.OutputToConsole(coStd, Format('  %10.0n: ', [Int(iLine)]), scRegExLineNumbers);
                FConsole.OutputToConsoleLn(coStd, slText[iLine - 1], scRegExLineOutput);
              End;
            FConsole.OutputToConsole(coStd, Format('  %10.0n: ', [Int(M.LineNum)]), scRegExLineNumbers);
            strLine := slText[M.LineNum - 1];
            For iGroup := 0 To M.Count - 1 Do
              Begin
                S := Copy(strLine, 1, M.Group[iGroup].FIndex - iStart);
                FConsole.OutputToConsole(coStd, S, scRegExLineOutput);
                FConsole.OutputToConsole(coStd, Copy(strLine, M.Group[iGroup].FIndex - iStart + 1,
                  M.Group[iGroup].FLength), scRegExFindOutputFG, scRegExFindOutputBG);
                Delete(strLine, 1, M.Group[iGroup].FIndex + M.Group[iGroup].FLength - iStart);
                iStart := M.Group[iGroup].FIndex + M.Group[iGroup].FLength;
              End;
            FConsole.OutputToConsoleLn(coStd, strLine, scRegExLineOutput);
            For iLine := Min(slText.Count, M.LineNum + 1) To Min(slText.Count, M.LineNum +
              FOptions.FRegExSurroundingLines) Do
              Begin
                FConsole.OutputToConsole(coStd, Format('  %10.0n: ', [Int(iLine)]), scRegExLineNumbers);
                FConsole.OutputToConsoleLn(coStd, slText[iLine - 1], scRegExLineOutput);
              End;
            If FOptions.FRegExSurroundingLines > 0 Then
              FConsole.OutputToConsoleLn(coStd);
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
        FConsole.OutputToConsoleLn(coStd);
        FConsole.OutputToConsoleLn(coStd, strErrorsWereEncountered, scException);
        For i := 0 To FErrorLog.Count - 1 Do
          FConsole.OutputToConsoleLn(coStd, #32#32 + FErrorLog[i]);
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
  strSearchResults = 'Found %s bytes in %1.0n of %1.0n Files in %1.0n Directories.';
  strMsg2 = '  Total space %s bytes and %s bytes free.';

Var
  iFreeBytesAvailableToCaller, iTotalNumberOfBytes, iTotalNumberOfFreeBytes: Int64;

Begin
  If Not(clsOutputAsCSV In CommandLineSwitches) Then
    Begin
      GetDiskFreeSpaceEx(PChar(strPath), iFreeBytesAvailableToCaller, iTotalNumberOfBytes,
        @iTotalNumberOfFreeBytes);
      If FConsole.CheckConsoleMode(coStd) Then
        FConsole.OutputToConsole(coStd, StringOfChar(#32, FConsole.Width - 1), scNone, scNone, False);
      FConsole.OutputToConsole(coStd, #32#32);
      If clsRegExSearch In CommandLineSwitches Then
        FConsole.OutputToConsole(coStd, Format(strGREPResults, [Int(FGREPCount)]), scFooter);
      FConsole.OutputToConsoleLn(coStd, Format(strSearchResults, [
        Trim(FormatSize(FFileSize)),
        Int(FFoundFiles),
        Int(FTotalFiles),
        Int(FDirectories)
      ]), scFooter);
      FConsole.OutputToConsoleLn(coStd, Format(strMsg2, [Trim(FormatSize(iTotalNumberOfBytes)),
          Trim(FormatSize(iTotalNumberOfFreeBytes))]), scFooter);
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
    FConsole.OutputToConsoleLn(coStd, Format(strSearchingForHeader, [strPath, strPattern]), scHeader);
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
      FConsole.OutputToConsoleLn(coStd, GetConsoleTitle, scTitle);
      FConsole.OutputToConsoleLn(coStd);
    End
  Else
    Begin
      strOutput := strDateAndTimeSize;
      If clsShowAttribs In CommandLineSwitches Then
        strOutput := strOutput + strAttrbutes;
      If clsOwner In CommandLineSwitches Then
        strOutput := strOutput + strOwner;
      strOutput := strOutput + strFilename;
      FConsole.OutputToConsoleLn(coStd, strOutput, scTitle);
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
Procedure TSearch.ProcessZipFileFailure(Sender: TObject; FileName: String; //FI:O801
  Operation: TZFProcessOperation; NativeError, ErrorCode: Integer;
  ErrorMessage: String; Var Action: TZFAction); //FI:O801

ResourceString
  strExceptionMsg =
    '%s (Native Error: %d, Error Code: %d, Operation: %s)';

Begin
  AddErrorToLog(
    Format(strExceptionMsg, [
      ErrorMessage,
      NativeError,
      ErrorCode,
      GetEnumName(TypeInfo(TZFProcessOperation), Ord(Operation))
    ]),
    (Sender As TZipForge).FileName + '\' + FileName
  );
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
  iResult := FindFirst(strPath + '*.*', faAnyFile, recSearch);
  While (iResult = 0) Do
    Begin
      If recSearch.Attr And faDirectory > 0 Then
        If (recSearch.Name <> '.') And (recSearch.Name <> '..') Then
          Inc(Result, SearchDirectory(strPath + recSearch.Name + '\', slPatterns, iLevel));
      If clsSearchZip In CommandLineSwitches Then
        If TSearchStrUtils.Like(strZipExt, recSearch.Name) Then
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
  strExclusionsNotFound = 'The filename "%s" was not found to process exclusions in the searches.' +
    'Type SEARCH /? for more help!';
                                  
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
  FRootKey := BuildRootKey(FParams);
  LoadSettings;
  PrintTitle;
  GetCommandLineSwitches;
  If Not (clsOutputAsCSV In CommandLineSwitches) Then
    If clsDisplayCriteria In CommandLineSwitches Then
      TSearchCriteria.DisplayCriteria(FConsole, FOptions, CommandLineSwitches);
  If Not(clsExclusions In CommandLineSwitches) Or
    ((clsExclusions In CommandLineSwitches) And FileExists(FOptions.FExlFileName)) Then
    Begin
      If clsExclusions In CommandLineSwitches Then
        SafeLoadFromFile(FOptions.FExclusions, FOptions.FExlFileName);
      FOptions.FExclusions.Text := LowerCase(FOptions.FExclusions.Text);
      If clsShowHelp In CommandLineSwitches Then
        TSearchHelp.PrintHelp(FConsole)
      Else If clsColours In CommandLineSwitches Then
        Begin
          If FColourSettings.Count = 0 Then
            OutputConfiguredColours
          Else
            ConfigureColour;
        End
      Else
        Begin
          If FOptions.FOwnerSearch Then
            FOwnerRegEx.Create(FOptions.FOwnerRegExSearch, [roIgnoreCase, roCompiled, roSingleLine]);
          For i := 0 To FSearchParams.Count - 1 Do
            Begin
              iPos := Pos('=', FSearchParams[i]);
              If iPos = 0 Then
                Raise ESearchException.CreateFmt(strErrorInPathSearchParamString, [FSearchParams[i]]);
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
                For iPattern := 1 To TSearchStrUtils.CharCount(';', strSearch) + 1 Do
                  FPatterns.Add(TSearchStrUtils.GetField(strSearch, ';', iPattern));
                FUNCPath := Copy(strPath, 1, iUNCPrefixLen) = '\\';
                SearchDirectory(strPath, FPatterns, FLevel);
              Finally
                FPatterns.Free;
              End;
              If clsSummaryLevel In CommandLineSwitches Then
                FConsole.OutputToConsoleLn(coStd);
              PrintFooter(strPath);
              PrintErrorLog;
            End;
        End;
    End
  Else
    Raise ESearchException.CreateFmt(strExclusionsNotFound, [FOptions.FExlFileName]);
End;

(**

  This method provide the RegEx functionality of SEARCH with a safe mechanism to load information from 
  files without any share violations.

  @precon  None.
  @postcon Opens the named file safely.

  @param   slText      as a TStringList as a constant
  @param   strFileName as a String as a constant

**)
Procedure TSearch.SafeLoadFromFile(Const slText : TStringList; Const strFileName: String);

Var
  Stream: TStream;

Begin
  Try
    Stream := TFileStream.Create(strFileName, fmOpenRead Or fmShareDenyNone);
    Try
      slText.LoadFromStream(Stream);
    Finally
      Stream.Free;
    End;
  Except
    On E : EFOpenError Do
      FErrorLog.Add(Format('%s: %s', [E.ClassName, E.Message]));
  End;
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
    iniFile.WriteString(strColoursINISection, strNoneKey, ColorToString(FConsole.Colour[scNone]));
    iniFile.WriteString(strColoursINISection, strSearchPathKey, ColorToString(FConsole.Colour[scSearchPath]));
    iniFile.WriteString(strColoursINISection, strTitleKey, ColorToString(FConsole.Colour[scTitle]));
    iniFile.WriteString(strColoursINISection, strHeaderKey, ColorToString(FConsole.Colour[scHeader]));
    iniFile.WriteString(strColoursINISection, strFooterKey, ColorToString(FConsole.Colour[scFooter]));
    iniFile.WriteString(strColoursINISection, strHelpHeaderKey, ColorToString(FConsole.Colour[scHelpHeader]));
    iniFile.WriteString(strColoursINISection, strHelpInfoKey, ColorToString(FConsole.Colour[scHelpInfo]));
    iniFile.WriteString(strColoursINISection, strHelpTextKey, ColorToString(FConsole.Colour[scHelpText]));
    iniFile.WriteString(strColoursINISection, strHelpSwitchKey, ColorToString(FConsole.Colour[scHelpSwitch]));
    iniFile.WriteString(strColoursINISection, strHelpFootNoteKey, ColorToString(FConsole.Colour[scHelpFootNote]));
    iniFile.WriteString(strColoursINISection, strFoundSearchPathKey, ColorToString(FConsole.Colour[scFoundSearchPath]));
    iniFile.WriteString(strColoursINISection, strFileInfoKey, ColorToString(FConsole.Colour[scFileInfo]));
    iniFile.WriteString(strColoursINISection, strRegExLineNumbersKey, ColorToString(FConsole.Colour[scRegExLineNumbers]));
    iniFile.WriteString(strColoursINISection, strRegExLineOutputKey, ColorToString(FConsole.Colour[scRegExLineOutput]));
    iniFile.WriteString(strColoursINISection, strRegExFindOutputFGKey, ColorToString(FConsole.Colour[scRegExFindOutputFG]));
    iniFile.WriteString(strColoursINISection, strRegExFindOutputBGKey, ColorToString(FConsole.Colour[scRegExFindOutputBG]));
    iniFile.WriteString(strColoursINISection, strSummaryOutputKey, ColorToString(FConsole.Colour[scSummaryOutput]));
    iniFile.WriteString(strColoursINISection, strSuccessKey, ColorToString(FConsole.Colour[scSuccess]));
    iniFile.WriteString(strColoursINISection, strWarningKey, ColorToString(FConsole.Colour[scWarning]));
    iniFile.WriteString(strColoursINISection, strExceptionKey, ColorToString(FConsole.Colour[scException]));
    iniFile.WriteString(strColoursINISection, strZipFileKey, ColorToString(FConsole.Colour[scZipFile]));
    iniFile.WriteString(strColoursINISection, strInputKey, ColorToString(FConsole.Colour[scInput]));
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
  strOutput: String;
  FilesCollection: ISearchFiles;

Begin

  OutputCurrentSearchPath(strPath);
  Inc(iLevel);
  Result := 0;
  Inc(FDirectories);
  boolDirPrinted := False;
  (* Find Files *)
  FilesCollection := TSearchFiles.Create(FilesExceptionHandler, FOptions.FRegExSearch);
  Try
    Inc(Result, SearchForPatterns(slPatterns, strPath, FilesCollection));
    FilesCollection.OrderBy(FOptions.FOrderFilesBy, FOptions.FOrderFilesDirection);
    OutputFilesToConsole(strPath, boolDirPrinted, FilesCollection);
  Finally
    FilesCollection := Nil;
  End;
  (* Search sub directories *)
  If clsSubDirectories In CommandLineSwitches Then
    Inc(Result, RecurseDirectories(strPath, iLevel, slPatterns));
  (* Output directory summary levels *)
  If clsSummaryLevel In CommandLineSwitches Then
    Begin
      If (iLevel > -1) And (iLevel <= FOptions.FSummaryLevel) Then
        If Not((Result = 0) And (clsSupressZeros In CommandLineSwitches)) Then
          Begin
            strOutput := Format('%s%s%s', [FormatSize(Result),
              StringOfChar(#32, iSummaryPadding + iSummaryIndent * iLevel), strPath]);
            FConsole.OutputToConsoleLn(coStd, strOutput + StringOfChar(#32,
                FConsole.Width - 1 - Length(strOutput)), scSummaryOutput);
          End;
    End;
  Dec(iLevel);
End;

(**

  This method search the current directory for files which match all the specified search patterns.

  @precon  None.
  @postcon Search the current directory for files which match all the specified search patterns.

  @param   slPatterns      as a TStringList as a constant
  @param   strPath         as a String as a constant
  @param   FilesCollection as an ISearchFiles as a constant
  @return  an Int64

**)
Function TSearch.SearchForPatterns(Const slPatterns: TStringList; Const strPath: String;
  Const FilesCollection: ISearchFiles): Int64;

Var
  iPattern: Integer;
  strOwner: String;
  recSearch: TSearchRec;
  iResult: Integer;
  boolFound: Boolean;
  ST: TSystemTime;
  dtDate: TDateTime;
  setAttributes : TSearchFileAttrs;

Begin
  Result := 0;
  FConsole.CheckForEscape;
  Inc(FTotalFiles, CountFiles(strPath));
  For iPattern := 0 To slPatterns.Count - 1 Do
    Begin
      iResult := FindFirst(strPath + slPatterns[iPattern], faAnyFile - faDirectory, recSearch);
      Try
        While iResult = 0 Do
          Begin
            FConsole.CheckForEscape;
            boolFound := True;
            setAttributes := FileAttrsToAttrsSet(recSearch.Attr);
            CheckFileAttributes(FOptions.FFileAttrs, setAttributes, boolFound);
            CheckSizeRange(recSearch.Size, FOptions.FLSize, FOptions.FUSize, boolFound);
            Case FOptions.FDateType Of
              dtCreation:
                Begin
                  FileTimeToSystemTime(recSearch.FindData.ftCreationTime, ST);
                  dtDate := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) + EncodeTime
                    (ST.wHour, ST.wMinute, ST.wSecond, ST.wMilliseconds);
                  CheckDateRange(dtDate, FOptions.FLDate, FOptions.FUDate, boolFound);
                End;
              dtLastAccess:
                Begin
                  FileTimeToSystemTime(recSearch.FindData.ftLastAccessTime, ST);
                  dtDate := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) + EncodeTime
                    (ST.wHour, ST.wMinute, ST.wSecond, ST.wMilliseconds);
                  CheckDateRange(dtDate, FOptions.FLDate, FOptions.FUDate, boolFound);
                End
              Else
                CheckDateRange(recSearch.TimeStamp, FOptions.FLDate, FOptions.FUDate, boolFound);
            End;
            CheckExclusions(strPath, recSearch.Name, boolFound, FOptions.FExclusions);
            If clsOwner In CommandLineSwitches Then
              Begin
                strOwner := OutputOwner(strPath + recSearch.Name);
                If FOptions.FOwnerSearch Then             
                  boolFound := boolFound And CheckOwner(FOwnerRegEx, strOwner);          
              End;                                             
            If boolFound Then                                  
              Inc(Result, CheckFiles(recSearch, setAttributes, strPath, strOwner, FilesCollection));
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
  FConsole.CheckForEscape;
  Z := TZipForge.Create(Nil);
  Try
    Z.FileName := strFileName;
    Z.OnProcessFileFailure := ProcessZipFileFailure;
    Z.OnDiskFull := ZipDiskFull;
    If Z.IsValidArchiveFile Then
      Begin
        Z.OpenArchive;
        Try
          Inc(FTotalFiles, Z.FileCount);
          For iPattern := 0 To slPatterns.Count - 1 Do
            Begin
              FConsole.CheckForEscape;
              boolResult := Z.FindFirst(slPatterns[iPattern], ZFAI);
              While boolResult And Not ZFAI.Encrypted Do
                Begin
                  OutputCurrentSearchPath(strFileName + '\' + ZFAI.StoredPath);
                  boolFound := True;
                  iSize := ZFAI.UncompressedSize;
                  CheckFileAttributes(FileAttrsToAttrsSet(ZFAI.ExternalFileAttributes),
                    FOptions.FFileAttrs, boolFound);
                  CheckSizeRange(iSize, FOptions.FLSize, FOptions.FUSize, boolFound);
                  // Zip files only contain the last write date of a file.
                  LongRec(iDateTime).Lo := ZFAI.LastModFileTime;
                  LongRec(iDateTime).Hi := ZFAI.LastModFileDate;
                  CheckDateRange(FileDateToDateTime(iDateTime), FOptions.FLDate, FOptions.FUDate,
                    boolFound);
                  CheckExclusions(strPath, ZFAI.StoredPath + ZFAI.FileName, boolFound,
                    FOptions.FExclusions);
                  If clsOwner In CommandLineSwitches Then
                    Begin
                      strOwner := OutputOwner(strFileName);
                      If FOptions.FOwnerSearch Then             
                        boolFound := boolFound And CheckOwner(FOwnerRegEx, strOwner);
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
  FilesCollection := TSearchFiles.Create(FilesExceptionHandler, FOptions.FRegExSearch);
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
          If (strPath <> ExtractFilePath(FilesCollection.FileInfo[iFile].FileName)) Or (Files = Nil) Then
            Begin
              Files := TSearchFiles.Create(FilesExceptionHandler, FOptions.FRegExSearch);
              PathCollections.Add(Files);
              strPath := ExtractFilePath(FilesCollection.FileInfo[iFile].FileName);
              Files.Path := strPath;
            End;
          If Assigned(Files) Then
            Files.Add(FilesCollection.FileInfo[iFile].Clone);
        End;
      For iPath := 0 To PathCollections.Count - 1 Do
        Begin
          PathCollection := PathCollections[iPath] As ISearchFiles;
          PathCollection.OrderBy(FOptions.FOrderFilesBy, FOptions.FOrderFilesDirection);
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
      FConsole.OutputToConsoleLn(coStd);
  Dec(iLevel);
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
  VolumeFileName: String; Var Cancel: Boolean); //FI:O801

ResourceString
  strDiskVolumeFull = 'Disk volume %d full [%s]';

Begin
  AddErrorToLog(Format(strDiskVolumeFull,
    [VolumeNumber, VolumeFileName]), (Sender As TZipForge).FileName);
  Cancel := True;
End;

//=================================================================================================

End.
