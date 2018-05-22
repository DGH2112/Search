(**

  This module contains applications function which can also be tested in a
  DUnit framework.

  @Version 1.0
  @Author  David Hoyle
  @Date    22 May 2018

  @todo    Break this code down in to smaller parts using a record to encapsulate.

**)
Unit Search.Functions;

Interface

Uses
  System.SysUtils,
  System.Classes,
  WinAPI.Windows,
  VCL.Graphics,
  Search.FilesCls, 
  Search.Types, 
  System.RegularExpressions;

ResourceString
  (** An exception message for a missing Search Text definition. **)
  strMissingSearchText = 'Missing Search Text';

Var
  (** Define the applications command line switches. **)
  CommandLineSwitches: TCommandLineSwitches;

  Procedure IncrementSwitchPosition(Const slParams: TStringList; Var iIndex, iSwitch: Integer;
    Const strExceptionMsg: String);
  Function  GetRangeString(Const slParams: TStringList; Const chEndToken: TSearchCharSet;
    Const strExceptionMsg: String; Var iIndex, iSwitch: Integer; Var strValue: String) : Char;
  Procedure GetDateRange(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var dtLDate, dtUdate: TDateTime);
  Procedure GetSummaryLevel(Const slParams: TStringList; Var iSwitch: Integer; Const iIndex: Integer;
    Var iSummaryLevel: Integer);
  Procedure GetSizeRange(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var iLSize, iUSize: Int64);
  Procedure GetAttributes(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var setFileAttrs: TSearchFileAttrs);
  Procedure GetOrderBy(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var OrderFilesDirection: TOrderDirection; Var OrderFilesBy: TOrderBy);
  Procedure GetSearchInInfo(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    var strRegExText: String; var iSurroundingLines : Integer);
  Procedure GetDateType(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var DateType: TDateType);
  Procedure GetExclusions(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var strExlFileName: String);
  Function GetOwnerSwitch(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var strOwnerRegEx : String) : Boolean;
  Function  OutputAttributes(Const setAttributes: TSearchFileAttrs): String;
  Procedure CheckDateRange(Const dtFileDateTime, dtLDate, dtUdate: TDateTime; Var boolFound: Boolean);
  Procedure CheckSizeRange(Const iSize, iLSize, iUSize: Int64; Var boolFound: Boolean);
  Procedure CheckFileAttributes(Const setSearchAttrs, setFileAttrs : TSearchFileAttrs;
    Var boolFound: Boolean);
  Procedure CheckExclusions(Const strPath, strFilename: String; Var boolFound: Boolean;
    Const slExclusions: TStringList);
  Function  CheckOwner(Const OwnerRegEx : TRegEx; Const strOwner : String) : Boolean;
  Procedure GetSizeFormat(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var SizeFormat: TSizeFormat);
  Function  CheckConsoleMode(Const hndConsole : THandle) : Boolean;
  Procedure OutputToConsoleLn(Const hndConsole : THandle; Const strText : String = '';
    Const iTextColour : TColor = clNone; Const iBackColour : TColor = clNone;
    Const boolUpdateCursor : Boolean = True);
  Procedure OutputToConsole(Const hndConsole : THandle; Const strText : String = '';
    Const iTextColour : TColor = clNone; Const iBackColour : TColor = clNone;
    Const boolUpdateCursor : Boolean = True);
  Function GetConsoleTitle : String;
  Function  BuildRootKey(Const slParams : TStringList) : String;
  Function  FileAttrsToAttrsSet(Const iAttributes : Cardinal) : TSearchFileAttrs;
  Procedure GetColoursSwitch(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Const slColourSettings : TStringList);
  
Implementation

Uses
  WinAPI.ShlObj,
  Search.StrUtils, 
  Search.ConvertDate, 
  System.RegularExpressionsCore;

Type
  (** An enumerate to define the current console output mode **)
  TConsoleMode = (cmUnknown, cmStandard, cmRedirected);

ResourceString
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
  (** An exception message for a missing ] in an attribute definition. **)
  strCloseSquareExpectedInAttribDef = '"]" Expected in Attribute Definition';
  (** An exception message for a missing [ in an attribute definition. **)
  strOpenSquareExpectedInAttribDef = '"[" Expected in Attribute Definition.';
  (** An exception message for a missing Attribute definition. **)
  strMissingAttribDirective = 'Missing Attribute directive';
  (** An exception message for an invalid Attribute criteria definition. **)
  strNotAValidAttrList = '"%s" is not a valid attribute criteria.';
  (** An exception message for a missing colon in the order by definition. **)
  strColonExpectedInOrderBy = 'Colon expected in Order By definition.';
  (** An execption message for a missing criteria in the order by definition. **)
  strMissingOrderByDirective = 'Missing Order By definition.';
  (** An exception message for an invalid order by criteria. **)
  strInvalidOrderByDirective = 'Invalid Order By definition.';
  (** An exception message for a missing colon in the date type definition. **)
  strColonExpectedInDateType = 'Colon expected in date Type definition.';
  (** An execption message for a missing criteria in the Date Type definition. **)
  strMissingDateTypeDirective = 'Missing date type definition.';
  (** An exception message for an invalid Date Type criteria. **)
  strInvalidDateTypeDirective = 'Invalid date type definition.';
  (** An exception message for a missing ] in an Search definition. **)
  strCloseSquareExpectedInRegExSearchDef = '"]" Expected in RegEx Search Definition';
  (** A resource string for formatting information. **)
  strSummaryLevelException = 'The summary level "%s" is invalid.';
  (** A resource string for formatting information. **)
  strSummaryAlreadySet = 'Then summary level is already set.';
  (** An exception message for a missing ] in an Search definition. **)
  strCloseSquareExpectedInExclSearchDef = '"]" Expected in Exclusion Search Definition';
  (** An exception message for a missing [ in an Search definition. **)
  strOpenSquareExpectedInExclSearchDef = '"[" Expected in Exclusion Search Definition.';
  (** An exception message for a missing ] in an Search definition. **)
  strCloseSquareExpectedInOwnerSearchDef = '"]" Expected in Owner Search Definition';
  (** An exception message for a missing [ in an Search definition. **)
  strOpenSquareExpectedInOwnerSearchDef = '"[" Expected in Owner Search Definition.';
  (** An execption messages for a missing colon on the size format definition. **)
  strColonExpectedInSizeFormat = 'Colon expected in size format definition.';
  (** An execption messages for a missing size format character in the definition. **)
  strMissingSizeFormatDirective = 'Missing size format definition.';
  (** An execption messages for a missing size format definition. **)
  strInvalidSizeFormatDirective = 'Invalid size format definition.';

Var
  (** A private variable to hold the console output mode **)
  ConsoleMode : TConsoleMode;

  Function ComputerName : String; Forward;
  Function UserName : String; Forward;
  
(**

  This function returns the background colour attribute for the console associated with the given cl#### 
  colour.

  @precon  CSBI must be a valid Console Screen Buffer Info structure.
  @postcon Returns the background colour attribute for the console associated with the given cl#### 
           colour.

  @param   iColour as a TColor as a constant
  @param   iNone   as a TColor as a constant
  @return  an Integer

**)
Function BackGroundColour(Const iColour, iNone : TColor) : Integer;

ResourceString
  strInvalidConsoleColour = 'Invalid console colour.';

Begin
  Case iColour Of
    clBlack  : Result := 0;
    clMaroon  :Result := BACKGROUND_RED;
    clGreen   :Result := BACKGROUND_GREEN;
    clNavy    :Result := BACKGROUND_BLUE;
    clOlive   :Result := BACKGROUND_RED Or BACKGROUND_GREEN;
    clPurple  :Result := BACKGROUND_RED or BACKGROUND_GREEN;
    clTeal    :Result := BACKGROUND_BLUE Or BACKGROUND_GREEN;
    clGray    :Result := BACKGROUND_BLUE Or BACKGROUND_GREEN Or BACKGROUND_RED;
    clRed     :Result := BACKGROUND_RED   Or BACKGROUND_INTENSITY;
    clLime    :Result := BACKGROUND_GREEN Or BACKGROUND_INTENSITY;
    clBlue    :Result := BACKGROUND_BLUE  Or BACKGROUND_INTENSITY;
    clYellow  :Result := BACKGROUND_RED Or BACKGROUND_GREEN Or BACKGROUND_INTENSITY;
    clFuchsia :Result := BACKGROUND_BLUE Or BACKGROUND_RED  Or BACKGROUND_INTENSITY;
    clAqua    :Result := BACKGROUND_BLUE Or BACKGROUND_GREEN  Or BACKGROUND_INTENSITY;
    clWhite   :Result := BACKGROUND_BLUE Or BACKGROUND_GREEN Or BACKGROUND_RED Or BACKGROUND_INTENSITY;
    clNone    :Result  := iNone;
  Else
    Raise Exception.Create(strInvalidConsoleColour);
  End;
End;

(**

  This method builds the root key INI filename for the loading and saving of settings from the instance 
  handle for the module.

  @precon  slParams must be a valid instance of a TStringList class.
  @postcon Builds the root key INI filename for the loading and saving of settings from the instance 
           handle for the module.

  @param   slParams      as a TStringList as a constant
  @return  a String

**)
Function BuildRootKey(Const slParams : TStringList) : String;

ResourceString
  strINIPattern = '%s Settings for %s on %s.INI';
  strSeasonsFall = '\Season''s Fall\';

var
  strModuleName : String;
  strINIFileName : String;
  strUserAppDataPath : String;
  strBuffer : String;
  iParam : Integer;
  iSize : Integer;                                                              

Begin
  SetLength(strBuffer, MAX_PATH);
  iSize := GetModuleFileName(hInstance, PChar(strBuffer), MAX_PATH);
  SetLength(strBuffer, iSize);
  strModuleName := strBuffer;
  strINIFileName := ChangeFileExt(ExtractFileName(strBuffer), '');
  While (Length(strINIFileName) > 0) And
    (CharInSet(strINIFileName[Length(strINIFileName)], ['0' .. '9'])) Do
    strINIFileName := Copy(strINIFileName, 1, Length(strINIFileName) - 1);
  strINIFileName := Format(strINIPattern, [strINIFileName, UserName, ComputerName]);
  SetLength(strBuffer, MAX_PATH);
  SHGetFolderPath(0, CSIDL_APPDATA Or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT, PChar(strBuffer));
  strBuffer := StrPas(PChar(strBuffer));
  strUserAppDataPath := strBuffer + strSeasonsFall;
  If Not DirectoryExists(strUserAppDataPath) Then
    ForceDirectories(strUserAppDataPath);
  Result := strUserAppDataPath + strINIFileName;
  For iParam := 1 To ParamCount Do
    slParams.Add(ParamStr(iParam));
End;

(**

  This function returns the current mode of output of the console functions. The first time its gets the 
  console mode from the Win32 API.

  @precon  hndConsole must be a valid console handle.
  @postcon Returns the current mode of output of the console functions.

  @param   hndConsole as a THandle as a constant
  @return  a Boolean

**)
Function CheckConsoleMode(Const hndConsole : THandle) : Boolean;

Var
  lpMode : Cardinal;

Begin
  If ConsoleMode = cmUnknown Then
    If Not GetConsoleMode(hndConsole, lpMode) Then
      ConsoleMode := cmRedirected
    Else
      ConsoleMode := cmStandard;
  Result := ConsoleMode = cmStandard;
End;

(**

  This method checks the currently found file information against the Date Range .

  @precon  None.
  @postcon Returns boolFound as true if the file date is within the date range.

  @param   dtFileDateTime as a TDateTime as a constant
  @param   dtLDate        as a TDateTime as a constant
  @param   dtUdate        as a TDateTime as a constant
  @param   boolFound      as a Boolean as a reference

**)
Procedure CheckDateRange(Const dtFileDateTime, dtLDate, dtUdate: TDateTime; Var boolFound: Boolean);

Begin
  If clsDateRange In CommandLineSwitches Then
    Begin
      boolFound := boolFound And (dtFileDateTime >= dtLDate);
      boolFound := boolFound And (dtFileDateTime <= dtUdate);
    End;
End;

(**

  This method check to see if the path and file name should be excluded from the search.

  @precon  recSearch must be a valid TSearchRec structure.
  @postcon Sets or maintains boolFound as True if the file should NOT be excluded else sets boolFound to
           False.

  @param   strPath      as a String as a constant
  @param   strFilename  as a String as a constant
  @param   boolFound    as a Boolean as a reference
  @param   slExclusions as a TStringList as a constant

**)
Procedure CheckExclusions(Const strPath, strFilename: String; Var boolFound: Boolean;
  Const slExclusions: TStringList);

Var
  strLFileName : String;
  j: Integer;

Begin
  strLFilename := LowerCase(strPath + strFilename);
  For j := 0 To slExclusions.Count - 1 Do
    boolFound := boolFound And (Pos(slExclusions[j], strLFilename) = 0);
End;

(**

  This method checks the currently found file against the file attributes.

  @precon  None.
  @postcon Returns boolFound as true is the file has the attributes.

  @param   setSearchAttrs as a TSearchFileAttrs as a constant
  @param   setFileAttrs   as a TSearchFileAttrs as a constant
  @param   boolFound      as a Boolean as a reference

**)
Procedure CheckFileAttributes(Const setSearchAttrs, setFileAttrs : TSearchFileAttrs;
  Var boolFound: Boolean);

Begin
  If clsAttrRange In CommandLineSwitches Then
    boolFound := boolFound And (setSearchAttrs <= setFileAttrs);
End;

(**

  This method check the owner of the file against the owner regex search criteria.

  @precon  None.
  @postcon Check the owner of the file against the owner search criteria.

  @param   OwnerRegEx as a TRegEx as a constant
  @param   strOwner   as a String as a constant
  @return  a Boolean

**)
Function CheckOwner(Const OwnerRegEx : TRegEx; Const strOwner : String) : Boolean;

Begin
  Result := OwnerRegEx.IsMatch(strOwner);
End;

(**

  This method checks the currently found file against the size range.

  @precon  None.
  @postcon Returns boolFound as true if the file is in the range.

  @param   iSize     as an Int64 as a constant
  @param   iLSize    as an Int64 as a constant
  @param   iUSize    as an Int64 as a constant
  @param   boolFound as a Boolean as a reference

**)
Procedure CheckSizeRange(Const iSize, iLSize, iUSize: Int64; Var boolFound: Boolean);

Begin
  If clsSizeRange In CommandLineSwitches Then
    Begin
      boolFound := boolFound And (iSize >= iLSize);
      boolFound := boolFound And (iSize <= iUSize);
    End;
End;

(**

  This function returns the users computer name as a String.

  @precon  None.
  @postcon Returns the users computer name as a String.

  @return  a String

**)
Function ComputerName : String;

Var
  i : Cardinal;

Begin
  i := MAX_PATH;
  SetLength(Result, i);
  GetComputerName(@Result[1], i);
  Win32Check(LongBool(i));
  SetLength(Result, i);
End;

(**

  This method returns a set of search file attributes for the given set of file attributes.

  @precon  None.
  @postcon A set of search file attributes is returned for the given file attributes.

  @param   iAttributes as an Cardinal as a constant
  @return  a TSearchFileAttrs

**)
Function  FileAttrsToAttrsSet(Const iAttributes : Cardinal) : TSearchFileAttrs;

  (**

    This procedure added the search file attribute to the result of the given attribute is in the files 
    attributes.

    @precon  None.
    @postcon The text attribute is added if found in the file attributes.

    @param   iAttr      as an Integer as a constant
    @param   eFileAttr  as a TSearchFileAttr as a constant
    @param   setResults as a TSearchFileAttrs as a reference
    @return  a Boolean

  **)
  Function TestAttr(Const iAttr : Integer; Const eFileAttr : TSearchFileAttr;
    Var setResults : TSearchFileAttrs) : Boolean;

  Begin
    Result := iAttr And iAttributes > 0;
    If Result Then
      Include(setResults, eFileAttr);
  End;

Begin
  Result := [sfaFile];
  TestAttr(faReadOnly, sfaReadOnly, Result);
  TestAttr(faSysFile, sfaSystem, Result);
  TestAttr(faHidden, sfaHidden, Result);
  If TestAttr(faDirectory, sfaDirectory, Result) Then
    Exclude(Result, sfaFile);
  If TestAttr(faVolumeID, sfaVolume, Result) Then
    Exclude(Result, sfaFile);
  TestAttr(faArchive, sfaArchive, Result);
  TestAttr(FILE_ATTRIBUTE_DEVICE, sfaDevice, Result);
  TestAttr(faNormal, sfaNormal, Result);
  TestAttr(faTemporary, sfaTemporary, Result);
  TestAttr(FILE_ATTRIBUTE_SPARSE_FILE, sfaSparse, Result);
  TestAttr(faSymLink, sfaReparse, Result);
  TestAttr(faCompressed, sfaCompressed, Result);
  TestAttr(FILE_ATTRIBUTE_OFFLINE, sfaOffLine, Result);
  TestAttr(FILE_ATTRIBUTE_NOT_CONTENT_INDEXED, sfaIndexed, Result);
  TestAttr(faEncrypted, sfaEncrypted, Result);
  TestAttr(faVirtual, sfaVirtual, Result);
End;

(**

  This function returns the background colour attribute for the console associated with the given cl#### 
  colour. Colour Matrix: Red(Maroon) Yellow(Olive) Lime(Green) White(Gray) Aqua(Teal) Blue(Navy)

  @precon  CSBI must be a valid Console Screen Buffer Info structure.
  @postcon Returns the background colour attribute for the console associated with the given cl#### 
           colour.

  @param   iColour as a TColor as a constant
  @param   iNone   as a TColor as a constant
  @return  an Integer

**)
Function ForeGroundColour(Const iColour, iNone : TColor): Integer;

ResourceString
  strInvalidConsoleColour = 'Invalid console colour.';

Begin
  Case iColour Of
    clBlack  : Result := 0;
    clMaroon  :Result := FOREGROUND_RED;
    clGreen   :Result := FOREGROUND_GREEN;
    clNavy    :Result := FOREGROUND_BLUE;
    clOlive   :Result := FOREGROUND_RED Or FOREGROUND_GREEN;
    clPurple  :Result := FOREGROUND_RED Or FOREGROUND_BLUE;
    clTeal    :Result := FOREGROUND_BLUE Or FOREGROUND_GREEN;
    clGray    :Result := FOREGROUND_BLUE Or FOREGROUND_GREEN Or FOREGROUND_RED;
    clRed     :Result := FOREGROUND_RED   Or FOREGROUND_INTENSITY;
    clLime    :Result := FOREGROUND_GREEN Or FOREGROUND_INTENSITY;
    clBlue    :Result := FOREGROUND_BLUE  Or FOREGROUND_INTENSITY;
    clFuchsia :Result := FOREGROUND_RED Or FOREGROUND_BLUE Or FOREGROUND_INTENSITY;
    clYellow  :Result := FOREGROUND_RED Or FOREGROUND_GREEN Or FOREGROUND_INTENSITY;
    clAqua    :Result := FOREGROUND_BLUE Or FOREGROUND_GREEN  Or FOREGROUND_INTENSITY;
    clWhite   :Result := FOREGROUND_BLUE Or FOREGROUND_GREEN Or FOREGROUND_RED Or FOREGROUND_INTENSITY;
    clNone  : Result  := iNone;
  Else
    Raise Exception.Create(strInvalidConsoleColour);
  End;
End;

(**

  This method determines the attribute range from the command line.

  @precon  None.
  @postcon Determines the attribute range from the command line.

  @param   slParams     as a TStringList as a constant
  @param   iSwitch      as an Integer as a reference
  @param   iIndex       as an Integer as a reference
  @param   setFileAttrs as a TSearchFileAttrs as a reference

**)
Procedure GetAttributes(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var setFileAttrs: TSearchFileAttrs);

Begin
  Include(CommandLineSwitches, clsAttrRange);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strOpenSquareExpectedInAttribDef);
  If slParams[iSwitch][iIndex] <> '[' Then
    Raise ESearchException.Create(strOpenSquareExpectedInAttribDef);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strMissingAttribDirective);
  setFileAttrs := [];
  While (iIndex <= Length(slParams[iSwitch])) And (slParams[iSwitch][iIndex] <> ']') Do
    Begin
      Case slParams[iSwitch][iIndex] Of
        'R': Include(setFileAttrs, sfaReadOnly);
        'A': Include(setFileAttrs, sfaArchive);
        'S': Include(setFileAttrs, sfaSystem);
        'H': Include(setFileAttrs, sfaHidden);
        'F': Include(setFileAttrs, sfaFile);
        'D': Include(setFileAttrs, sfaDirectory);
        'V': Include(setFileAttrs, sfaVolume);
        'd': Include(setFileAttrs, sfaDevice);
        'N': Include(setFileAttrs, sfaNormal);
        'T': Include(setFileAttrs, sfaTemporary);
        's': Include(setFileAttrs, sfaSparse);
        'r': Include(setFileAttrs, sfaReparse);
        'C': Include(setFileAttrs, sfaCompressed);
        'O': Include(setFileAttrs, sfaOffLine);
        'I': Include(setFileAttrs, sfaIndexed);
        'E': Include(setFileAttrs, sfaEncrypted);
        'v': Include(setFileAttrs, sfaVirtual);
      Else
        Raise ESearchException.CreateFmt(strNotAValidAttrList,
          [slParams[iSwitch][iIndex]]);
      End;
      IncrementSwitchPosition(slParams, iIndex, iSwitch,
        strCloseSquareExpectedInAttribDef);
    End;
End;

(**

  This routine extract the build number from the EXE resources for display in the app title.

  @precon  None.
  @postcon Extract the build number from the EXE resources for display in the app title.

  @param   strFileName as a String as a constant
  @param   iMajor      as an Integer as a reference
  @param   iMinor      as an Integer as a reference
  @param   iBugfix     as an Integer as a reference
  @param   iBuild      as an Integer as a reference
  @return  a String

**)
Function GetBuildNumber(Const strFileName : String; var iMajor, iMinor, iBugfix,
  iBuild : Integer) : String;

ResourceString
  strExeHasNoVerInfo = 'The executable "%s" does not contain any version information.';

Const
  strBuild = '%d.%d.%d.%d';
  iShiftRight = 16;
  iWordMask = $FFFF;

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;

Begin
  VerInfoSize := GetFileVersionInfoSize(PChar(strFileName), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      Try
        GetFileVersionInfo(PChar(strFileName), 0, VerInfoSize, VerInfo);
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
        iMajor := VerValue^.dwFileVersionMS shr iShiftRight;
        iMinor := VerValue^.dwFileVersionMS and iWordMask;
        iBugfix := VerValue^.dwFileVersionLS shr iShiftRight;
        iBuild := VerValue^.dwFileVersionLS and iWordMask;
        Result := Format(strBuild, [iMajor, iMinor, iBugfix, iBuild]);
      Finally
        FreeMem(VerInfo, VerInfoSize);
      End;
    End Else
      Raise ESearchException.CreateFmt(strExeHasNoVerInfo, [strFileName]);
End;

(**

  This method enables colour configations and if provided extracts colour settings.

  @precon  slParams and slColourSettings must be valid instances.
  @postcon Colour configuration is enabled and colour settings extracted else an exception is raised.

  @param   slParams         as a TStringList as a constant
  @param   iSwitch          as an Integer as a reference
  @param   iIndex           as an Integer as a reference
  @param   slColourSettings as a TStringList as a constant

**)
Procedure GetColoursSwitch(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  Const slColourSettings : TStringList);

ResourceString
  strInvalidColourSettingSpec = 'Invalid colour setting specification "%s"!';

Const
  strRegExSwitchPattern = '[[].*?\]';
  strRegExConfigPattern = '^(?<Def1>\w+=\w+)$';
  iFirfstCharOfDef = 2;
  iSquareBracketPadding = 2;

Var
  RE : TRegEx;
  M: TMatch;
  i: Integer;
  strConfig: String;
  
Begin
  Include(CommandLineSwitches, clsColours);
  If iIndex < Length(slParams[iSwitch]) Then
    Inc(iIndex);
  If (iIndex < Length(slParams[iSwitch])) And (slParams[iSwitch][iIndex] = '[') Then
    Begin
      RE.Create(strRegExSwitchPattern, [roIgnoreCase, roSingleLine, roCompiled]);
      strConfig := Copy(slParams[iSwitch], iIndex, Length(slParams[iSwitch]) - iIndex + 1);
      M := RE.Match(strConfig);
      If M.Success Then
        Begin
          strConfig := Copy(M.Value, iFirfstCharOfDef, Length(M.Value) - iSquareBracketPadding);
          RE.Create(strRegExConfigPattern, [roIgnoreCase, roSingleLine, roCompiled, roExplicitCapture]);
          M := RE.Match(strConfig);
          If M.Success Then
            slColourSettings.Add(strConfig)
          Else
            Raise ESearchException.CreateFmt(strInvalidColourSettingSpec, [strConfig]);
          For i := 0 To Length(M.Value) Do //FI:W514 //FI:W528
            IncrementSwitchPosition(slParams, iIndex, iSwitch, strCloseSquareExpectedInRegExSearchDef);
        End Else
          Raise ESearchException.CreateFmt(strInvalidColourSettingSpec, [strConfig]);
    End;
End;

(**

  This method returns a string for the title of a console application containing the version number and 
  build number.

  @precon  Expects a string to contains %d.%d%s for the version number followed by %s for the build 
           number.
  @postcon Returns a string for the title of a console application containing the version number and 
           build number.

  @return  a String

**)
Function GetConsoleTitle : String;

ResourceString
  strTitle = 'Search %d.%d%s (%sBuild %s) File Find and Summary Tool [%s].';
  strBugFix = ' abcdefghijklmnopqrstuvwxyz';
  strWrittenByDavidHoyle = 'Written by David Hoyle (c) %s';

Const
  {$IFDEF DEBUG}
  strConfig = 'DEBUG ';
  {$ELSE}
  strConfig = '';
  {$ENDIF}
  {$IFDEF WIN64}
  strPlatform = '64-bit';
  {$ELSE}
  strPlatform = '32-bit';
  {$ENDIF}
  strDateFmt = 'ddd dd/mmm/yyyy';

Var
  iMajor, iMinor, iBugfix, iBuild : Integer;
  strBuildNumber  : String;
  dtDate : TDateTime;

Begin
  strBuildNumber := GetBuildNumber(ParamStr(0), iMajor, iMinor, iBugFix, iBuild);
  Result := Format(strTitle, [iMajor, iMinor, strBugFix[iBugFix + 1], strConfig, strBuildNumber,
    strPlatform]);
  FileAge(ParamStr(0), dtDate);
  Result := Result + #13#10 +
    Format(strWrittenByDavidHoyle, [FormatDateTime(strDateFmt, dtDate)]);
End;

(**

  This method determines if a date range has been specified on the command line.

  @precon  None.
  @postcon Determines if a date range has been specified on the command line.

  @param   slParams as a TStringList as a constant
  @param   iSwitch  as an Integer as a reference
  @param   iIndex   as an Integer as a reference
  @param   dtLDate  as a TDateTime as a reference
  @param   dtUdate  as a TDateTime as a reference

**)
Procedure GetDateRange(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var dtLDate, dtUdate: TDateTime);

  (**

    This method checks the finish date in the command line switch.

    @precon  None.
    @postcon Sets dtUDate to the finish date in the command line switch if found else assigns the default
             value.

    @param   cDelimiter as a Char as a constant

  **)
  Procedure CheckFinishDate(Const cDelimiter : Char);

  ResourceString
    strCloseSquareExpectedInDate = '"]" Expected in Date Range.';

  Const
    strDefaultEndDate = '31/Dec/2099 23:59:59.999';
    iMilliSecInSec = 999;
  
  Var
    strDate : String;
    wH, wM, wS, wMS: Word;
    
  Begin
    GetRangeString(slParams, [']'], strCloseSquareExpectedInDate, iIndex, iSwitch, strDate);
    If strDate <> '' Then
      Begin
        dtUdate := TConvertDate.ConvertDate(strDate);
        DecodeTime(dtUdate, wH, wM, wS, wMS);
        If (wH = 0) And (wM = 0) And (wS = 0) And (wMS = 0) Then
          dtUdate := dtUdate + EncodeTime(iHoursInDay, iSecondsInMinute, iMinutesInHour, iMilliSecInSec);
      End
    Else If cDelimiter = ']' Then
      Begin
        dtUdate := Int(dtLDate);   
        dtUdate := dtUdate + EncodeTime(iHoursInDay, iSecondsInMinute, iMinutesInHour, iMilliSecInSec);
      End
    Else
        dtUdate := TConvertDate.ConvertDate(strDefaultEndDate);
  End;

  (**

    This method checks for the starting date and returns that date in the strDate parameter.

    @precon  None.
    @postcon Sets dtLDate to the start date in the command line switch else returns the default value.

    @return  a Char

  **)
  Function CheckStartDate : Char;

  ResourceString
    strMissingDateRangeSeparater = 'Missing Start Date Range Separater "-".';

  Const
    strDefaultStartDate = '01/Jan/1900 00:00:00';

  Var    
    strDate : String;
    wH, wM, wS, wMS: Word;
    
  Begin
    Result := GetRangeString(slParams, ['-', ']'], strMissingDateRangeSeparater, iIndex, iSwitch,
      strDate);
    If strDate <> '' Then
      Begin
        dtLDate := TConvertDate.ConvertDate(strDate);
        DecodeTime(dtLDate, wH, wM, wS, wMS);
        If (wH = 0) And (wM = 0) And (wS = 0) And (wMS = 0) Then
          dtLDate := dtLDate + EncodeTime(0, 0, 0, 0);
      End
    Else 
        dtLDate := TConvertDate.ConvertDate(strDefaultStartDate);
  End;

ResourceString
  strOpenSquareExpectedInDate = '"[" Expected in Date Range.';

Begin
  Include(CommandLineSwitches, clsDateRange);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strOpenSquareExpectedInDate);
  If slParams[iSwitch][iIndex] <> '[' Then
    Raise ESearchException.Create(strOpenSquareExpectedInDate);
  CheckFinishDate(CheckStartDate);
End;

(**

  This method gets the command line information for the date type of the found files.

  @precon  None.
  @postcon Gets the command line information for the date type of the found files.

  @param   slParams as a TStringList as a constant
  @param   iSwitch  as an Integer as a reference
  @param   iIndex   as an Integer as a reference
  @param   DateType as a TDateType as a reference

**)
Procedure GetDateType(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var DateType: TDateType);

Begin
  Include(CommandLineSwitches, clsDateType);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strColonExpectedInDateType);
  If slParams[iSwitch][iIndex] <> ':' Then
    Raise ESearchException.Create(strColonExpectedInDateType);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strMissingDateTypeDirective);
  Case slParams[iSwitch][iIndex] Of
    'c', 'C': DateType := dtCreation;
    'a', 'A': DateType := dtLastAccess;
    'w', 'W': DateType := dtLastWrite;
  Else
    Raise ESearchException.Create(strInvalidDateTypeDirective);
  End
End;

(**

  This method extracts from the command line switch the filename to be used as the source of the 
  exclusion list.

  @precon  None.
  @postcon Extracts from the command line switch the filename to be used as the source of the exclusion 
           list.

  @param   slParams       as a TStringList as a constant
  @param   iSwitch        as an Integer as a reference
  @param   iIndex         as an Integer as a reference
  @param   strExlFileName as a String as a reference

**)
Procedure GetExclusions(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var strExlFileName: String);

Begin
  Include(CommandLineSwitches, clsExclusions);
  IncrementSwitchPosition(slParams, iIndex, iSwitch,
    strOpenSquareExpectedInExclSearchDef);
  If slParams[iSwitch][iIndex] <> '[' Then
    Raise ESearchException.Create(strOpenSquareExpectedInExclSearchDef);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strMissingSearchText);
  strExlFileName := '';
  While (iIndex <= Length(slParams[iSwitch])) And (slParams[iSwitch][iIndex] <> ']') Do
    Begin
      strExlFileName := strExlFileName + slParams[iSwitch][iIndex];
      IncrementSwitchPosition(slParams, iIndex, iSwitch,
        strCloseSquareExpectedInExclSearchDef);
    End;
End;

(**

  This method gets the command line information for the ordering of the found files.

  @precon  None.
  @postcon Gets the command line information for the ordering of the found files.

  @param   slParams            as a TStringList as a constant
  @param   iSwitch             as an Integer as a reference
  @param   iIndex              as an Integer as a reference
  @param   OrderFilesDirection as a TOrderDirection as a reference
  @param   OrderFilesBy        as a TOrderBy as a reference

**)
Procedure GetOrderBy(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var OrderFilesDirection: TOrderDirection; Var OrderFilesBy: TOrderBy);

Begin
  Include(CommandLineSwitches, clsOrderBy);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strColonExpectedInOrderBy);
  If slParams[iSwitch][iIndex] <> ':' Then
    Raise ESearchException.Create(strColonExpectedInOrderBy);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strMissingOrderByDirective);
  OrderFilesDirection := odAscending;
  If slParams[iSwitch][iIndex] = '+' Then
    IncrementSwitchPosition(slParams, iIndex, iSwitch, strMissingOrderByDirective);
  If slParams[iSwitch][iIndex] = '-' Then
    Begin
      OrderFilesDirection := odDescending;
      IncrementSwitchPosition(slParams, iIndex, iSwitch, strMissingOrderByDirective);
    End;
  Case slParams[iSwitch][iIndex] Of
    'n', 'N': OrderFilesBy := obName;
    'd', 'D': OrderFilesBy := obDate;
    's', 'S': OrderFilesBy := obSize;
    'o', 'O': OrderFilesBy := obOwner;
  Else
    Raise ESearchException.Create(strInvalidOrderByDirective);
  End
End;

(**

  This method obtains the owner search information from the command line.

  @precon  None.
  @postcon Obtains the owner search information from the command line.

  @param   slParams      as a TStringList as a constant
  @param   iSwitch       as an Integer as a reference
  @param   iIndex        as an Integer as a reference
  @param   strOwnerRegEx as a String as a reference
  @return  a Boolean

**)
Function GetOwnerSwitch(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var strOwnerRegEx : String) : Boolean;

  (**

    This method extracts the owner search string from the command line switch.

    @precon  None.
    @postcon The strOwnerSearch string is populated with the information from the command line switch.

    @return  a String

  **)
  Function ExtractOwnerSearch : String;

  Begin
    Result := '';
    While (iIndex <= Length(slParams[iSwitch])) And (slParams[iSwitch][iIndex] <> ']') Do
      Begin
        Result := Result + slParams[iSwitch][iIndex];
        IncrementSwitchPosition(slParams, iIndex, iSwitch, strCloseSquareExpectedInOwnerSearchDef);
      End;
  End;

Begin
  Result := False;
  Include(CommandLineSwitches, clsOwner);
  If Length(slParams[iSwitch]) = iIndex Then
    Exit;
  IncrementSwitchPosition(slParams, iIndex, iSwitch,
    strOpenSquareExpectedInOwnerSearchDef);
  If slParams[iSwitch][iIndex] = '[' Then
    Begin
      Result := True;
      IncrementSwitchPosition(slParams, iIndex, iSwitch, strMissingSearchText);
      strOwnerRegEx := ExtractOwnerSearch;
    End Else
      Dec(iIndex);
End;

(**

  This local function searches the current command line parameter for the given end token while 
  collecting the date string.

  @precon  None.
  @postcon Searches the current command line parameter for the given end token while collecting the date
           string.

  @param   slParams        as a TStringList as a constant
  @param   chEndToken      as a TSearchCharSet as a constant
  @param   strExceptionMsg as a String as a constant
  @param   iIndex          as an Integer as a reference
  @param   iSwitch         as an Integer as a reference
  @param   strValue        as a String as a reference
  @return  a Char

**)
Function GetRangeString(Const slParams: TStringList; Const chEndToken: TSearchCharSet;
  Const strExceptionMsg: String; Var iIndex, iSwitch: Integer; Var strValue: String) : Char;

Begin
  Result := #0;
  strValue := '';
  While (Length(slParams[iSwitch]) >= iIndex) And (Not CharInSet(slParams[iSwitch][iIndex], chEndToken)) Do
    Begin
      IncrementSwitchPosition(slParams, iIndex, iSwitch, strExceptionMsg);
      If Not CharInSet(slParams[iSwitch][iIndex], chEndToken) Then
        strValue := strValue + slParams[iSwitch][iIndex]
      Else
        Result := slParams[iSwitch][iIndex];
    End;
End;

(**

  This method gets the command line parameter for the search in text file for a text string option.

  @precon  None.
  @postcon Gets the command line parameter for the search in text file for a text string option.

  @param   slParams          as a TStringList as a constant
  @param   iSwitch           as an Integer as a reference
  @param   iIndex            as an Integer as a reference
  @param   strRegExText      as a String as a reference
  @param   iSurroundingLines as an Integer as a reference

**)
Procedure GetSearchInInfo(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  var strRegExText: String; var iSurroundingLines : Integer);

ResourceString
  strInvalidGREPSpec = 'Invalid regular expression specification "%s"!';
  strExpectsRegExpr = 'Commandline switch /I expects an option digit and regular expression in ' + 
    'square brackets immediately after';
  
Const
  strRegExSwitchPattern = '(?<SurroundLines>[0-9]*)[[](?<RegExText>.*?)\]';
  iNoOfSurroundingLines = 1;
  iRegExText = 2;

Var
  RE : TRegEx;
  M: TMatch;
  i: Integer;
  iErrorCode: Integer;
  strConfig: String;

Begin
  iSurroundingLines := 0;
  Include(CommandLineSwitches, clsRegExSearch);
  If iIndex < Length(slParams[iSwitch]) Then
    Inc(iIndex);
  If (iIndex <= Length(slParams[iSwitch])) And (CharInSet(slParams[iSwitch][iIndex], ['[', '0'..'9'])) Then
    Begin
      RE.Create(strRegExSwitchPattern, [roIgnoreCase, roSingleLine, roCompiled]);
      strConfig := Copy(slParams[iSwitch], iIndex, Length(slParams[iSwitch]) - iIndex + 1);
      M := RE.Match(strConfig);
      If M.Success Then
        Begin
          Val(M.Groups[iNoOfSurroundingLines].Value, iSurroundingLines, iErrorCode);
          strRegExText := M.Groups[iRegExText].Value;
          For i := 1 To Pred(Length(M.Value)) Do //FI:W528
            IncrementSwitchPosition(slParams, iIndex, iSwitch, strCloseSquareExpectedInRegExSearchDef);
        End Else
          Raise ESearchException.CreateFmt(strInvalidGREPSpec, [strConfig]);
    End Else
      Raise ESearchException.Create(strExpectsRegExpr);
End;

(**

  This function parses the size format directive.

  @precon  slParams must eb avalid instance with iSwitch and iIndex being valid indexes into the 
           stringlist and characters of the string list respectively.
  @postcon Returns the size format requested else raises an exception.

  @param   slParams   as a TStringList as a constant
  @param   iSwitch    as an Integer as a reference
  @param   iIndex     as an Integer as a reference
  @param   SizeFormat as a TSizeFormat as a reference

**)
Procedure GetSizeFormat(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var SizeFormat: TSizeFormat);

Begin
  Include(CommandLineSwitches, clsSizeOutput);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strColonExpectedInSizeFormat);
  If slParams[iSwitch][iIndex] <> ':' Then
    Raise ESearchException.Create(strColonExpectedInSizeFormat);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strMissingSizeFormatDirective);
  Case slParams[iSwitch][iIndex] Of
    'k', 'K': SizeFormat := sfKilobytes;
    'm', 'M': SizeFormat := sfMegaBytes;
    'g', 'G': SizeFormat := sfGigaBytes;
    't', 'T': SizeFormat := sfTeraBytes;
  Else
    Raise ESearchException.Create(strInvalidSizeFormatDirective);
  End
End;

(**

  This method determines of a size range has been specified on the command line.

  @precon  None.
  @postcon Determines of a size range has been specified on the command line.

  @param   slParams as a TStringList as a constant
  @param   iSwitch  as an Integer as a reference
  @param   iIndex   as an Integer as a reference
  @param   iLSize   as an Int64 as a reference
  @param   iUSize   as an Int64 as a reference

**)
Procedure GetSizeRange(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var iLSize, iUSize: Int64);

Const
  iUpperLimit = $F00000000000000;

Var
  strSize: String;

  (**

    This method converts the size as a string into a Int64 in the passed variable.

    @precon  None.
    @postcon Converts the size as a string into a Int64 in the passed variable.

    @param   iSize           as an Int64 as a reference
    @param   strExceptionMsg as a String as a constant
    @param   iDefault        as an Int64 as a constant

  **)
  Procedure GetSize(Var iSize: Int64; Const strExceptionMsg: String; Const iDefault: Int64);

  Const
    iTerraByte = $10000000000;
    iGigaByte = $40000000;
    iMegaByte = $100000;
    iKiloByte = $400;

  Var
    iErrorCode: Integer;
    iFactor   : Int64;

  Begin
    If strSize <> '' Then
      Begin
        Case strSize[Length(strSize)] Of
          'k', 'K': iFactor := iKiloByte;
          'm', 'M': iFactor := iMegaByte;
          'g', 'G': iFactor := iGigaByte;
          't', 'T': iFactor := iTerraByte;
        Else
          iFactor := 1;
        End;
        If CharInSet(strSize[Length(strSize)], ['k', 'K', 'm', 'M', 'g', 'G', 't', 'T']) Then
          strSize := Copy(strSize, 1, Length(strSize) - 1);
        Val(strSize, iSize, iErrorCode);
        If iErrorCode > 0 Then
          Raise ESearchException.Create(strExceptionMsg);
        iSize := iSize * iFactor;
      End
    Else
      iSize := iDefault;
  End;

Begin
  Include(CommandLineSwitches, clsSizeRange);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strOpenSquareExpectedInSize);
  If slParams[iSwitch][iIndex] <> '[' Then
    Raise ESearchException.Create(strOpenSquareExpectedInSize);
  GetRangeString(slParams, ['-'], strMissingSizeRangeSeparater, iIndex, iSwitch, strSize);
  GetSize(iLSize, strInvalidLowerSizeRange, 0);
  GetRangeString(slParams, [']'], strCloseSquareExpectedInSize, iIndex, iSwitch, strSize);
  GetSize(iUSize, strInvalidUpperSizeRange, iUpperLimit);
End;

(**

  This method determines the summary level required.

  @precon  None.
  @postcon Determines the summary level required.

  @param   slParams      as a TStringList as a constant
  @param   iSwitch       as an Integer as a reference
  @param   iIndex        as an Integer as a constant
  @param   iSummaryLevel as an Integer as a reference

**)
Procedure GetSummaryLevel(Const slParams: TStringList; Var iSwitch: Integer; Const iIndex: Integer;
  Var iSummaryLevel: Integer);

Var
  iCode: Integer;

Begin
  Include(CommandLineSwitches, clsSummaryLevel);
  If iSummaryLevel > 0 Then
    Raise ESearchException.Create(strSummaryAlreadySet);
  Val(slParams[iSwitch][iIndex], iSummaryLevel, iCode);
  If iCode > 0 Then
    Raise ESearchException.CreateFmt(strSummaryLevelException, [slParams[iSwitch]]);
End;

(**

  This method increments the position of the index within the current command line switch and checks to 
  see if the index is valid else raise the exception message provided.

  @precon  None.
  @postcon Checks the index to be valid else raises an exception.

  @param   slParams        as a TStringList as a constant
  @param   iIndex          as an Integer as a reference
  @param   iSwitch         as an Integer as a reference
  @param   strExceptionMsg as a String as a constant

**)
Procedure IncrementSwitchPosition(Const slParams: TStringList; Var iIndex, iSwitch: Integer;
  Const strExceptionMsg: String);

Begin
  Inc(iIndex);
  If Length(slParams[iSwitch]) < iIndex Then
    Raise ESearchException.Create(strExceptionMsg);
End;

(**

  This method outputs the files attributes if they are requireed at the command line.

  @precon  None.
  @postcon Outputs the files attributes if they are requireed at the command line.

  @param   setAttributes as a TSearchFileAttrs as a constant
  @return  a String

**)
Function OutputAttributes(Const setAttributes: TSearchFileAttrs): String;

Type
  TSearchAttrPos = (sapReadOnlyPos, sapArchivePos, sapSystemPos, sapHiddenPos, sapDirFileVolPos,
    sapNormalPos, sapDevicePos, sapTemporaryPos, sapSparsePos, sapReparsePos,
    sapCompressedPos, sapOffLinePos, sapIndexedPos, sapEncryptedPos, sapVirtualPos);

  (**

    This procedure added the text attribute to the result of tge given attrivute is in the files 
    attributes.

    @precon  None.
    @postcon The text attribute is added if found in the file attributes.

    @param   iAttr     as a TSearchFileAttr as a constant
    @param   eFileAttr as a TSearchAttrPos as a constant
    @param   C         as a Char as a constant

  **)
  Procedure TestAttr(Const iAttr : TSearchFileAttr; Const eFileAttr : TSearchAttrPos; Const C : Char);

  Begin
    If iAttr In setAttributes Then
      Result[Succ(Integer(eFileAttr))] := C;
  End;

Begin
  //         RASHDNdTsrCOIEv
  Result := '....F..........';
  TestAttr(sfaReadOnly, sapReadOnlyPos, 'R');
  TestAttr(sfaSystem, sapSystemPos, 'S');
  TestAttr(sfaHidden, sapHiddenPos, 'H');
  TestAttr(sfaDirectory, sapDirFileVolPos, 'D');
  TestAttr(sfaVolume, sapDirFileVolPos, 'V');
  TestAttr(sfaArchive, sapArchivePos, 'A');
  TestAttr(sfaDevice, sapDevicePos, 'd');
  TestAttr(sfaNormal, sapNormalPos, 'N');
  TestAttr(sfaTemporary, sapTemporaryPos, 'T');
  TestAttr(sfaSparse, sapSparsePos, 's');
  TestAttr(sfaReparse, sapReparsePos, 'r');
  TestAttr(sfaCompressed, sapCompressedPos, 'C');
  TestAttr(sfaOffLine, sapOffLinePos, 'O');
  TestAttr(sfaIndexed, sapIndexedPos, 'I');
  TestAttr(sfaEncrypted, sapEncryptedPos, 'E');
  TestAttr(sfaVirtual, sapVirtualPos, 'v');
End;

(**

  This function outputs the given text to the console references by the given handle using the text and 
  background colours provided. If xlNone is used for the colours then the consoles default colours are 
  used. DOES NOT add a carraige return at the end of each line.

  @precon  hndConsole must be a valid console handle.
  @postcon Outputs the given text to the console references by the given handle using the text and 
           background colours provided.

  @param   hndConsole       as a THandle as a constant
  @param   strText          as a String as a constant
  @param   iTextColour      as a TColor as a constant
  @param   iBackColour      as a TColor as a constant
  @param   boolUpdateCursor as a Boolean as a constant

**)
Procedure OutputToConsole(Const hndConsole : THandle; Const strText : String = '';
  Const iTextColour : TColor = clNone; Const iBackColour : TColor = clNone;
  Const boolUpdateCursor : Boolean = True);

Const
  iForegroundMask = $0F;
  iBackgroundMask = $F0;

  (**

    This method sets the fore and backgroudn colours for the console output.

    @precon  None.
    @postcon The console colour attributes are updated with the required colours.

    @param   ConsoleInfo as a TConsoleScreenBufferInfo as a constant
    @param   wChars      as a DWORD as a reference
    @param   strTABText  as a String as a constant

  **)
  Procedure UpdateConsoleColours(Const ConsoleInfo : TConsoleScreenBufferInfo; Var wChars : DWORD;
    Const strTABText : String);

  Var
    iForeAttrColour, iBackAttrColour : Integer;
    iChar : Integer;
    Attrs : Array of Word;
  
  Begin
    SetLength(Attrs, wChars);
    iForeAttrColour := ForeGroundColour(iTextColour, ConsoleInfo.wAttributes And iForegroundMask);
    iBackAttrColour := BackGroundColour(iBackColour, ConsoleInfo.wAttributes And iBackgroundMask);
    If wChars > 0 Then
      For iChar := 0 To wChars - 1 Do
        Attrs[iChar] := iForeAttrColour Or iBackAttrColour;
    Win32Check(WriteConsoleOutputAttribute(hndConsole, Attrs,
      Length(strTABText), ConsoleInfo.dwCursorPosition, wChars));
  End;
  
  (**

    This method sets the position of the cursor after the output of the string to the console.

    @precon  None.
    @postcon The position of the console cursor is updated.

    @param   OldPos as a TCoord as a constant
    @param   NewPos as a TCoord as a constant
    @param   wChars as a DWORD as a reference

  **)
  Procedure UpdateCursor(Const OldPos, NewPos : TCoord; Var wChars : DWORD);

  Begin
    If boolUpdateCursor Then
      Begin
        // The only time the below fails is at the end of the buffer and a new
        // line is required, hence the new line on failure.
        If Not SetConsoleCursorPosition(hndConsole, NewPos) Then
          Win32Check(WriteConsole(hndConsole, PChar(#13#10), Length(#13#10), wChars, Nil));
      End Else
        Win32Check(SetConsoleCursorPosition(hndConsole, OldPos));
  End;
  
  (**

    This procedure update the NewPos record based on the console information.

    @precon  None.
    @postcon The NewPos record is updated.

    @param   ConsoleInfo as a TConsoleScreenBufferInfo as a constant
    @param   NewPos      as a TCoord as a reference

  **)
  Procedure UpdateNewPos(Const ConsoleInfo : TConsoleScreenBufferInfo; Var NewPos : TCoord);

  Begin
    While NewPos.X >= ConsoleInfo.dwSize.X Do
      Begin
        Inc(NewPos.Y);
        Dec(NewPos.X, ConsoleInfo.dwSize.X);
      End;
  End;

Var
  ConsoleInfo : TConsoleScreenBufferInfo;
  wChars : DWord;
  OldPos : TCoord;
  NewPos : TCoord;
  strTABText : String;

Begin
  strTabText := StringReplace(strText, #9, #175, [rfReplaceAll]);
  If CheckConsoleMode(hndConsole) Then
    Begin
      Repeat
        Win32Check(GetConsoleScreenBufferInfo(hndConsole, ConsoleInfo));
        OldPos := ConsoleInfo.dwCursorPosition;
        NewPos := OldPos;
        Win32Check(WriteConsoleOutputCharacter(hndConsole, PChar(strTABText), Length(strTABText),
          ConsoleInfo.dwCursorPosition, wChars));
        UpdateConsoleColours(ConsoleInfo, wChars, strTABText);
        If wChars > 0 Then
          Delete(strTABText, 1, wChars);
        Inc(NewPos.X, wChars);
        UpdateNewPos(ConsoleInfo, NewPos);
        If strTABText <> '' Then
          Begin
            Win32Check(WriteConsole(hndConsole, PChar(#13#10), Length(#13#10), wChars, Nil));
            Inc(NewPos.Y);
            NewPos.X := 0;
          End;
      Until strTABText = '';
      UpdateCursor(OldPos, NewPos, wChars);
    End Else
      Write(strTABText);
End;

(**

  This function outputs the given text to the console references by the given handle using the text and 
  background colours provided. If xlNone is used for the colours then the consoles default colours are 
  used. Adds a Carriage Return at the end of each line.

  @precon  hndConsole must be a valid console handle.
  @postcon Outputs the given text to the console references by the given handle using the text and
           background colours provided.

  @param   hndConsole       as a THandle as a constant
  @param   strText          as a String as a constant
  @param   iTextColour      as a TColor as a constant
  @param   iBackColour      as a TColor as a constant
  @param   boolUpdateCursor as a Boolean as a constant

**)
Procedure OutputToConsoleLn(Const hndConsole : THandle; Const strText : String = '';
  Const iTextColour : TColor = clNone; Const iBackColour : TColor = clNone;
  Const boolUpdateCursor : Boolean = True);

Var
  sl : TStringList;
  i : Integer;
  wChars : Cardinal;

Begin
  sl := TStringList.Create;
  Try
    sl.Text := strText;
    If sl.Text = '' Then
      sl.Text := #13#10;
    For i := 0 to sl.Count - 1 Do
      Begin
        If CheckConsoleMode(hndConsole) Then
          Begin
            OutputToConsole(hndConsole, sl[i], iTextColour, iBackColour,
              boolUpdateCursor);
            Win32Check(WriteConsole(hndConsole, PChar(#13#10), Length(#13#10), wChars, Nil));
          End Else
            WriteLn(sl[i]);
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This function returns the users logon name as a String.

  @precon  None.
  @postcon Returns the users logon name as a String.

  @return  a String

**)
Function UserName : String;

Var
  i : Cardinal;

Begin
  i := MAX_PATH;
  SetLength(Result, i);
  GetUserName(@Result[1], i);
  Win32Check(LongBool(i));
  SetLength(Result, i - 1);
End;

(** Initialises the console more to Unknown to force a call to the Win32 API **)
Initialization
  ConsoleMode := cmUnknown;
End.
