(**

  This module contains applications function which can also be tested in a
  DUnit framework.

  @Version 1.0
  @Author  David Hoyle
  @Date    07 Apr 2018

**)
Unit Search.Functions;

Interface

Uses
  System.SysUtils,
  System.Classes,
  WinAPI.Windows,
  VCL.Graphics,
  FileHandling;

Type
  (** A custom Exception for any exceptions raised by incorrect information
      in the search criteria. **)
  ESearchException = Class(Exception);

  (** This is a list of boolean on / off command line switches. **)
  TCommandLineSwitch = (clsShowHelp, { /? or -? or /h or -h }
    clsSubDirectories,               { /s or -s }
    clsDebug,                        { /!       }
    clsShowAttribs,                  { /a or -a }
    clsSummaryLevel,                 { /1..9 or -1..9 }
    clsSupressZeros,                 { /0 or -0 }
    clsDateRange,                    { /d or -d }
    clsSizeRange,                    { /z or -z }
    clsAttrRange,                    { /t or -t }
    clsQuiet,                        { /q or -q }
    clsOwner,                        { /w or -w }
    clsOrderBy,                      { /o or -o }
    clsRegExSearch,                  { /i or -i }
    clsDateType,                     { /e or -e }
    clsDisplayCriteria,              { /c or -c }
    clsExclusions,                   { /x or -x }
    clsSearchZip,                    { /p or -p }
    clsSizeOutput,                   { /f or -f }
    clsOutputAsCSV                   { /v or -v }
    );

  (** This is a set of boolean command line switches. **)
  TCommandLineSwitches = Set Of TCommandLineSwitch;

  (** An enumerate to define the type of date to display and search on. **)
  TDateType = (dtCreation, dtLastAccess, dtLastWrite);

  (** This is an enumerate to defines whether the owner should be searched. **)
  TOwnerSearch = (osEquals, osNotEquals);

  (** This is an enumerate to defines where the owner should be searched. **)
  TOwnerSearchPos = (ospNone, ospExact, ospStart, ospMiddle, ospEnd);

  (** This is an enumerate to defines the output size formats. **)
  TSizeFormat = (sfNone, sfKilobytes, sfMegaBytes, sfGigaBytes, sfTeraBytes);

  (** A Procedure for feeding back errors. **)
  TLogErrorProc = Procedure(strErrorMsg, strFileNmae: String) Of Object;

  (** This is a procedure type for handling Exception messages in ParseMacro. **)
  TExceptionProcedure = Procedure(strExceptionMsg : String) Of Object;

Const
  (** A constant to define that only files should be listed. **)
  iFileOnly = $0100;

ResourceString
  (** An exception message for a missing Search Text definition. **)
  strMissingSearchText = 'Missing Search Text';

Var
  (** Define the applications command line switches. **)
  CommandLineSwitches: TCommandLineSwitches;

  Procedure IncrementSwitchPosition(Const slParams: TStringList; Var iIndex, iSwitch: Integer;
    Const strExceptionMsg: String);
  Procedure GetRangeString(Const slParams: TStringList; Const chEndToken: Char;
    Const strExceptionMsg: String; Var iIndex, iSwitch: Integer; Var strValue: String);
  Procedure GetDateRange(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var dtLDate, dtUdate: Double);
  Procedure GetSummaryLevel(Const slParams: TStringList; Var iSwitch: Integer; Const iIndex: Integer;
    Var iSummaryLevel: Integer);
  Procedure GetSizeRange(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var iLSize, iUSize: Int64);
  Procedure GetAttributes(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var iFileAttrs, iTypeAttrs: Integer);
  Procedure GetOrderBy(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var OrderFilesDirection: TOrderDirection; Var OrderFilesBy: TOrderBy);
  Procedure GetSearchInInfo(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    var strRegExText: String; var iSurroundingLines : Integer);
  Procedure GetDateType(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var DateType: TDateType);
  Procedure GetExclusions(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var strExlFileName: String);
  Procedure GetOwnerSwitch(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var OwnerSearch: TOwnerSearch; Var OwnerSearchPos: TOwnerSearchPos;
  Var strOwnerSearch: String);
  Function OutputAttributes(Const iAttr: Integer): String;
  Procedure CheckDateRange(Const iDateTime: Integer; Const dtLDate, dtUdate: Double;
    Var boolFound: Boolean; Const strFileName : String; Const LogErrorProc : TLogErrorProc);
  Procedure CheckSizeRange(Const iSize, iLSize, iUSize: Int64; Var boolFound: Boolean);
  Procedure CheckFileAttributes(Const SearchAttrs: Integer; Const iFileAttrs, iTypeAttrs: Integer;
    Var boolFound: Boolean);
  Procedure CheckExclusions(Const strPath, strFilename: String; Var boolFound: Boolean;
    Const slExclusions: TStringList);
  Procedure CheckOwner(Const strOwner, strOwnerSearch: String; Const OwnerSearchPos: TOwnerSearchPos;
    Const OwnerSearch: TOwnerSearch; Var boolFound: Boolean);
  Procedure GetSizeFormat(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var SizeFormat: TSizeFormat);
  Function SafeFileDateToDateTime(Const iFileDate: Integer; Const strFilename: String;
    Const LogErrorProc: TLogErrorProc): TDateTime;
  Function PosOfNthChar(Const strText : String; Const Ch : Char; Const iIndex : Integer;
    Const boolIgnoreQuotes : Boolean = True): Integer;
  Function CheckConsoleMode(Const hndConsole : THandle) : Boolean;
  Procedure OutputToConsoleLn(Const hndConsole : THandle; Const strText : String = '';
    Const iTextColour : TColor = clNone; Const iBackColour : TColor = clNone;
    Const boolUpdateCursor : Boolean = True);
  Procedure OutputToConsole(Const hndConsole : THandle; Const strText : String = '';
    Const iTextColour : TColor = clNone; Const iBackColour : TColor = clNone;
    Const boolUpdateCursor : Boolean = True);
  Function GetConsoleTitle : String;
  Function Like(Const strPattern, strText : String) : Boolean;
  Function CharCount(Const cChar : Char; Const strText : String;
    Const boolIgnoreQuotes : Boolean = True) : Integer;
  Function BuildRootKey(Const slParams : TStringList;
    Const ExceptionProc : TExceptionProcedure) : String;
  Function GetField(Const strText : String; Const Ch : Char; Const iIndex : Integer;
    Const boolIgnoreQuotes : Boolean = True): String;
  
Implementation

Uses
  WinAPI.ShlObj,
  RegularExpressions;

Type
  (** An enumerate to define the current console output mode **)
  TConsoleMode = (cmUnknown, cmStandard, cmRedirected);

ResourceString
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
  (** An exception message for the Owner Search Criteria being empty. **)
  strOwnerSearchIsEmpty = 'You must specify a valid Owner Search Criteria.';
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
  Function IsKeyWord(Const strWord : String; Const strWordList : Array Of String): Boolean; Forward;
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
    Raise Exception.Create('Invalid console colour.');
  End;
End;

(**

  This method builds the root key INI filename for the loading and saving of settings from the instance 
  handle for the module.

  @precon  slParams must be a valid instance of a TStringList class.
  @postcon Builds the root key INI filename for the loading and saving of settings from the instance 
           handle for the module.

  @param   slParams      as a TStringList as a constant
  @param   ExceptionProc as a TExceptionProcedure as a constant
  @return  a String

**)
Function BuildRootKey(Const slParams : TStringList; Const ExceptionProc : TExceptionProcedure) : String;

ResourceString
  strExpectedSquare = 'Expected "[" at position 3 in alternate INI file parameter.';
  strExpectedClosingSquare = 'Expected a closing "]" in alternate INI file parameter.';
  strPathDoesNotExist = 'The path "%s" does not exist for the alternate INI file.';
  strINIPattern = '%s Settings for %s on %s.INI';
  strSeasonsFall = '\Season''s Fall\';

  (**

    This function parses the alternate INI filename from the parameter.

    @precon  None.
    @postcon Parses the alternate INI filename from the parameter.

    @param   strDefaultINI as a String
    @param   strParam      as a String
    @return  a String

  **)
  Function ParseAlternateINIFile(strDefaultINI, strParam : String) : String;

  Var
    i : Integer;
    strFileName : String;

  Begin
    Result := strDefaultINI;
    i := 3;
    If strParam[i] <> '[' Then
      If Assigned(ExceptionProc) Then
        Begin
          ExceptionProc(strExpectedSquare);
          Exit;
        End;
    Inc(i);
    strFileName := '';
    While (i <= Length(strParam)) And (strParam[i] <> ']') Do
      Begin
        strFileName := strFileName + strParam[i];
        Inc(i);
        If i > Length(strParam) Then
          If Assigned(ExceptionProc) Then
            Begin
              ExceptionProc(strExpectedClosingSquare);
              Exit;
            End;
      End;
    strFileName := ExpandUNCFileName(strFileName);
    If DirectoryExists(ExtractFilePath(strFileName)) Then
      Result := strFileName
    Else
      If Assigned(ExceptionProc) Then
        ExceptionProc(Format(strPathDoesNotExist, [ExtractFilePath(strFileName)]));
  End;

var
  strModuleName : String;
  strINIFileName : String;
  strUserAppDataPath : String;
  strBuffer : String;
  iParam : Integer;
  iSize : Integer;                                                              

begin
  SetLength(strBuffer, MAX_PATH);
  iSize := GetModuleFileName(hInstance, PChar(strBuffer), MAX_PATH);
  SetLength(strBuffer, iSize);
  strModuleName := strBuffer;
  strINIFileName := ChangeFileExt(ExtractFileName(strBuffer), '');
  While (Length(strIniFilename) > 0) And
    (CharInSet(strIniFileName[Length(strIniFilename)], ['0'..'9'])) Do
    strIniFileName := Copy(strIniFileName, 1, Length(strIniFileName) - 1);
  strINIFileName :=  Format(strINIPattern, [strIniFileName, UserName, ComputerName]);
  SetLength(strBuffer, MAX_PATH);
  SHGetFolderPath(0, CSIDL_APPDATA Or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT, PChar(strBuffer));
  strBuffer := StrPas(PChar(strBuffer));
  strUserAppDataPath := strBuffer + strSeasonsFall;
  If Not DirectoryExists(strUserAppDataPath) Then
    ForceDirectories(strUserAppDataPath);
  Result := strUserAppDataPath + strINIFileName;
  If Like('*.exe', ExtractFileExt(strModuleName)) Then
    If slParams <> Nil Then
      For iParam := 1 To ParamCount Do
        Begin
          If Length(ParamStr(iParam)) > 0 Then
            If CharInSet(ParamStr(iParam)[1], ['-', '/']) Then
              If Length(ParamStr(iParam)) > 1 Then
                If CharInSet(ParamStr(iParam)[2], ['@']) Then
                  Begin
                    Result := ParseAlternateINIFile(Result, ParamStr(iParam));
                    Continue;
                  End;
          slParams.Add(ParamStr(iParam));
        End;
end;

(**

  This routine returns the number of occurrances of the char found in the string .

  @precon  None.
  @postcon Returns the number of occurrances of the char found in the string.

  @param   cChar            as a Char as a constant
  @param   strText          as a String as a constant
  @param   boolIgnoreQuotes as a Boolean as a constant
  @return  an Integer

**)
Function CharCount(Const cChar : Char; Const strText : String;
  Const boolIgnoreQuotes : Boolean = True) : Integer;

Var
  iCount : Integer;
  boolInQuotes : Boolean;

Begin
  Result := 0;
  boolInQuotes := False;
  For iCount := 1 to Length(strText) Do
    Begin
      If Not boolIgnoreQuotes Then
        If strText[iCount] = '"' Then
          boolInQuotes := Not boolInQuotes;
      If strText[iCount] = cChar Then
        If Not boolInQuotes Then
          Inc(Result);
    End;
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

  @param   iDateTime    as an Integer as a constant
  @param   dtLDate      as a Double as a constant
  @param   dtUdate      as a Double as a constant
  @param   boolFound    as a Boolean as a reference
  @param   strFileName  as a String as a constant
  @param   LogErrorProc as a TLogErrorProc as a constant

**)
Procedure CheckDateRange(Const iDateTime: Integer; Const dtLDate, dtUdate: Double;
  Var boolFound: Boolean; Const strFileName : String; Const LogErrorProc : TLogErrorProc);

Begin
  If clsDateRange In CommandLineSwitches Then
    Begin
      boolFound := boolFound And 
        (SafeFileDateToDateTime(iDateTime, strFileName, LogErrorProc) >= dtLDate);
      boolFound := boolFound And
        (SafeFileDateToDateTime(iDateTime, strFileName, LogErrorProc) <= dtUdate);
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

  @param   SearchAttrs as an Integer as a constant
  @param   iFileAttrs  as an Integer as a constant
  @param   iTypeAttrs  as an Integer as a constant
  @param   boolFound   as a Boolean as a reference

**)
Procedure CheckFileAttributes(Const SearchAttrs: Integer; Const iFileAttrs, iTypeAttrs: Integer;
  Var boolFound: Boolean);

Var
  iAttributes: Integer;

Begin
  If clsAttrRange In CommandLineSwitches Then
    Begin
      If faDirectory And SearchAttrs > 0 Then
        iAttributes := faDirectory
      Else If faVolumeID And SearchAttrs > 0 Then
        iAttributes := faVolumeID
      Else
        iAttributes := iFileOnly;
      boolFound     := boolFound And (iAttributes And iTypeAttrs > 0);
      boolFound := boolFound And ((SearchAttrs And iFileAttrs > 0) Or (iFileAttrs = 0));
    End;
End;

(**

  This method check the owner of the file against the owner search criteria.

  @precon  None.
  @postcon Check the owner of the file against the owner search criteria.

  @param   strOwner       as a String as a constant
  @param   strOwnerSearch as a String as a constant
  @param   OwnerSearchPos as a TOwnerSearchPos as a constant
  @param   OwnerSearch    as a TOwnerSearch as a constant
  @param   boolFound      as a Boolean as a reference

**)
Procedure CheckOwner(Const strOwner, strOwnerSearch: String; Const OwnerSearchPos: TOwnerSearchPos;
  Const OwnerSearch: TOwnerSearch; Var boolFound: Boolean);

Var
  i   : Integer;
  bool: Boolean;

Begin
  i := Length(strOwnerSearch);
  Case OwnerSearchPos Of
    ospExact:  bool := AnsiCompareText(strOwner, strOwnerSearch) = 0;
    ospStart:  bool := AnsiCompareText(strOwnerSearch, Copy(strOwner, 1, i)) = 0;
    ospMiddle: bool := Pos(LowerCase(strOwnerSearch), LowerCase(strOwner)) > 0;
    ospEnd:    bool := AnsiCompareText(strOwnerSearch,
        Copy(strOwner, Length(strOwner) - i + 1, i)) = 0;
  Else
    bool := True;
  End;
  If OwnerSearch = osNotEquals Then
    bool    := Not bool;
  boolFound := boolFound And bool
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
  i := 1024;
  SetLength(Result, i);
  GetComputerName(@Result[1], i);
  Win32Check(LongBool(i));
  SetLength(Result, i);
End;

(**

  This function converts a freeform text string representing dates and times
  in standard formats in to a TDateTime value.

  @precon  strDate is the string to convert into a date.
  @postcon Returns a valid TDateTime value.

  @param   strDate as a String as a Constant
  @return  a TDateTime

**)
Function ConvertDate(Const strDate : String) : TDateTime;

Type
  (** This is a record that defined the date and time for a date. **)
  TDateRec = Record
    iDay, iMonth, iYear, iHour, iMinute, iSecond, iMilli : Word;
  End;

Const
  strErrMsg = 'Can not convert the date "%s" to a valid TDateTime value.';
  Delimiters : Set Of AnsiChar = ['-', ' ', '\', '/', ':', '.'];
  Days : Array[1..7] Of String = ('fri', 'mon', 'sat', 'sun', 'thu', 'tue', 'wed');
  Months : Array[1..24] Of String = (
    'apr', 'april',
    'aug', 'august',
    'dec', 'december',
    'feb', 'february',
    'jan', 'january',
    'jul', 'july',
    'jun', 'june',
    'mar', 'march',
    'may', 'may',
    'nov', 'november',
    'oct', 'october',
    'sep', 'september'
    );
  MonthIndexes : Array[1..24] Of Word = (
    4, 4,
    8, 8,
    12, 12,
    2, 2,
    1, 1,
    7, 7,
    6, 6,
    3, 3,
    5, 5,
    11, 11,
    10, 10,
    9, 9
  );

Var
  i : Integer;
  sl : TStringList;
  strToken : String;
  iTime : Integer;
  recDate : TDateRec;
  tmp : Word;
  iIndex0, iIndex1, iIndex2 : Integer;

  (**

    This procedure adds the token to the specified string list and clears the token.

    @precon  StringList is the string list to add the token too and strToken is the token to add to the 
             list.
    @postcon Adds the token to the specified string list and clears the token.

    @param   StringList as a TStringList as a constant
    @param   strToken   as a String as a reference

  **)
  Procedure AddToken(Const StringList : TStringList; var strToken  : String);

  Begin
    If strToken <> '' Then
      Begin
        StringList.Add(strToken);
        strToken := '';
      End;
  End;

  (**

    This procedure assigns string list indexes to the three index values
    according to the short date format and what information is supplied.

    @precon  None.
    @postcon Assigns string list indexes to the three index values
             according to the short date format and what information is
             supplied.

  **)
  Procedure AssignIndexes();

  Var
    slFormat : TStringList;
    str : String;
    j : Integer;

  Begin
    iIndex0 := 0; // Default Day / Month / Year
    iIndex1 := 1;
    iIndex2 := 2;
    slFormat := TStringList.Create;
    Try
      str := '';
      For j := 1 To Length(FormatSettings.ShortDateFormat) Do
        If CharInSet(FormatSettings.ShortDateFormat[j], Delimiters) Then
          AddToken(slFormat, str)
        Else
          str := str + FormatSettings.ShortDateFormat[j];
      AddToken(slFormat, str);
      // Remove day of week
      For j := slFormat.Count - 1 DownTo 0 Do
        If (CharInSet(slFormat[j][1], ['d', 'D'])) And (Length(slFormat[j]) > 2) Then
          slFormat.Delete(j);
      For j := 0 To slFormat.Count - 1 Do
        Begin
          If CharInSet(slFormat[j][1], ['d', 'D']) Then
            iIndex0 := j;
          If CharInSet(slFormat[j][1], ['m', 'M']) Then
            iIndex1 := j;
          If CharInSet(slFormat[j][1], ['y', 'Y']) Then
            iIndex2 := j;
        End;
    Finally
      slFormat.Free;
    End;
  End;

  (**

    This procedure tries to extract the value from the indexed string list item into the passed variable 
    reference. It delete is true it remove the item from the string list.

    @precon  iIndex is the index of the item from the string list to extract, iValue is a word variable 
             to place the converted item into and Delete determines whether the item is removed from 
             the string list.
    @postcon Tries to extract the value from the indexed string list item into the passed variable 
             reference. It delete is true it remove the item from the string list.

    @param   iIndex as an Integer as a constant
    @param   iValue as a Word as a reference
    @param   Delete as a Boolean as a constant

  **)
  Procedure ProcessValue(Const iIndex : Integer; var iValue : Word; Const Delete : Boolean);

  Begin
    If iIndex > sl.Count - 1 Then Exit;
    Val(sl[iIndex], iValue, i);
    If i <> 0 Then
      Raise ESearchException.CreateFmt(strErrMsg, [strDate]);
    If Delete Then
      sl.Delete(iIndex);
  End;

Begin
  sl := TStringList.Create;
  Try
    strToken := '';
    iTime := -1;
    For i := 1 To Length(strDate) Do
      If CharInSet(strDate[i], Delimiters) Then
        Begin
          AddToken(sl, strToken);
          If (CharInSet(strDate[i], [':'])) And (iTime = -1) Then
            iTime := sl.Count - 1;
        End Else
          strToken := strToken + strDate[i];
    AddToken(sl, strToken);
    FillChar(recDate, SizeOf(recDate), 0);
    // Decode time
    If iTime > -1 Then
      Begin
        ProcessValue(iTime,recDate.iHour, True);
        ProcessValue(iTime,recDate.iMinute, True);
        ProcessValue(iTime,recDate.iSecond, True);
        ProcessValue(iTime,recDate.iMilli, True);
      End;
    // Remove day value if present
    For i := sl.Count - 1 DownTo 0 Do
      If IsKeyWord(sl[i], Days) Then
        sl.Delete(i);
    // Decode date
    Case sl.Count Of
      1 :
        Begin
          DecodeDate(Now, recDate.iYear, recDate.iMonth, tmp);
          ProcessValue(0, recDate.iDay, False); // Day only
        End;
      2, 3 : // Day and Month (Year)
        Begin
          DecodeDate(Now, recDate.iYear, tmp, tmp);
          AssignIndexes;
          ProcessValue(iIndex0, recDate.iDay, False); // Get day
          If IsKeyWord(sl[iIndex1], Months) Then
            Begin
              For i := Low(Months) To High(Months) Do
                If CompareText(Months[i], sl[iIndex1]) = 0 Then
                  Begin
                    recDate.iMonth := MonthIndexes[i];
                    Break;
                  End;
            End Else
              ProcessValue(iIndex1, recDate.iMonth, False); // Get Month
            If sl.Count = 3 Then
              Begin
                ProcessValue(iIndex2, recDate.iYear, False); // Get Year
                If recDate.iYear < 1900 Then Inc(recDate.iYear, 2000);
              End;
        End;
    Else
      If sl.Count <> 0 Then
        Raise ESearchException.CreateFmt(strErrMsg, [strDate]);
    End;
    // Output result.
    If Not (recDate.iHour In [0..23]) Then
      Raise ESearchException.CreateFmt(strErrMsg, [strDate]);
    If Not (recDate.iMinute In [0..59]) Then
      Raise ESearchException.CreateFmt(strErrMsg, [strDate]);
    If Not (recDate.iSecond In [0..59]) Then
      Raise ESearchException.CreateFmt(strErrMsg, [strDate]);
    Result := EncodeTime(recDate.iHour, recDate.iMinute, recDate.iSecond, recDate.iMilli);
    If recDate.iYear * recDate.iMonth * recDate.iDay <> 0 Then
      Begin
        If Not (recDate.iDay In [1..31]) Then
          Raise ESearchException.CreateFmt(strErrMsg, [strDate]);
        If Not (recDate.iMonth In [1..12]) Then
          Raise ESearchException.CreateFmt(strErrMsg, [strDate]);
        Result := Result + EncodeDate(recDate.iYear, recDate.iMonth, recDate.iDay);
      End;
  Finally
    sl.Free;
  End;
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
    Raise Exception.Create('Invalid console colour.');
  End;
End;

(**

  This method determines the attribute range from the command line.

  @precon  None.
  @postcon Determines the attribute range from the command line.

  @param   slParams   as a TStringList as a constant
  @param   iSwitch    as an Integer as a reference
  @param   iIndex     as an Integer as a reference
  @param   iFileAttrs as an Integer as a reference
  @param   iTypeAttrs as an Integer as a reference

**)
Procedure GetAttributes(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var iFileAttrs, iTypeAttrs: Integer);

Begin
  Include(CommandLineSwitches, clsAttrRange);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strOpenSquareExpectedInAttribDef);
  If slParams[iSwitch][iIndex] <> '[' Then
    Raise ESearchException.Create(strOpenSquareExpectedInAttribDef);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strMissingAttribDirective);
  iFileAttrs := 0;
  iTypeAttrs := 0;
  While (iIndex <= Length(slParams[iSwitch])) And (slParams[iSwitch][iIndex] <> ']') Do
    Begin
      Case slParams[iSwitch][iIndex] Of
        'r', 'R': iFileAttrs := iFileAttrs Or faReadOnly;
        'a', 'A': iFileAttrs := iFileAttrs Or faArchive;
        's', 'S': iFileAttrs := iFileAttrs Or faSysFile;
        'h', 'H': iFileAttrs := iFileAttrs Or faHidden;
        'f', 'F': iTypeAttrs := iTypeAttrs Or iFileOnly;
        'd', 'D': iTypeAttrs := iTypeAttrs Or faDirectory;
        'v', 'V': iTypeAttrs := iTypeAttrs Or faVolumeID;
      Else
        Raise ESearchException.CreateFmt(strNotAValidAttrList,
          [slParams[iSwitch][iIndex]]);
      End;
      IncrementSwitchPosition(slParams, iIndex, iSwitch,
        strCloseSquareExpectedInAttribDef);
    End;
  If iTypeAttrs = 0 Then
    iTypeAttrs := iFileOnly;
End;

(**

  This routine extract the build number from the EXE resources for
  display in the app title.

  @precon  None.
  @postcon Extract the build number from the EXE resources for display in the
           app title.

  @param   strFileName as a String
  @param   iMajor      as an Integer as a reference
  @param   iMinor      as an Integer as a reference
  @param   iBugfix     as an Integer as a reference
  @param   iBuild      as an Integer as a reference
  @return  a String

**)
Function GetBuildNumber(strFileName : String; var iMajor, iMinor, iBugfix,
  iBuild : Integer) : String;

Const
  strBuild = '%d.%d.%d.%d';

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
      GetFileVersionInfo(PChar(strFileName), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      with VerValue^ do
      begin
        iMajor := dwFileVersionMS shr 16;
        iMinor := dwFileVersionMS and $FFFF;
        iBugfix := dwFileVersionLS shr 16;
        iBuild := dwFileVersionLS and $FFFF;
        Result := Format(strBuild, [iMajor, iMinor, iBugfix, iBuild]);
      end;
      FreeMem(VerInfo, VerInfoSize);
    End Else
      Raise ESearchException.CreateFmt(
        'The executable "%s" does not contain any version information.', [strFileName]);
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
  
Var
  iMajor, iMinor, iBugfix, iBuild : Integer;
  strBuildNumber  : String;
  dtDate : TDateTime;
  strPlatform : String;
  strConfig : String;

Begin
  strBuildNumber := GetBuildNumber(ParamStr(0), iMajor, iMinor, iBugFix, iBuild);
  {$IFDEF DEBUG}
  strConfig := 'DEBUG ';
  {$ELSE}
  strConfig := '';
  {$ENDIF}
  {$IFDEF WIN64}
  strPlatform := '64-bit';
  {$ELSE}
  strPlatform := '32-bit';
  {$ENDIF}
  Result := Format(strTitle, [iMajor, iMinor, strBugFix[iBugFix + 1], strConfig, strBuildNumber,
    strPlatform]);
  FileAge(ParamStr(0), dtDate);
  Result := Result + #13#10 +
    Format('Written by David Hoyle (c) %s', [FormatDateTime('ddd dd/mmm/yyyy', dtDate)]);
End;

(**

  This method determines if a date range has been specified on the command line.

  @precon  None.
  @postcon Determines if a date range has been specified on the command line.

  @param   slParams as a TStringList as a constant
  @param   iSwitch  as an Integer as a reference
  @param   iIndex   as an Integer as a reference
  @param   dtLDate  as a Double as a reference
  @param   dtUdate  as a Double as a reference

**)
Procedure GetDateRange(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var dtLDate, dtUdate: Double);

Const
  iHoursInDay = 23;
  iMinutesInHour = 59;
  iSecondsInMinute = 59;
  iMilliSecInSec = 999;
  strDefaultEndDate = '31/Dec/2099 23:59:59.999';
  strDefaultStartDate = '01/Jan/1900 00:00:00';

Var
  strDate        : String;
  wH, wM, wS, wMS: Word;

Begin
  Include(CommandLineSwitches, clsDateRange);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strOpenSquareExpectedInDate);
  If slParams[iSwitch][iIndex] <> '[' Then
    Raise ESearchException.Create(strOpenSquareExpectedInDate);
  GetRangeString(slParams, '-', strMissingDateRangeSeparater, iIndex, iSwitch, strDate);
  If strDate <> '' Then
    Begin
      dtLDate := ConvertDate(strDate);
      DecodeTime(dtLDate, wH, wM, wS, wMS);
      If (wH = 0) And (wM = 0) And (wS = 0) And (wMS = 0) Then
        dtLDate := dtLDate + EncodeTime(0, 0, 0, 0);
    End
  Else
    dtLDate := ConvertDate(strDefaultStartDate);
  GetRangeString(slParams, ']', strCloseSquareExpectedInDate, iIndex, iSwitch, strDate);
  If strDate <> '' Then
    Begin
      dtUdate := ConvertDate(strDate);
      DecodeTime(dtUdate, wH, wM, wS, wMS);
      If (wH = 0) And (wM = 0) And (wS = 0) And (wMS = 0) Then
        dtUdate := dtUdate + EncodeTime(iHoursInDay, iSecondsInMinute, iMinutesInHour, iMilliSecInSec);
    End
  Else
    dtUdate := ConvertDate(strDefaultEndDate);
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

  This function returns the contents of the specified field in the delimited text.

  @precon  None.
  @postcon Returns the contents of the specified field in the delimited text.

  @param   strText          as a String as a constant
  @param   Ch               as a Char as a constant
  @param   iIndex           as an Integer as a constant
  @param   boolIgnoreQuotes as a Boolean as a constant
  @return  a String

**)
Function GetField(Const strText : String; Const Ch : Char; Const iIndex : Integer;
  Const boolIgnoreQuotes : Boolean = True): String;

Var
  iNumOfFields : Integer;
  iStart, iEnd : Integer;

Begin
  Result := '';
  iNumOfFields := CharCount(Ch, strText, boolIgnoreQuotes) + 1;
  If iIndex = 1 Then
    Begin
      If iNumOfFields > 1  Then
        Begin
          iEnd := PosOfNthChar(strText, Ch, 1, boolIgnoreQuotes);
          Result := Copy(strText, 1, iEnd - 1);
        End Else
          Result := strText;
    End
  Else If (iIndex > 1) And (iIndex < iNumOfFields) Then
    Begin
      iStart := PosOfNthChar(strText, Ch, iIndex - 1, boolIgnoreQuotes);
      iEnd := PosOfNthChar(strText, Ch, iIndex, boolIgnoreQuotes);
      Result := Copy(strText, iStart + 1, iEnd - iStart - 1);
    End
  Else If iIndex = iNumOfFields Then
    Begin
      iStart := PosOfNthChar(strText, Ch, iIndex - 1, boolIgnoreQuotes);
      Result := Copy(strText, iStart + 1, Length(strText) - iStart);
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
    'n', 'N':
      OrderFilesBy := obName;
    'd', 'D':
      OrderFilesBy := obDate;
    's', 'S':
      OrderFilesBy := obSize;
    'a', 'A':
      OrderFilesBy := obAttribute;
    'o', 'O':
      OrderFilesBy := obOwner;
  Else
    Raise ESearchException.Create(strInvalidOrderByDirective);
  End
End;

(**

  This method obtains the owner search information from the command line.

  @precon  None.
  @postcon Obtains the owner search information from the command line.

  @param   slParams       as a TStringList as a constant
  @param   iSwitch        as an Integer as a reference
  @param   iIndex         as an Integer as a reference
  @param   OwnerSearch    as a TOwnerSearch as a reference
  @param   OwnerSearchPos as a TOwnerSearchPos as a reference
  @param   strOwnerSearch as a String as a reference

**)
Procedure GetOwnerSwitch(Const slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var OwnerSearch: TOwnerSearch; Var OwnerSearchPos: TOwnerSearchPos;
  Var strOwnerSearch: String);

Begin
  Include(CommandLineSwitches, clsOwner);
  OwnerSearch    := osEquals;
  OwnerSearchPos := ospNone;
  If Length(slParams[iSwitch]) = iIndex Then
    Exit;
  IncrementSwitchPosition(slParams, iIndex, iSwitch,
    strOpenSquareExpectedInOwnerSearchDef);
  If slParams[iSwitch][iIndex] = '[' Then
    Begin
      OwnerSearchPos := ospExact;
      strOwnerSearch := '';
      IncrementSwitchPosition(slParams, iIndex, iSwitch, strMissingSearchText);
      While (iIndex <= Length(slParams[iSwitch])) And
        (slParams[iSwitch][iIndex] <> ']') Do
        Begin
          strOwnerSearch := strOwnerSearch + slParams[iSwitch][iIndex];
          IncrementSwitchPosition(slParams, iIndex, iSwitch,
            strCloseSquareExpectedInOwnerSearchDef);
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
    End
  Else
    Dec(iIndex);
End;

(**

  This local function searches the current command line parameter for the given end token while 
  collecting the date string.

  @precon  None.
  @postcon Searches the current command line parameter for the given end token while collecting the date
           string.

  @param   slParams        as a TStringList as a constant
  @param   chEndToken      as a Char as a constant
  @param   strExceptionMsg as a String as a constant
  @param   iIndex          as an Integer as a reference
  @param   iSwitch         as an Integer as a reference
  @param   strValue        as a String as a reference

**)
Procedure GetRangeString(Const slParams: TStringList; Const chEndToken: Char;
  Const strExceptionMsg: String; Var iIndex, iSwitch: Integer; Var strValue: String);

Begin
  strValue := '';
  While (Length(slParams[iSwitch]) >= iIndex) And
    (slParams[iSwitch][iIndex] <> chEndToken) Do
    Begin
      IncrementSwitchPosition(slParams, iIndex, iSwitch, strExceptionMsg);
      If slParams[iSwitch][iIndex] <> chEndToken Then
        strValue := strValue + slParams[iSwitch][iIndex];
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

Var
  RE : TRegEx;
  M: TMatch;
  i: Integer;
  iErrorCode: Integer;

Begin
  iSurroundingLines := 0;
  Include(CommandLineSwitches, clsRegExSearch);
  RE.Create('([0-9]*)[[](.*)\]', [roIgnoreCase, roSingleLine, roCompiled]);
  M := RE.Match(slParams[iSwitch]);
  If M.Success Then
    Begin
      Val(M.Groups[1].Value, iSurroundingLines, iErrorCode);
      strRegExText := M.Groups[2].Value;
      For i := 1 To Length(M.Value) Do
        IncrementSwitchPosition(slParams, iIndex, iSwitch, strCloseSquareExpectedInRegExSearchDef);
    End;
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

  Var
    iErrorCode: Integer;
    iFactor   : Int64;

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
        If CharInSet(strSize[Length(strSize)], ['k', 'K', 'm', 'M', 'g', 'G', 't',
            'T']) Then
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
  GetRangeString(slParams, '-', strMissingSizeRangeSeparater, iIndex, iSwitch, strSize);
  GetSize(iLSize, strInvalidLowerSizeRange, 0);
  GetRangeString(slParams, ']', strCloseSquareExpectedInSize, iIndex, iSwitch, strSize);
  GetSize(iUSize, strInvalidUpperSizeRange, $F00000000000000);
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

  This function returns true if the given word is in the supplied word list. It uses a binary search, so 
  the word lists need to be sorted.

  @precon  strWord is the word to be searches for in the word list and strWordList is a static array of 
           words in lowercase and alphabetical order.
  @postcon Returns true if the word is found in the list.

  @param   strWord     as a String as a constant
  @param   strWordList as an Array Of String as a constant
  @return  a Boolean

**)
function IsKeyWord(Const strWord : String; Const strWordList : Array Of String): Boolean;

Var
  l, m, r : Integer;
  str : String;

begin
  Result := False;
  str := LowerCase(strWord);
  l := Low(strWordList);
  r := High(strWordList);
  While l <= r Do
    Begin
      m := (l + r) Div 2;
      If strWordList[m] < str Then
        l := Succ(m)
      Else If strWordList[m] > str Then
        r:= Pred(m)
      Else
        Begin
          Result := True;
          Exit;
        End;
    End;
end;

(**

  This function returns true if the pattern matches the text.

  @precon  None.
  @postcon Returns true if the pattern matches the text.

  @param   strPattern as a String as a constant
  @param   strText    as a String as a constant
  @return  a Boolean

**)
Function Like(Const strPattern, strText : String) : Boolean;

Type
  TMatchType = (mtStart, mtEnd);
  TMatchTypes = Set Of TMatchType;

Var
  MatchTypes : TMatchTypes;
  sl : TStringList;
  i: Integer;
  //iTokenIndex : Integer;
  iStartIndex : Integer;
  iPos: Integer;
  strLPattern : String;

Begin
  Result := False;
  MatchTypes := [];
  strLPattern := strPattern;
  If Length(strLPattern) = 0 Then
    Exit;
  If strLPattern = '*' Then
    Begin
      Result := True;
      Exit;
    End;
  If strLPattern[1] <> '*' Then
    Include(MatchTypes, mtStart)
  Else
    Delete(strLPattern, 1, 1);
  If Length(strLPattern) > 0 Then
    If strLPattern[Length(strLPattern)] <> '*' Then
      Include(MatchTypes, mtEnd)
    Else
      Delete(strLPattern, Length(strLPattern), 1);
  sl := TStringList.Create;
  Try
    For i := 1 To CharCount('*', strLPattern) + 1 Do
      sl.Add(lowercase(GetField(strLPattern, '*', i)));
    // Check start
    //iTokenIndex := 1;
    iStartIndex := 1;
    If sl.Count > 0 Then
      If mtStart In MatchTypes Then
        If CompareText(sl[0], Copy(strText, 1, Length(sl[0]))) <> 0 Then
          Exit
        Else
          Inc(iStartIndex, Length(sl[0]));
    // Check in between
    For i := Integer(mtStart In MatchTypes) To sl.Count - 1 - Integer(mtEnd In MatchTypes) Do
      Begin
        iPos := Pos(sl[i], lowercase(strText));
        If (iPos = 0) Or (iPos < iStartIndex) Then
          Exit;
        //Inc(iTokenIndex, iPos);
        Inc(iStartIndex, Length(sl[i]));
      End;
    // Check end
    If sl.Count > 0 Then
      If mtEnd In MatchTypes Then
        If CompareText(sl[sl.Count - 1], Copy(strText, Length(strText) -
          Length(sl[sl.Count - 1]) + 1, Length(sl[sl.Count - 1]))) <> 0 Then
          Exit;
    Result := True;
  Finally
    sl.Free;
  End;
End;

(**

  This method outputs the files attributes if they are requireed at the command line.

  @precon  None.
  @postcon Outputs the files attributes if they are requireed at the command line.

  @param   iAttr as an Integer as a constant
  @return  a String

**)
Function OutputAttributes(Const iAttr: Integer): String;

Begin
  Result := '.......';
  If faReadOnly And iAttr > 0 Then
    Result[1] := 'R';
  If faArchive And iAttr > 0 Then
    Result[2] := 'A';
  If faSysFile And iAttr > 0 Then
    Result[3] := 'S';
  If faHidden And iAttr > 0 Then
    Result[4] := 'H';
  If faDirectory And iAttr > 0 Then
    Result[6] := 'D'
  Else If faVolumeID And iAttr > 0 Then
    Result[7] := 'V'
  Else
    Result[5] := 'F';
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

Var
  ConsoleInfo : TConsoleScreenBufferInfo;
  wChars : DWord;
  iChar : Integer;
  Attrs : Array of Word;
  OldPos : TCoord;
  NewPos : TCoord;
  iForeAttrColour, iBackAttrColour : Integer;
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
        SetLength(Attrs, wChars);
        iForeAttrColour := ForeGroundColour(iTextColour, ConsoleInfo.wAttributes And $0F);
        iBackAttrColour := BackGroundColour(iBackColour, ConsoleInfo.wAttributes And $F0);
        If wChars > 0 Then
          For iChar := 0 To wChars - 1 Do
            Attrs[iChar] := iForeAttrColour Or iBackAttrColour;
        Win32Check(WriteConsoleOutputAttribute(hndConsole, Attrs,
          Length(strTABText), ConsoleInfo.dwCursorPosition, wChars));
        If wChars > 0 Then
          Delete(strTABText, 1, wChars);
        Inc(NewPos.X, wChars);
        While NewPos.X >= ConsoleInfo.dwSize.X Do
          Begin
            Inc(NewPos.Y);
            Dec(NewPos.X, ConsoleInfo.dwSize.X);
          End;
        If strTABText <> '' Then
          Begin
            Win32Check(WriteConsole(hndConsole, PChar(#13#10), 2, wChars, Nil));
            Inc(NewPos.Y);
            NewPos.X := 0;
          End;
      Until strTABText = '';
      If boolUpdateCursor Then
        Begin
          // The only time the below fails is at the end of the buffer and a new
          // line is required, hence the new line on failure.
          If Not SetConsoleCursorPosition(hndConsole, NewPos) Then
            Win32Check(WriteConsole(hndConsole, PChar(#13#10), 2, wChars, Nil));
        End Else
          Win32Check(SetConsoleCursorPosition(hndConsole, OldPos));
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
            Win32Check(WriteConsole(hndConsole, PChar(#13#10), 2, wChars, Nil));
          End Else
            WriteLn(sl[i]);
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This routine returns the position of the Nth occurrance of the character in the text.

  @precon  None.
  @postcon Returns the position of the Nth occurrance of the character in the text.

  @param   strText          as a String as a constant
  @param   Ch               as a Char as a constant
  @param   iIndex           as an Integer as a constant
  @param   boolIgnoreQuotes as a Boolean as a constant
  @return  an Integer

**)
Function PosOfNthChar(Const strText : String; Const Ch : Char; Const iIndex : Integer;
  Const boolIgnoreQuotes : Boolean = True): Integer;

Var
  i : Integer;
  iCount : Integer;
  boolInQuotes : Boolean;

Begin
  Result := 0;
  iCount := 0;
  boolInQuotes := False;
  For i := 1 To Length(strText) Do
    Begin
      If Not boolIgnoreQuotes Then
        If strText[i] = '"' Then
          boolInQuotes := Not boolInQuotes;
      If strText[i] = Ch Then
        If Not boolInQuotes Then
          Inc(iCount);
      If iIndex = iCount Then
        Begin
          Result := i;
          Exit;
        End;
    End;
End;

(**

  This method encapsulates the FileDateToDateTime function in an exception handler to handle instance
  where fils have invalid dates.

  @precon  None.
  @postcon Returns the Date Time for the file.

  @param   iFileDate    as an Integer as a constant
  @param   strFilename  as a String as a constant
  @param   LogErrorProc as a TLogErrorProc as a constant
  @return  a TDateTime

**)
Function SafeFileDateToDateTime(Const iFileDate: Integer; Const strFilename: String;
  Const LogErrorProc: TLogErrorProc): TDateTime;

Begin
  Result := 0;
  Try
    Result := FileDateToDateTime(iFileDate);
  Except
    On E: EConvertError Do
      If Assigned(LogErrorProc) Then
        LogErrorProc(E.Message, strFilename);
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
  i := 1024;
  SetLength(Result, i);
  GetUserName(@Result[1], i);
  Win32Check(LongBool(i));
  SetLength(Result, i - 1);
End;

(** Initialises the console more to Unknown to force a call to the Win32 API **)
Initialization
  ConsoleMode := cmUnknown;
End.
