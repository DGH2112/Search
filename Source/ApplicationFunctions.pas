(**

  This module contains applications function which can also be tested in a
  DUnit framework.

  @Version 1.0
  @Author  David Hoyle
  @Date    15 Dec 2012

**)
Unit ApplicationFunctions;

Interface

Uses
  SysUtils,
  Classes,
  Windows,
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
    clsSearchIn,                     { /i or -i }
    clsDateType,                     { /e or -e }
    clsDisplayCriteria,              { /c or -c }
    clsExclusions,                   { /x or -x }
    clsUpdate,                       { /u or -u }
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

Const
  (** A constant to define that only files should be listed. **)
  iFileOnly = $0100;

ResourceString
  (** An exception message for a missing Search Text definition. **)
  strMissingSearchText = 'Missing Search Text';

Var
  (** Define the applications command line switches. **)
  CommandLineSwitches: TCommandLineSwitches;

  Procedure IncrementSwitchPosition(slParams: TStringList; Var iIndex, iSwitch: Integer;
    strExceptionMsg: String);
  Procedure GetRangeString(slParams: TStringList; chEndToken: Char; strExceptionMsg: String;
    Var iIndex, iSwitch: Integer; Var strValue: String);
  Procedure GetDateRange(slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var dtLDate, dtUdate: Double);
  Procedure GetSummaryLevel(slParams: TStringList; Var iSwitch: Integer; iIndex: Integer;
    Var iSummaryLevel: Integer);
  Procedure GetSizeRange(slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var iLSize, iUSize: Int64);
  Procedure GetAttributes(slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var iFileAttrs, iTypeAttrs: Integer);
  Procedure GetOrderBy(slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var OrderFilesDirection: TOrderDirection; Var OrderFilesBy: TOrderBy);
  Procedure GetSearchInInfo(slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var strSearchInText: String);
  Procedure GetDateType(slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var DateType: TDateType);
  Procedure GetExclusions(slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var strExlFileName: String);
  Procedure GetOwnerSwitch(slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var OwnerSearch: TOwnerSearch; Var OwnerSearchPos: TOwnerSearchPos;
    Var strOwnerSearch: String);
  Function OutputAttributes(iAttr: Integer): String;
  Procedure CheckDateRange(iDateTime: Integer; dtLDate, dtUdate: Double;
    Var boolFound: Boolean; strFileName : String; LogErrorProc : TLogErrorProc);
  Procedure CheckSizeRange(iSize, iLSize, iUSize: Int64; Var boolFound: Boolean);
  Procedure CheckFileAttributes(SearchAttrs: Integer; iFileAttrs, iTypeAttrs: Integer;
    Var boolFound: Boolean);
  Procedure CheckExclusions(strPath, strFilename: String; Var boolFound: Boolean;
    slExclusions: TStringList);
  Procedure CheckOwner(strOwner, strOwnerSearch: String; OwnerSearchPos: TOwnerSearchPos;
    OwnerSearch: TOwnerSearch; Var boolFound: Boolean);
  Procedure GetSizeFormat(slParams: TStringList; Var iSwitch, iIndex: Integer;
    Var SizeFormat: TSizeFormat);
  Function SafeFileDateToDateTime(iFileDate: Integer; strFilename: String;
    LogErrorProc: TLogErrorProc): TDateTime;

Implementation

Uses
  DGHLibrary;

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
  strCloseSquareExpectedInGREPSearchDef = '"]" Expected in GREP Search Definition';
  (** An exception message for a missing [ in an Search definition. **)
  strOpenSquareExpectedInGREPSearchDef = '"[" Expected in GREP Search Definition.';
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

(**

  This method increments the position of the index within the current command
  line switch and checks to see if the index is valid else raise the exception
  message provided.

  @precon  None.
  @postcon Checks the index to be valid else raises an exception.

  @param   slParams        as a TStringList
  @param   iIndex          as an Integer as a reference
  @param   iSwitch         as an Integer as a reference
  @param   strExceptionMsg as a String

**)
Procedure IncrementSwitchPosition(slParams: TStringList; Var iIndex, iSwitch: Integer;
  strExceptionMsg: String);

Begin
  Inc(iIndex);
  If Length(slParams[iSwitch]) < iIndex Then
    Raise ESearchException.Create(strExceptionMsg);
End;

(**

  This local function searches the current command line parameter for the
  given end token while collecting the date string.

  @precon  None.
  @postcon Searches the current command line parameter for the given end token
           while collecting the date string.

  @param   slParams        as a TStringList
  @param   chEndToken      as a Char
  @param   strExceptionMsg as a String
  @param   iIndex          as an Integer as a reference
  @param   iSwitch         as an Integer as a reference
  @param   strValue        as a String as a reference

**)
Procedure GetRangeString(slParams: TStringList; chEndToken: Char; strExceptionMsg: String;
  Var iIndex, iSwitch: Integer; Var strValue: String);

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

  This method determines if a date range has been specified on the command line.

  @precon  None.
  @postcon Determines if a date range has been specified on the command line.

  @param   slParams as a TStringList
  @param   iSwitch  as an Integer as a Reference
  @param   iIndex   as an Integer as a Reference
  @param   dtLDate  as an Double as a Reference
  @param   dtUDate  as an Double as a Reference

**)
Procedure GetDateRange(slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var dtLDate, dtUdate: Double);

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
    dtLDate := ConvertDate('01/Jan/1900 00:00:00');
  GetRangeString(slParams, ']', strCloseSquareExpectedInDate, iIndex, iSwitch, strDate);
  If strDate <> '' Then
    Begin
      dtUdate := ConvertDate(strDate);
      DecodeTime(dtUdate, wH, wM, wS, wMS);
      If (wH = 0) And (wM = 0) And (wS = 0) And (wMS = 0) Then
        dtUdate := dtUdate + EncodeTime(23, 59, 59, 999);
    End
  Else
    dtUdate := ConvertDate('31/Dec/2099 23:59:59.999');
End;

(**

  This method determines of a size range has been specified on the command line.

  @precon  None.
  @postcon Determines of a size range has been specified on the command line.

  @param   slParams   as a TStringList
  @param   iSwitch as an Integer as a Reference
  @param   iIndex  as an Integer as a Reference
  @param   iLSize  as an Int64 as a Reference
  @param   iUSize  as an Int64 as a Reference

**)
Procedure GetSizeRange(slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var iLSize, iUSize: Int64);

Var
  strSize: String;

  (**

    This method converts the size as a string into a Int64 in the passed
    variable.

    @precon  None.
    @postcon Converts the size as a string into a Int64 in the passed variable.

    @param   iSize           as an Int64 as a reference
    @param   strExceptionMsg as a String
    @param   iDefault        as an Int64

  **)
  Procedure GetSize(Var iSize: Int64; strExceptionMsg: String; iDefault: Int64);

  Var
    iErrorCode: Integer;
    iFactor   : Int64;

  Begin
    If strSize <> '' Then
      Begin
        Case strSize[Length(strSize)] Of
          'k', 'K':
            iFactor := $400;
          'm', 'M':
            iFactor := $100000;
          'g', 'G':
            iFactor := $40000000;
          't', 'T':
            iFactor := $10000000000;
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

  This method determines the attribute range from the command line.

  @precon  None.
  @postcon Determines the attribute range from the command line.

  @param   slParams   as a TStringList
  @param   iSwitch    as an Integer as a reference
  @param   iIndex     as an Integer as a reference
  @param   iFileAttrs as an Integer as a reference
  @param   iTypeAttrs as an Integer as a reference

**)

Procedure GetAttributes(slParams: TStringList; Var iSwitch, iIndex: Integer;
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
        'r', 'R':
          iFileAttrs := iFileAttrs Or faReadOnly;
        'a', 'A':
          iFileAttrs := iFileAttrs Or faArchive;
        's', 'S':
          iFileAttrs := iFileAttrs Or faSysFile;
        'h', 'H':
          iFileAttrs := iFileAttrs Or faHidden;
        'f', 'F':
          iTypeAttrs := iTypeAttrs Or iFileOnly;
        'd', 'D':
          iTypeAttrs := iTypeAttrs Or faDirectory;
        'v', 'V':
          iTypeAttrs := iTypeAttrs Or faVolumeID;
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

  This method gets the command line information for the ordering of the found
  files.

  @precon  None.
  @postcon Gets the command line information for the ordering of the found
           files.

  @param   slParams            as a TStringList
  @param   iSwitch             as an Integer as a reference
  @param   iIndex              as an Integer as a reference
  @param   OrderFilesDirection as a TOrderDirection as a reference
  @param   OrderFilesBy        as a TOrderBy as a reference

**)
Procedure GetOrderBy(slParams: TStringList; Var iSwitch, iIndex: Integer;
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

  This method gets the command line information for the date type of the found
  files.

  @precon  None.
  @postcon Gets the command line information for the date type of the found
           files.

  @param   slParams as a TStringList
  @param   iSwitch  as an Integer as a reference
  @param   iIndex   as an Integer as a reference
  @param   DateType as a TDateType as a reference

**)
Procedure GetDateType(slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var DateType: TDateType);

Begin
  Include(CommandLineSwitches, clsDateType);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strColonExpectedInDateType);
  If slParams[iSwitch][iIndex] <> ':' Then
    Raise ESearchException.Create(strColonExpectedInDateType);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strMissingDateTypeDirective);
  Case slParams[iSwitch][iIndex] Of
    'c', 'C':
      DateType := dtCreation;
    'a', 'A':
      DateType := dtLastAccess;
    'w', 'W':
      DateType := dtLastWrite;
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

  @param   slParams        as a TStringList
  @param   iSwitch         as an Integer as a reference
  @param   iIndex          as an Integer as a reference
  @param   strSearchInText as a String as a reference

**)
Procedure GetSearchInInfo(slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var strSearchInText: String);

Begin
  Include(CommandLineSwitches, clsSearchIn);
  IncrementSwitchPosition(slParams, iIndex, iSwitch,
    strOpenSquareExpectedInGREPSearchDef);
  If slParams[iSwitch][iIndex] <> '[' Then
    Raise ESearchException.Create(strOpenSquareExpectedInGREPSearchDef);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strMissingSearchText);
  strSearchInText := '';
  While (iIndex <= Length(slParams[iSwitch])) And (slParams[iSwitch][iIndex] <> ']') Do
    Begin
      strSearchInText := strSearchInText + slParams[iSwitch][iIndex];
      IncrementSwitchPosition(slParams, iIndex, iSwitch,
        strCloseSquareExpectedInGREPSearchDef);
    End;
End;

(**

  This method determines the summary level required.

  @precon  None.
  @postcon Determines the summary level required.

  @param   slParams      as a TStringList
  @param   iSwitch       as an Integer as a reference
  @param   iIndex        as an Integer
  @param   iSummaryLevel as an Integer as a reference

**)
Procedure GetSummaryLevel(slParams: TStringList; Var iSwitch: Integer; iIndex: Integer;
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

  This method extracts from the command line switch the filename to be used
  as the source of the exclusion list.

  @precon  None.
  @postcon Extracts from the command line switch the filename to be used
           as the source of the exclusion list.

  @param   slParams       as a TStringList
  @param   iSwitch        as an Integer as a reference
  @param   iIndex         as an Integer as a reference
  @param   strExlFileName as a String as a reference

**)
Procedure GetExclusions(slParams: TStringList; Var iSwitch, iIndex: Integer;
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

  This method obtains the owner search information from the command line.

  @precon  None.
  @postcon Obtains the owner search information from the command line.

  @param   slParams       as a TStringList
  @param   iSwitch        as an Integer as a reference
  @param   iIndex         as an Integer as a reference
  @param   OwnerSearch    as a TOwnerSearch as a reference
  @param   OwnerSearchPos as a TOwnerSearchPos as a reference
  @param   strOwnerSearch as a String as a reference

**)
Procedure GetOwnerSwitch(slParams: TStringList; Var iSwitch, iIndex: Integer;
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

  This method outputs the files attributes if they are requireed at the command
  line.

  @precon  None.
  @postcon Outputs the files attributes if they are requireed at the command
           line.

  @param   iAttr as an Integer
  @return  a String

**)
Function OutputAttributes(iAttr: Integer): String;

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

  This method encapsulates the FileDateToDateTime function in an exception handler to 
  handle instance where fils have invalid dates.

  @precon  None.
  @postcon Returns the Date Time for the file.

  @param   iFileDate    as an Integer
  @param   strFileName  as a String
  @param   LogErrorProc as a TLogErrorProc
  @return  a TDateTime

**)
Function SafeFileDateToDateTime(iFileDate: Integer; strFilename: String;
  LogErrorProc: TLogErrorProc): TDateTime;

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

  This method checks the currently found file information against the Date Range .

  @precon  None.
  @postcon Returns boolFound as true if the file date is within the date range.

  @param   iDateTime    as an Integer
  @param   dtLDate      as a Double
  @param   dtUdate      as a Double
  @param   boolFound    as a Boolean as a reference
  @param   strFileName  as a String
  @param   LogErrorProc as a TLogErrorProc

**)
Procedure CheckDateRange(iDateTime: Integer; dtLDate, dtUdate: Double;
  Var boolFound: Boolean; strFileName : String; LogErrorProc : TLogErrorProc);

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

  This method checks the currently found file against the size range.

  @precon  None.
  @postcon Returns boolFound as true if the file is in the range.

  @param   iSize     as an Int64
  @param   iLSize    as an Int64
  @param   iUSize    as an Int64
  @param   boolFound as a Boolean as a reference

**)
Procedure CheckSizeRange(iSize, iLSize, iUSize: Int64; Var boolFound: Boolean);

Begin
  If clsSizeRange In CommandLineSwitches Then
    Begin
      boolFound := boolFound And (iSize >= iLSize);
      boolFound := boolFound And (iSize <= iUSize);
    End;
End;

(**

  This method checks the currently found file against the file attributes.

  @precon  None.
  @postcon Returns boolFound as true is the file has the attributes.

  @param   SearchAttrs as an Integer
  @param   iFileAttrs  as an Integer
  @param   iTypeAttrs  as an Integer
  @param   boolFound   as a Boolean as a reference

**)
Procedure CheckFileAttributes(SearchAttrs: Integer; iFileAttrs, iTypeAttrs: Integer;
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

  This method check to see if the path and file name should be excluded from the
  search.

  @precon  recSearch must be a valid TSearchRec structure.
  @postcon Sets or maintains boolFound as True if the file should NOT be 
           excluded else sets boolFound to False.

  @param   strPath      as a String
  @param   strFileName  as a String
  @param   boolFound    as a Boolean as a reference
  @param   slExclusions as a TStringList

**)
Procedure CheckExclusions(strPath, strFilename: String; Var boolFound: Boolean;
  slExclusions: TStringList);

Var
  j: Integer;

Begin
  strFilename := LowerCase(strPath + strFilename);
  For j       := 0 To slExclusions.Count - 1 Do
    boolFound := boolFound And (Pos(slExclusions[j], strFilename) = 0);
End;

(**

  This method check the owner of the file against the owner search criteria.

  @precon  None.
  @postcon Check the owner of the file against the owner search criteria.

  @param   strOwner       as a String
  @param   strOwnerSearch as a String
  @param   OwnerSearchPos as a TOwnerSearchPos
  @param   OwnerSearch    as a TOwnerSearch
  @param   boolFound      as a Boolean as a reference

**)
Procedure CheckOwner(strOwner, strOwnerSearch: String; OwnerSearchPos: TOwnerSearchPos;
  OwnerSearch: TOwnerSearch; Var boolFound: Boolean);

Var
  i   : Integer;
  bool: Boolean;

Begin
  i := Length(strOwnerSearch);
  Case OwnerSearchPos Of
    ospExact:
      bool := AnsiCompareText(strOwner, strOwnerSearch) = 0;
    ospStart:
      bool := AnsiCompareText(strOwnerSearch, Copy(strOwner, 1, i)) = 0;
    ospMiddle:
      bool := Pos(LowerCase(strOwnerSearch), LowerCase(strOwner)) > 0;
    ospEnd:
      bool := AnsiCompareText(strOwnerSearch,
        Copy(strOwner, Length(strOwner) - i + 1, i)) = 0;
  Else
    bool := True;
  End;
  If OwnerSearch = osNotEquals Then
    bool    := Not bool;
  boolFound := boolFound And bool
End;

(**

  This function parses the size format directive.

  @precon  slParams must eb avalid instance with iSwitch and iIndex being valid
           indexes into the stringlist and characters of the string list
           respectively.
  @postcon Returns the size format requested else raises an exception.

  @param   slParams   as a TStringList
  @param   iSwitch    as an Integer as a reference
  @param   iIndex     as an Integer as a reference
  @param   SizeFormat as a TSizeFormat as a reference

**)
Procedure GetSizeFormat(slParams: TStringList; Var iSwitch, iIndex: Integer;
  Var SizeFormat: TSizeFormat);

Begin
  Include(CommandLineSwitches, clsSizeOutput);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strColonExpectedInSizeFormat);
  If slParams[iSwitch][iIndex] <> ':' Then
    Raise ESearchException.Create(strColonExpectedInSizeFormat);
  IncrementSwitchPosition(slParams, iIndex, iSwitch, strMissingSizeFormatDirective);
  Case slParams[iSwitch][iIndex] Of
    'k', 'K':
      SizeFormat := sfKilobytes;
    'm', 'M':
      SizeFormat := sfMegaBytes;
    'g', 'G':
      SizeFormat := sfGigaBytes;
    't', 'T':
      SizeFormat := sfTeraBytes;
  Else
    Raise ESearchException.Create(strInvalidSizeFormatDirective);
  End
End;

End.
