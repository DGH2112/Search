(**
  
  This module contains common simple types to be used through the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    08 Apr 2018
  
**)
Unit Search.Types;

Interface

Uses
  System.SysUtils;

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
  TLogErrorProc = Procedure(Const strErrorMsg, strFileNmae: String) Of Object;

  (** An enumerate to define the order for sorting the files. **)
  TOrderBy = (obNone, obName, obSize, obDate, obOwner, obAttribute);

  (** An enumberate to define whether the files should be ascending or
      descending **)
  TOrderDirection = (odAscending, odDescending);

Implementation

End.
