(**
  
  This module contains common simple types to be used through the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    28 Apr 2018
  
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
    clsOutputAsCSV,                  { /v or -v }
    clsColours                       { /l or -l }
  );

  (** This is a set of boolean command line switches. **)
  TCommandLineSwitches = Set Of TCommandLineSwitch;

  (** An enumerate to define the type of date to display and search on. **)
  TDateType = (dtCreation, dtLastAccess, dtLastWrite);

  (** This is an enumerate to defines the output size formats. **)
  TSizeFormat = (sfNone, sfKilobytes, sfMegaBytes, sfGigaBytes, sfTeraBytes);

  (** A Procedure for feeding back errors. **)
  TLogErrorProc = Procedure(Const strErrorMsg, strFileNmae: String) Of Object;

  (** An enumerate to define the order for sorting the files. **)
  TOrderBy = (obNone, obName, obSize, obDate, obOwner);

  (** An enumberate to define whether the files should be ascending or
      descending **)
  TOrderDirection = (odAscending, odDescending);

  (** An enumerate to define the attributes of a file. **)
  TSearchFileAttr = (
    sfaReadOnly,
    sfaArchive,
    sfaSystem,
    sfaHidden,
    sfaDirectory,
    sfaFile,
    sfaVolume,
    sfaNormal,
    sfaDevice,
    sfaTemporary,
    sfaSparse,
    sfaReparse,
    sfaCompressed,
    sfaOffLine,
    sfaIndexed,
    sfaEncrypted,
    sfaVirtual
  );

  (** A set of file attributes. **)
  TSearchFileAttrs = Set Of TSearchFileAttr;

  (** A record of file data for passing to the files collection. **)
  TSearchFileRec = Record
    FDate : TDateTime;
    FSize, FCompressedSize : Int64;
    FAttrs : TSearchFileAttrs;
    FOwner, FName : String;
    Constructor Create(Const dtDate : TDateTime; Const iSize, iCompressedSize : Int64;
      Const setAttrs : TSearchFileAttrs; Const strOwner, strName : String);
  End;

  (** A set of characters - for delimiting text. **)
  TSearchCharSet = Set Of AnsiChar;

  (** An enumerate to describe each console output colour. **)
  TSearchColour = (
    scUnknown,
    scTitle,
    scHeader,
    scFooter,
    scSearchPath,
    scFoundSearchPath,
    scFileInfo,
    scRegExLineNumbers,
    scRegExLineOutput,
    scRegExFindOutputFG,
    scRegExFindOutputBG,
    scSummaryOutput,
    scZipFile,
    scSuccess,
    scWarning,
    scException,
    scHelpHeader,
    scHelpInfo,
    scHelpText,
    scHelpSwitch,
    scHelpFootNote
  );    

  (** A list of available colours for the console output. **)
  TSearchColourList = (
    sclBlack,
    sclMaroon, 
    sclGreen,  
    sclNavy,   
    sclOlive,  
    sclPurple, 
    sclTeal,   
    sclGray,   
    sclRed,    
    sclLime,   
    sclBlue,  
    sclYellow, 
    sclFuchsia,
    sclAqua,   
    sclWhite,    
    sclNone
  );


Implementation

(**

  A constructor for the TSearchFileRecord Record.

  @precon  None.
  @postcon Initialises the record.

  @param   dtDate          as a TDateTime as a constant
  @param   iSize           as an Int64 as a constant
  @param   iCompressedSize as an Int64 as a constant
  @param   setAttrs        as a TSearchFileAttrs as a constant
  @param   strOwner        as a String as a constant
  @param   strName         as a String as a constant

**)
Constructor TSearchFileRec.Create(Const dtDate: TDateTime; Const iSize, iCompressedSize: Int64;
  Const setAttrs: TSearchFileAttrs; Const strOwner, strName : String);

Begin
  FDate := dtDate;
  FSize := iSize;
  FCompressedSize := iCompressedSize;
  FAttrs := setAttrs;
  FOwner := strOwner;
  FName := strName;
End;

End.
