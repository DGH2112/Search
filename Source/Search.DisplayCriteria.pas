(**
  
  This module contains code to output to the console all the criteria specified in the searchy.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Jun 2018
  
**)
Unit Search.DisplayCriteria;

Interface

Uses
  Search.Types, 
  Search.Interfaces, 
  Search.Options;

Type
  (** A record to encapsulate all the functionality to output the search criteria to the console. **)
  TSearchCriteria = Record
  Strict Private
    Class Var
      (** A class variable to temporary hold the ISearchConsole interface reference. **)
      FConsole : ISearchConsole;
  Strict Private
    Class Function  DisplayText(Const ASwitch: TCommandLineSwitch;
      Const CommandLineSwitches : TCommandLineSwitches ;Const strSwitch, strText: String;
      Const boolCarriageReturn: Boolean = True): Boolean; Static;
    Class Procedure DisplayAttributes(Const CommandLineSwitches : TCommandLineSwitches;
      Const Options : TSearchOptions); Static;
    Class function ReduceSize(Const iSize: Int64): String; Static;
    Class Procedure DisplayFileDateType(Const Options: TSearchOptions;
      Const CommandLineSwitches: TCommandLineSwitches); Static;
    Class Procedure DisplaySizeFormat(Const Options: TSearchOptions;
      Const CommandLineSwitches: TCommandLineSwitches); Static;
  private
      class procedure DisplayFileOrder(const Options: TSearchOptions; const
        CommandLineSwitches: TCommandLineSwitches); static;
  Public
    Class Procedure DisplayCriteria(Const Console : ISearchConsole; Const Options : TSearchOptions;
      Const CommandLineSwitches : TCommandLineSwitches); Static;
  End;

Implementation

uses
  System.SysUtils, 
  Search.Constants;

(**

  This method output a text relating to the attribute options used in the search.

  @precon  None.
  @postcon Output a text relating to the attribute options used in the search.

  @param   CommandLineSwitches as a TCommandLineSwitches as a constant
  @param   Options             as a TSearchOptions as a constant

**)
Class Procedure TSearchCriteria.DisplayAttributes(Const CommandLineSwitches : TCommandLineSwitches;
  Const Options : TSearchOptions);

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

  (**

    This method outputs the attribute description to the help screen if its in the set of file
    attribute filters.

    @precon  None.
    @postcon The attribute name is output to the helper screen if in the filter.

    @param   eAttribute       as a TSearchFileAttr as a constant
    @param   strAttributeName as a String as a constant

  **)
  Procedure OutputAttribute(Const eAttribute : TSearchFileAttr; Const strAttributeName : String);

  Begin
    If eAttribute in Options.FFileAttrs Then
      FConsole.OutputToConsoleLn(coStd, strAttributeName, scHelpInfo);
  End;

Begin
  If DisplayText(clsAttrRange, CommandLineSwitches, 't', strSearchingForFiles) Then
    Begin
      OutputAttribute(sfaReadOnly, strReadOnly);
      OutputAttribute(sfaArchive, strArchive);
      OutputAttribute(sfaSystem, strSystemFile);
      OutputAttribute(sfaHidden, strHidden);
      OutputAttribute(sfaFile, strFile);
      OutputAttribute(sfaDirectory, strDirectory);
      OutputAttribute(sfaVolume, strVolume);
      OutputAttribute(sfaNormal, strNormal);
      OutputAttribute(sfaDevice, strDevice);
      OutputAttribute(sfaTemporary, strTemporary);
      OutputAttribute(sfaSparse, strSparse);
      OutputAttribute(sfaReparse, strReparse);
      OutputAttribute(sfaCompressed, strCompressed);
      OutputAttribute(sfaOffLine, strOffline);
      OutputAttribute(sfaIndexed, strIndexed);
      OutputAttribute(sfaEncrypted, strEncrypted);
      OutputAttribute(sfaVirtual, strVirtual);
    End;
End;

(**

  This method displays the criteria and command line options used during the
  session.

  @precon  None.
  @postcon Displays the criteria and command line options used during the
           session.

  @param   Console             as an ISearchConsole as a constant
  @param   Options             as a TSearchOptions as a constant
  @param   CommandLineSwitches as a TCommandLineSwitches as a constant

**)
Class Procedure TSearchCriteria.DisplayCriteria(Const Console : ISearchConsole;
  Const Options : TSearchOptions; Const CommandLineSwitches : TCommandLineSwitches);

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
  strOwnerSearchingFor = '. Searching for Owners ';
  strOwnerMatching = 'matching "%s"';
  strSearchInZips = 'Search within ZIP files.';
  strApplyingExclusions = 'Applying exclusions from the file "%s".';
  strSearchForText = 'Search for the text "%s" within the files.';

Begin
  FConsole := Console;
  Try
    FConsole.OutputToConsoleLn(coStd, strSearchCriteria, scHelpHeader);
    DisplayText(clsDisplayCriteria, CommandLineSwitches, 'c', strDisplayingCriteria);
    DisplayText(clsSubDirectories, CommandLineSwitches, 's', strRecursingSubdirectories);
    DisplayText(clsDebug, CommandLineSwitches, '!', strDEBUGReadLnPause);
    DisplayText(clsShowAttribs, CommandLineSwitches, 'a', strDisplayingFileAttibs);
    If Not(clsRegExSearch In CommandLineSwitches) Then
      DisplayText(clsSummaryLevel, CommandLineSwitches, IntToStr(Options.FSummaryLevel),
        Format(strSummarisingOutput, [Options.FSummaryLevel]));
    DisplayText(clsSupressZeros, CommandLineSwitches, '0', strSupressingSummary);
    DisplayText(clsDateRange, CommandLineSwitches, 'd', Format(strDisplayFilesWithDates, [
      FormatDateTime(strOutputDateFmt, Options.FLDate),
      FormatDateTime(strOutputDateFmt, Options.FUDate)]));
    DisplayText(clsSizeRange, CommandLineSwitches, 'd', Format(strDisplayFilesWithSIzes,
      [ReduceSize(Options.FLSize), ReduceSize(Options.FUSize)]));
    DisplayAttributes(CommandLineSwitches, Options);
    DisplayText(clsQuiet, CommandLineSwitches, 'q', strQuietMode);
    If DisplayText(clsOwner, CommandLineSwitches, 'w', strDisplayTheOwners, False) Then
      Begin
        If Options.FOwnerSearch Then
          Begin
            FConsole.OutputToConsole(coStd, strOwnerSearchingFor, scHelpText);
            FConsole.OutputToConsole(coStd, Format(strOwnerMatching, [Options.FOwnerRegExSearch]),
              scHelpText);
          End;
        FConsole.OutputToConsoleLn(coStd, '.', scHelpText);
      End;
    DisplayFileOrder(Options, CommandLineSwitches);
    DisplayFileDateType(Options, CommandLineSwitches);
    DisplayText(clsRegExSearch, CommandLineSwitches, 'i', Format(strSearchForText,
      [Options.FRegExSearch]));
    DisplayText(clsExclusions, CommandLineSwitches, 'x', Format(strApplyingExclusions,
      [Options.FExlFileName]));
    DisplayText(clsSearchZip, CommandLineSwitches, 'p', strSearchInZips);
    DisplaySizeFormat(Options, CommandLineSwitches);
    FConsole.OutputToConsoleLn(coStd);
  Finally
    FConsole := Nil;
  End;
End;

(**

  This method outputs the file date type to be compared and output to the console.

  @precon  None.
  @postcon Outputs the date type being processed to the console.

  @param   Options             as a TSearchOptions as a constant
  @param   CommandLineSwitches as a TCommandLineSwitches as a constant

**)
Class Procedure TSearchCriteria.DisplayFileDateType(Const Options: TSearchOptions;
  Const CommandLineSwitches: TCommandLineSwitches);

ResourceString
  strDisplayingFileCreationDates = 'Displaying file Creation Dates.';
  strDisplayingFileLastAccessDates = 'Displaying file Last Access Dates.';
  strDisplayingFileLastWriteDates = 'Displaying file Last Write Dates.';

Begin
  Case Options.FDateType Of
    dtCreation: DisplayText(clsDateType, CommandLineSwitches, 'e', strDisplayingFileCreationDates);
    dtLastAccess: DisplayText(clsDateType, CommandLineSwitches, 'a', strDisplayingFileLastAccessDates);
    dtLastWrite: DisplayText(clsDateType, CommandLineSwitches, 'w', strDisplayingFileLastWriteDates);
  End;
End;

(**

  This method outputs to the console the file ordering for the search.

  @precon  None.
  @postcon The file sort order is output to the console.

  @param   Options             as a TSearchOptions as a constant
  @param   CommandLineSwitches as a TCommandLineSwitches as a constant

**)
Class Procedure TSearchCriteria.DisplayFileOrder(Const Options: TSearchOptions;
  Const CommandLineSwitches: TCommandLineSwitches);

ResourceString
  strOrderingFilesByName = 'Ordering files by Name';
  strOrderingFilesBySize = 'Ordering files by Size';
  strOrderingFilesByDate = 'Ordering files by Date';
  strOrderingFilesByOwner = 'Ordering files by Owner';
  strOrderingFilesInDescOrder = 'Descending Order';

Begin
  If clsOrderBy In CommandLineSwitches Then
    Begin
      Case Options.FOrderFilesBy Of
        obName: DisplayText(clsOrderBy, CommandLineSwitches, 'o', strOrderingFilesByName, False);
        obSize: DisplayText(clsOrderBy, CommandLineSwitches, 'o', strOrderingFilesBySize, False);
        obDate: DisplayText(clsOrderBy, CommandLineSwitches, 'o', strOrderingFilesByDate, False);
        obOwner: DisplayText(clsOrderBy, CommandLineSwitches, 'o', strOrderingFilesByOwner, False);
      End;
      If Options.FOrderFilesDirection = odDescending Then
        Begin
          FConsole.OutputToConsole(coStd, ' (', scHelpText);
          FConsole.OutputToConsole(coStd, strOrderingFilesInDescOrder, scHelpFootNote);
          FConsole.OutputToConsole(coStd, ')', scHelpText);
        End;
      FConsole.OutputToConsoleLn(coStd, '.', scHelpText);
    End;
End;

(**

  This method outputs the size formats configured for the search output.

  @precon  None.
  @postcon The size format is output to the console.

  @param   Options             as a TSearchOptions as a constant
  @param   CommandLineSwitches as a TCommandLineSwitches as a constant

**)
Class Procedure TSearchCriteria.DisplaySizeFormat(Const Options: TSearchOptions;
  Const CommandLineSwitches: TCommandLineSwitches);

ResourceString
  strSizeFormatTeraBytes = 'Sizes formatted in TeraBytes';
  strSizeFormatGigaBytes = 'Sizes formatted in GigaBytes';
  strSizeFormatMegaBytes = 'Sizes formatted in MegaBytes';
  strSizeFormatKiloBytes = 'Sizes formatted in KiloBytes';

Begin
  Case Options.FSizeFormat Of
    sfKilobytes: DisplayText(clsSizeOutput, CommandLineSwitches, 'f', strSizeFormatKiloBytes);
    sfMegaBytes: DisplayText(clsSizeOutput, CommandLineSwitches, 'f', strSizeFormatMegaBytes);
    sfGigaBytes: DisplayText(clsSizeOutput, CommandLineSwitches, 'f', strSizeFormatGigaBytes);
    sfTeraBytes: DisplayText(clsSizeOutput, CommandLineSwitches, 'f', strSizeFormatTeraBytes);
  End;
End;

(**

  This method outputs a single switch and its correpsonding message.

  @precon  None.
  @postcon Outputs a single switch and its correpsonding message.

  @param   ASwitch             as a TCommandLineSwitch as a constant
  @param   CommandLineSwitches as a TCommandLineSwitches as a constant
  @param   strSwitch           as a String as a constant
  @param   strText             as a String as a constant
  @param   boolCarriageReturn  as a Boolean as a constant
  @return  a Boolean

**)
Class Function TSearchCriteria.DisplayText(Const ASwitch: TCommandLineSwitch;
  Const CommandLineSwitches : TCommandLineSwitches ;Const strSwitch, strText: String;
  Const boolCarriageReturn: Boolean = True): Boolean;

Begin
  Result := ASwitch In CommandLineSwitches;
  If Result Then
    Begin
      FConsole.OutputToConsole(coStd, Format('  %2s', [strSwitch]), scHelpSwitch);
      FConsole.OutputToConsole(coStd, Format(') %s', [strText]), scHelpText);
      If boolCarriageReturn Then
        FConsole.OutputToConsoleLn(coStd);
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
class function TSearchCriteria.ReduceSize(Const iSize: Int64): String;

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

End.
