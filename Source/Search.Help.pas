(**
  
  This module outputs all the search applications help information to the console.

  @Author  David Hoyle
  @Version 1.0
  @Date    02 Jun 2018
  
**)
Unit Search.Help;

Interface

Uses
  Search.Interfaces;

Type
  (** A record to encapsulate all the help output methods. **)
  TSearchHelp = Record
  Strict Private
    Class Var
      (** A class variable to hold an instance reference to the ISearchConsole interface. **)
      FConsole : ISearchConsole;
  Strict Private
    Class Procedure PrintHelpSyntax; Static;
    Class Procedure PrintHelpHelp; Static;
    Class Procedure PrintHelpShowAttributes; Static;
    Class Procedure PrintHelpRecurseSubdirectories; Static;
    Class Procedure PrintHelpSummarise; Static;
    Class Procedure PrintHelpHideEmptySummarise; Static;
    Class Procedure PrintHelpFilterByAttributes; Static;
    Class Procedure PrintHelpFilterByDate; Static;
    Class Procedure PrintHelpFilterBySize; Static;
    Class Procedure PrintHelpQuietMode; Static;
    Class Procedure PrintHelpShowOwner; Static;
    Class Procedure PrintHelpOrderBy; Static;
    Class Procedure PrintHelpDateType; Static;
    Class Procedure PrintHelpGrepSearch; Static;
    Class Procedure PrintHelpDisplayCriteria; Static;
    Class Procedure PrintHelpExclusions; Static;
    Class Procedure PrintHelpSearchWithinZips; Static;
    Class Procedure PrintHelpFileOutputSize; Static;
    Class Procedure PrintHelpOutputInCSV; Static;
    Class Procedure PrintHelpFooter; Static;
    Class Procedure PrintHelpColourConfig; Static;
  Public
    Class Procedure PrintHelp(Const SearchConsole : ISearchConsole); Static;
  End;

Implementation

Uses
  Search.Types, 
  System.SysUtils;

Const
  (** This is the width of the help information. **)
  iWidth = 14;
  (** This is the indent for the main helptext **)
  iTextIndent = 16;
  (** This is the indent for the main help items. **)
  iMainIndent = 2;
  (** This is the indent for the minor help items. **)
  iMinorIndent = 4;
  
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

  This method prints on the console screen the command line search help information.

  @precon  None.
  @postcon Prints on the console screen the command line search help information.

  @param   SearchConsole as an ISearchConsole as a constant

**)
Class Procedure TSearchHelp.PrintHelp(Const SearchConsole : ISearchConsole);

Begin
  FConsole := SearchConsole;
  Try
    PrintHelpSyntax;
    PrintHelpHelp;                    //?
    PrintHelpSummarise;               //1-9
    PrintHelpHideEmptySummarise;      //0
    PrintHelpShowAttributes;          //A
    PrintHelpDisplayCriteria;         //C
    PrintHelpFilterByDate;            //D
    PrintHelpDateType;                //E
    PrintHelpFileOutputSize;          //F
    PrintHelpGrepSearch;              //I
    PrintHelpColourConfig;            //L
    PrintHelpOrderBy;                 //O
    PrintHelpSearchWithinZips;        //P
    PrintHelpQuietMode;               //Q
    PrintHelpRecurseSubdirectories;   //S
    PrintHelpFilterByAttributes;      //T
    PrintHelpOutputInCSV;             //V
    PrintHelpShowOwner;               //W
    PrintHelpExclusions;              //X
    PrintHelpFilterBySize;            //Z
    PrintHelpFooter;
  Finally
    FConsole := Nil;
  End;
End;

(**

  This methods outputs the command line switches for showing and updating the console output colours.

  @precon  None.
  @postcon The help associated with the console colours is output.

**)
Class Procedure TSearchHelp.PrintHelpColourConfig;

ResourceString
  strSwitch = '/L{[...]}';
  strDescription = 'Updates the colour configuration with /L[ColourName=ColourValue]';
  strOptions1 = 'Use /L on its own to display the current colour configure and';
  strOptions2 = 'available names and values';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
  FConsole.OutputToConsoleLn(coStd, Indent(strOptions1, iTextIndent + iMainIndent, 0), scHelpInfo);
  FConsole.OutputToConsoleLn(coStd, Indent(strOptions2, iTextIndent + iMainIndent, 0), scHelpInfo);
End;

(**

  This method outputs the command line switches for changing the type of date displayed and filtered for
  to the console.

  @precon  None.
  @postcon The command line switches for changing the type of date display and filterd for is output to
           the console.

**)
Class Procedure TSearchHelp.PrintHelpDateType;

ResourceString
  strSwitch = '/E:CAW';
  strDescription = 'Type of file date to display and search on:';
  strOptions = 'C = Creation, A = Last Access, W = Last Write (Default)';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
  FConsole.OutputToConsoleLn(coStd, Indent(strOptions, iTextIndent + iMainIndent, 0), scHelpInfo);
End;

(**

  This method outputs the command line switches for displaying the search criteria to the console.

  @precon  None.
  @postcon The command line switches for displaying the search criteria are output to the console.

**)
Class Procedure TSearchHelp.PrintHelpDisplayCriteria;

ResourceString
  strSwitch = '/C';
  strDescription = 'Display a list of Criteria and Command Line Options.';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
End;

(**

  This method outputs the command line switches for exclusing files from the search to the console.

  @precon  None.
  @postcon The command line switches for excluding files from the search are output to the console.

**)
Class Procedure TSearchHelp.PrintHelpExclusions;

ResourceString
  strSwitch = '/X[FileName]';
  strDescription = 'A list of exclusions to apply to paths and filenames.';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
End;

(**

  This method prints the command line switch for setting the file output size format to the console.

  @precon  None.
  @postcon The command line switch for setting the file output size is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpFileOutputSize;

ResourceString
  strSwitch = '/F:KMGT';
  strDescription = 'Changes the output size format to [K]ilo, [M]ega, [G]iga or [T]erabytes.';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
End;

(**

  This method prints the command line switch for filtering files by their attributes to the console.

  @precon  None.
  @postcon The command line switch for filtering files by their attributes is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpFilterByAttributes;

ResourceString
  strSwitch = '/T[RASH...]';
  strDescription = 'Show only files with certain attributes';
  strOptions1 = 'R = Read Only, A = Archive,    S = System,    H = Hidden,';
  strOptions2 = 'F = File,      D = Directory,  V = Volume ID,';
  strOptions3 = 'd = Device,    N = Normal,     T = Temporary, s = Sparse,';
  strOptions4 = 'r = Reparse,   C = Compressed, O = Offline,   I = Not Indexed,';
  strOptions5 = 'E = Encrypted, v = Virtual';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
  FConsole.OutputToConsoleLn(coStd, Indent(strOptions1, iTextIndent + iMainIndent, 0), scHelpInfo);
  FConsole.OutputToConsoleLn(coStd, Indent(strOptions2, iTextIndent + iMainIndent, 0), scHelpInfo);
  FConsole.OutputToConsoleLn(coStd, Indent(strOptions3, iTextIndent + iMainIndent, 0), scHelpInfo);
  FConsole.OutputToConsoleLn(coStd, Indent(strOptions4, iTextIndent + iMainIndent, 0), scHelpInfo);
  FConsole.OutputToConsoleLn(coStd, Indent(strOptions5, iTextIndent + iMainIndent, 0), scHelpInfo);
End;

(**

  This method prints the command line switch for filtering files by their dates to the console.

  @precon  None.
  @postcon The command line switch for filtering files by their dates is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpFilterByDate;

ResourceString
  strSwitch = '/D[{l}-{u}]';
  strDescription = 'Searches for files between dates (l) and (u).';
  strOptions = 'l = lower bounding date and time, u = upper bounding date and time';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
  FConsole.OutputToConsoleLn(coStd, Indent(strOptions, iTextIndent + iMainIndent, 0), scHelpInfo);
End;

(**

  This method prints the command line switch for filtering files by their size to the console.

  @precon  None.
  @postcon The command line switch for filtering files by their size is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpFilterBySize;

ResourceString
  strSwitch = '/Z[{l}-{u}]';
  strDescription = 'Searches for files between sizes (l) and (u).';
  strOptions = 'l = lower bounding size in bytes, u = upper bounding size in bytes';
  strFootNote = 'Sizes can be postfixed with k, m, g or t for kilo, mega, giga and terabytes';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
  FConsole.OutputToConsoleLn(coStd, Indent(strOptions, iTextIndent + iMainIndent, 0), scHelpInfo);
  FConsole.OutputToConsoleLn(coStd, Indent(strFootNote, iTextIndent + iMainIndent, 0), scHelpInfo);
End;

(**

  This method prints the help footer.

  @precon  None.
  @postcon The help footer is printed to the console.

**)
Class Procedure TSearchHelp.PrintHelpFooter;

ResourceString
  strFooterText = 'NOTE: The date time input format is dependent on your local settings';

Begin
  FConsole.OutputToConsoleLn(coStd);
  FConsole.OutputToConsoleLn(coStd, strFooterText, scHelpFootNote);
  FConsole.OutputToConsoleLn(coStd);
End;

(**

  This method prints the command line switch for search for text within files to the console.

  @precon  None.
  @postcon The command line switch for search for text within files is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpGrepSearch;

ResourceString
  strSwitch = '/I[text]';
  strDescription = 'Search within files for text using Perl regular expressions.';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
End;

(**

  This method prints the command line switch for displaying help to the console.

  @precon  None.
  @postcon The command line switch for displaying help is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpHelp;

ResourceString
  strSwitch = '/?';
  strDescription = 'This help screen';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
End;

(**

  This method prints the command line switch for hiding empty summaries to the console.

  @precon  None.
  @postcon The command line switch for hiding empty summaries is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpHideEmptySummarise;

ResourceString
  strSwitch = '/0';
  strDescription = 'Hide empty Summarised subdirectories';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
End;

(**

  This method prints the command line switch for ordering files to the console.

  @precon  None.
  @postcon The command line switch for ordering files is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpOrderBy;

ResourceString
  strSwitch = '/O:NADOS';
  strDescription = 'Order files ascending [+] and descending [-]';
  strOptions = 'N = Name, D = Date and Time, O = Owner, S = Size';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
  FConsole.OutputToConsoleLn(coStd, Indent(strOptions, iTextIndent + iMainIndent, 0), scHelpInfo);
End;

(**

  This method prints the command line switch for outputting the search in CSV format.

  @precon  None.
  @postcon The command line switch for outputting the search in CSV format is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpOutputInCSV;

ResourceString
  strSwitch = '/V';
  strDescription = 'Output the found information in CSV format.';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
End;

(**

  This method prints the command line switch for outputting in quiet mode to the console.

  @precon  None.
  @postcon The command line switch for outputting in quiet mode is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpQuietMode;

ResourceString
  strSwitch = '/Q';
  strDescription = 'Quiet mode';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
End;

(**

  This method prints the command line switch for recursing subdirectories to the console.

  @precon  None.
  @postcon The command line switch for recursing subdirectories is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpRecurseSubdirectories;

ResourceString
  strSwitch = '/S';
  strDescription = 'Recurse subdirectories';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
End;

(**

  This method print to the console the help information for enabling searching within zip files.

  @precon  None.
  @postcon The inform ation for searching within ZIP files is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpSearchWithinZips;

ResourceString
  strSwitch = '/P';
  strDescription = 'Enables the searching within ZIP archives.';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
End;

(**

  This method prints the command line switch for showing attributes to the console.

  @precon  None.
  @postcon The command line switch for showing attributes is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpShowAttributes;

ResourceString
  strSwitch = '/A';
  strDescription = 'Show file and directory attributes';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
End;

(**

  This method prints the command line switch for searching for file withn an owner to the console.

  @precon  None.
  @postcon The command line switch for searching for file withn an owner is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpShowOwner;

ResourceString
  strSwitch = '/W{[Owner]}';
  strDescription = 'Owner Information. The owner search can be a wildcard search with an *.';
  strOptions = 'Searches beginning with ! force a negated criteria.';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
  FConsole.OutputToConsoleLn(coStd, Indent(strOptions, iTextIndent, 0));
End;

(**

  This method prints the command line switch for summarising results at directory level to the console.

  @precon  None.
  @postcon The command line switch for summarising results at directory level is output to the console.

**)
Class Procedure TSearchHelp.PrintHelpSummarise;

ResourceString
  strSwitch = '/1..9';
  strDescription = 'Summarise subdirectories';

Begin
  FConsole.OutputToConsole(coStd, Indent(strSwitch, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strDescription, scHelpText);
End;

(**

  This method prints the Syntax of the command line options to the console.

  @precon  None.
  @postcon The syntax of the command line otions are printed to the console.

**)
Class Procedure TSearchHelp.PrintHelpSyntax;

ResourceString
  strHelp0010 = 'Syntax:';
  strHelp0020 = 'Search searchparam1 {searchparam2}... {/A} {/S} {/Q} {/W}';
  strHelp0030 = '{/T[RASHFDV]} {/D:[{DD{/MM{/YY {HH:MM{:SS}}}}}-{DD{/MM{/YY {HH:MM{:SS}}}}}]';
  strHelp0040 = '{/Z[{LowerByteSize}-{UpperByteSize}]} {/O:NDAOS} {/E:CAW} {/I[text]}';
  strHelp0050 = '{/X[filename]}';
  strHelp0060 = 'searchparam# = {drive:\path\}filter1{;filter2{;filter3{;.' + '..}}}';
  strHelp0070 = 'filter#';
  strHelp0075 = 'can contain wildcards * and ? within the filename. ';
  strHelp0080 = '{ ... } denotes optional items.';

Begin
  FConsole.OutputToConsoleLn(coStd, strHelp0010, scHelpHeader);
  FConsole.OutputToConsoleLn(coStd, Indent(strHelp0020, iMainIndent, 0), scHelpInfo);
  FConsole.OutputToConsoleLn(coStd, Indent(strHelp0030, iMinorIndent, 0), scHelpInfo);
  FConsole.OutputToConsoleLn(coStd, Indent(strHelp0040, iMinorIndent, 0), scHelpInfo);
  FConsole.OutputToConsoleLn(coStd, Indent(strHelp0050, iMinorIndent, 0), scHelpInfo);
  FConsole.OutputToConsoleLn(coStd);
  FConsole.OutputToConsoleLn(coStd, Indent(strHelp0060, iMainIndent, 0), scHelpInfo);
  FConsole.OutputToConsoleLn(coStd);
  FConsole.OutputToConsole(coStd, Indent(strHelp0070, iMainIndent, iWidth), scHelpSwitch);
  FConsole.OutputToConsoleLn(coStd, strHelp0075, scHelpText);
  FConsole.OutputToConsoleLn(coStd);
  FConsole.OutputToConsoleLn(coStd, Indent(strHelp0080, iTextIndent, 0), scHelpText);
  FConsole.OutputToConsoleLn(coStd);
End;

End.
