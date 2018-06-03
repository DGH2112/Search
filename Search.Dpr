(**

  This module defines the Search Win32 console application. This application is designed to provide
  general file search and regular expression search capabilities for files on disk.

  @version 1.2
  @Author  David Hoyle
  @Date    03 Jun 2018
  @todo    Add switch for RegEx filename matching
  @todo    Check that GREP will work with multi-line matches

**)
Program Search;

{$APPTYPE CONSOLE}
{$R *.RES}

{$R 'SearchVersionInfo.res' 'SearchVersionInfo.RC'}

uses
  {$IFDEF EurekaLog}
  EMemLeaks,
  EResLeaks,
  ESendMailMAPI,
  ESendMailSMAPI,
  EDialogConsole,
  EDebugExports,
  EDebugJCL,
  EMapWin32,
  EAppConsole,
  ExceptionLog7,
  {$ENDIF EurekaLog}
  System.SysUtils,
  System.RegularExpressionsCore,
  Search.FilesCls in 'Source\Search.FilesCls.pas',
  Search.Functions in 'Source\Search.Functions.pas',
  Search.Engine in 'Source\Search.Engine.pas',
  Search.Types in 'Source\Search.Types.pas',
  Search.RegExMatches in 'Source\Search.RegExMatches.pas',
  Search.FileCls in 'Source\Search.FileCls.pas',
  Search.Interfaces in 'Source\Search.Interfaces.pas',
  Search.Constants in 'Source\Search.Constants.pas',
  Search.StrUtils in 'Source\Search.StrUtils.pas',
  Search.ConvertDate in 'Source\Search.ConvertDate.pas',
  Search.ResourceStrings in 'Source\Search.ResourceStrings.pas',
  Search.Help in 'Source\Search.Help.pas',
  Search.Console in 'Source\Search.Console.pas',
  Search.DisplayCriteria in 'Source\Search.DisplayCriteria.pas',
  Search.Options in 'Source\Search.Options.pas';

ResourceString
  (** A resource string to define the Exception output format. **)
  strException = #13#10'Exception raised:'#13#10'  %s';
  (** A resource string for formatting information. **)
  strPressEnterToFinish = 'Debug Mode: Press <Enter> to finish.';

Var
  (** A variable to hold whether an exception has occurred. **)
  boolException: Boolean;
  (** An interface reference for calling the search engine. **)
  SearchEngine : ISearchEngine;

Begin
  {$IFDEF EUREKALOG}
  SetEurekaLogState(DebugHook = 0);
  {$ENDIF}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  boolException := False;
  SearchEngine := TSearch.Create;
  Try
    SearchEngine.Run;
  Except
    On E: EAbort Do
      Begin
        SearchEngine.Console.OutputToConsoleLn(coStd, strSearchAborted);
        boolException := False;
      End;
    On E: ESearchException Do
      Begin
        SearchEngine.Console.OutputToConsoleLn(coErr, Format(strException, [E.Message]), scException);
        boolException := True;
      End;
    On E : ERegularExpressionError Do
      Begin
        SearchEngine.Console.OutputToConsoleLn(coErr, Format(strException, [E.Message]), scException);
        boolException := True;
      End;
  End;
  If (clsDebug In CommandLineSwitches) Or (DebugHook <> 0) Then
    Begin
      SearchEngine.Console.OutputToConsoleLn(coStd);
      SearchEngine.Console.OutputToConsole(coStd, strPressEnterToFinish);
      Readln;
    End;
  If boolException Then
    Halt(1);
End.
