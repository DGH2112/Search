(**

  This module defines the Search Win32 console application. This application is designed to provide
  general file search and regular expression search capabilities for files on disk.

  @version 1.2
  @Author  David Hoyle
  @Date    28 Apr 2018

  @todo    Undo change so that now search pattern is an except - this will stop accidently mass searches.
  @todo    Add switch for RegEx filename matching
  @todo    Check that GREP will work with multi-line matches
  @todo    Replaced Owner search with regular expressions.

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
  SysUtils,
  Search.FilesCls in 'Source\Search.FilesCls.pas',
  Search.Functions in 'Source\Search.Functions.pas',
  SearchEngine in 'Source\SearchEngine.pas',
  Search.Types in 'Source\Search.Types.pas',
  Search.RegExMatches in 'Source\Search.RegExMatches.pas',
  Search.FileCls in 'Source\Search.FileCls.pas',
  Search.Interfaces in 'Source\Search.Interfaces.pas',
  Search.Constants in 'Source\Search.Constants.pas',
  Search.StrUtils in 'Source\Search.StrUtils.pas';

ResourceString
  (** A resource string to define the Exception output format. **)
  strException = 'ESearchException: %s';
  (** A resource string for formatting information. **)
  strPressEnterToFinish = 'Press <Enter> to finish.';

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
    On E: ESearchException Do
      Begin
        OutputToConsoleLn(SearchEngine.ErrHnd, Format(strException, [E.Message]), 
          SearchEngine.ExceptionColour);
        boolException := True;
      End;
  End;
  If (clsDebug In CommandLineSwitches) Or (DebugHook <> 0) Then
    Begin
      OutputToConsoleLn(SearchEngine.StdHnd);
      OutputToConsole(SearchEngine.StdHnd, strPressEnterToFinish);
      Readln;
    End;
  If boolException Then
    Halt(1);
End.
