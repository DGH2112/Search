(**

 This module defines the Search Win32 console application. This application
 is designed to provide regular expression search capabilities for files on
 disk.

 @Version 1.2
 @Author  David Hoyle
 @Date    07 Apr 2018

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
  FileHandling in 'Source\FileHandling.pas',
  Search.Functions in 'Source\Search.Functions.pas',
  SearchEngine in 'Source\SearchEngine.pas',
  Search.Types in 'Source\Search.Types.pas';

ResourceString
  (** A resource string to define the Exception output format. **)
  strException = 'ESearchException: %s';
  (** A resource string for formatting information. **)
  strPressEnterToFinish = 'Press <Enter> to finish.';

Var
  (** A variable to hold whether an exception has occurred. **)
  boolException: Boolean;

Begin
  {$IFDEF EUREKALOG}
  SetEurekaLogState(DebugHook = 0);
  {$ENDIF}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  boolException := False;
  With TSearch.Create Do
    Try
      Try
        Run;
      Except
        On E: ESearchException Do
          Begin
            OutputToConsoleLn(ErrorHnd, Format(strException, [E.Message]), ExceptionColour);
            boolException := True;
          End;
      End;
      If clsDebug In CommandLineSwitches Then
        Begin
          OutputToConsoleLn(StdHnd);
          OutputToConsole(StdHnd, strPressEnterToFinish);
          Readln;
        End;
    Finally
      Free;
    End;
  If boolException Then
    Halt(1);
End.
