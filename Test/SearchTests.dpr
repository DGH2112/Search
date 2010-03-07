program SearchTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options 
  to use the console test runner.  Otherwise the GUI test runner will be used by 
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}



uses
  ExceptionLog,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  dghlibrary in '..\..\..\LIBRARY\dghlibrary.pas',
  TestDGHLibrary in '..\..\..\LIBRARY\Test\Source\TestDGHLibrary.pas',
  TestApplicationFunction in 'Source\TestApplicationFunction.pas',
  ApplicationFunctions in '..\Source\ApplicationFunctions.pas',
  FileHandling in '..\Source\FileHandling.pas';

{$R *.RES}

Var
  T : TTestResult;
  iErrors : Integer;

begin
  iErrors := 0;
  Application.Initialize;
  if IsConsole then
    Begin
      T := TextTestRunner.RunRegisteredTests;
      Try
        iErrors := T.FailureCount + T.ErrorCount;
      Finally
        T.Free;
      End;
    End else
      GUITestRunner.RunRegisteredTests;
  If iErrors > 0 Then
    Halt(iErrors);
end.


