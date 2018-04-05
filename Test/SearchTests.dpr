program SearchTests;

uses
  TestInsight.DUnit,
  DGHLibrary in '..\Externals\DGHLibrary.pas',
  TestDGHLibrary in 'Source\TestDGHLibrary.pas',
  TestApplicationFunction in 'Source\TestApplicationFunction.pas',
  ApplicationFunctions in '..\Source\ApplicationFunctions.pas',
  FileHandling in '..\Source\FileHandling.pas';

{$R *.RES}

begin
  RunRegisteredTests;
end.


