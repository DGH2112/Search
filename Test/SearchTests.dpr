program SearchTests;

uses
  TestInsight.DUnit,
  TestApplicationFunction in 'Source\TestApplicationFunction.pas',
  Search.Functions in '..\Source\Search.Functions.pas',
  FileHandling in '..\Source\FileHandling.pas';

{$R *.RES}

begin
  RunRegisteredTests;
end.


