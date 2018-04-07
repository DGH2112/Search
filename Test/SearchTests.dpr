program SearchTests;

uses
  TestInsight.DUnit,
  TestApplicationFunction in 'Source\TestApplicationFunction.pas',
  Search.Functions in '..\Source\Search.Functions.pas',
  FileHandling in '..\Source\FileHandling.pas',
  Search.Types in '..\Source\Search.Types.pas',
  Search.RegExMatches in '..\Source\Search.RegExMatches.pas',
  Search.FileCls in '..\Source\Search.FileCls.pas',
  Search.Interfaces in '..\Source\Search.Interfaces.pas';

{$R *.RES}

begin
  RunRegisteredTests;
end.


