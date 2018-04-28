program SearchTests;

uses
  TestInsight.DUnit,
  Test.Search.Functions in 'Source\Test.Search.Functions.pas',
  Search.Functions in '..\Source\Search.Functions.pas',
  Search.FilesCls in '..\Source\Search.FilesCls.pas',
  Search.Types in '..\Source\Search.Types.pas',
  Search.RegExMatches in '..\Source\Search.RegExMatches.pas',
  Search.FileCls in '..\Source\Search.FileCls.pas',
  Search.Interfaces in '..\Source\Search.Interfaces.pas',
  Test.Search.SearchFile in 'Source\Test.Search.SearchFile.pas',
  Test.Search.SearchFiles in 'Source\Test.Search.SearchFiles.pas',
  Search.StrUtils in '..\Source\Search.StrUtils.pas',
  Test.Search.SearchStrUtils in 'Source\Test.Search.SearchStrUtils.pas';

{$R *.RES}

begin
  RunRegisteredTests;
end.


