{ JUnit XML results writer for the fpcunit test runner.

  FPC 3.2 ships only plain/latex/xml report writers for fpcunit, none of
  which is understood by the GitLab CI test report parser. This writer
  produces the JUnit XML schema instead. It is meant to be added as an
  additional listener to the TTestResult next to the normal console writer;
  the XML document is built from the listener callbacks during the test run
  and serialized to FileName on WriteResult.
}
unit slftpUnitTestsJUnitReport;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, fpcunit, fpcunitreport, dom;

type
  {* fpcunit results writer producing a JUnit XML report (the schema
     understood by GitLab CI test reports). All test cases are collected in
     a single testsuite element named after the suite; the classname of each
     testcase is the dot-joined path of its enclosing test suites. *}
  TslJUnitResultsWriter = class(TCustomResultsWriter)
  private
    FDoc: TXMLDocument; //< the JUnit document being built
    FSuite: TDOMElement; //< the single testsuite element holding all testcase elements
    FSuiteNames: TStringList; //< stack of enclosing suite names, used to build the testcase classname
    FCurrentCase: TDOMElement; //< testcase element of the currently running test, nil between tests
    function GetCurrentClassName: String; //< @returns dot-joined names of the currently open test suites
  protected
    procedure WriteHeader; override;
    procedure WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure); override;
    procedure AddError(ATest: TTest; AError: TTestFailure); override;
    procedure StartTest(ATest: TTest); override;
    procedure StartTestSuite(ATestSuite: TTestSuite); override;
    procedure EndTestSuite(ATestSuite: TTestSuite); override;
    procedure WriteResult(aResult: TTestResult); override;
  end;

implementation

uses
  xmlwrite;

const
  { characters below #32 which are not allowed in XML 1.0 (all except tab, LF and CR) }
  XML_INVALID_CHARS = [#0..#8, #11, #12, #14..#31];

{* formats a TDateTime fraction as JUnit time attribute (seconds with '.'
   as decimal separator, independent of the system locale)
   @param(aTime duration as TDateTime fraction)
   @returns(duration in seconds, 3 decimals) *}
function _FormatSeconds(const aTime: TDateTime): String;
var
  fFormatSettings: TFormatSettings;
begin
  fFormatSettings := DefaultFormatSettings;
  fFormatSettings.DecimalSeparator := '.';
  Result := FloatToStrF(aTime * SecsPerDay, ffFixed, 15, 3, fFormatSettings);
end;

{* removes the characters which are not allowed in XML 1.0 from a test
   message (e.g. the mIRC color code #3); the XML writer raises an
   EConvertError on them, which would abort the test run
   @param(aText message text)
   @returns(aText without characters that are invalid in XML) *}
function _XmlText(const aText: String): String;
var
  i, fLen: Integer;
begin
  SetLength(Result, Length(aText));
  fLen := 0;
  for i := 1 to Length(aText) do
  begin
    if aText[i] in XML_INVALID_CHARS then
      Continue;
    Inc(fLen);
    Result[fLen] := aText[i];
  end;
  SetLength(Result, fLen);
end;

constructor TslJUnitResultsWriter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSuiteNames := TStringList.Create;
end;

destructor TslJUnitResultsWriter.Destroy;
begin
  FSuiteNames.Free;
  FDoc.Free;
  inherited Destroy;
end;

procedure TslJUnitResultsWriter.WriteHeader;
begin
  inherited WriteHeader;
  FDoc := TXMLDocument.Create;
  FDoc.AppendChild(FDoc.CreateElement('testsuites'));
  FSuite := FDoc.CreateElement('testsuite');
  FSuite.SetAttribute('name', 'slftpUnitTests');
  FDoc.DocumentElement.AppendChild(FSuite);
end;

function TslJUnitResultsWriter.GetCurrentClassName: String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FSuiteNames.Count - 1 do
  begin
    if FSuiteNames[i] = '' then
      Continue;
    if Result <> '' then
      Result := Result + '.';
    Result := Result + FSuiteNames[i];
  end;
end;

procedure TslJUnitResultsWriter.StartTestSuite(ATestSuite: TTestSuite);
begin
  inherited StartTestSuite(ATestSuite);
  FSuiteNames.Add(ATestSuite.TestName);
end;

procedure TslJUnitResultsWriter.EndTestSuite(ATestSuite: TTestSuite);
begin
  inherited EndTestSuite(ATestSuite);
  if FSuiteNames.Count > 0 then
    FSuiteNames.Delete(FSuiteNames.Count - 1);
end;

procedure TslJUnitResultsWriter.StartTest(ATest: TTest);
begin
  inherited StartTest(ATest);
  FCurrentCase := FDoc.CreateElement('testcase');
  FCurrentCase.SetAttribute('classname', GetCurrentClassName);
  FCurrentCase.SetAttribute('name', ATest.TestName);
  FSuite.AppendChild(FCurrentCase);
end;

procedure TslJUnitResultsWriter.WriteTestFooter(ATest: TTest; ALevel: integer; ATiming: TDateTime);
begin
  if FCurrentCase <> nil then
  begin
    FCurrentCase.SetAttribute('time', _FormatSeconds(ATiming));
    FCurrentCase := nil;
  end;
  inherited WriteTestFooter(ATest, ALevel, ATiming);
end;

procedure TslJUnitResultsWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  fElement: TDOMElement;
begin
  if FCurrentCase <> nil then
  begin
    if AFailure.IsIgnoredTest then
    begin
      fElement := FDoc.CreateElement('skipped');
      fElement.SetAttribute('message', _XmlText(AFailure.ExceptionMessage));
    end
    else
    begin
      fElement := FDoc.CreateElement('failure');
      fElement.SetAttribute('message', _XmlText(AFailure.ExceptionMessage));
      fElement.SetAttribute('type', AFailure.ExceptionClassName);
      fElement.AppendChild(FDoc.CreateTextNode(_XmlText(AFailure.AsString)));
    end;
    FCurrentCase.AppendChild(fElement);
  end;
  inherited AddFailure(ATest, AFailure);
end;

procedure TslJUnitResultsWriter.AddError(ATest: TTest; AError: TTestFailure);
var
  fElement: TDOMElement;
begin
  if FCurrentCase <> nil then
  begin
    fElement := FDoc.CreateElement('error');
    fElement.SetAttribute('message', _XmlText(AError.ExceptionMessage));
    fElement.SetAttribute('type', AError.ExceptionClassName);
    fElement.AppendChild(FDoc.CreateTextNode(_XmlText(AError.AsString)));
    FCurrentCase.AppendChild(fElement);
  end;
  inherited AddError(ATest, AError);
end;

procedure TslJUnitResultsWriter.WriteResult(aResult: TTestResult);
begin
  FSuite.SetAttribute('tests', IntToStr(aResult.RunTests));
  FSuite.SetAttribute('failures', IntToStr(aResult.NumberOfFailures));
  FSuite.SetAttribute('errors', IntToStr(aResult.NumberOfErrors));
  FSuite.SetAttribute('skipped', IntToStr(aResult.NumberOfIgnoredTests + aResult.NumberOfSkippedTests));
  FSuite.SetAttribute('time', _FormatSeconds(Now - aResult.StartingTime));
  if FileName <> '' then
    WriteXMLFile(FDoc, FileName)
  else
    WriteXMLFile(FDoc, Output);
  inherited WriteResult(aResult);
end;

end.
