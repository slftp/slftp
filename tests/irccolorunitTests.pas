unit irccolorunitTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestIrcColorUnit = class(TTestCase)
  published
    procedure TestReplaceThemeMSG1;
    procedure TestReplaceThemeMSG2;
    procedure TestReplaceThemeMSG3;
    procedure TestReplaceThemeMSG4;
    procedure TestReplaceThemeMSG5;
  end;

implementation

uses
  SysUtils, irccolorunit;

{ TTestIrcColorUnit }

procedure TTestIrcColorUnit.TestReplaceThemeMSG1;
var
  fInput, fResult, fExpect: String;
begin
  fInput := '<b>This is a<u>TEST</u></b>';
  fExpect := #2 + 'This is a' + #31 + 'TEST' + #31#2;
  fResult := ReplaceThemeMSG(fInput);

  CheckEqualsString(fExpect, fResult);
end;

procedure TTestIrcColorUnit.TestReplaceThemeMSG2;
var
  fInput, fResult, fExpect: String;
begin
  fInput := '<c4>Another test <b>with bold</b> and colors</c>';
  fExpect := #3 + '04Another test ' + #2 + 'with bold' + #2 + ' and colors' + #3;
  fResult := ReplaceThemeMSG(fInput);

  CheckEqualsString(fExpect, fResult);
end;

procedure TTestIrcColorUnit.TestReplaceThemeMSG3;
var
  fInput, fResult, fExpect: String;
begin
  fInput := '<c12>Testing <b>with bold</b> and high color code</c>';
  fExpect := #3 + '12Testing ' + #2 + 'with bold' + #2 + ' and high color code' + #3;
  fResult := ReplaceThemeMSG(fInput);

  CheckEqualsString(fExpect, fResult);
end;

procedure TTestIrcColorUnit.TestReplaceThemeMSG4;
var
  fInput, fResult, fExpect: String;
begin
  fInput := '<c7>[<b>NEW</b>]</c> MP3 VA_-_2000er_Dance_Music_Vol._2-WEB-2019-MARiBOR @ <b>SITE</b> (<c7><b>Not found in PreDB</b></c>)';
  fExpect := #3 + '07[' + #2 + 'NEW' + #2 + ']' + #3 + ' MP3 VA_-_2000er_Dance_Music_Vol._2-WEB-2019-MARiBOR @ ' + #2 + 'SITE' + #2 + ' (' + #3 + '07' + #2 + 'Not found in PreDB' + #2 + #3 + ')';
  fResult := ReplaceThemeMSG(fInput);

  CheckEqualsString(fExpect, fResult);
end;

procedure TTestIrcColorUnit.TestReplaceThemeMSG5;
var
  fInput, fResult, fExpect: String;
begin
  fInput := '<c10>[<b>TVInfo</b>]</c> <b>Country</b> USA - <b>Network</b> NBC - <b>Language</b> English - <b>Rating</b> 62/100';
  fExpect := #3 + '10[' + #2 + 'TVInfo' + #2 + ']' + #3 + ' ' + #2 + 'Country' + #2 + ' USA - ' + #2 + 'Network' + #2 + ' NBC - ' + #2 + 'Language' + #2 + ' English - ' + #2 + 'Rating' + #2 + ' 62/100'; 
  fResult := ReplaceThemeMSG(fInput);

  CheckEqualsString(fExpect, fResult);
end;

initialization
  {$IFDEF FPC}
    RegisterTest('irccolorunit', TTestIrcColorUnit.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIrcColorUnit);
  {$ENDIF}
end.
