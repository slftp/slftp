unit ircparsingTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestIrcParsingUnit = class(TTestCase)
  published
    // INVITE
    procedure TestParsingInvite1;
    // JOIN
    procedure TestParsingJoin1;
    procedure TestParsingJoin2;
    procedure TestParsingJoin3;
    procedure TestParsingJoin4;
    // KICK
    procedure TestParsingKick1;
    // NICK
    procedure TestParsingNick1;
    // PART
    procedure TestParsingPart1;
    // QUIT
    procedure TestParsingQuit1;
    // TOPIC
    procedure TestParsingTopic1;
    procedure TestParsingTopic2;
  end;

implementation

uses
  SysUtils, irc.parse;

{ TTestIrcParsingUnit }

// INVITE
procedure TTestIrcParsingUnit.TestParsingInvite1;
var
  fInput, fExpectChan, fExpectNick, fChan, fNick: String;
begin
  fInput := ':invitee!ident@ircserver.local INVITE slnick :#test1';
  fExpectChan := '#test1';
  fExpectNick := 'invitee';
  parseIrcINVITE(fInput, fChan, fNick);

  CheckEqualsString(fExpectChan, fChan);
  CheckEqualsString(fExpectNick, fNick);
end;


// JOIN
procedure TTestIrcParsingUnit.TestParsingJoin1;
var
  fInput, fExpectChan, fExpectNick, fChan, fNick: String;
begin
  fInput := ':sltest!ident@ircserver.local JOIN :#test1';
  fExpectChan := '#test1';
  fExpectNick := 'sltest';
  parseIrcJOIN(fInput, fChan, fNick);

  CheckEqualsString(fExpectChan, fChan);
  CheckEqualsString(fExpectNick, fNick);
end;

procedure TTestIrcParsingUnit.TestParsingJoin2;
var
  fInput, fExpectChan, fExpectNick, fChan, fNick: String;
begin
  fInput := ':sltest!ident@ircserver.local JOIN #test1';
  fExpectChan := '#test1';
  fExpectNick := 'sltest';
  parseIrcJOIN(fInput, fChan, fNick);

  CheckEqualsString(fExpectChan, fChan);
  CheckEqualsString(fExpectNick, fNick);
end;

procedure TTestIrcParsingUnit.TestParsingJoin3;
var
  fInput, fExpectChan, fExpectNick, fChan, fNick: String;
begin
  fInput := ':sltest!ident@ircserver.local JOIN #test1 :Real name';
  fExpectChan := '#test1';
  fExpectNick := 'sltest';
  parseIrcJOIN(fInput, fChan, fNick);

  CheckEqualsString(fExpectChan, fChan);
  CheckEqualsString(fExpectNick, fNick);
end;

procedure TTestIrcParsingUnit.TestParsingJoin4;
var
  fInput, fExpectChan, fExpectNick, fChan, fNick: String;
begin
  fInput := ':sltest!ident@ircserver.local JOIN #test1 :Real name';
  fExpectChan := '#test1';
  fExpectNick := 'sltest';
  parseIrcJOIN(fInput, fChan, fNick);

  CheckEqualsString(fExpectChan, fChan);
  CheckEqualsString(fExpectNick, fNick);
end;


// KICK
procedure TTestIrcParsingUnit.TestParsingKick1;
var
  fInput, fExpectChan, fExpectNick, fChan, fNick: String;
begin
  fInput := ':nick!ident@ircserver.local KICK #test1 sltest :sltest';
  fExpectChan := '#test1';
  fExpectNick := 'sltest';
  parseIrcKICK(fInput, fChan, fNick);

  CheckEqualsString(fExpectChan, fChan);
  CheckEqualsString(fExpectNick, fNick);
end;

// NICK
procedure TTestIrcParsingUnit.TestParsingNick1;
var
  fInput, fExpectNick, fNick: String;
begin
  fInput := ':oldnick!ident@ircserver.local NICK :newnick';
  fExpectNick := 'oldnick';
  parseIrcNICK(fInput, fNick);

  CheckEqualsString(fExpectNick, fNick);
end;

// PART
procedure TTestIrcParsingUnit.TestParsingPart1;
var
  fInput, fExpectChan, fExpectNick, fChan, fNick: String;
begin
  fInput := ':sltest!ident@ircserver.local PART #test1';
  fExpectChan := '#test1';
  fExpectNick := 'sltest';
  parseIrcPART(fInput, fChan, fNick);

  CheckEqualsString(fExpectChan, fChan);
  CheckEqualsString(fExpectNick, fNick);
end;

// QUIT
procedure TTestIrcParsingUnit.TestParsingQuit1;
var
  fInput, fExpectQuit, aQuit: String;
begin
  fInput := ':user!ident@ircserver.local QUIT :Quit: Reason';
  fExpectQuit := 'Reason';
  parseIrcQUIT(fInput, aQuit);

  CheckEqualsString(fExpectQuit, aQuit);
end;

// TOPIC
procedure TTestIrcParsingUnit.TestParsingTopic1;
var
  fInput, fExpectChan, fExpectTopic, fChan, fTopic: String;
begin
  fInput := ':user!ident@ircserver.local TOPIC #test1 :newtopic test';
  fExpectChan := '#test1';
  fExpectTopic := 'newtopic test';
  parseIrcTOPIC(fInput, fChan, fTopic);

  CheckEqualsString(fExpectChan, fChan);
  CheckEqualsString(fExpectTopic, fTopic);
end;

procedure TTestIrcParsingUnit.TestParsingTopic2;
var
  fInput, fExpectChan, fExpectTopic, fChan, fTopic: String;
begin
  fInput := ':user!ident@ircserver.local TOPIC #test1 :newtopic containing : colon';
  fExpectChan := '#test1';
  fExpectTopic := 'newtopic containing : colon';
  parseIrcTOPIC(fInput, fChan, fTopic);

  CheckEqualsString(fExpectChan, fChan);
  CheckEqualsString(fExpectTopic, fTopic);
end;


initialization
  {$IFDEF FPC}
    RegisterTest('IRC Parsing test', TTestIrcParsingUnit.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIrcParsingUnit);
  {$ENDIF}

end.