unit irccommands.indexer;

interface

{ slftp indexer commands functions }
function IrcIndexStat(const netname, channel, params: String): boolean;
function IrcIndexQuery(const netname, channel, params: String): boolean;
function IrcIndexDropSection(const netname, channel, params: String): boolean;
function IrcAutoIndex(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, indexer, debugunit, irc, mystrings, sitesunit, rcmdline;

const
  section = 'irccommands.indexer';

function IrcIndexStat(const netname, channel, params: String): boolean;
var
  s, ss: String;
begin
  s := indexerStat;

  while (True) do
  begin
    ss := GetFirstLineFromTextViaNewlineIndicators(s);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;
  Result := True;
end;

function IrcIndexQuery(const netname, channel, params: String): boolean;
var
  fSearchName, s, ss: String;
  i, fQueryLimit: Integer;
  fCLReader: TCommandLineReader;
begin
  Result := False;
  fSearchName := '';

  fCLReader := TCommandLineReader.Create;
  try
    try
      fCLReader.allowDOSStyle := True;
      fCLReader.automaticalShowError := False;
      fCLReader.declareString('limit', '', '');
      fCLReader.addAbbreviation('l', 'limit');
      fCLReader.parse(params);
    except
      on e: Exception do
      begin
        irc_addtext(Netname, Channel, '<c4><b>%s</b></c>', [e.Message]);
        Debug(dpError, section, '[EXCEPTION] IrcIndexQuery(TCommandLineReader.parse): %s', [e.Message]);
        exit;
      end;
    end;

    for i := 0 to High(fCLReader.readNamelessString()) do
    begin
      fSearchName := fSearchName + fCLReader.readNamelessString()[i] + ' ';
    end;
    // remove last whitespace
    SetLength(fSearchName, Length(fSearchName) - 1);
    // use 10 query outputs as default
    fQueryLimit := StrToIntDef(fCLReader.readString('limit'), 10);
  finally
    fCLReader.Free;
  end;

  s := indexerQueryPartially(fSearchName, fQueryLimit);

  if s <> '' then
  begin
    while (True) do
    begin
      ss := GetFirstLineFromTextViaNewlineIndicators(s);
      if ss = '' then
        break;

      irc_addtext(Netname, Channel, ss);
    end;
  end
  else
    irc_addtext(Netname, Channel, 'Cant find rip %s indexed.', [fSearchName]);

  Result := True;
end;

function IrcIndexDropSection(const netname, channel, params: String): boolean;
var
  fParams: String;
begin
  fParams := UpperCase(params);
  indexerRemoveSiteSection(SubString(fParams, ' ', 1), SubString(fParams, ' ', 2));
  Result := True;
end;

function IrcAutoIndex(const netname, channel, params: String): boolean;
var
  sitename: String;
  status: integer;
  s: TSite;
  kell: boolean;
  sections: String;
  ss: String;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);
  sections := UpperCase(mystrings.RightStr(params, length(sitename) + 1 + length(IntToStr(status)) + 1));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found', [sitename]);
    exit;
  end;
  if (s.PermDown) then
  begin
    irc_addtext(Netname, Channel, 'Site %s is set as PermDown', [sitename]);
    Exit;
  end;
  if ((status > -1) and (status <> 0)) then
  begin
    // hitelesitjuk a szekciokat
    for i := 1 to 1000 do
    begin
      ss := SubString(sections, ' ', i);
      if ss = '' then
        break;

      if s.sectiondir[ss] = '' then
      begin
        irc_addtext(Netname, Channel, 'Site %s has no %s section', [sitename, ss]);
        exit;
      end;
    end;
  end;

  kell := False;
  if status > -1 then
  begin
    if status <> 0 then
    begin
      if s.AutoIndexInterval <= 0 then
        kell := True;
      s.AutoIndexInterval := status;
      s.AutoIndexSections := sections;
    end
    else
    begin
      s.DeleteKey('autoindex');
      s.DeleteKey('autoindexsections');
      s.DeleteKey('nextautoindex');
      s.RemoveAutoIndex;
    end;
  end;
  irc_addtext(Netname, Channel, 'Autoindex of %s is: %d (%s)', [sitename, s.AutoIndexInterval, s.AutoIndexSections]);

  if kell then
    s.AutoIndex;

  Result := True;
end;

end.
