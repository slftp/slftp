unit tags;

interface

function TagComplete(filename: AnsiString): Integer;
procedure TagsInit;
procedure TagsUninit;

implementation

uses Classes, SysUtils, mystrings, configunit, debugunit, RegExpr;

const
  section = 'tags';
  ftprush_regex: AnsiString = '([^\w]*100%[^\w]*)|([^\w]*-\sCOMPLETE\s\)[^\w]*)|([^\w]*-\sCOMPLETE\s-[^\w]*)';

var
  tags_complete: TStringList = nil;
  tags_incomplete: TStringList = nil;


//  1 if complete
// -1 if incomplete
//  0 otherwise, if nothing matched
function TagComplete(filename: AnsiString): Integer;
var
  i, j: Integer;
  voltszam: Boolean; // voltszam = I was an ass
  cr: TRegExpr;
begin
  Result := 0;

  //we should quit if the regex don't match or? -- atm we will try tags then
  //will be faster if we drop tags usage if regex is enabled. save memory too
  //because we can exit the tagsinit and tagsuninit before creating Stringlist
  if (config.ReadBool(section, 'useregex', false)) then
  begin
    cr := TRegExpr.Create;
    try
      cr.ModifierI := True;
      cr.Expression := config.ReadString(section, 'regex', ftprush_regex);
      if cr.Exec(filename) then
      begin
        Debug(dpSpam, section, 'TagComplete By RegExp %s', [filename]);
        result := 1;
        exit;
      end;
    finally
      cr.Free;
    end;
  end;

  i := AnsiPos(AnsiUpperCase('% complete'), AnsiUpperCase(filename));
  if i > 4 then
  begin
    voltszam := False;
    for j := 1 to 4 do
      if ((not voltszam) and (filename[i-j] = ' ')) then
        Continue
      else
      begin
        voltszam := True;
        if (filename[i-j] < '0') or (filename[i-j] > '9') then
        begin
          i := StrToIntDef(Trim(Copy(filename, i-j+1, j-1)), -1);
          break;
        end;
      end;

    if i = 100 then
    begin
      Result := 1;
      exit;
    end else
    begin
      Result := -1;
      exit;
    end;
  end;

  for i := 0 to tags_incomplete.Count-1 do
    if (0 <> AnsiPos(AnsiUpperCase(tags_incomplete[i]), AnsiUpperCase(filename))) then
    begin
      Result := -1;
      exit;
    end;

  for i := 0 to tags_complete.Count-1 do
    if (0 <> AnsiPos(AnsiUpperCase(tags_complete[i]), AnsiUpperCase(filename))) then
    begin
      Debug(dpSpam, section, 'TagComplete By Tag %s', [filename]);
      Result:= 1;
      exit;
    end;
end;

procedure TagsInit;
var 
  i: Integer;
  s, ss: AnsiString;
begin
  tags_complete := TStringList.Create;
  s := LowerCase(config.ReadString(section, 'complete', '')); // milyen elbaszott egy kibaszott szarfoshugygeci ez
  for i := 1 to 1000 do
  begin
    ss := SubString(s, ',', i);
    if Trim(ss) = '' then break;
    tags_complete.Add(ss);
  end;

  tags_incomplete := TStringList.Create;
  s := LowerCase(config.ReadString(section, 'incomplete', '')); // milyen elbaszott egy kibaszott szarfoshugygeci ez
  for i := 1 to 1000 do
  begin
    ss := SubString(s, ',', i);
    if Trim(ss) = '' then break;
    tags_incomplete.Add(ss);
  end;

end;

procedure TagsUninit;
begin
  Debug(dpSpam, section, 'Uninit tags_complete');
  if tags_complete <> nil then
  begin
    tags_complete.Free;
    tags_complete := nil;
  end;

  if tags_incomplete <> nil then
  begin
    tags_incomplete.Free;
    tags_incomplete := nil;
  end;
  Debug(dpSpam, section, 'Uninit tags_incomplete');
end;

end.
