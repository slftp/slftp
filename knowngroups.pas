unit knowngroups;

interface

type
  TKnownGroup = (grp_known, grp_unknown, grp_notconfigured);

procedure KnowngroupsInit;
procedure KnowngroupsUnInit;
procedure KnowngroupsStart;

function IsKnownGroup(section, groupname: AnsiString): TKnownGroup;

function RemoveINT(grp: AnsiString): AnsiString;

implementation

uses Classes, SysUtils, StrUtils, configunit, regexpr;

var kg: TStringList;

procedure KnowngroupsInit;
begin
  kg:= TStringList.Create;
end;
procedure KnowngroupsUnInit;
begin
  kg.Free;
end;

procedure KnowngroupsStart;
var f: TextFile;
    s, section: AnsiString;
    i: Integer;
begin
  kg.clear;
  s:= ExtractFilePath(ParamStr(0))+'slftp.knowngroups';
  if FileExists(s) then
  begin
    AssignFile(f, s);
    Reset(f);
    while not Eof(f) do
    begin
      ReadLn(f,s);
      i:= Pos('=', s);
      if i > 1 then
      begin
        section:=  UpperCase(Copy(s, 1, i-1));
        kg.Values[section]:= ' '+kg.Values[section]+' '+Copy(s, i+1, length(s))+' ';
      end;
    end;

    // support placeholder like TV-720=%TV-1080%
    Reset(f);
    while not Eof(f) do
    begin
      ReadLn(f,s);
      i:= Pos('=', s);
      if i > 1 then
      begin
        section:=  UpperCase(Copy(s, 1, i-1));
        i:= Pos('%', s);
        if i > 1 then
        begin
          s := Copy(s, i+1, length(s)-i);
          i:= Pos('%', s);
          if i > 1 then
          begin
            s := Copy(s, 0, i-1);
            kg.Values[section]:= kg.Values[s];
            s := kg.Values[section];
          end;
        end;
      end;
    end;

    CloseFile(f);
  end;
end;


function RemoveINT(grp: AnsiString): AnsiString;
var r: TRegexpr;
begin
  result:= grp;
  try
    r := TRegexpr.Create;
    try
      r.ModifierI := True;
      r.Expression := '[\-\_]int$';
      result := r.Replace(grp, '', False);
    finally
      r.free;
    end;
  except
    result := grp;
  end;
end;

function RemoveWEB(grp: AnsiString): AnsiString;
var r: TRegexpr;
begin
  result := grp;
  try
    r := TRegexpr.Create;;
    try
      r.ModifierI := True;
      r.Expression := '[\-\_]web$';
      result := r.Replace(grp, '', False);
    finally
      r.free;
    end;
  except
    result := grp;
  end;
end;

function IsKnownGroup(section, groupname: AnsiString): TKnownGroup;
var s: AnsiString;
begin
  Result:= grp_notconfigured;
  try
    if config.ReadBool('kb','remove_internal_tag_on_knowgroup',False) then groupname:=  RemoveINT(groupname);
    if config.ReadBool('kb','remove_web_tag_on_knowgroup',False) then groupname:=  RemoveWEB(groupname);

    s:= kg.Values[section];
    if s = '' then exit;

    if AnsiContainsText(s, ' '+groupname+' ') then
      Result:= grp_known
    else
      Result:= grp_unknown;
  except
    Result:= grp_notconfigured;
  end;
end;

end.
