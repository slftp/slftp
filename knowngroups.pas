unit knowngroups;

interface

type
  { Different types of knowngroup status
    @definitionList(
      @itemLabel(grp_known)
      @item(Means that the group was defined as known group in config file.)

      @itemLabel(grp_unknown)
      @item(Group was not found in list but a list for the section does exist.)

      @itemLabel(grp_notconfigured)
      @item(Group is not found in known group check, means it wasn't added as known group or
        list for section is empty)
    )
  }
  TKnownGroup = (grp_known, grp_unknown, grp_notconfigured);

{ Just a helper function to create @link(kg) on init }
procedure KnowngroupsInit;

{ Just a helper function to free @link(kg) on uninit }
procedure KnowngroupsUnInit;

{ Just a helper function to set values from knowngroups file to @link(kg). It even handles the support for placeholders like TV-720=%TV-1080% }
procedure KnowngroupsStart;

{ Checks what type of @link(TKnownGroup) a given group is in a given sectionname
  @param(section Sectioname of the knowngroup list which should be used)
  @param(groupname Groupname to check)
  @returns(@link(TKnownGroup)) }
function IsKnownGroup(const section: String; groupname: String): TKnownGroup;

implementation

uses
  Classes, SysUtils, StrUtils, configunit, mygrouphelpers;

var
  kg: TStringList; //< stringlist which stores all knowngroups from config file

procedure KnowngroupsInit;
begin
  kg := TStringList.Create;
end;

procedure KnowngroupsUnInit;
begin
  kg.Free;
end;

procedure KnowngroupsStart;
var
  f: TextFile;
  s, section: String;
  i: Integer;
begin
  kg.clear;
  s := ExtractFilePath(ParamStr(0)) + 'slftp.knowngroups';
  if FileExists(s) then
  begin
    AssignFile(f, s);
    Reset(f);
    while not Eof(f) do
    begin
      ReadLn(f,s);
      i := Pos('=', s);
      if i > 1 then
      begin
        section := UpperCase(Copy(s, 1, i - 1));
        kg.Values[section] := ' ' + kg.Values[section] + ' ' + Copy(s, i + 1, length(s)) + ' ';
      end;
    end;

    // support placeholder like TV-720=%TV-1080%
    Reset(f);
    while not Eof(f) do
    begin
      ReadLn(f,s);
      i := Pos('=', s);
      if i > 1 then
      begin
        section := UpperCase(Copy(s, 1, i - 1));
        i := Pos('%', s);
        if i > 1 then
        begin
          s := Copy(s, i + 1, length(s) - i);
          i := Pos('%', s);
          if i > 1 then
          begin
            s := Copy(s, 0, i - 1);
            kg.Values[section] := kg.Values[s];
            s := kg.Values[section];
          end;
        end;
      end;
    end;

    CloseFile(f);
  end;
end;

function IsKnownGroup(const section: String; groupname: String): TKnownGroup;
var
  s: String;
begin
  Result := grp_notconfigured;
  try
    if config.ReadBool('kb', 'remove_internal_tag_on_knowgroup', False) then
      groupname := RemoveINT(groupname);
    if config.ReadBool('kb', 'remove_web_tag_on_knowgroup', False) then
      groupname := RemoveWEB(groupname);

    s := kg.Values[section];
    if s = '' then exit;

    if AnsiContainsText(s, ' ' + groupname + ' ') then
      Result := grp_known
    else
      Result := grp_unknown;
  except
    Result := grp_notconfigured;
  end;
end;

end.
