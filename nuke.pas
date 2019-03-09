unit nuke;

interface

uses
  Contnrs;

{ Just a helper function to initialize @link(nukequeue) }
procedure NukeInit;
{ Just a helper function to free @link(nukequeue) }
procedure NukeUnInit;
{ Reads existing values from slftp.nukequeue on startup }
procedure NukeStart;
{ Saves existing values from memory to slftp.nukequeue when closing slftp. }
procedure NukeSave;

type
  TNukeQueueItem = class
    site: String;
    section: String;
    yyyy: String;
    yy: String;
    mm: String;
    dd: String;
    rip: String;
    multiplier: Integer;
    reason: String;
  end;

var
  nukequeue: TObjectList; //< Queue of added nukes which need to be send to site

implementation

uses
  Classes, SysUtils, encinifile, configunit, mystrings;

procedure NukeInit;
begin
  nukequeue := TObjectList.Create(True);
end;

procedure NukeUnInit;
begin
  if Assigned(nukequeue) then
    nukequeue.Free;
end;

procedure NukeStart;
var
  x: TEncStringlist;
  i: Integer;
  n: TNukeQueueItem;
  fn: String;
begin
  fn := ExtractfilePath(ParamStr(0)) + 'slftp.nukequeue';
  if not fileExists(fn) then
    exit;

  x := TEncStringlist.Create(passphrase);
  try
    x.LoadFromFile(fn);
    for i := 0 to x.Count - 1 do
    begin
      x[i] := trim(x[i]);
      n := TNukeQueueItem.Create;
      n.site := SubString(x[i], ' ', 1);
      n.section := SubString(x[i], ' ', 2);
      n.yyyy := SubString(x[i], ' ', 3);
      n.yy := Copy(n.yyyy, 3, 2);
      n.mm := SubString(x[i], ' ', 4);
      n.dd := SubString(x[i], ' ', 5);
      n.rip := SubString(x[i], ' ', 6);
      n.multiplier := StrToIntDef( SubString(x[i], ' ', 7) , 0) ;
      n.reason := Copy(x[i], Length(n.site) + 1 + Length(n.section) + 1 + 4 + 1 + 2 + 1 + 2 + 1 + Length(n.rip) + 1 + Length(SubString(x[i], ' ', 7)) + 1 + 1, 1000);

      nukequeue.Add(n);
    end;
  finally
    x.Free;
  end;
end;

procedure NukeSave;
var
  x: TEncStringlist;
  i: Integer;
  n: TNukeQueueItem;
  s: String;
begin
  x := TEncStringlist.Create(passphrase);
  try
    for i := 0 to nukequeue.Count - 1 do
    begin
      n := TNukeQueueItem(nukequeue[i]);
      s := Format('%s %s %s %s %s %s %d %s',[n.site, n.section, n.yyyy, n.mm, n.dd, n.rip, n.multiplier, n.reason]);
      x.Add(s);
    end;
    x.SaveToFile(ExtractfilePath(ParamStr(0)) + 'slftp.nukequeue');
  finally
    x.Free;
  end;
end;

end.
