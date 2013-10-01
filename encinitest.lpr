program encinitest;

{$MODE Delphi}

{$APPTYPE CONSOLE}

uses
  SysUtils, encinifile;

var s: string;
begin
  { TODO -oUser -cConsole Main : Insert code here }
  ReadLn(s);
  with TEncIniFile.CreatE(ExtractFilePath(ParamStr(0))+'sites.dat', s) do
  begin
    writeln(ReadString('sites', 'default', ''));
    Free;
  end;
end.
