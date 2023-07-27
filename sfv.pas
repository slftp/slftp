unit sfv;

interface

uses
  Classes, Generics.Collections, SyncObjs;

type
  TPazoSFV = class
  private
    FSFVList_cs: TCriticalSection;
    FSFVList: TObjectDictionary<String, TDictionary<string, integer>>;
    FSFVDownloadRunning: boolean;
    FSFVFileType: string;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterSFV(const aDir: String): boolean;
    function HasSFV(const aDir: String): boolean;
    function SetSFVDownloadRunning(const aIsRunning: boolean): boolean;
    procedure SetSFVList(const aDir: string; const aFiles: TDictionary<string, integer>);
    function CheckSFV(const aDir, aFilename, aExtension: string): boolean;
  end;

implementation

uses SysUtils, mystrings, slconstants;

constructor TPazoSFV.Create;
begin
  self.FSFVList_cs := TCriticalSection.Create;
  self.FSFVList := TObjectDictionary < String, TDictionary < string, integer >>.Create([doOwnsValues]);
end;

destructor TPazoSFV.Destroy;
begin
  FreeAndNil(FSFVList_cs);
  FreeAndNil(FSFVList);
end;

function TPazoSFV.HasSFV(const aDir: String): boolean;
begin
  FSFVList_cs.Enter;
  try
    Result := FSFVList.ContainsKey(aDir) and (FSFVList[aDir].Count > 0);
  finally
    FSFVList_cs.Leave;
  end;
end;

function TPazoSFV.RegisterSFV(const aDir: String): boolean;
var
  fKey: String;
begin
  FSFVList_cs.Enter;
  try
    if not FSFVList.ContainsKey(aDir) then
    begin
      FSFVList.Add(aDir, TDictionary<string, integer>.Create);
      Result := True;
    end;
  finally
    FSFVList_cs.Leave;
  end;
end;

function TPazoSFV.SetSFVDownloadRunning(const aIsRunning: boolean): boolean;
begin
  if aIsRunning then
  begin
    if not FSFVDownloadRunning then
    begin
      FSFVList_cs.Enter;
      try
        if not FSFVDownloadRunning then
        begin
          FSFVDownloadRunning := True;
          Result := True;
        end;
      finally
        FSFVList_cs.Leave;
      end;
    end;
  end
  else
  begin
    FSFVDownloadRunning := False;
    Result := True;
  end;
end;

procedure TPazoSFV.SetSFVList(const aDir: string; const aFiles: TDictionary<string, integer>);
var
  fExtension: String;
begin
  FSFVList_cs.Enter;
  try
    FSFVList[aDir] := aFiles;

    if (aFiles.Count > 0) and (FSFVFileType = '') then
    begin
      fExtension := ExtractFileExt(LowerCase(aFiles.Keys.ToArray()[0]));

      if IsRarExtension(fExtension) then
        FSFVFileType := CONST_RAR_FILES
      else
        FSFVFileType := fExtension;

    end;
  finally
    FSFVList_cs.Leave;
  end;
end;

function TPazoSFV.CheckSFV(const aDir, aFilename, aExtension: string): boolean;
var
  fSFVFiles: TDictionary<string, integer>;
begin
  Result := True;

  // only check files which match the files types contained in the SFV
  if (FSFVFileType = CONST_RAR_FILES) and not IsRarExtension(aExtension) then
    exit;

  if FSFVFileType <> aExtension then
    exit;

  FSFVList_cs.Enter;
  try
    Result := not FSFVList.TryGetValue(aDir, fSFVFiles) or fSFVFiles.ContainsKey(aFilename);
  finally
    FSFVList_cs.Leave;
  end;
end;

end.
