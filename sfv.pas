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
    function RegisterSFV(const aDir: String): boolean; //< Try to register a SFV file. Returns true, if the SFV was registered, False if the SFV was already registered.
    function HasSFV(const aDir: String): boolean; //< Returns true, if an SFV file has been registered for the given dictionary.
    function SetSFVDownloadRunning(const aIsRunning: boolean): boolean; //< Sets the SFV download running flag so that there will not be multiple downloads of the same SFV file at once.
    procedure SetSFVList(const aDir: string; const aFiles: TDictionary<string, integer>); //< Sets the list of files contained in the SFV file after it has been parsed.
    function CheckSFV(const aDir, aFilename, aExtension: string): boolean;  //< Returns true, if the given file exists in the SFV
  end;

implementation

uses SysUtils, mystrings, globals, debugunit;

const
  section = 'sfv';

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
    Result := FSFVList.ContainsKey(aDir) and (FSFVList[aDir] <> nil);
  finally
    FSFVList_cs.Leave;
  end;
end;

function TPazoSFV.RegisterSFV(const aDir: String): boolean;
var
  fKey: String;
begin
  Result := False;
  FSFVList_cs.Enter;
  try
    if not FSFVList.ContainsKey(aDir) then
    begin
      FSFVList.Add(aDir, nil);
      Result := True;
    end;
  finally
    FSFVList_cs.Leave;
  end;
end;

function TPazoSFV.SetSFVDownloadRunning(const aIsRunning: boolean): boolean;
begin
  Result := False;
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

    if aFiles.Count = 0 then
    begin
      Debug(dpError, section, 'Try to set empty SFV list for dir ' + aDir);
      exit;
    end;

    if self.HasSFV(aDir) then
    begin
      if FSFVList[aDir].Count = aFiles.Count then
      begin
        Debug(dpMessage, section, 'SFV file already registered for dir ' + aDir)
      end
      else
      begin
        Debug(dpError, section, 'Try to set different SFV for already already registered dir ' + aDir)
      end;

      exit;
    end;

    FSFVList[aDir] := aFiles;

    if FSFVFileType = '' then
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
  if (FSFVFileType = CONST_RAR_FILES) then
  begin
    if not IsRarExtension(aExtension) then
      exit;
  end
  else if FSFVFileType <> aExtension then
    exit;

  FSFVList_cs.Enter;
  try
    Result := not FSFVList.TryGetValue(aDir, fSFVFiles) or (fSFVFiles = nil) or fSFVFiles.ContainsKey(aFilename);
  finally
    FSFVList_cs.Leave;
  end;
end;

end.
