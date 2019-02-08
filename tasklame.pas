unit tasklame;

interface

uses
  sitesunit, lame, ID3v2, tasksunit, Classes, sltcp, mpeginfo;

type
  TLameTask = class(TTask)
    private
      filename: String;
      dir: String;
      genremode: Boolean;
      filesize: Int64;
    public
      constructor Create(const netname, channel, site, dir, filename: String; const filesize: Int64; genremode: Boolean);
      destructor Destroy; override;
      function Execute(slot: Pointer): Boolean; override;
      function Name: String; override;
  end;

function LameID3v1ID3v2Info(s: TSiteSlot; id3v1only: Boolean; const filesize: Int64; const dir, filename: String; var id3v1: TID3v1Info; var id3v2: TID3v2Info; var lame: TLameInfo; var mpeg: TMpegInfo): Integer;

implementation

uses
  SysUtils, DebugUnit;

const
  section = 'lame';

{ TLameTask }

constructor TLameTask.Create(const netname, channel, site, dir, filename: String; const filesize: Int64; genremode: Boolean);
begin
  self.filename := filename;
  self.filesize := filesize;
  self.dir := dir;
  self.genremode := genremode;
  self.wanted_dn := True;
  inherited Create(netname, channel, site);
end;

destructor TLameTask.Destroy;
begin
  inherited;
end;

function LameID3v1ID3v2Info(s: TSiteSlot; id3v1only: Boolean; const filesize: Int64; const dir, filename: String; var id3v1: TID3v1Info; var id3v2: TID3v2Info; var lame: TLameInfo; var mpeg: TMpegInfo): Integer;
type
  TTartas = (ttid3v1, ttid3v2, ttlame, ttmpeg);
var
  i, restFrom, mennyitolvass: Integer;
  tartas: TTartas;
  ms: TMemoryStream;
  buffer: array[1..LAME_BUFFER_SIZE] of Byte;
begin
  Result := 0;
  id3v2.exists := False;

  ms := TMemoryStream.Create;
  try
    if (not s.Cwd(dir, true)) then exit;
    if not s.SendProtP then exit;

    tartas := ttid3v2;
    restfrom := 0;
    mennyitolvass := LAME_BUFFER_SIZE;
    if id3v1only then
    begin
      tartas := ttid3v1;
      restfrom := filesize - ID3V1_BUFFER_SIZE;
      mennyitolvass := ID3V1_BUFFER_SIZE;
    end;

    while(true) do
    begin
      ms.Clear;

      // trying to get the nfo
      s.downloadingfrom := True;
      i := s.LeechFile(ms, filename, restfrom, mennyitolvass);

      if i <= 0 then
      begin
        Result := i;
        exit;
      end;
      // else success

      ms.Position := 0;

      if tartas = ttid3v1 then
      begin
        ms.Read(buffer, ID3V1_BUFFER_SIZE);
        ID3_Check(buffer, id3v1);
        break;
      end;

      if tartas = ttid3v2 then
      begin
        with TID3v2.Create do
        begin
          ReadFromStream(ms);
          FillID3v2Info(id3v2);
          Free;
        end;

        if id3v2.Size > mennyitolvass then
        begin
          restfrom := 0;
          mennyitolvass := id3v2.Size + LAME_BUFFER_SIZE;
        end
        else
        begin
          tartas := ttlame;
          ms.Position := 0;
        end;
      end;

      if tartas = ttlame then
      begin
        ms.Read(buffer, LAME_BUFFER_SIZE);

        i := Lame_Check(buffer, 0, lame);
        if i <= 0 then
        begin
          tartas := ttmpeg;
          ms.Position := 0;
        end
        else
        begin
          mennyitolvass := LAME_BUFFER_SIZE;
          restFrom := i;
          // tartas marad
        end;
      end;

      if tartas = ttmpeg then
      begin
        ms.Read(buffer, LAME_BUFFER_SIZE);
        Mpeg_Check(buffer, filesize, mpeg);
        tartas := ttid3v1;
        mennyitolvass := ID3V1_BUFFER_SIZE;
        restfrom := filesize-ID3V1_BUFFER_SIZE;
      end;
    end;

    Result := 1;
  finally
    ms.Free;
  end;
end;

function TLameTask.Execute(slot: Pointer): Boolean;
label
  ujra;
var
  s: TSiteSlot;
  i, hibak: Integer;
  re0: TID3v2Info;
  re1: TLameInfo;
  re2: TID3v1Info;
  m: TMpegInfo;
begin
  Result := False;
  s := slot;
  Debug(dpMessage, section, Name);
  hibak := 0;

ujra:
  inc(hibak);
  if(hibak >= 3) then
  begin
    readyerror := True;
    exit;
  end;

  if s.status <> ssOnline then
    if not s.ReLogin then
    begin
      readyerror := True;
      exit;
    end;

  i := LameID3v1ID3v2Info(s, genremode, filesize, dir, filename, re2, re0, re1, m);

  if i < 0 then
  begin
    readyerror := True;
    exit;
  end;
  if i = 0 then goto ujra;

  response := LameInfoToString(re1) + ' / ' + ID3v1InfoToString(re2) + ' / ' + ID3v2InfoToString(re0) + ' / ' + filename;

  Result := True;
  ready := True;
end;

function TLameTask.Name: String;
begin
  try
    Result := Format('::LAMECHK:: <b>%s</b>: %s', [site1, filename]);
  except
    Result := '::LAMECHK::';
  end;
end;

end.