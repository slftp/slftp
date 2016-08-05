unit slblowfish;

interface

uses
  delphiblowfish,
{$IFDEF FPC}
  zstream,
{$ELSE}
  ZLib,
{$ENDIF}
 Classes;

type
  TslBlowfishBlock= array[0..7] of byte;

  { TslBlowfish }

  TslBlowfish = class
  private
    KeyData: TBlowfishData;
  public
    constructor Create(const key: array of byte); overload;
    constructor Create(const key, iv: array of byte); overload;

    procedure Reset;

    procedure EncryptECB(InData, OutData: Pointer);
    procedure EncryptCBC(InData, OutData: Pointer);
    procedure DecryptECB(InData, OutData: Pointer);
    procedure DecryptCBC(InData, OutData: Pointer);
  end;

procedure EncryptStreamToStream(input, ostream: TStream; const Key: array of byte; compressstream: Boolean = False);
procedure EncryptStreamToFile(input: TStream; output: AnsiString; const Key: array of byte; compressstream: Boolean = False);
procedure EncryptFileToFile(input: AnsiString; output: AnsiString; const Key: array of byte; compressstream: Boolean = False);
procedure DecryptStreamToStream(istream, output: TStream; const Key: array of byte; decompressstream: Boolean = False);
procedure DecryptFileToStream(input: AnsiString; output: TStream; const Key: array of byte; decompressstream: Boolean = False);
procedure DecryptFileToFile(input: AnsiString; output: AnsiString; const Key: array of byte; decompressstream: Boolean = False);

implementation

uses SysUtils{$IFDEF MSWINDOWS},Windows {$ENDIF};

const
  defaultIV: array[0..7] of byte= ($11, $22, $33, $44, $55, $66, $77, $88);
  BUFSIZE = 16384; // ez 8 tobbszorose kell hogy legyen

{$IFDEF FPC}
procedure XorBlock(I1, I2, O1: PByteArray; len: Integer);
var
  i: integer;
begin
  for i:= 0 to Len-1 do
    O1^[i]:= I1^[i] xor I2^[i];
end;
{$ENDIF}

procedure EncryptStreamToStream(input, ostream: TStream; const Key: array of byte; compressstream: Boolean = False);
var read, aktread: Integer;
    buf: array[1..BUFSIZE] of AnsiChar;
    istream: TMemoryStream;
    zl: TCompressionStream;
    i, iteraciok: Integer;
    size: Int64;

    b: TslBlowfish;
begin
  b:= TslBlowfish.Create(key);

  input.Position:= 0;
  istream:= TMemoryStream.Create;
  if compressstream then
  begin
    zl:= TCompressionStream.Create(clFastest, istream);
    zl.CopyFrom(input, input.Size);
    zl.Free;
  end
  else
    istream.CopyFrom(input, input.Size);

  try
    // eltaroljuk a meretet eloszor
    size:= istream.Size;
    b.EncryptCBC(@size,@buf);
    ostream.Write(buf, 8);

    istream.Position:= 0;
    read:= 0;
    while read < istream.Size do
    begin
      aktread:= istream.Read(buf, BUFSIZE);
      iteraciok:= (aktread div 8);
      if aktread mod 8 <> 0 then
      begin
        for i:= aktread mod 8+1 to 8 do
          buf[iteraciok*8+i]:= #0;
        inc(iteraciok);
      end;
      for i:= 1 to iteraciok do
        b.EncryptCBC(@buf[(i-1)*8+1],@buf[(i-1)*8+1]);
      ostream.Write(buf, iteraciok*8);

      inc(read, aktread);
    end;
  finally
    istream.Free;

    b.Free;

  end;
end;

procedure EncryptStreamToFile(input: TStream; output: AnsiString; const Key: array of byte; compressstream: Boolean = False);
var
    ostream: TFileStream;
begin
  ostream:= TFileStream.Create(output, fmCreate or fmOpenWrite);
  try
    EncryptStreamToStream(input, ostream, Key, compressstream);
  finally
    ostream.Free;
  end;
end;
procedure EncryptFileToFile(input: AnsiString; output: AnsiString; const Key: array of byte; compressstream: Boolean = False);
var x: TFileStream;
begin
  if input = output then
  begin
    RenameFile(input,input+'.tmp');
    input:= input+'.tmp';
  end;

  x:= TFileStream.Create(input, fmOpenRead);
  try
    EncryptStreamToFile(x, output, Key, compressstream);
  finally
    x.Free;
  end;
end;
procedure DecryptStreamToStream(istream, output: TStream; const Key: array of byte; decompressstream: Boolean = False);
var read, aktread: Integer;
    buf: array[1..BUFSIZE] of AnsiChar;
    ostream: TMemoryStream;
    zl: TDecompressionStream;
    i, iteraciok: Integer;
    size: Int64;
    b: TslBlowfish;
begin
  b:= TslBlowfish.Create(Key);

  ostream:= TMemoryStream.Create;

  try
    // kiolvassuk a meretet eloszor
    // eltaroljuk a meretet eloszor
    istream.Read(size, 8);
    b.DecryptCBC(@size,@size);

    read:= 8;
    while read < istream.Size do
    begin
      aktread:= istream.Read(buf, BUFSIZE);
      iteraciok:= (aktread div 8);
      if aktread mod 8 <> 0 then inc(iteraciok);
      for i:= 1 to iteraciok do
        b.DecryptCBC(@buf[(i-1)*8+1],@buf[(i-1)*8+1]);
      inc(read, aktread);

      if read - 8 > size then
        ostream.Write(buf, ((aktread-1) div 8)*8+(size mod 8))
      else
        ostream.Write(buf, aktread);

    end;

    ostream.Position:= 0;
    if decompressstream then
    begin
      zl:= TDecompressionStream.Create(ostream);
      aktread:= 1;
      while aktread <> 0 do
      begin
       aktread:= zl.Read(buf, BUFSIZE);
       if aktread > 0 then
         output.Write(buf, aktread);
      end;
      zl.Free;
    end
    else
      output.CopyFrom(ostream, ostream.Size);

    output.Position:= 0;
  finally
    ostream.Free;
    b.Free;
  end;
end;
procedure DecryptFileToStream(input: AnsiString; output: TStream; const Key: array of byte; decompressstream: Boolean = False);
var istream: TFileStream;
begin
  istream:= TFileStream.Create(input, fmOpenRead);
  try
    DecryptStreamToStream(istream, output, Key, decompressstream);
  finally
    istream.Free;
  end;
end;
procedure DecryptFileToFile(input: AnsiString; output: AnsiString; const Key: array of byte; decompressstream: Boolean = False);
var x: TFileStream;
begin
  if input = output then
  begin
    RenameFile(input,input+'.tmp');
    input:= input+'.tmp';
  end;

  x:= TFileStream.Create(output, fmCreate or fmOpenWrite);
  try
    DecryptFileToStream(input, x, Key, decompressstream);
  finally
    x.Free;
  end;
end;



{ TslBlowfish }

constructor TslBlowfish.Create(const key, iv: array of byte);
begin
  BlowfishInit(KeyData, @Key,Length(Key),@IV);
end;

procedure TslBlowfish.Reset;
begin
  BlowfishBurn(KeyData);
end;

constructor TslBlowfish.Create(const key: array of byte);
begin
  Create(key, defaultIV);
end;

procedure TslBlowfish.DecryptCBC(InData, OutData: pointer);
begin
  BlowfishDecryptCBC(KeyData, InData, OutData);
end;

procedure TslBlowfish.DecryptECB(InData, OutData: pointer);
begin
  BlowfishDecryptECB(KeyData, InData, OutData);
end;

procedure TslBlowfish.EncryptCBC(InData, OutData: pointer);
begin
  BlowfishEncryptCBC(KeyData, InData, OutData);
end;

procedure TslBlowfish.EncryptECB(InData, OutData: pointer);
begin
  BlowfishEncryptECB(KeyData, InData, OutData);
end;


end.
