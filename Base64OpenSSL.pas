unit Base64OpenSSL;

interface

uses
  SysUtils;

{ Creates a base64 encoded string from @link(aInput)
  @param(aInput String which should be encoded (does an automatic UTF8 1-byte string conversion))
  @param(aOutput Base64 encoded String)
  @returns(Length of base64 encoded string, 0, -1 and -101 means it failed) }
function DoBase64Encode(const aInput: String; out aOutput: String): integer; overload;

{ Creates a base64 encoded byte array from @link(aInput)
  @param(aInput byte array which should be converted)
  @param(aOutput Base64 encoded bytes)
  @returns(Length of base64 encoded bytes, 0, -1 and -101 means it failed) }
function DoBase64Encode(const aInput: TBytes; out aOutput: TBytes): integer; overload;

{ Creates a base64 decoded string from @link(aInput)
  @param(aInput Base64 string which should be decoded)
  @param(aOutput Base64 decoded String (does an automatic UTF8 string reconversion))
  @returns(Length of base64 decoded string, 0, -1 and -101 means it failed) }
function DoBase64Decode(const aInput: String; out aOutput: String): integer; overload;

{ Creates a base64 decoded byte array from @link(aInput)
  @param(aInput byte array which should be converted)
  @param(aOutput Base64 decoded String)
  @returns(Length of base64 decoded string, 0, -1 and -101 means it failed) }
function DoBase64Decode(const aInput: TBytes; out aOutput: TBytes): integer; overload;

implementation

uses
  IdSSLOpenSSLHeaders;

type
  {
  @value(Base64Encode do a base64 encode)
  @value(Base64Decode do a base64 decode)
  }
  TEncodingMethod = (Base64Encode, Base64Decode);

{ Creates a base64 bytes array from @link(aInput) with given @link(aInputLen)
  @param(aInput Input bytes which should be used)
  @param(aInputLen Length of input)
  @param(aOutput Output of encoded/decoded bytes)
  @param(aEncodingMethod Use @link(TEncodingMethod) to do encode/decode of base64)
  @returns(Length of output bytes, 0 means it hasn't written any bytes. If return result is unexpected, check BIO_write (values BIO_*: 0, -1, -2) }
function DoBase64(const aInput: TBytes; const aInputLen: integer; out aOutput: TBytes; const aEncodingMethod: TEncodingMethod): integer;
var
  fWrittenBytes: integer;
  fB64, fBIO: PBIO;
begin
  Result := 0; // default number for failure as it sets bytes length correctly

  fWrittenBytes := 0;
  SetLength(aOutput, 256);

  fB64 := BIO_new(BIO_f_base64());
  try
    fBIO := BIO_new(BIO_s_mem());
    try
      BIO_set_flags(fB64, BIO_FLAGS_BASE64_NO_NL);
      BIO_push(fB64, fBIO);

      case aEncodingMethod of
        Base64Encode:
          begin
            fWrittenBytes := BIO_write(fB64, Pointer(@aInput[0]), aInputLen);

            BIO_flush(fB64);

            if (fWrittenBytes > 0) then
            begin
              Result := BIO_read(fBIO, Pointer(@aOutput[0]), 256);
            end;
          end;

        Base64Decode:
          begin
            fWrittenBytes := BIO_write(fBIO, Pointer(@aInput[0]), aInputLen);

            BIO_flush(fBIO);

            if (fWrittenBytes > 0) then
            begin
              Result := BIO_read(fB64, Pointer(@aOutput[0]), 256);
            end;
          end;
      end;

      // set length according to written bytes or to 0 if nothing written
      SetLength(aOutput, Result);
    finally
      BIO_free(fBIO);
    end;
  finally
    BIO_free(fB64);
  end;
end;

function DoBase64Encode(const aInput: String; out aOutput: String): integer;
var
  fInBytes, fOutBytes: TBytes;
begin
  {$IFDEF UNICODE}
    fInBytes := TEncoding.UTF8.GetBytes(aInput);
  {$ELSE}
    SetLength(fInBytes, Length(aInput));
    move(aInput[1], fInBytes[0], Length(aInput));
  {$ENDIF}

  Result := DoBase64Encode(fInBytes, fOutBytes);

  {$IFDEF UNICODE}
    aOutput := StringOf(fOutBytes);
  {$ELSE}
    SetLength(aOutput, Length(fOutBytes));
    move(fOutBytes[0], aOutput[1], Length(fOutBytes));
  {$ENDIF}
end;

function DoBase64Encode(const aInput: TBytes; out aOutput: TBytes): integer;
begin
  Result := DoBase64(aInput, Length(aInput), aOutput, Base64Encode);
end;

function DoBase64Decode(const aInput: String; out aOutput: String): integer;
var
  fInBytes, fOutBytes: TBytes;
begin
  // no explicit 1-byte string conversion needed due to base64 alphabet
  fInBytes := BytesOf(aInput);
  Result := DoBase64Decode(fInBytes, fOutBytes);

  {$IFDEF UNICODE}
    aOutput := TEncoding.UTF8.GetString(fOutBytes);
  {$ELSE}
    SetLength(aOutput, Length(fOutBytes));
    move(fOutBytes[0], aOutput[1], Length(fOutBytes));
  {$ENDIF}
end;

function DoBase64Decode(const aInput: TBytes; out aOutput: TBytes): integer;
begin
  Result := DoBase64(aInput, Length(aInput), aOutput, Base64Decode);
end;

end.
