unit Base64OpenSSL;

interface

{ Creates a base64 encoded string from @value(aInput)
  @param(aInput String which should be used)
  @param(aOutput Base64 encoded String)
  @returns(Length of base64 encoded string, 0, -1 and -101 means it failed) }
function DoBase64Encode(const aInput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; out aOutput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): integer;

{ Creates a base64 decoded string from @value(aInput)
  @param(aInput String which should be used)
  @param(aOutput Base64 decoded String)
  @returns(Length of base64 decoded string, 0, -1 and -101 means it failed) }
function DoBase64Decode(const aInput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; out aOutput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): integer;

implementation

uses
  IdSSLOpenSSLHeaders;

{ Creates a base64 string from @value(aInput) with given @value(aInputLen)
  @param(aInput String which should be used)
  @param(aInputLen Length of input String)
  @param(aOutput Output of encoded/decoded String)
  @param(aDoEncode Use @True for base64 encode, @false for base64 decode)
  @returns(Length of output String, -101 means it failed and also 0 or -1 (due to BIO_write)!) }
function DoBase64(const aInput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; const aInputLen: integer; out aOutput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; aDoEncode: boolean): integer;
var
  fWrittenBytes: integer;
  fB64, fBIO: PBIO;
begin
  Result := -101; // default number for failure

  fWrittenBytes := 0;
  SetLength(aOutput, 256);

  fB64 := BIO_new(BIO_f_base64());
  try
    fBIO := BIO_new(BIO_s_mem());
    try
      BIO_set_flags(fB64, BIO_FLAGS_BASE64_NO_NL);
      BIO_push(fB64, fBIO);

      if (aDoEncode) then
      begin
        fWrittenBytes := BIO_write(fB64, Pointer(@aInput[1]), aInputLen);

        BIO_flush(fB64);

        if (fWrittenBytes > 0) then
        begin
          Result := BIO_read(fBIO, Pointer(@aOutput[1]), 256);
          SetLength(aOutput, Result);
        end;
      end
      else
      begin
        fWrittenBytes := BIO_write(fBIO, Pointer(@aInput[1]), aInputLen);

        BIO_flush(fBIO);

        if (fWrittenBytes <> 0) then
        begin
          Result := BIO_read(fB64, Pointer(@aOutput[1]), 256);
        end;
      end;
    finally
      BIO_free(fBIO);
    end;
  finally
    BIO_free(fB64);
  end;
end;

function DoBase64Encode(const aInput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; out aOutput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): integer;
begin
  Result := DoBase64(aInput, Length(aInput), aOutput, True);
end;

function DoBase64Decode(const aInput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; out aOutput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): integer;
begin
  Result := DoBase64(aInput, Length(aInput), aOutput, False);
end;

end.
