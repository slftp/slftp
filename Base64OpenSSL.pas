unit Base64OpenSSL;

interface

{ Creates a base64 encoded string from @link(aInput)
  @param(aInput String which should be used)
  @param(aOutput Base64 encoded String)
  @returns(Length of base64 encoded string, 0, -1 and -101 means it failed) }
function DoBase64Encode(const aInput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; out aOutput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): integer;

{ Creates a base64 decoded string from @link(aInput)
  @param(aInput String which should be used)
  @param(aOutput Base64 decoded String)
  @returns(Length of base64 decoded string, 0, -1 and -101 means it failed) }
function DoBase64Decode(const aInput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; out aOutput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): integer;

implementation

uses
  IdSSLOpenSSLHeaders;

type
  {
  @value(Base64Encode do a base64 encode)
  @value(Base64Decode do a base64 encode)
  }
  TEncodingMethod = (Base64Encode, Base64Decode);

{ Creates a base64 string from @link(aInput) with given @link(aInputLen)
  @param(aInput String which should be used)
  @param(aInputLen Length of input String)
  @param(aOutput Output of encoded/decoded String)
  @param(aEncodingMethod Use @link(TEncodingMethod) to do encode/decode of base64)
  @returns(Length of output String, 0 means it hasn't written any bytes. If return result is unexpected, check BIO_write (values BIO_*: 0, -1, -2) }
function DoBase64(const aInput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; const aInputLen: integer; out aOutput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; const aEncodingMethod: TEncodingMethod): integer;
var
  fWrittenBytes: integer;
  fB64, fBIO: PBIO;
begin
  Result := 0; // default number for failure as it sets string length correctly

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
            fWrittenBytes := BIO_write(fB64, Pointer(@aInput[1]), aInputLen);

            BIO_flush(fB64);

            if (fWrittenBytes > 0) then
            begin
              Result := BIO_read(fBIO, Pointer(@aOutput[1]), 256);
            end;
          end;

        Base64Decode:
          begin
            fWrittenBytes := BIO_write(fBIO, Pointer(@aInput[1]), aInputLen);

            BIO_flush(fBIO);

            if (fWrittenBytes > 0) then
            begin
              Result := BIO_read(fB64, Pointer(@aOutput[1]), 256);
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

function DoBase64Encode(const aInput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; out aOutput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): integer;
begin
  Result := DoBase64(aInput, Length(aInput), aOutput, Base64Encode);
end;

function DoBase64Decode(const aInput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}; out aOutput: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF}): integer;
begin
  Result := DoBase64(aInput, Length(aInput), aOutput, Base64Decode);
end;

end.
