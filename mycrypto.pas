unit mycrypto;

interface

uses Classes, slmd5;

function GenPem(certpath: string; keylen: Integer; commonname: string): Boolean;
procedure MycryptoStart(pp: TslMD5Data);
procedure MycryptoStop;
function DecryptUDP(s: string): string;
function EncryptUDP(s: string): string;
procedure  MyCryptoInit;
procedure MyCryptoUnInit;

implementation

uses SysUtils, delphiblowfish, slssl, helper, configunit, debugunit,
  Math, mystrings;

const section = 'crypto';
      UDP_MIN_PADDING = 8;
      UDP_MAX_PADDING = 32;
      MAX_UDP_PACKET = 16384;

var KeyData: TBlowfishData;


function GenPem(certpath: string; keylen: Integer; commonname: string): Boolean;
var b, r, xn, x, xr, xne, evp: Pointer;
begin
  Result:= False;

  b:= slBIO_new_file(PChar(certpath), 'w');
  if( b = nil) then exit;

  r:= slRSA_generate_key(keylen, 65537, nil, nil);
  if( r = nil) then
  begin
    slBIO_free(b);
    exit;
  end;

  xr := slX509_REQ_new();
  if( xr = nil) then
  begin
    slRSA_free(r);
    slBIO_free(b);
    exit;
  end;

  xn := slX509_NAME_new();
  if(xn = nil) then
  begin
    slBIO_free(b);
    slRSA_free(r);
    slX509_REQ_free(xr);
    exit;
  end;

  xne := slX509_NAME_ENTRY_create_by_txt(nil, 'CN', OPENSSL_V_ASN1_APP_CHOOSE, PChar(commonname), Length(commonname));
  if (xne = nil) then
  begin
    slBIO_free(b);
    slRSA_free(r);
    slX509_REQ_free(xr);
    slX509_NAME_free(xn);
    exit;
  end;

  slX509_NAME_add_entry(xn, xne, 0, 0);
  slX509_REQ_set_subject_name(xr, xn);

  evp := slEVP_PKEY_new();
  if(evp = nil) then
  begin
    slBIO_free(b);
    slRSA_free(r);
    slX509_REQ_free(xr);
    slX509_NAME_free(xn);
    exit;
  end;


  if (0 = slEVP_PKEY_set1_RSA(evp,r)) then
  begin
      slBIO_free(b);
      slRSA_free(r);
      slX509_REQ_free(xr);
      slX509_NAME_free(xn);
      slEVP_PKEY_free(evp);

      exit;
  end;

  if (0 = slX509_REQ_set_pubkey(xr, evp)) then
  begin
      slBIO_free(b);
      slRSA_free(r);
      slX509_REQ_free(xr);
      slX509_NAME_free(xn);
      slEVP_PKEY_free(evp);
      exit;
  end;

  if (0 = slX509_REQ_sign(xr, evp, slEVP_sha256())) then
  begin
    slX509_REQ_free(xr);
    slX509_NAME_free(xn);
    slBIO_free(b);
    slRSA_free(r);
    slEVP_PKEY_free(evp);
    exit;
  end;

  // na mar nincs sok hatra
  x := slX509_REQ_to_X509(xr, 3000, evp);
  if (x = nil) then
  begin
    slBIO_free(b);
    slRSA_free(r);
    slX509_REQ_free(xr);
    slX509_NAME_free(xn);
    slEVP_PKEY_free(evp);
    exit;
  end;

  
  slPEM_write_bio_RSAPrivateKey(b, r, nil, nil, 0, nil, nil);
  slPEM_write_bio_X509(b, x);



  slX509_free(x);
  slX509_REQ_free(xr);
  slX509_NAME_free(xn);
  slBIO_free(b);
  slRSA_free(r);
  slEVP_PKEY_free(evp);

  Result:= True;
end;

procedure MycryptoStart(pp: TslMD5Data);
const IV: array[0..7] of Byte = (0,0,0,0,0,0,0,0);
var cert: string;
begin
  cert:= config.ReadString(section, 'certificate', 'slftp.pem');
  if not FileExists(cert) then
  begin
    Debug(dpError, section, 'Certificate not found, generating new one');
    GenPem(cert, config.ReadInteger(section, 'keylen', 2048), MyGetusername());
  end;


  BlowfishInit(KeyData, @pp, SizeOf(pp), @iv);
end;

procedure MycryptoStop;
begin
//nothing to do here
  BlowfishBurn(KeyData);
end;


function DecryptUDP(s: string): string;
var p: Byte;
    l: Integer;
begin
  Result:= '';

  l:= length(s);
  if l > MAX_UDP_PACKET then exit;

  BlowfishReset(KeyData);
  BlowfishDecryptCFB(KeyData, @s[1], @s[1], l);
  p:= Ord(s[1]);
  if (p >= UDP_MIN_PADDING) and (p <= UDP_MAX_PADDING) then
    Result:= Copy(s, p+1, l-p-1);
//  writeln(result);
end;


function EncryptUDP(s: string): string;
var p: Byte;
    block: array[0..MAX_UDP_PACKET-1] of Char;
begin
  Result:= '';

  if Length(s) + UDP_MAX_PADDING > MAX_UDP_PACKET then exit;

  p:= Byte(RandomRange(UDP_MIN_PADDING, UDP_MAX_PADDING));
  block[0]:= Char(p);
  Move(s[1], block[p], length(s));

  BlowfishReset(KeyData);
  BlowfishEncryptCFB(KeyData, @block, @block, p+1+length(s));
  SetLength(Result, p+length(s)+1);
  Move(block[0], Result[1], p+1+length(s));
//  writeln(inttostr(length(result)));
end;

procedure MyCryptoInit;
begin
  Randomize;
end;

procedure MyCryptoUnInit;
begin
// nope right now
end;

end.
