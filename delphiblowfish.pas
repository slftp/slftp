{
***************************************************
* A binary compatible Blowfish implementation     *
* written by Dave Barton (davebarton@bigfoot.com) *
***************************************************
* 64bit block encryption                          *
* Variable size key - up to 448bit                *
***************************************************
}
unit delphiblowfish;

interface
uses
  Sysutils, delphiblowfishtools;

type
  TBlowfishData= record
    InitBlock: array[0..7] of byte;    { initial IV }
    LastBlock: array[0..7] of byte;    { current IV }
    SBoxM: array[0..3,0..255] of DWord;
    PBoxM: array[0..17] of DWord;
  end;

function BlowfishSelfTest: boolean;
  { performs a self test on this implementation }
procedure BlowfishInit(var Data: TBlowfishData; Key: pointer; Len: integer; IV: pointer);
  { initializes the TBlowfishData structure with the key information and IV if applicable }
procedure BlowfishBurn(var Data: TBlowfishData);
  { erases all information about the key }

procedure BlowfishEncryptECB(var Data: TBlowfishData; InData, OutData: pointer);
  { encrypts the data in a 64bit block using the ECB mode }
procedure BlowfishEncryptCBC(var Data: TBlowfishData; InData, OutData: pointer);
  { encrypts the data in a 64bit block using the CBC chaining mode }
procedure BlowfishEncryptOFB(var Data: TBlowfishData; InData, OutData: pointer);
  { encrypts the data in a 64bit block using the OFB chaining mode }
procedure BlowfishEncryptCFB(var Data: TBlowfishData; InData, OutData: pointer; Len: integer);
  { encrypts Len bytes of data using the CFB chaining mode }
procedure BlowfishEncryptOFBC(var Data: TBlowfishData; InData, OutData: pointer; Len: integer);
  { encrypts Len bytes of data using the OFB counter chaining mode }

procedure BlowfishDecryptECB(var Data: TBlowfishData; InData, OutData: pointer);
  { decrypts the data in a 64bit block using the ECB mode }
procedure BlowfishDecryptCBC(var Data: TBlowfishData; InData, OutData: pointer);
  { decrypts the data in a 64bit block using the CBC chaining mode }
procedure BlowfishDecryptOFB(var Data: TBlowfishData; InData, OutData: pointer);
  { decrypts the data in a 64bit block using the OFB chaining mode }
procedure BlowfishDecryptCFB(var Data: TBlowfishData; InData, OutData: pointer; Len: integer);
  { decrypts Len bytes of data using the CFB chaining mode }
procedure BlowfishDecryptOFBC(var Data: TBlowfishData; InData, OutData: pointer; Len: integer);
  { decrypts Len bytes of data using the OFB counter chaining mode }

procedure BlowfishReset(var Data: TBlowfishData);
  { resets the chaining mode information }

{******************************************************************************}
implementation

{$I delphiblowfish.inc}
{$R-}

function BlowfishSelfTest;
const
  Key: array[0..7] of byte= ($01,$23,$45,$67,$89,$AB,$CD,$EF);
  InBlock: array[0..7] of byte= ($11,$11,$11,$11,$11,$11,$11,$11);
  OutBlock: array[0..7] of byte= ($61,$F9,$C3,$80,$22,$81,$B0,$96);
var
  Block: array[0..7] of byte;
  Data: TBlowfishData;
begin
  BlowfishInit(Data,@Key,Sizeof(Key),nil);
  BlowfishEncryptECB(Data,@InBlock,@Block);
  Result:= CompareMem(@Block,@OutBlock,Sizeof(Block));
  BlowfishDecryptECB(Data,@Block,@Block);
  Result:= Result and CompareMem(@Block,@InBlock,Sizeof(Block));
  BlowfishBurn(Data);
end;

procedure BlowfishInit;
var
  i, k: integer;
  A: DWord;
  KeyB: PByteArray;
  Block: array[0..7] of byte;
begin
  if (Len<= 0)  then // or (Len> 56)
    raise Exception.Create('Blowfish: Key must be between 1 and 56 bytes long');

//  if Len > 56 then Len:= 56;


  KeyB:= Key;
  Move(SBox,Data.SBoxM,Sizeof(SBox));
  Move(PBox,Data.PBoxM,Sizeof(PBox));
  with Data do
  begin
    if IV= nil then
    begin
      FillChar(InitBlock,8,0);
      FillChar(LastBlock,8,0);
    end
    else
    begin
      Move(IV^,InitBlock,8);
      Move(IV^,LastBlock,8);
    end;
    k:= 0;
    for i:= 0 to 17 do
    begin
      A:= KeyB[(k+3) mod Len];
      A:= A + (KeyB[(k+2) mod Len] shl 8);
      A:= A + (KeyB[(k+1) mod Len] shl 16);
      A:= A + (KeyB[k] shl 24);
      PBoxM[i]:= PBoxM[i] xor A;
      k:= (k+4) mod Len;
    end;
    FillChar(Block,Sizeof(Block),0);
    for i:= 0 to 8 do
    begin
      BlowfishEncryptECB(Data,@Block,@Block);
      PBoxM[i*2]:= Block[3] + (Block[2] shl 8) + (Block[1] shl 16) + (Block[0] shl 24);
      PBoxM[i*2+1]:= Block[7] + (Block[6] shl 8) + (Block[5] shl 16) + (Block[4] shl 24);
    end;
    for k:= 0 to 3 do
    begin
      for i:= 0 to 127 do
      begin
        BlowfishEncryptECB(Data,@Block,@Block);
        SBoxM[k,i*2]:= Block[3] + (Block[2] shl 8) + (Block[1] shl 16) + (Block[0] shl 24);
        SBoxM[k,i*2+1]:= Block[7] + (Block[6] shl 8) + (Block[5] shl 16) + (Block[4] shl 24);
      end;
    end;
  end;
end;

procedure BlowfishBurn;
begin
  FillChar(Data,Sizeof(Data),0);
end;

function BF_F(Data: TBlowfishData; xL: DWord): DWord;
begin
  Result:= (((Data.SBoxM[0,(xL shr 24) and $FF] + Data.SBoxM[1,(xL shr 16) and $FF]) xor Data.SBoxM[2,(xL shr 8) and $FF]) + Data.SBoxM[3,xL and $FF]);
end;

procedure BFDoRound(Data: TBlowfishData; var xL, xR: DWord; RNum: integer);
begin
  xL:= xL xor BF_F(Data,xR) xor Data.PBoxM[RNum];
end;

procedure BlowfishEncryptECB;
var
  xL, xR: DWord;
  o: PAnsiChar;
begin
  Move(InData^,xL,4);
  o:= PAnsiChar(InData)+4;
  Move(o^,xR,4);
  xL:= (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
  xR:= (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);
  xL:= xL xor Data.PBoxM[0];
  BFDoRound(Data,xR,xL,1);
  BFDoRound(Data,xL,xR,2);
  BFDoRound(Data,xR,xL,3);
  BFDoRound(Data,xL,xR,4);
  BFDoRound(Data,xR,xL,5);
  BFDoRound(Data,xL,xR,6);
  BFDoRound(Data,xR,xL,7);
  BFDoRound(Data,xL,xR,8);
  BFDoRound(Data,xR,xL,9);
  BFDoRound(Data,xL,xR,10);
  BFDoRound(Data,xR,xL,11);
  BFDoRound(Data,xL,xR,12);
  BFDoRound(Data,xR,xL,13);
  BFDoRound(Data,xL,xR,14);
  BFDoRound(Data,xR,xL,15);
  BFDoRound(Data,xL,xR,16);
  xR:= xR xor Data.PBoxM[17];
  xL:= (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
  xR:= (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);
  Move(xR,OutData^,4);
  o:= PAnsiChar(outdata)+4;
  Move(xL,o^,4);
end;

procedure BlowfishDecryptECB;
var
  xL, xR: DWord;
  o: PAnsiChar;
begin
  Move(InData^,xL,4);
  o:= PAnsiChar(InData)+4;
  Move(o^,xR,4);
  xL:= (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
  xR:= (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);
  xL:= xL xor Data.PBoxM[17];
  BFDoRound(Data,xR,xL,16);
  BFDoRound(Data,xL,xR,15);
  BFDoRound(Data,xR,xL,14);
  BFDoRound(Data,xL,xR,13);
  BFDoRound(Data,xR,xL,12);
  BFDoRound(Data,xL,xR,11);
  BFDoRound(Data,xR,xL,10);
  BFDoRound(Data,xL,xR,9);
  BFDoRound(Data,xR,xL,8);
  BFDoRound(Data,xL,xR,7);
  BFDoRound(Data,xR,xL,6);
  BFDoRound(Data,xL,xR,5);
  BFDoRound(Data,xR,xL,4);
  BFDoRound(Data,xL,xR,3);
  BFDoRound(Data,xR,xL,2);
  BFDoRound(Data,xL,xR,1);
  xR:= xR xor Data.PBoxM[0];
  xL:= (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
  xR:= (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);
  Move(xR,OutData^,4);
  o:= PAnsiChar(outdata)+4;
  Move(xL,o^,4);
end;

procedure BlowfishEncryptCBC;
begin
  XorBlock(InData,@Data.LastBlock,OutData,8);
  BlowfishEncryptECB(Data,OutData,OutData);
  Move(OutData^,Data.LastBlock,8);
end;

procedure BlowfishDecryptCBC;
var
  TempBlock: array[0..7] of byte;
begin
  Move(InData^,TempBlock,8);

  BlowfishDecryptECB(Data,InData,OutData);

  XorBlock(OutData,@Data.LastBlock,OutData,8);
  Move(TempBlock,Data.LastBlock,8);
end;

procedure BlowfishEncryptCFB;
var
  i: integer;
  TempBlock: array[0..7] of byte;
begin
  for i:= 0 to Len-1 do
  begin
    BlowfishEncryptECB(Data,@Data.LastBlock,@TempBlock);
    PByteArray(OutData)[i]:= PByteArray(InData)[i] xor TempBlock[0];
    Move(Data.LastBlock[1],Data.LastBlock[0],7);
    Data.LastBlock[7]:= PByteArray(OutData)[i];
  end;
end;

procedure BlowfishDecryptCFB;
var
  i: integer;
  TempBlock: array[0..7] of byte;
  b: byte;
begin
  for i:= 0 to Len-1 do
  begin
    b:= PByteArray(InData)[i];
    BlowfishEncryptECB(Data,@Data.LastBlock,@TempBlock);
    PByteArray(OutData)[i]:= PByteArray(InData)[i] xor TempBlock[0];
    Move(Data.LastBlock[1],Data.LastBlock[0],7);
    Data.LastBlock[7]:= b;
  end;
end;

procedure BlowfishEncryptOFB;
begin
  BlowfishEncryptECB(Data,@Data.LastBlock,@Data.LastBlock);
  XorBlock(@Data.LastBlock,InData,OutData,8);
end;

procedure BlowfishDecryptOFB;
begin
  BlowfishEncryptECB(Data,@Data.LastBlock,@Data.LastBlock);
  XorBlock(@Data.LastBlock,InData,OutData,8);
end;

procedure BlowfishEncryptOFBC;
var
  i: integer;
  TempBlock: array[0..7] of byte;
begin
  for i:= 0 to Len-1 do
  begin
    BlowfishEncryptECB(Data,@Data.LastBlock,@TempBlock);
    PByteArray(OutData)[i]:= PByteArray(InData)[i] xor TempBlock[0];
    IncBlock(@Data.LastBlock,8);
  end;
end;

procedure BlowfishDecryptOFBC;
var
  i: integer;
  TempBlock: array[0..7] of byte;
begin
  for i:= 0 to Len-1 do
  begin
    BlowfishEncryptECB(Data,@Data.LastBlock,@TempBlock);
    PByteArray(OutData)[i]:= PByteArray(InData)[i] xor TempBlock[0];
    IncBlock(@Data.LastBlock,8);
  end;
end;

procedure BlowfishReset;
begin
  Move(Data.InitBlock,Data.LastBlock,8);
end;


end.
