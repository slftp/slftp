{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library (Freeware)                                              }
{ Class TMPEGaudio - for manipulating with MPEG audio file information        }
{                                                                             }
{ Uses:                                                                       }
{   - Class TID3v1                                                            }
{   - Class TID3v2                                                            }
{                                                                             }
{ Copyright (c) 2001 by Jurgen Faul                                           }
{ E-mail: jfaul@gmx.de                                                        }
{ http://jfaul.de/atl                                                         }
{                                                                             }
{ Version 1.0 (31 August 2001)                                                }
{   - Support for MPEG audio (versions 1, 2, 2.5, layers I, II, III)          }
{   - Support for Xing & FhG VBR                                              }
{   - Ability to guess audio encoder (Xing, FhG, LAME, Blade, GoGo, Shine)    }
{   - Class TID3v1: reading & writing support for ID3v1.x tags                }
{   - Class TID3v2: reading support for ID3v2.3.x tags                        }
{                                                                             }
{ *************************************************************************** }

unit mpeginfo;

interface

uses SysUtils, Classes;

type
  TMpegInfo = record
    mpeg_present: Boolean;
    mpeg_version: string;
    mpeg_layer: string;
    mpeg_stereomode: string;
    mpeg_vbr: Boolean;
    mpeg_bitrate: Integer;
    mpeg_frequency: string;
    mpeg_size: Longword;
    mpeg_duration: LongWord;
    mpeg_frames: Longword;
  end;

function Mpeg_Check(var buffer: array of Byte; filesize: Integer; var dest_info: TMpegInfo): Integer; overload;
function Mpeg_Check(filename: string; var dest_info: TMpegInfo): Integer; overload;
function MPEGInfoToString(const m: TMpegInfo): string;

const
  MPEG_BUFFER = 16384*2;



const
  { Table for bit rates }
  MPEG_BIT_RATE: array [0..3, 0..3, 0..15] of Word =
    (
    { For MPEG 2.5 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0)),
    { Reserved }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
    { For MPEG 2 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0)),
    { For MPEG 1 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384, 0),
    (0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 0))
    );

  { Sample rate codes }
  MPEG_SAMPLE_RATE_LEVEL_3 = 0;                                     { Level 3 }
  MPEG_SAMPLE_RATE_LEVEL_2 = 1;                                     { Level 2 }
  MPEG_SAMPLE_RATE_LEVEL_1 = 2;                                     { Level 1 }
  MPEG_SAMPLE_RATE_UNKNOWN = 3;                               { Unknown value }

  { Table for sample rates }
  MPEG_SAMPLE_RATE: array [0..3, 0..3] of Word =
    (
    (11025, 12000, 8000, 0),                                   { For MPEG 2.5 }
    (0, 0, 0, 0),                                                  { Reserved }
    (22050, 24000, 16000, 0),                                    { For MPEG 2 }
    (44100, 48000, 32000, 0)                                     { For MPEG 1 }
    );

  { VBR header ID for Xing/FhG }
  VBR_ID_XING = 'Xing';                                         { Xing VBR ID }
  VBR_ID_FHG = 'VBRI';                                           { FhG VBR ID }

  { MPEG version codes }
  MPEG_VERSION_2_5 = 0;                                            { MPEG 2.5 }
  MPEG_VERSION_UNKNOWN = 1;                                 { Unknown version }
  MPEG_VERSION_2 = 2;                                                { MPEG 2 }
  MPEG_VERSION_1 = 3;                                                { MPEG 1 }

  { MPEG version names }
  MPEG_VERSION: array [0..3] of string =
    ('2.5', '?', '2', '1');

  { MPEG layer codes }
  MPEG_LAYER_UNKNOWN = 0;                                     { Unknown layer }
  MPEG_LAYER_III = 1;                                             { Layer III }
  MPEG_LAYER_II = 2;                                               { Layer II }
  MPEG_LAYER_I = 3;                                                 { Layer I }

  { MPEG layer names }
  MPEG_LAYER: array [0..3] of string =
    ('?', '3', '2', '1');

  { Channel mode codes }
  MPEG_CM_STEREO = 0;                                                { Stereo }
  MPEG_CM_JOINT_STEREO = 1;                                    { Joint Stereo }
  MPEG_CM_DUAL_CHANNEL = 2;                                    { Dual Channel }
  MPEG_CM_MONO = 3;                                                    { Mono }
  MPEG_CM_UNKNOWN = 4;                                         { Unknown mode }

  { Channel mode names }
  MPEG_CM_MODE: array [0..4] of string =
    ('Stereo', 'Joint', 'Dual', 'Single', '?');

  { Extension mode codes (for Joint Stereo) }
  MPEG_CM_EXTENSION_OFF = 0;                        { IS and MS modes set off }
  MPEG_CM_EXTENSION_IS = 1;                             { Only IS mode set on }
  MPEG_CM_EXTENSION_MS = 2;                             { Only MS mode set on }
  MPEG_CM_EXTENSION_ON = 3;                          { IS and MS modes set on }
  MPEG_CM_EXTENSION_UNKNOWN = 4;                     { Unknown extension mode }

  { Emphasis mode codes }
  MPEG_EMPHASIS_NONE = 0;                                              { None }
  MPEG_EMPHASIS_5015 = 1;                                          { 50/15 ms }
  MPEG_EMPHASIS_UNKNOWN = 2;                               { Unknown emphasis }
  MPEG_EMPHASIS_CCIT = 3;                                         { CCIT J.17 }

  { Emphasis names }
  MPEG_EMPHASIS: array [0..3] of string =
    ('None', '50/15 ms', 'Unknown', 'CCIT J.17');

  { Encoder codes }
  MPEG_ENCODER_UNKNOWN = 0;                                 { Unknown encoder }
  MPEG_ENCODER_XING = 1;                                               { Xing }
  MPEG_ENCODER_FHG = 2;                                                 { FhG }
  MPEG_ENCODER_LAME = 3;                                               { LAME }
  MPEG_ENCODER_BLADE = 4;                                             { Blade }
  MPEG_ENCODER_GOGO = 5;                                               { GoGo }
  MPEG_ENCODER_SHINE = 6;                                             { Shine }

  { Encoder names }
  MPEG_ENCODER: array [0..6] of string =
    ('Unknown', 'Xing', 'FhG', 'LAME', 'Blade', 'GoGo', 'Shine');

type
  { Xing/FhG VBR header data }
  VBRData = record
    Found: Boolean;                                { True if VBR header found }
    ID: array [1..4] of Char;                   { Header ID: "Xing" or "VBRI" }
    Frames: Integer;                                 { Total number of frames }
    Bytes: Integer;                                   { Total number of bytes }
    Scale: Byte;                                         { VBR scale (1..100) }
    VendorID: array [1..8] of Char;                  { Vendor ID (if present) }
  end;

  { MPEG frame header data}
  FrameData = record
    Found: Boolean;                                     { True if frame found }
    Position: Integer;                           { Frame position in the file }
    Size: Word;                                          { Frame size (bytes) }
    Empty: Boolean;                       { True if no significant frame data }
    Data: array [1..4] of Byte;                 { The whole frame header data }
    VersionID: Byte;                                        { MPEG version ID }
    LayerID: Byte;                                            { MPEG layer ID }
    ProtectionBit: Boolean;                        { True if protected by CRC }
    BitRateID: Word;                                            { Bit rate ID }
    SampleRateID: Word;                                      { Sample rate ID }
    PaddingBit: Boolean;                               { True if frame padded }
    PrivateBit: Boolean;                                  { Extra information }
    ModeID: Byte;                                           { Channel mode ID }
    ModeExtensionID: Byte;             { Mode extension ID (for Joint Stereo) }
    CopyrightBit: Boolean;                        { True if audio copyrighted }
    OriginalBit: Boolean;                            { True if original media }
    EmphasisID: Byte;                                           { Emphasis ID }
  end;

  { Class TMPEGaudio }
  TMPEGaudio = class(TObject)
    private
      { Private declarations }
      FFileLength: Integer;
      FWaveHeader: Boolean;
      FVBR: VBRData;
      FFrame: FrameData;
      fid3v2size: Integer;
      procedure FResetData;
      procedure ReadID3V2Size(var buffer: array of byte);
      function FGetVersion: string;
      function FGetLayer: string;
      function FGetBitRate: Word;
      function FGetSampleRate: Word;
      function FGetChannelMode: string;
      function FGetEmphasis: string;
      function FGetFrames: Integer;
      function FGetSize: Cardinal;
      function FGetDuration: Double;
      function FGetVBREncoderID: Byte;
      function FGetCBREncoderID: Byte;
      function FGetEncoderID: Byte;
      function FGetEncoder: string;
      function FGetValid: Boolean;
    public
      { Public declarations }
      constructor Create;                                     { Create object }
      destructor Destroy; override;                          { Destroy object }
      function ReadFromStream(const s: TStream): Boolean;     { Load data }
      function ReadFromBuffer(var buffer: array of byte; filesize: Integer): Boolean;
      function ReadFromFile(const FileName: string): Boolean;     { Load data }
      property FileLength: Integer read FFileLength;    { File length (bytes) }
      property VBR: VBRData read FVBR;                      { VBR header data }
      property Frame: FrameData read FFrame;              { Frame header data }
      property Version: string read FGetVersion;          { MPEG version name }
      property Layer: string read FGetLayer;                { MPEG layer name }
      property BitRate: Word read FGetBitRate;            { Bit rate (kbit/s) }
      property SampleRate: Word read FGetSampleRate;       { Sample rate (hz) }
      property ChannelMode: string read FGetChannelMode;  { Channel mode name }
      property Emphasis: string read FGetEmphasis;            { Emphasis name }
      property Size: Cardinal read FGetSize;      { Total number of frames }
      property Frames: Integer read FGetFrames;      { Total number of frames }
      property Duration: Double read FGetDuration;      { Song duration (sec) }
      property EncoderID: Byte read FGetEncoderID;       { Guessed encoder ID }
      property Encoder: string read FGetEncoder;       { Guessed encoder name }
      property Valid: Boolean read FGetValid;       { True if MPEG file valid }
  end;

implementation


const
  { Limitation constants }
  MAX_MPEG_FRAME_LENGTH = 1729;                      { Max. MPEG frame length }
  MIN_MPEG_BIT_RATE = 8;                                { Min. bit rate value }
  MAX_MPEG_BIT_RATE = 448;                              { Max. bit rate value }
  MIN_ALLOWED_DURATION = 0.1;                      { Min. song duration value }

  { VBR Vendor ID strings }
  VBR_VENDOR_ID_LAME = 'LAME';                                     { For LAME }
  VBR_VENDOR_ID_GOGO_NEW = 'GOGO';                           { For GoGo (New) }
  VBR_VENDOR_ID_GOGO_OLD = 'MPGE';                           { For GoGo (Old) }

{ ********************* Auxiliary functions & procedures ******************** }

function WaveHeaderPresent(const Index: Integer; Data: array of Byte): Boolean;
begin
  { Check for WAV header }
  Result :=
    (Chr(Data[Index + 8]) = 'W') and
    (Chr(Data[Index + 9]) = 'A') and
    (Chr(Data[Index + 10]) = 'V') and
    (Chr(Data[Index + 11]) = 'E');
end;

{ --------------------------------------------------------------------------- }

function IsFrameHeader(const HeaderData: array of Byte): Boolean;
begin
  { Check for valid frame header }
  if ((HeaderData[0] and $FF) <> $FF) or
    ((HeaderData[1] and $E0) <> $E0) or
    (((HeaderData[1] shr 3) and 3) = 1) or
    (((HeaderData[1] shr 1) and 3) = 0) or
    ((HeaderData[2] and $F0) = $F0) or
    ((HeaderData[2] and $F0) = 0) or
    (((HeaderData[2] shr 2) and 3) = 3) or
    ((HeaderData[3] and 3) = 2) then
    Result := false
  else
    Result := true;
end;

{ --------------------------------------------------------------------------- }

procedure DecodeHeader(const HeaderData: array of Byte; var Frame: FrameData);
begin
  { Decode frame header data }
  Move(HeaderData, Frame.Data, SizeOf(Frame.Data));
  Frame.VersionID := (HeaderData[1] shr 3) and 3;
  Frame.LayerID := (HeaderData[1] shr 1) and 3;
  Frame.ProtectionBit := (HeaderData[1] and 1) <> 1;
  Frame.BitRateID := HeaderData[2] shr 4;
  Frame.SampleRateID := (HeaderData[2] shr 2) and 3;
  Frame.PaddingBit := ((HeaderData[2] shr 1) and 1) = 1;
  Frame.PrivateBit := (HeaderData[2] and 1) = 1;
  Frame.ModeID := (HeaderData[3] shr 6) and 3;
  Frame.ModeExtensionID := (HeaderData[3] shr 4) and 3;
  Frame.CopyrightBit := ((HeaderData[3] shr 3) and 1) = 1;
  Frame.OriginalBit := ((HeaderData[3] shr 2) and 1) = 1;
  Frame.EmphasisID := HeaderData[3] and 3;
end;

{ --------------------------------------------------------------------------- }

function ValidFrameAt(const Index: Word; Data: array of Byte): Boolean;
var
  HeaderData: array [1..4] of Byte;
begin
  { Check for frame at given position }
  HeaderData[1] := Data[Index];
  HeaderData[2] := Data[Index + 1];
  HeaderData[3] := Data[Index + 2];
  HeaderData[4] := Data[Index + 3];
  if IsFrameHeader(HeaderData) then Result := true
  else Result := false;
end;

{ --------------------------------------------------------------------------- }

function GetCoefficient(const Frame: FrameData): Byte;
begin
  { Get frame coefficient }
  if Frame.VersionID = MPEG_VERSION_1 then
    if Frame.LayerID = MPEG_LAYER_I then Result := 48
    else Result := 144
  else
    if Frame.LayerID = 3 then Result := 24
    else Result := 72;
end;

{ --------------------------------------------------------------------------- }

function GetBitRate(const Frame: FrameData): Word;
begin
  { Get bit rate }
  Result := MPEG_BIT_RATE[Frame.VersionID, Frame.LayerID, Frame.BitRateID];
end;

{ --------------------------------------------------------------------------- }

function GetSampleRate(const Frame: FrameData): Word;
begin
  { Get sample rate }
  Result := MPEG_SAMPLE_RATE[Frame.VersionID, Frame.SampleRateID];
end;

{ --------------------------------------------------------------------------- }

function GetPadding(const Frame: FrameData): Byte;
begin
  { Get frame padding }
  if Frame.PaddingBit then
    if Frame.LayerID = MPEG_LAYER_I then Result := 4
    else Result := 1
  else Result := 0;
end;

{ --------------------------------------------------------------------------- }

function GetFrameLength(const Frame: FrameData): Word;
var
  Coefficient, BitRate, SampleRate, Padding: Word;
begin
  { Calculate MPEG frame length }
  Coefficient := GetCoefficient(Frame);
  BitRate := GetBitRate(Frame);
  SampleRate := GetSampleRate(Frame);
  Padding := GetPadding(Frame);
  Result := Trunc(Coefficient * BitRate * 1000 / SampleRate) + Padding;
end;

{ --------------------------------------------------------------------------- }

function FrameIsEmpty(const Index: Word; Data: array of Byte): Boolean;
begin
  { Get true if frame has no significant data }
  Result :=
    (Data[Index] = 0) and
    (Data[Index + 1] = 0) and
    (Data[Index + 2] = 0) and
    (Data[Index + 3] = 0) and
    (Data[Index + 4] = 0) and
    (Data[Index + 5] = 0);
end;

{ --------------------------------------------------------------------------- }

function GetXingInfo(const Index: Word; Data: array of Byte): VBRData;
begin
  { Extract Xing VBR info at given position }
  FillChar(Result, SizeOf(Result), 0);
  Result.Found := true;
  Result.ID := VBR_ID_XING;
  Result.Frames :=
    Data[Index + 8] * $1000000 +
    Data[Index + 9] * $10000 +
    Data[Index + 10] * $100 +
    Data[Index + 11];
  Result.Bytes :=
    Data[Index + 12] * $1000000 +
    Data[Index + 13] * $10000 +
    Data[Index + 14] * $100 +
    Data[Index + 15];
  Result.Scale := Data[Index + 119];
  { Encoder ID can be not present }
  Result.VendorID[1] := Chr(Data[Index + 120]);
  Result.VendorID[2] := Chr(Data[Index + 121]);
  Result.VendorID[3] := Chr(Data[Index + 122]);
  Result.VendorID[4] := Chr(Data[Index + 123]);
  Result.VendorID[5] := Chr(Data[Index + 124]);
  Result.VendorID[6] := Chr(Data[Index + 125]);
  Result.VendorID[7] := Chr(Data[Index + 126]);
  Result.VendorID[8] := Chr(Data[Index + 127]);
end;

{ --------------------------------------------------------------------------- }

function GetFhGInfo(const Index: Word; Data: array of Byte): VBRData;
begin
  { Extract FhG VBR info at given position }
  FillChar(Result, SizeOf(Result), 0);
  Result.Found := true;
  Result.ID := VBR_ID_FHG;
  Result.Scale := Data[Index + 9];
  Result.Bytes :=
    Data[Index + 10] * $1000000 +
    Data[Index + 11] * $10000 +
    Data[Index + 12] * $100 +
    Data[Index + 13];
  Result.Frames :=
    Data[Index + 14] * $1000000 +
    Data[Index + 15] * $10000 +
    Data[Index + 16] * $100 +
    Data[Index + 17];
end;

{ --------------------------------------------------------------------------- }

function FindVBR(const Index: Word; Data: array of Byte): VBRData;
begin
  { Check for VBR header at given position }
  FillChar(Result, SizeOf(Result), 0);
  if Chr(Data[Index]) +
    Chr(Data[Index + 1]) +
    Chr(Data[Index + 2]) +
    Chr(Data[Index + 3]) = VBR_ID_XING then Result := GetXingInfo(Index, Data);
  if Chr(Data[Index]) +
    Chr(Data[Index + 1]) +
    Chr(Data[Index + 2]) +
    Chr(Data[Index + 3]) = VBR_ID_FHG then Result := GetFhGInfo(Index, Data);
end;

{ --------------------------------------------------------------------------- }

function GetVBRDeviation(const Frame: FrameData): Byte;
begin
  { Calculate VBR deviation }
  if Frame.VersionID = MPEG_VERSION_1 then
    if Frame.ModeID <> MPEG_CM_MONO then Result := 36
    else Result := 21
  else
    if Frame.ModeID <> MPEG_CM_MONO then Result := 21
    else Result := 13;
end;

{ --------------------------------------------------------------------------- }

function FindFrame(var Data: array of Byte; var VBR: VBRData; offset: Integer): FrameData;
var
  HeaderData: array [1..4] of Byte;
  Iterator: Integer;
begin
  { Search for valid frame }
  FillChar(Result, SizeOf(Result), 0);


  HeaderData[1] := Data[offset];
  HeaderData[2] := Data[offset+1];
  HeaderData[3] := Data[offset+2];
  HeaderData[4] := Data[offset+3];

  for Iterator := offset to SizeOf(Data) - MAX_MPEG_FRAME_LENGTH do
  begin
    { Decode data if frame header found }
    if IsFrameHeader(HeaderData) then
    begin
      DecodeHeader(HeaderData, Result);
      { Check for next frame and try to find VBR header }
      if ValidFrameAt(Iterator + GetFrameLength(Result), Data) then
      begin
        Result.Found := true;
        Result.Position := Iterator;
        Result.Size := GetFrameLength(Result);
        Result.Empty := FrameIsEmpty(Iterator + SizeOf(HeaderData), Data);
        VBR := FindVBR(Iterator + GetVBRDeviation(Result), Data);
        break;
      end;
    end;
    { Prepare next data block }
    HeaderData[1] := HeaderData[2];
    HeaderData[2] := HeaderData[3];
    HeaderData[3] := HeaderData[4];
    HeaderData[4] := Data[Iterator + SizeOf(HeaderData)];
  end;
end;

{ ********************** Private functions & procedures ********************* }

procedure TMPEGaudio.FResetData;
begin
  { Reset all variables }
  FFileLength := 0;
  FillChar(FVBR, SizeOf(FVBR), 0);
  FillChar(FFrame, SizeOf(FFrame), 0);
  FFrame.VersionID := MPEG_VERSION_UNKNOWN;
  FFrame.SampleRateID := MPEG_SAMPLE_RATE_UNKNOWN;
  FFrame.ModeID := MPEG_CM_UNKNOWN;
  FFrame.ModeExtensionID := MPEG_CM_EXTENSION_UNKNOWN;
  FFrame.EmphasisID := MPEG_EMPHASIS_UNKNOWN;
  fid3v2size:= 0;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetVersion: string;
begin
  { Get MPEG version name }
  Result := MPEG_VERSION[FFrame.VersionID];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetLayer: string;
begin
  { Get MPEG layer name }
  Result := MPEG_LAYER[FFrame.LayerID];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetBitRate: Word;
begin
  { Get bit rate, calculate average bit rate if VBR header found }
  if (FVBR.Found) and (FVBR.Frames > 0) then
    Result := Round((FVBR.Bytes / FVBR.Frames - GetPadding(FFrame)) *
      GetSampleRate(FFrame) / GetCoefficient(FFrame) / 1000)
  else
    Result := GetBitRate(FFrame);
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetSampleRate: Word;
begin
  { Get sample rate }
  Result := GetSampleRate(FFrame);
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetChannelMode: string;
begin
  { Get channel mode name }
  Result := MPEG_CM_MODE[FFrame.ModeID];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetEmphasis: string;
begin
  { Get emphasis name }
  Result := MPEG_EMPHASIS[FFrame.EmphasisID];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetFrames: Integer;
begin
  { Get total number of frames, calculate if VBR header not found }
  if FVBR.Found then
    Result := FVBR.Frames
  else
    Result := (FFileLength - fid3v2Size - FFrame.Position) div
      GetFrameLength(FFrame);
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetDuration: Double;
begin
  { Calculate song duration }
  if FFrame.Found then
    if (FVBR.Found) and (FVBR.Frames > 0) then
      Result := FVBR.Frames * GetCoefficient(FFrame) * 8 /
        GetSampleRate(FFrame)
    else
      Result := (FFileLength - fID3v2Size - FFrame.Position) * 8 /
        GetBitRate(FFrame) / 1000
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetVBREncoderID: Byte;
begin
  { Guess VBR encoder and get ID }
  Result := 0;
  if Copy(FVBR.VendorID, 1, 4) = VBR_VENDOR_ID_LAME then
    Result := MPEG_ENCODER_LAME;
  if Copy(FVBR.VendorID, 1, 4) = VBR_VENDOR_ID_GOGO_NEW then
    Result := MPEG_ENCODER_GOGO;
  if Copy(FVBR.VendorID, 1, 4) = VBR_VENDOR_ID_GOGO_OLD then
    Result := MPEG_ENCODER_GOGO;
  if (FVBR.ID = VBR_ID_XING) and
    (Copy(FVBR.VendorID, 1, 4) <> VBR_VENDOR_ID_LAME) and
    (Copy(FVBR.VendorID, 1, 4) <> VBR_VENDOR_ID_GOGO_NEW) and
    (Copy(FVBR.VendorID, 1, 4) <> VBR_VENDOR_ID_GOGO_OLD) then
    Result := MPEG_ENCODER_XING;
  if FVBR.ID = VBR_ID_FHG then
    Result := MPEG_ENCODER_FHG;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetCBREncoderID: Byte;
begin
  { Guess CBR encoder and get ID }
  Result := 0;
  if (FFrame.OriginalBit) and
    (FFrame.ProtectionBit) then
    Result := MPEG_ENCODER_LAME;
  if (FFrame.ModeID = MPEG_CM_JOINT_STEREO) and
    (not FFrame.CopyrightBit) and
    (not FFrame.OriginalBit) then
    Result := MPEG_ENCODER_FHG;
  if (GetBitRate(FFrame) <= 112) and
    (FFrame.ModeID = MPEG_CM_STEREO) then
    Result := MPEG_ENCODER_BLADE;
  if (FFrame.CopyrightBit) and
    (FFrame.OriginalBit) and
    (not FFrame.ProtectionBit) then
    Result := MPEG_ENCODER_XING;
  if (FFrame.Empty) and
    (FFrame.OriginalBit) then
    Result := MPEG_ENCODER_XING;
  if (FWaveHeader) then
    Result := MPEG_ENCODER_FHG;
  if (FFrame.ModeID = MPEG_CM_DUAL_CHANNEL) and
    (FFrame.ProtectionBit) then
    Result := MPEG_ENCODER_SHINE;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetEncoderID: Byte;
begin
  { Get guessed encoder ID }
  if FFrame.Found then
    if FVBR.Found then Result := FGetVBREncoderID
    else Result := FGetCBREncoderID
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetEncoder: string;
begin
  { Get guessed encoder name }
  Result := MPEG_ENCODER[FGetEncoderID];
  if (FVBR.Found) and
    (FGetEncoderID = MPEG_ENCODER_LAME) and
    (FVBR.VendorID[5] in ['0'..'9']) and
    (FVBR.VendorID[6] = '.') and
    (FVBR.VendorID[7] in ['0'..'9']) and
    (FVBR.VendorID[8] in ['0'..'9']) then
    Result :=
      Result + #32 +
      FVBR.VendorID[5] +
      FVBR.VendorID[6] +
      FVBR.VendorID[7] +
      FVBR.VendorID[8];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetValid: Boolean;
begin
  { Check for right MPEG file data }
  Result :=
    (FFrame.Found) and
    (FGetBitRate >= MIN_MPEG_BIT_RATE) and
    (FGetBitRate <= MAX_MPEG_BIT_RATE) and
    (FGetDuration >= MIN_ALLOWED_DURATION);
end;

{ ********************** Public functions & procedures ********************** }

constructor TMPEGaudio.Create;
begin
  { Object constructor }
  inherited;
  FResetData;
end;

{ --------------------------------------------------------------------------- }

destructor TMPEGaudio.Destroy;
begin
  { Object destructor }
  inherited;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.ReadFromFile(const FileName: string): Boolean;
var fs: TFileStream;
begin
  fs:= TFileStream.Create(filename, fmOpenRead);
  try
    Result:= ReadFromStream(fs);
  finally
    fs.Free;
  end;
end;

function TMPEGaudio.ReadFromStream(const s: TStream): Boolean;
var buffer: array[1..MPEG_BUFFER] of Byte;
begin
  s.Read(buffer, Sizeof(buffer));
  Result:= ReadFromBuffer(buffer, s.Size);
end;

function SyncSafeIntToNormal(var buffer: array of byte): Cardinal;
var re, b1, b2, b3, b4: Cardinal;
begin
//memcpy(&re, buffer, 4);  ez nem jo endian miatt */
	re := (buffer[0] shl 24) or (buffer[1] shl 16) or (buffer[2] shl 8) or (buffer[3] shl 0);

	//most meg at kell konvertalni.*/
	b1 := (re and $FF000000) shr 3;
	b2 := (re and $00FF0000) shr 2;
	b3 := (re and $0000FF00) shr 1;
	b4 := (re and $000000FF) shr 0;

	Result:= b1 or b2 or b3 or b4;
end;

procedure TMpegAudio.ReadID3V2Size(var buffer: array of byte);
var offset: Integer;
begin
  offset:= 0;
  // ID3
	if((buffer[offset+0] = $49)and(buffer[offset+1] = $44) and (buffer[offset+2] = $33)) then
	begin
		fid3v2size := SyncSafeIntToNormal(buffer[offset+6]);

		if (buffer[offset+5] and 16 <> 0) then // if footer is present */
			inc(fid3v2size, 20)
		else
			inc(fid3v2size, 10);

    exit;
	end;
end;

function TMPEGaudio.ReadFromBuffer(var buffer: array of byte; filesize: Integer): Boolean;
var
  offset: Integer;
begin
  Result := false;
  FResetData;
  { At first search for tags, then search for a MPEG frame and VBR data }

  ReadID3V2Size(buffer);

    try
      { Open file, read first block of data and search for a frame }
      FFileLength := filesize;
      offset:= fID3v2Size;
      if offset + MAX_MPEG_FRAME_LENGTH >= MPEG_BUFFER then
      begin
        fresetdata;
        exit;
      end;

      FWaveHeader := WaveHeaderPresent(offset, buffer);
      FFrame := FindFrame(buffer, FVBR, offset);

      { Try to search in the middle if no frame at the beginning found }
      if (not FFrame.Found) and (offset + MAX_MPEG_FRAME_LENGTH < MPEG_BUFFER) then
      begin
        offset:= (MPEG_BUFFER - fID3v2Size) div 2;
        FFrame := FindFrame(buffer, FVBR, offset);
      end;

      Result := true;
    except
    end;
  if not FFrame.Found then FResetData;
end;




function Mpeg_Check(var buffer: array of Byte; filesize: Integer; var dest_info: TMpegInfo): Integer; overload;
var m: TMpegAudio;
begin
  Result:= 0;

  dest_info.mpeg_present:= False;
  dest_info.mpeg_version:= '';
  dest_info.mpeg_layer:= '';
  dest_info.mpeg_stereomode:= '';
  dest_info.mpeg_vbr:= False;
  dest_info.mpeg_bitrate:= 0;
  dest_info.mpeg_frequency:= '';
  dest_info.mpeg_size:= 0;
  dest_info.mpeg_duration:= 0;
  dest_info.mpeg_frames:= 0;

  m:= TMpegAudio.Create();
  try
    m.ReadFromBuffer(buffer, filesize);

    dest_info.mpeg_present:= m.Valid;
    if dest_info.mpeg_present then
    begin
      dest_info.mpeg_version:= m.Version;
      dest_info.mpeg_layer:= m.Layer;
      dest_info.mpeg_stereomode:= m.ChannelMode;
      dest_info.mpeg_vbr:= m.VBR.Found;
      dest_info.mpeg_bitrate:= m.BitRate;
//      dest_info.mpeg_frequency:= Format('%d.%d', [ m.SampleRate div 1000, (m.SampleRate - (m.SampleRate div 1000)*1000) div 100]);
      DecimalSeparator:= '.';
      dest_info.mpeg_frequency:= Format('%.1f', [ m.SampleRate / 1000]);
      dest_info.mpeg_duration:= Round(m.Duration );
      dest_info.mpeg_frames:= m.Frames;
      dest_info.mpeg_size:= m.size;
    end;
  finally
    m.Free;
  end;
end;
function Mpeg_Check(filename: string; var dest_info: TMpegInfo): Integer; overload;
var fs: TFileStream;
    buffer: array[0..MPEG_BUFFER-1] of Byte;
begin
  fs:= TFileStream.Create(filename, fmOpenRead);
  try
    fs.Read(buffer, MPEG_BUFFER);
    Result:= Mpeg_Check(buffer, fs.Size, dest_info);
  finally
    fs.Free;
  end;
end;

function MPEGInfoToString(const m: TMpegInfo): string;
begin
  Result:= '';
  if m.mpeg_present then
    Result:= Format('MPEG%s Layer%s bitrate %d',[m.mpeg_version, m.mpeg_layer, m.mpeg_bitrate]);
end;


function TMPEGaudio.FGetSize: Cardinal;
begin
  Result:= ffilelength - fid3v2size;
end;

end.
