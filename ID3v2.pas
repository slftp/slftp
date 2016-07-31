{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library (Freeware)                                              }
{ Class TID3v2 - for manipulating with ID3v2 tags                             }
{                                                                             }
{ Copyright (c) 2001 by Jurgen Faul                                           }
{ E-mail: jfaul@gmx.de                                                        }
{ http://jfaul.de/atl                                                         }
{                                                                             }
{ Version 1.2 (17 October 2001)                                               }
{   - Writing support for ID3v2.3.x tags                                      }
{   - Fixed bug with track number detection                                   }
{   - Fixed bug with tag reading                                              }
{                                                                             }
{ Version 1.1 (31 August 2001)                                                }
{   - Added public procedure ResetData                                        }
{                                                                             }
{ Version 1.0 (14 August 2001)                                                }
{   - Reading support for ID3v2.3.x tags                                      }
{   - Tag info: title, artist, album, track, year, genre, comment             }
{                                                                             }
{ *************************************************************************** }

unit ID3v2;

interface

uses
  Classes, SysUtils;

const
  TAG_VERSION_2_3 = 3;                               { Code for ID3v2.3.x tag }

type
  TID3v2Info = record
      exists: Boolean;
      Version: string;
      Size: Integer;
      Title: string;
      Artist: string;
      Album: string;
      Track: Byte;
      Year: string;
      Genre: string;
      Comment: string;
  end;

  { Class TID3v2 }
  TID3v2 = class(TObject)
    private
      { Private declarations }
      FExists: Boolean;
      FVersionID: Byte;
      FSize: Integer;
      FTitle: string;
      FArtist: string;
      FAlbum: string;
      FTrack: Byte;
      FYear: string;
      FGenre: string;
      FComment: string;
      procedure FSetTitle(const NewTitle: string);
      procedure FSetArtist(const NewArtist: string);
      procedure FSetAlbum(const NewAlbum: string);
      procedure FSetTrack(const NewTrack: Byte);
      procedure FSetYear(const NewYear: string);
      procedure FSetGenre(const NewGenre: string);
      procedure FSetComment(const NewComment: string);
      function GetVersionString: string;
    public
      { Public declarations }
      constructor Create;                                     { Create object }
      procedure ResetData;                                   { Reset all data }
      procedure FillID3v2Info(var info: TID3v2Info);
      function ReadFromStream(s: TStream): Boolean;    { Load tag }
      function ReadFromFile(const FileName: string): Boolean;     { Load tag }
      property Exists: Boolean read FExists;              { True if tag found }
      property VersionID: Byte read FVersionID;                { Version code }
      property Version: string read GetVersionString;                { Version code }
      property Size: Integer read FSize;                     { Total tag size }
      property Title: string read FTitle write FSetTitle;        { Song title }
      property Artist: string read FArtist write FSetArtist;    { Artist name }
      property Album: string read FAlbum write FSetAlbum;       { Album title }
      property Track: Byte read FTrack write FSetTrack;        { Track number }
      property Year: string read FYear write FSetYear;         { Release year }
      property Genre: string read FGenre write FSetGenre;        { Genre name }
      property Comment: string read FComment write FSetComment;     { Comment }
  end;

function ID3v2InfoToString( const i: TID3v2Info): string;  

implementation

const
  { ID3v2 tag ID }
  ID3V2_ID = 'ID3';

  { Max. number of supported tag frames }
  ID3V2_FRAME_COUNT = 7;

  { Names of supported tag frames }
  ID3V2_FRAME: array [1..ID3V2_FRAME_COUNT] of string =
    ('TIT2', 'TPE1', 'TALB', 'TRCK', 'TYER', 'TCON', 'COMM');

type
  { ID3v2 frame header }
  FrameHeader = record
    ID: array [1..4] of Char;                                      { Frame ID }
    Size: Integer;                                    { Size excluding header }
    Flags: Word;                                                      { Flags }
  end;

  { ID3v2 header data - for internal use }
  TagInfo = record
    { Real structure of ID3v2 header }
    ID: array [1..3] of Char;                                  { Always "ID3" }
    Version: Byte;                                           { Version number }
    Revision: Byte;                                         { Revision number }
    Flags: Byte;                                               { Flags of tag }
    Size: array [1..4] of Byte;                   { Tag size excluding header }
    { Extended data }
    FileSize: Integer;                                    { File size (bytes) }
    Frame: array [1..ID3V2_FRAME_COUNT] of string;  { Information from frames }
  end;

function ID3v2InfoToString( const i: TID3v2Info): string;
begin
  Result:= '';
  if i.exists then
    Result:= i.Version;
end;


{ ********************* Auxiliary functions & procedures ******************** }

function ReadHeader(s: TStream; var Tag: TagInfo): Boolean;
begin
  try
    Result := true;
    { Read header and get file size }
    s.ReadBuffer(Tag, 10);
    Tag.FileSize := s.Size;
    { if transfer is not complete }
    if s.Position < 10 then Result := false;
  except
    { Error }
    Result := false;
  end;
end;

{ --------------------------------------------------------------------------- }

function GetVersionID(const Tag: TagInfo): Byte;
begin
  { Get tag version from header }
  Result := Tag.Version;
end;

{ --------------------------------------------------------------------------- }

function GetTagSize(const Tag: TagInfo): Integer;
begin
  { Get total tag size }
  Result :=
    Tag.Size[1] * $200000 +
    Tag.Size[2] * $4000 +
    Tag.Size[3] * $80 +
    Tag.Size[4] + 10;
//  if Result > Tag.FileSize then Result := 0;
end;

{ --------------------------------------------------------------------------- }

procedure SetTagItem(const ID, Data: string; var Tag: TagInfo);
var
  Iterator: Byte;
begin
  { Set tag item if supported frame found }
  for Iterator := 1 to ID3V2_FRAME_COUNT do
    if ID3V2_FRAME[Iterator] = ID then Tag.Frame[Iterator] := Data;
end;

{ --------------------------------------------------------------------------- }

function Swap32(const Figure: Integer): Integer;
var
  ByteArray: array [1..4] of Byte absolute Figure;
begin
  { Swap 4 bytes }
  Result :=
    ByteArray[1] * $1000000 +
    ByteArray[2] * $10000 +
    ByteArray[3] * $100 +
    ByteArray[4];
end;

{ --------------------------------------------------------------------------- }

procedure ReadFrames(s: TStream; var Tag: TagInfo);
var
  Frame: FrameHeader;
  Data: array [1..250] of Char;
  DataPosition: Integer;
begin
  try
    { Set read-access, open file }
    while ((s.Position < GetTagSize(tag)) and (s.Position < s.Size)) do
    begin
      FillChar(Data, SizeOf(Data), 0);
      { Read frame header and check frame ID }
      s.ReadBuffer(Frame, 10);
      if not (Frame.ID[1] in ['A'..'Z']) then break;
      DataPosition := s.Position;
      { Read frame data and set tag item if frame supported }
      s.ReadBuffer(Data, Swap32(Frame.Size) mod SizeOf(Data));
      SetTagItem(Frame.ID, Data, Tag);
      s.Position:= DataPosition + Swap32(Frame.Size);
    end;
  except
  end;
end;

{ --------------------------------------------------------------------------- }

function ExtractTrack(const TrackString: string): Byte;
var
  Index, Value, Code: Integer;
begin
  { Extract track from string }
  Index := Pos('/', Trim(TrackString));
  if Index = 0 then Val(Trim(TrackString), Value, Code)
  else Val(Copy(Trim(TrackString), 1, Index - 1), Value, Code);
  if Code = 0 then Result := Value
  else Result := 0;
end;

{ --------------------------------------------------------------------------- }

function ExtractGenre(const GenreString: string): string;
begin
  { Extract genre from string }
  Result := Trim(GenreString);
  if Pos(')', Result) > 0 then Delete(Result, 1, LastDelimiter(')', Result));
end;

{ --------------------------------------------------------------------------- }


{ --------------------------------------------------------------------------- }

procedure BuildHeader(var Tag: TagInfo);
var
  Iterator, TagSize: Integer;
begin
  { Build tag header }
  Tag.ID := ID3V2_ID;
  Tag.Version := TAG_VERSION_2_3;
  Tag.Revision := 0;
  Tag.Flags := 0;
  TagSize := 0;
  for Iterator := 1 to ID3V2_FRAME_COUNT do
    if Tag.Frame[Iterator] <> '' then
      Inc(TagSize, Length(Tag.Frame[Iterator]) + 11);
  { Convert tag size }
  Tag.Size[1] := TagSize div $200000;
  Tag.Size[2] := TagSize div $4000;
  Tag.Size[3] := TagSize div $80;
  Tag.Size[4] := TagSize mod $80;
end;

{ ********************** Private functions & procedures ********************* }

procedure TID3v2.FSetTitle(const NewTitle: string);
begin
  { Set song title }
  FTitle := TrimRight(NewTitle);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetArtist(const NewArtist: string);
begin
  { Set artist name }
  FArtist := TrimRight(NewArtist);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetAlbum(const NewAlbum: string);
begin
  { Set album title }
  FAlbum := TrimRight(NewAlbum);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetTrack(const NewTrack: Byte);
begin
  { Set track number }
  FTrack := NewTrack;
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetYear(const NewYear: string);
begin
  { Set release year }
  FYear := TrimRight(NewYear);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetGenre(const NewGenre: string);
begin
  { Set genre name }
  FGenre := TrimRight(NewGenre);
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.FSetComment(const NewComment: string);
begin
  { Set comment }
  FComment := TrimRight(NewComment);
end;

{ ********************** Public functions & procedures ********************** }

constructor TID3v2.Create;
begin
  { Create object }
  inherited;
  ResetData;
end;

{ --------------------------------------------------------------------------- }

procedure TID3v2.ResetData;
begin
  { Reset all variables }
  FExists := false;
  FVersionID := 0;
  FSize := 0;
  FTitle := '';
  FArtist := '';
  FAlbum := '';
  FTrack := 0;
  FYear := '';
  FGenre := '';
  FComment := '';
end;

{ --------------------------------------------------------------------------- }

function TID3v2.ReadFromStream(s: TStream): Boolean;
var
  Tag: TagInfo;
begin
  { Reset data and load header from file to variable }
  ResetData;
  Result := ReadHeader(s, Tag);
  { Process data if loaded and header valid }
  if (Result) and (Tag.ID = ID3V2_ID) then
  begin
    FExists := true;
    { Fill properties with header data }
    FVersionID := GetVersionID(Tag);
    FSize := GetTagSize(Tag);
    { Get information from frames if version supported }
    if ((FVersionID >= TAG_VERSION_2_3) and (FSize > 0) and (FSize <= tag.FileSize)) then
    begin
      ReadFrames(s, Tag);
      { Fill properties with data from frames }
      FTitle := Trim(Tag.Frame[1]);
      FArtist := Trim(Tag.Frame[2]);
      FAlbum := Trim(Tag.Frame[3]);
      FTrack := ExtractTrack(Tag.Frame[4]);
      FYear := Trim(Tag.Frame[5]);
      FGenre := ExtractGenre(Tag.Frame[6]);
      FComment := Trim(Copy(Tag.Frame[7], 5, Length(Tag.Frame[7]) - 4));
    end;
  end;
end;

function TID3v2.ReadFromFile(const FileName: string): Boolean;
var
  x: TFileStream;
begin
  x:= TFileStream.Create(Filename, fmOpenRead);
  try
    Result:= ReadFromStream(x);
  finally
    x.Free;
  end;
end;

{ --------------------------------------------------------------------------- }



function TID3v2.GetVersionString: string;
begin
  Result:= '';;
  if VersionId <> 0 then
    Result:= 'ID3v2.'+IntToStr(VersionId)+'.0';
end;

procedure TID3v2.FillID3v2Info(var info: TID3v2Info);
begin
  info.exists:= self.Exists;
  info.Version:= self.Version;
  info.Size:= self.Size;
  info.Title:= self.Title;
  info.Artist:= self.Artist;
  info.Album:= self.Album;
  info.Track:= self.Track;
  info.Year:= self.Year;
  info.Genre:= self.Genre;
  info.Comment:= self.Comment;
end;


end.
