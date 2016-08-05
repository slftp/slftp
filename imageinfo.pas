unit imageinfo;

interface

type
  TImageInfo = record
    width: Word;
    height: Word;
    imagecreated_ts: AnsiString;
    software: AnsiString;
    camera: AnsiString;
  end;

function JPEGDimensions(Filename : AnsiString; var X, Y : Word) : boolean;
function GetImageInfo(const filename: AnsiString; var i: TImageInfo): boolean;

implementation

uses exif_EXIF, SysUtils, Classes;

function GetImageInfo(const filename: AnsiString; var i: TImageInfo): boolean;
var id: TImgData;
begin
  Result:= False;
  try
    with i do
    begin
      width:= 0;
      height:= 0;
      imagecreated_ts:= '';
      software:= '';
      camera:= '';
    end;
    JPEGDimensions(filename, i.width, i.height);
    if ((i.width > 0) and (i.height > 0)) then
    begin
      id := TImgData.create;
      try

        if id.ProcessFile(filename) then
        begin
          if id.ExifObj <> nil then
          begin
            i.camera:= id.ExifObj.CameraMake+' '+id.ExifObj.CameraModel;
            i.software:= id.ExifObj.Data['Software'].Data;
            if id.ExifObj.Data['DateTimeOriginal'].Data <> '' then
              i.imagecreated_ts:= id.ExifObj.Data['DateTimeOriginal'].Data
            else
              i.imagecreated_ts:= id.ExifObj.Data['DateTime'].Data;
          end;
        end;
        Result:= True;
      finally
        id.Free;
      end;
    end;
  except

  end;
end;


function JPEGDimensions(Filename : AnsiString; var X, Y : Word) : boolean;
var
  SegmentPos : Integer;
  SOIcount : Integer;
  b : byte;
begin
  Result  := False;
  with TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone) do
  begin
    try
      Position := 0;
      Read(X, 2);
      if (X <> $D8FF) then
        exit;
      SOIcount  := 0;
      Position  := 0;
      while (Position + 7 < Size) do
      begin
        Read(b, 1);
        if (b = $FF) then begin
          Read(b, 1);
          if (b = $D8) then
            inc(SOIcount);
          if (b = $DA) then
            break;
        end; {if}
      end; {while}
      if (b <> $DA) then
        exit;
      SegmentPos  := -1;
      Position    := 0;
      while (Position + 7 < Size) do
      begin
        Read(b, 1);
        if (b = $FF) then
        begin
          Read(b, 1);
          if (b in [$C0, $C1, $C2]) then
          begin
            SegmentPos  := Position;
            dec(SOIcount);
            if (SOIcount = 0) then
              break;
          end; {if}
        end; {if}
      end; {while}
      if (SegmentPos = -1) then
        exit;
      if (Position + 7 > Size) then
        exit;
      Position := SegmentPos + 3;
      Read(Y, 2);
      Read(X, 2);
      X := Swap(X);
      Y := Swap(Y);
      Result  := true;
    finally
      Free;
    end; {try}
  end; {with}
end; {JPEGDimensions}


end.
