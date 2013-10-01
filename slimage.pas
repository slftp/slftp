unit slimage;

interface

function MyImgResize(const src, dst: string;ujXmeret,ujYmeret: Integer): Boolean; overload;

implementation

uses
{$IFNDEF FPC}
//  jpeg_uxlbitmap
{$ELSE}
  process,
{$ENDIF}
  SysUtils
;
function MyImgResize(const src, dst: string;ujXmeret,ujYmeret: Integer): Boolean; overload;
{$IFDEF FPC}
var p: TProcess;
{$ENDIF}
begin
{$IFNDEF FPC}
  Result:= False; // ImgResize(src, dst, ujXmeret, ujYmeret);
{$ELSE}
  p:= TProcess.Create(nil);
  try
    p.CommandLine:= 'minjpg '''+src+''' '''+dst+''' '+IntToStr(ujXmeret)+' '+IntToStr(ujYMeret);
    p.Options:= [poWaitOnExit, poNoConsole];
    try
      p.Execute();
    except on e: Exception do
      // writeln(e.Message);
    end;
  finally
    p.Free;
  end;
{$ENDIF}
end;


end.
