unit irccolorunit;
(*******************(c)*(S)cene*(D)evelopment**********************************
- Version 0.5
- Remarks:          Freeware, Copyright must be included
- Original Author:  mr. dOH!
- Last change:      2011-01 - Added: <d#> <u> <i> <l> <f> <r>
- Description:      Replace text like: <c7>f0<b>0</c>ba</b>r into this:
                    7f00bar

****************************************************************************
* THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS       *
* OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED        *
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE       *
* ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE        *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR      *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF     *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR          *
* BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,    *
* WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE     *
* OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,        *
* EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                       *
*******************(c)*(S)cene*(D)evelopment********************************)
interface

uses classes, sysutils;

function ReplaceThemeMSG(const msg: string): string;

implementation

uses FLRE;

const
  // Note: Forward slash does not need escaping in FLRE (unlike TRegExpr)
  mColorExpression: RawByteString = '<c(\d+)>(.*?)<\/c>';
  mColorChar: string = #3;
  bColorExpression: RawByteString = '<d(\d+)>(.*?)<\/d>';
  bColorChar: string = #4;


function ReplaceThemeMSG(const msg: string): string;
var
  bcolrx, colrx: TFLRE;
  colrx_captures, bcolrx_captures: TFLREMultiCaptures;
  smsg, s: string;
  i: Integer;
  fUtf8Msg: RawByteString;
begin
  smsg := msg;

  // Simple replacements (no regex needed)
  smsg := StringReplace(smsg, '<b>', #2, [rfReplaceAll]);
  smsg := StringReplace(smsg, '</b>', #2, [rfReplaceAll]);
  smsg := StringReplace(smsg, '<u>', #31, [rfReplaceAll]);
  smsg := StringReplace(smsg, '</u>', #31, [rfReplaceAll]);
  smsg := StringReplace(smsg, '<i>', #22, [rfReplaceAll]);
  smsg := StringReplace(smsg, '</i>', #22, [rfReplaceAll]);
  smsg := StringReplace(smsg, '<l>', #10, [rfReplaceAll]);
  smsg := StringReplace(smsg, '</l>', #10, [rfReplaceAll]);
  smsg := StringReplace(smsg, '<f>', #17, [rfReplaceAll]);
  smsg := StringReplace(smsg, '</f>', #17, [rfReplaceAll]);
  smsg := StringReplace(smsg, '<r>', #18, [rfReplaceAll]);
  smsg := StringReplace(smsg, '</r>', #18, [rfReplaceAll]);

  // Convert to UTF8 for FLRE processing
  fUtf8Msg := RawByteString(smsg);

  // Use FLRE.TFLREFlag to avoid naming conflict with SysUtils.TReplaceFlags
  colrx := TFLRE.Create(mColorExpression, [FLRE.rfIGNORECASE]);
  bcolrx := TFLRE.Create(bColorExpression, [FLRE.rfIGNORECASE]);
  try
    {   mIRC Color - migrated to FLRE for performance   }
    if colrx.MatchAll(fUtf8Msg, colrx_captures) then
    begin
      // Process matches in reverse order to maintain string positions
      for i := High(colrx_captures) downto 0 do
      begin
        if Length(colrx_captures[i]) >= 3 then
        begin
          s := Format('%s%.2d%s%:0s', [
            mColorChar,
            StrToInt(string(Copy(fUtf8Msg, colrx_captures[i][1].Start, colrx_captures[i][1].Length))),
            string(Copy(fUtf8Msg, colrx_captures[i][2].Start, colrx_captures[i][2].Length))
          ]);
          smsg := StringReplace(smsg,
            string(Copy(fUtf8Msg, colrx_captures[i][0].Start, colrx_captures[i][0].Length)),
            s, [rfReplaceAll, SysUtils.rfIgnoreCase]);
        end;
      end;
      SetLength(colrx_captures, 0);
    end;

    // Update UTF8 message for next regex
    fUtf8Msg := RawByteString(smsg);

    {   bersirc (RGB) Color - migrated to FLRE for performance   }
    if bcolrx.MatchAll(fUtf8Msg, bcolrx_captures) then
    begin
      // Process matches in reverse order to maintain string positions
      for i := High(bcolrx_captures) downto 0 do
      begin
        if Length(bcolrx_captures[i]) >= 3 then
        begin
          s := Format('%s%.2d%s%:0s', [
            bColorChar,
            StrToInt(string(Copy(fUtf8Msg, bcolrx_captures[i][1].Start, bcolrx_captures[i][1].Length))),
            string(Copy(fUtf8Msg, bcolrx_captures[i][2].Start, bcolrx_captures[i][2].Length))
          ]);
          smsg := StringReplace(smsg,
            string(Copy(fUtf8Msg, bcolrx_captures[i][0].Start, bcolrx_captures[i][0].Length)),
            s, [rfReplaceAll, SysUtils.rfIgnoreCase]);
        end;
      end;
      SetLength(bcolrx_captures, 0);
    end;

    Result := smsg;
  finally
    bcolrx.Free;
    colrx.Free;
  end;
end;

end.

