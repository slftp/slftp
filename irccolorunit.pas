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

uses RegExpr;

const
  mColorExpression: string = '<c(\d+)>(.*?)<\/c>';
  mColorChar: string = #3;
  bColorExpression: string = '<d(\d+)>(.*?)<\/d>';
  bColorChar: string = #4;

function ReplaceThemeMSG(const msg: string): string;
var
  bcolrx, colrx: TRegExpr;
  smsg, s: string;
begin
  smsg := msg;
  colrx := TRegExpr.Create;
  bcolrx := TRegExpr.Create;
  try
    bcolrx.ModifierI := True;
    colrx.ModifierI := True;
    colrx.Expression := mColorExpression;
    bcolrx.Expression := bColorExpression;

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

  //TODO: Implement it in FLRE, used very often and TRegExpr is super slow...

    {   mIRC Color    }
    if colrx.Exec(smsg) then
    begin
      repeat
        s := Format('%s%.2d%s%:0s', [mColorChar, StrToInt(colrx.Match[1]), colrx.Match[2]]);
        smsg := StringReplace(smsg, colrx.Match[0], s, [rfReplaceAll, rfIgnoreCase]);
      until not colrx.ExecNext;
    end;

    {   bersirc (RGB) Color    }
    if bcolrx.Exec(smsg) then
    begin
      repeat
        s := Format('%s%.2d%s%:0s', [bColorChar, StrToInt(bcolrx.Match[1]), bcolrx.Match[2]]);
        smsg := StringReplace(smsg, bcolrx.Match[0], s, [rfReplaceAll, rfIgnoreCase]);
      until not bcolrx.ExecNext;
    end;

    Result := smsg;
  finally
    bcolrx.Free;
    colrx.Free;
  end;
end;

end.

