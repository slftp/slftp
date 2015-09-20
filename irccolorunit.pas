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


function ReplaceThemeMSG(msg: string): string;

implementation

uses RegExpr;

const
  mColorExpression:String = '<c(\d+)>(.*?)<\/c>';
  mColorChar:string = #3;
  bColorExpression:String = '<d(\d+)>(.*?)<\/d>';
  bColorChar:string = #4;

function ReplaceThemeMSG(msg: string): string;
var
bcolrx,colrx: TRegExpr;
//fixrx, swirx,  linkrx, itarx, unlrx, bolrx: TRegExpr;
smsg,s: string;
begin

  smsg := msg;
  colrx := TRegExpr.Create;
  bcolrx := TRegExpr.Create;
  try
  bcolrx.ModifierI := True;
  colrx.ModifierI := True;
  colrx.Expression := mColorExpression;
  bcolrx.Expression:=bColorExpression;

smsg:=StringReplace(smsg,'<b>',#2,[rfReplaceAll]);
smsg:=StringReplace(smsg,'</b>',#2,[rfReplaceAll]);

smsg:=StringReplace(smsg,'<u>',#31,[rfReplaceAll]);
smsg:=StringReplace(smsg,'</u>',#31,[rfReplaceAll]);

smsg:=StringReplace(smsg,'<i>',#22,[rfReplaceAll]);
smsg:=StringReplace(smsg,'</i>',#22,[rfReplaceAll]);

smsg:=StringReplace(smsg,'<l>',#10,[rfReplaceAll]);
smsg:=StringReplace(smsg,'</l>',#10,[rfReplaceAll]);

smsg:=StringReplace(smsg,'<f>',#17,[rfReplaceAll]);
smsg:=StringReplace(smsg,'</f>',#17,[rfReplaceAll]);

smsg:=StringReplace(smsg,'<r>',#18,[rfReplaceAll]);
smsg:=StringReplace(smsg,'</r>',#18,[rfReplaceAll]);


{   mIRC Color    }
  if colrx.Exec(smsg) then
  begin
    REPEAT
      s:=Format('%s%s%s%:0s',[mColorChar,colrx.Match[1],colrx.Match[2]]);
      smsg := StringReplace(smsg, colrx.Match[0], s,
        [rfReplaceAll, rfIgnoreCase]);
    UNTIL not colrx.ExecNext;
  end;
{   bersirc (RGB) Color    }
  if bcolrx.Exec(smsg) then
  begin
    REPEAT
      s:=Format('%s%s%s%:0s',[bColorChar,bcolrx.Match[1],bcolrx.Match[2]]);
      smsg := StringReplace(smsg, bcolrx.Match[0], s,
        [rfReplaceAll, rfIgnoreCase]);
    UNTIL not bcolrx.ExecNext;
  end;
  (*
{   Bold    }
  if bolrx.Exec(smsg) then
  begin
    REPEAT
      s:= Format('%s%s%:0s',[BoldChar,bolrx.Match[1]]);
      smsg := StringReplace(smsg, bolrx.Match[0], s,
        [rfReplaceAll, rfIgnoreCase]);
    UNTIL not bolrx.ExecNext;
  end;

{   Underline   }
  if unlrx.Exec(smsg) then
  	begin
  		REPEAT
s:= Format('%s%s%:0s',[UnderlineChar,unlrx.Match[1]]);
      smsg := StringReplace(smsg, unlrx.Match[0], s,
        [rfReplaceAll, rfIgnoreCase]);
    UNTIL not unlrx.ExecNext;
  end;
{   Italics   }
  if itarx.Exec(smsg) then
  	begin
  		REPEAT
      s:= Format('%s%s%:0s',[ItalicsChar,itarx.Match[1]]);
      smsg := StringReplace(smsg, itarx.Match[0], s,
        [rfReplaceAll, rfIgnoreCase]);
    UNTIL not itarx.ExecNext;
  end;
{   Link   }
  if linkrx.Exec(smsg) then
  	begin
  		REPEAT
      s:= Format('%s%s%:0s',[LinkChar,linkrx.Match[1]]);
      smsg := StringReplace(smsg, linkrx.Match[0], s,
        [rfReplaceAll, rfIgnoreCase]);
    UNTIL not linkrx.ExecNext;
  end;

{   Switch fg/bg   }
  if swirx.Exec(smsg) then
  	begin
  		REPEAT
      s:= Format('%s%s%:0s',[SwitchChar,swirx.Match[1]]);
      smsg := StringReplace(smsg, swirx.Match[0], s,
        [rfReplaceAll, rfIgnoreCase]);
    UNTIL not swirx.ExecNext;
  end;

{   Fixed width font   }
  if fixrx.Exec(smsg) then
  	begin
  		REPEAT
      s:= Format('%s%s%:0s',[FixedChar,fixrx.Match[1]]);
      smsg := StringReplace(smsg, fixrx.Match[0], s,
        [rfReplaceAll, rfIgnoreCase]);
    UNTIL not fixrx.ExecNext;
  end;
  *)
  result := smsg;
  finally

  bcolrx.Free;
  colrx.Free;
(*
  bolrx.Free;
  unlrx.Free;

  linkrx.Free;
  swirx.Free;
  fixrx.Free;
  itarx.Free;
*)
  end;

end;

end.
