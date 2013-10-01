{*****************************************************************************

 - Soulless robotic engine aka SLFTP  Version 1.4
 
 - Remarks:          Freeware, Copyright must be included
 
 - Original Author:  believe                   
                    
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

*****************************************************************************}

program slftp;

{$MODE Delphi}

{$APPTYPE CONSOLE}

uses
{$IFDEF FPC}
 {$ifdef unix}
   cthreads,
   cmem, // the c memory manager is on some systems much faster for multithreading
 {$endif}
{$ENDIF}
  console;

// {$R *.res}


begin
  ConsoleStart;
end.
