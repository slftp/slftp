{*****************************************************************************

 - Soulless robotic engine aka SLFTP
 - Version 1.4

 - Remarks:          Freeware, Copyright must be included

 - Original Author:  believe

 - Modifications:    zohan, uuser, sigge, xerox_, aKRAUT aka dOH

 - Last change:
                      30/03/2010 - Edit Regex for new Style.....
                      25/06/2010 - Fixxing Language and Country Regex.
                      04/10/2010 - Adapting new iMDB Style for MainInfoPage
                      14/02/2011 - Fixing some regex (STV, Festival, CineYear)
                      04/07/2011 - Votes and ratings fixed

 - Description:      Fetch and handle IMDB Data


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

unit taskimdb;

interface

uses Classes, pazo, taskrace, sltcp, dbaddimdb;

type
    TPazoImdbTask = class(TPazoPlainTask)
    private
        ss: TStringStream;
        attempt: Integer;
    public
        constructor Create(const netname, channel: String;site: String; pazo: TPazo; attempt: Integer);
        destructor Destroy; override;
        function Execute(slot: Pointer): Boolean; override;
        function Name: String; override;
        procedure PostResults(imdb : TDbImdb);
end;

implementation

uses SysUtils, irc, StrUtils, kb, debugunit, dateutils, queueunit, tags,
     configunit, tasksunit, dirlist, mystrings, sitesunit, regexpr,
     tasksitenfo, Contnrs;

const
    section = 'taskimdb';

{ TPazoImdbTask }

constructor TPazoImdbTask.Create(const netname, channel: String;site: String; pazo: TPazo; attempt: Integer);
begin
    ss:=TStringStream.Create('');
    self.attempt:=attempt;
    self.wanted_dn:=True;
    inherited Create(netname, channel, site, '', pazo);
end;



function TPazoImdbTask.Execute(slot: Pointer): Boolean;
var r:TPazoImdbTask;
    imdbrls : TDbImdbRls;
    imdb : TDbImdb;
    ir:TIMDBRelease;
begin
  Result:=False;

  if mainpazo.stopped then begin
    readyerror:= True;
    exit;
  end;

  Debug(dpMessage, section, Name);

  imdbrls := dbaddimdb_getimdbrls(mainpazo.rls.rlsname);
  imdb := nil;
  if imdbrls <> nil then
  begin
    imdb := dbaddimdb_getimdb(imdbrls.imdb_id);
  end;

  if imdb = nil then
  begin
    // start leeching nfo
    AddTask(TPazoSiteNfoTask.Create(netname, channel, ps1.name, mainpazo, 1));

    if attempt < config.readInteger(section, 'readd_attempts', 5) then begin
      Debug(dpSpam, section, 'READD: do not have the nfo file!');
      r:= TPazoImdbTask.Create(netname, channel, ps1.name, mainpazo, attempt+1);
      r.startat:= IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
      try
        AddTask(r);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[Exception] in TPazoImdbTask AddTask %s', [e.Message]));
          irc_Adderror(Format('<c4>[Exception]</c> in TPazoImdbTask AddTask %s', [e.Message]));
          readyerror:= True;
          exit;
        end;
      end;
    end else begin
      Debug(dpSpam, section, 'READD: no more readd...');
    end;
    ready:= True;
    Result:= True;
    exit;
  end;

  ir:=TImdbRelease(mainpazo.rls);
  imdb.SetImdbRelease(ir);

  try
    kb_add(netname, channel, ps1.name, mainpazo.rls.section, '', 'UPDATE', mainpazo.rls.rlsname, '');
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in TPazoImdbTask kb_add: %s', [e.Message]));
    end;
  end;

  Result:= True;
  ready:= True;
end;

destructor TPazoImdbTask.Destroy;
begin
    ss.Free;
    inherited;
end;

function TPazoImdbTask.Name: String;
begin
  try
    Result:=Format('PIMDB (PazoID:%d) %s [Count:%d]',[pazo_id,mainpazo.rls.rlsname,attempt]);
  except
    Result:= 'PIMDB';
  end;
end;

procedure TPazoImdbTask.PostResults(imdb : TDbImdb);
begin
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <b><c0>info for</b></c> .......: <b>%s</b>',[mainpazo.rls.rlsname]));
  imdb.PostResults;
end;


end.

