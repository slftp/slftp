{
 Freeware, Copyright              .
 must be included GLP3            ;    .
 it under the terms of the GNU   01.   .
 General Public License as       01;, .:
 as published by the             0100 11
 Free Software Foundation;       1010 01
 either version 1 or any later   0100 01
 version....                   : 1001 00 :
                               :   `0 01 :
         _______     _         :  _____._:___              _
        /      /    / \___     __/          /             / \________
       /  ,___/____/     /     \    _      / \___________/          /
    ___\____      /     /______/    /_____/             /    _     /
   /__     /     /     /      /     ___/ /____      ___/     /    /
     /    /     /     /      /     /2o!  :   /     /  /     _____/
    /     _____/_____       /__   /     .:  /     /  /__   /
   /__   /          /___   /...\_/....:::: /__   /  .   \_/
      \_.001.  1    .100._/            ...    \_/   .
         01    10      10               ::          :
        `10.11 .0  11.01'                `          :
          1000.  .  .000'                 ........  :
          `'`101. .101'`1.......:.........:      :..'
            . `10100'.:         :         :      :
     --->   :.  `10z.`:  <-- ---+- slFtp -+-     :
            1:   .`10f.         :         `......:
            01  .1  `00r.       :.............'
            00  0:  .100'       :...Legend...:'
            01  01.101'
            10  0101' .   This program is distributed in the hope that it will be useful,
            01  01'  .1   but WITHOUT ANY WARRANTY; without even the implied warranty of
            0:  10   00   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
            :.  1    10   GNU General Public License for more details.
            .   1    0:
                0.    :.
                .     .            http://www.gnu.org/licenses/gpl-3.0.en.html
}

program slftp;

{$if CompilerVersion < 33}
  {$MESSAGE Fatal 'Please upgrade your compiler to at least Delphi 10.3 Rio (10.3.x)'}
{$endif}

uses
  FastMM5,
  SysUtils,
  commandlineutil,
  console,
  Clipbrd in 'Clipbrd.pas',
  backupunit in 'backupunit.pas',
  configunit in 'configunit.pas',
  dbaddgenre in 'dbaddgenre.pas',
  dbaddimdb in 'dbaddimdb.pas',
  dbaddnfo in 'dbaddnfo.pas',
  dbaddpre in 'dbaddpre.pas',
  dbaddurl in 'dbaddurl.pas',
  dbhandler in 'dbhandler.pas',
  dbtvinfo in 'dbtvinfo.pas',
  debugunit in 'debugunit.pas',
  delphiblowfish in 'delphiblowfish.pas',
  delphiblowfishtools in 'delphiblowfishtools.pas',
  delphimasks in 'delphimasks.pas',
  delphimd5 in 'delphimd5.pas',
  Diff in 'Diff.pas',
  dirlist.helpers in 'dirlist.helpers.pas',
  dirlist in 'dirlist.pas',
  encinifile in 'encinifile.pas',
  fake in 'fake.pas',
  globals in 'globals.pas',
  globalskipunit in 'globalskipunit.pas',
  HashUnit in 'HashUnit.pas',
  http in 'http.pas',
  identserver in 'identserver.pas',
  indexer in 'indexer.pas',
  irc.parse in 'irc.parse.pas',
  irc in 'irc.pas',
  ircblowfish.CBC in 'ircblowfish.CBC.pas',
  ircblowfish.ECB in 'ircblowfish.ECB.pas',
  ircblowfish.plaintext in 'ircblowfish.plaintext.pas',
  ircchansettings in 'ircchansettings.pas',
  irccolorunit in 'irccolorunit.pas',
  irccommandsunit in 'irccommandsunit.pas',
  kb in 'kb.pas',
  kb.releaseinfo in 'kb.releaseinfo.pas',
  knowngroups in 'knowngroups.pas',
  mainthread in 'mainthread.pas',
  midnight in 'midnight.pas',
  mrdohutils in 'mrdohutils.pas',
  mslproxys in 'mslproxys.pas',
  mycrypto in 'mycrypto.pas',
  mygrouphelpers in 'mygrouphelpers.pas',
  mystrings in 'mystrings.pas',
  news in 'news.pas',
  notify in 'notify.pas',
  nuke in 'nuke.pas',
  pazo in 'pazo.pas',
  precatcher.helpers in 'precatcher.helpers.pas',
  precatcher in 'precatcher.pas',
  queueunit in 'queueunit.pas',
  ranksunit in 'ranksunit.pas',
  rulesunit in 'rulesunit.pas',
  sitesunit in 'sitesunit.pas',
  skiplists in 'skiplists.pas',
  slblowfish in 'slblowfish.pas',
  slconsole in 'slconsole.pas',
  slcriticalsection in 'slcriticalsection.pas',
  slhelper in 'slhelper.pas',
  sllanguagebase in 'sllanguagebase.pas',
  slmasks in 'slmasks.pas',
  slmd5 in 'slmd5.pas',
  slssl in 'slssl.pas',
  slstack in 'slstack.pas',
  sltcp in 'sltcp.pas',
  slvision in 'slvision.pas',
  socks5 in 'socks5.pas',
  speedstatsunit in 'speedstatsunit.pas',
  statsunit in 'statsunit.pas',
  tags in 'tags.pas',
  taskautodirlist in 'taskautodirlist.pas',
  taskautoindex in 'taskautoindex.pas',
  taskautonuke in 'taskautonuke.pas',
  taskcwd in 'taskcwd.pas',
  taskdel in 'taskdel.pas',
  taskdirlist in 'taskdirlist.pas',
  taskfilesize in 'taskfilesize.pas',
  taskgame in 'taskgame.pas',
  taskgenredirlist in 'taskgenredirlist.pas',
  taskgenrenfo in 'taskgenrenfo.pas',
  taskhttpimdb in 'taskhttpimdb.pas',
  taskhttpnfo in 'taskhttpnfo.pas',
  taskidle in 'taskidle.pas',
  tasklogin in 'tasklogin.pas',
  taskmvidunit in 'taskmvidunit.pas',
  taskpretime in 'taskpretime.pas',
  taskquit in 'taskquit.pas',
  taskrace in 'taskrace.pas',
  taskraw in 'taskraw.pas',
  taskrules in 'taskrules.pas',
  tasksitenfo in 'tasksitenfo.pas',
  taskspeedtest in 'taskspeedtest.pas',
  tasksunit in 'tasksunit.pas',
  tasktvinfolookup in 'tasktvinfolookup.pas',
  versioninfo in 'versioninfo.pas';

{$APPTYPE CONSOLE}

// allow more user mode address space
{$SetPEFlags $20}

var
  fBinaryFilename, fCmdLine: String;
  i: Integer;

begin
  fBinaryFilename := ExtractFileName(ParamStr(0));
  if ParamCount <> 0 then
  begin
    // execute command line util
    for i := 1 to ParamCount do
    begin
      fCmdLine := fCmdLine + ParamStr(i) + ' ';
    end;
    fCmdLine := fCmdLine.Trim;

    ParseCommandLine(fBinaryFilename, fCmdLine);
    Exit;
  end;

  ConsoleStart;
end.
