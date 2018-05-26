program slftp;
{$MODE Delphi}

{$IFDEF FPC}
  {$if FPC_FULlVERSION < 30004}
    {$stop Please upgrade your Free Pascal Compiler version to at least 3.0.4 }
  {$endif}
{$ENDIF}

{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}
{*******************************************************************************\

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
            00  0:  .100'       :...100100...:'
            01  01.101'
            10  0101' .   This program is distributed in the hope that it will be useful,
            01  01'  .1   but WITHOUT ANY WARRANTY; without even the implied warranty of
            0:  10   00   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
            :.  1    10   GNU General Public License for more details.
            .   1    0:
                0.    :.
                .     .            http://www.gnu.org/licenses/gpl-3.0.en.html

\*******************************************************************************}
uses
  {$IFDEF FPC}
    {$IFDEF UNIX}
      {$IFNDEF CPUARM}
        FastMM4,
      {$ENDIF}
        cthreads,
        cmem,
    {$ENDIF}
  {$ENDIF}
  console;

{$IFNDEF CPUARM}
  // allow more user mode address space
  {$SetPEFlags $20}
{$ENDIF}

begin
  ConsoleStart;
end.
