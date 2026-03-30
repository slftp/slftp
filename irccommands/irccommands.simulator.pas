unit irccommands.simulator;

interface

{ slftp simulator commands functions }
function IrcSimulate(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, StrUtils, simulator, irc, mystrings;

const
  section = 'irccommands.simulator';

function IrcSimulate(const netname, channel, params: String): boolean;
var
  fSection, fReleasename: String;
  fSimulatePre: Boolean;
  fSimResult: TSimulationResult;
  fOutput: TStringList;
  i: Integer;
begin
  Result := False;

  // Parse parameters
  fSection := UpperCase(SubString(params, ' ', 1));
  fReleasename := SubString(params, ' ', 2);

  if (fSection = '') or (fReleasename = '') then
  begin
    irc_addtext(Netname, Channel, 'Usage: <cmd> <section> <releasename>');
    irc_addtext(Netname, Channel, 'Example: <cmd> MP3 Artist-Album-2024-GROUP');
    Result := True;
    Exit;
  end;

  // Check if third parameter is "pre" to simulate as PRE instead of NEWDIR
  fSimulatePre := (UpperCase(SubString(params, ' ', 3)) = 'PRE');

  irc_addtext(Netname, Channel, Format('Simulating release: <b>%s</b> in section <b>%s</b>%s...',
    [fReleasename, fSection, IfThen(fSimulatePre, ' (as PRE)', '')]));

  // Run simulation
  fSimResult := SimulateRelease(fSection, fReleasename, fSimulatePre);
  try
    // Format and output results
    fOutput := FormatSimulationResult(fSimResult);
    try
      for i := 0 to fOutput.Count - 1 do
      begin
        irc_addtext(Netname, Channel, fOutput[i]);
      end;
    finally
      fOutput.Free;
    end;
  finally
    fSimResult.Free;
  end;

  Result := True;
end;

end.
