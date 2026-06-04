unit slapi.issueshook;

interface

type
  TIssueLogProc = procedure(const aIssueType, aSection, aReleaseName, aSiteName, aReason, aKbEvent: string;
    const aDedupKey: string; const aDedupTtlSeconds: integer);

var
  GlIssueLogProc: TIssueLogProc = nil;

procedure IssueLog(const aIssueType, aSection, aReleaseName, aSiteName, aReason, aKbEvent: string;
  const aDedupKey: string = ''; const aDedupTtlSeconds: integer = 0);

implementation

procedure IssueLog(const aIssueType, aSection, aReleaseName, aSiteName, aReason, aKbEvent: string;
  const aDedupKey: string; const aDedupTtlSeconds: integer);
var
  proc: TIssueLogProc;
begin
  proc := GlIssueLogProc;
  if Assigned(proc) then
    proc(aIssueType, aSection, aReleaseName, aSiteName, aReason, aKbEvent, aDedupKey, aDedupTtlSeconds);
end;

end.
