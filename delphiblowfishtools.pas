unit delphiblowfishtools;

interface

uses
  Sysutils;

procedure XorBlock(I1, I2, O1: PByteArray; const Len: integer);
procedure IncBlock(P: PByteArray; const Len: integer);

implementation

procedure XorBlock(I1, I2, O1: PByteArray; const Len: integer);
var
  i: integer;
begin
  for i := 0 to Len - 1 do
    O1[i] := I1[i] xor I2[i];
end;

procedure IncBlock(P: PByteArray; const Len: integer);
begin
  Inc(P[Len - 1]);
  if (P[Len - 1] = 0) and (Len > 1) then
    IncBlock(P, Len - 1);
end;

end.
