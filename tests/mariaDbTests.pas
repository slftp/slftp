unit mariaDbTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestSitesunit = class(TTestCase)
  published
    procedure TestInsertRls;
  end;

implementation

uses
  dbaddpre;

{ TTestSitesunit }

procedure TTestSitesunit.TestInsertRls;
begin
  dbaddpre_InsertRlz('rls_name-asdf', 'Section', 'src');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('sitesunit', TTestSitesunit.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSitesunit);
  {$ENDIF}
end.
