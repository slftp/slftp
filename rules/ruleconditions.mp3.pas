unit ruleconditions.mp3;

interface

uses
  Classes, pazo, rulesunit, sllanguagebase;

type
  TConditionMP3Genre = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMP3Year = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMP3CurrentYear = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMP3Language = class(TStringCondition)
    function Verify(const s: String): boolean; override;
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMP3Foreign = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMP3Source = class(TStringCondition)
    function Verify(const s: String): boolean; override;
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMP3Live = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMP3Type = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMP3Bootleg = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMP3NumDisks = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMP3VA = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

implementation

uses
  SysUtils, Contnrs, kb.releaseinfo, debugunit;

const
  dsection = 'rules.mp3';

{$I ruleconditions.mp3.inc}

{ TConditionMP3Genre }

function TConditionMP3Genre.SupplyValue(r: TPazo): String;
begin
  Result := '';
  try
    if r.rls is TMP3Release then
      Result := TMP3Release(r.rls).mp3genre;
  except
    Result := '';
  end;
end;

class function TConditionMP3Genre.Name: String;
begin
  Result := 'mp3genre';
end;

class function TConditionMP3Genre.Description: String;
begin
  Result := MP3GenreDescription;
end;

{ TConditionMP3Year }

function TConditionMP3Year.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  try
    if (r.rls is TMP3Release) then
      Result := TMP3Release(r.rls).mp3year;
  except
    Result := 0;
  end;
end;

class function TConditionMP3Year.Name: String;
begin
  Result := 'mp3year';
end;

class function TConditionMP3Year.Description: String;
begin
  Result := MP3YearDescription;
end;

{ TConditionMP3CurrentYear }

function TConditionMP3CurrentYear.SupplyValue(r: TPazo): boolean;
begin
  Result := False;

  if (r.rls is TMP3Release) then
  begin
    Result := (TMP3Release(r.rls).mp3year = r.rls.CurrentYear);
  end;
end;

class function TConditionMP3CurrentYear.Name: String;
begin
  Result := 'mp3currentyear';
end;

class function TConditionMP3CurrentYear.Description: String;
begin
  Result := MP3CurrentYearDescription;
end;

{ TConditionMP3Language }

function TConditionMP3Language.Verify(const s: String): boolean;
begin
  try
    Result := VerifyMusicLanguage(s);
  except
    Result := False;
  end;
end;

function TConditionMP3Language.SupplyValue(r: TPazo): String;
begin
  Result := '';
  try
    if (r.rls is TMP3Release) then
      Result := TMP3Release(r.rls).mp3lng;
  except
    Result := '';
  end;
end;

class function TConditionMP3Language.Name: String;
begin
  Result := 'mp3language';
end;

class function TConditionMP3Language.Description: String;
begin
  Result := MP3LanguageDescription;
end;

{ TConditionMP3Foreign }

function TConditionMP3Foreign.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if (r.rls is TMP3Release) then
      Result := TMP3Release(r.rls).mp3lng <> 'EN';
  except
    Result := False;
  end;
end;

class function TConditionMP3Foreign.Name: String;
begin
  Result := 'mp3foreign';
end;

class function TConditionMP3Foreign.Description: String;
begin
  Result := MP3ForeignDescription;
end;

{ TConditionMP3Source }

function TConditionMP3Source.Verify(const s: String): boolean;
begin
  try
    Result := mp3sources.IndexOfName(s) <> -1;
  except
    Result := False;
  end;
end;

function TConditionMP3Source.SupplyValue(r: TPazo): String;
begin
  Result := '';
  try
    if (r.rls is TMP3Release) then
      Result := TMP3Release(r.rls).mp3source;
  except
    Result := '';
  end;
end;

class function TConditionMP3Source.Name: String;
begin
  Result := 'mp3source';
end;

class function TConditionMP3Source.Description: String;
begin
  Result := MP3SourceDescription;
end;

{ TConditionMP3Live }

function TConditionMP3Live.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if (r.rls is TMP3Release) then
      with TMP3Release(r.rls) do
        Result := ((mp3source = 'LIVE') or (mp3type('LIVE')));
  except
    Result := False;
  end;
end;

class function TConditionMP3Live.Name: String;
begin
  Result := 'mp3live';
end;

class function TConditionMP3Live.Description: String;
begin
  Result := MP3LiveDescription;
end;

{ TConditionMP3Type }

procedure TConditionMP3Type.SupplyValues(r: TPazo; re: TStringList);
var
  mp: TMP3Release;
begin
  try
    if r.rls is TMP3Release then
    begin
      mp := TMP3Release(r.rls);
      if mp.mp3types1 <> '' then
        re.Add(mp.mp3types1);
      if mp.mp3types2 <> '' then
        re.Add(mp.mp3types2);
      if mp.mp3types3 <> '' then
        re.Add(mp.mp3types3);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionMP3Type.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionMP3Type.Name: String;
begin
  Result := 'mp3type';
end;

class function TConditionMP3Type.Description: String;
begin
  Result := MP3TypeDescription;
end;

{ TConditionMP3Bootleg }

function TConditionMP3Bootleg.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if (r.rls is TMP3Release) then
      Result := TMP3Release(r.rls).bootleg;
  except
    Result := False;
  end;
end;

class function TConditionMP3Bootleg.Name: String;
begin
  Result := 'mp3bootleg';
end;

class function TConditionMP3Bootleg.Description: String;
begin
  Result := MP3BootlegDescription;
end;

{ TConditionMP3NumDisks }

function TConditionMP3NumDisks.SupplyValue(r: TPazo): integer;
begin
  Result := 1;
  try
    if r.rls is TMP3Release then
      Result := TMP3Release(r.rls).Numdisks;
  except
    Result := 1;
  end;
end;

class function TConditionMP3NumDisks.Name: String;
begin
  Result := 'mp3numdisks';
end;

class function TConditionMP3NumDisks.Description: String;
begin
  Result := MP3NumDisksDescription;
end;

{ TConditionMP3VA }

function TConditionMP3VA.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if (r.rls is TMP3Release) then
      Result := TMP3Release(r.rls).mp3_va;
  except
    Result := False;
  end;
end;

class function TConditionMP3VA.Name: String;
begin
  Result := 'mp3va';
end;

class function TConditionMP3VA.Description: String;
begin
  Result := MP3VADescription;
end;

end.
