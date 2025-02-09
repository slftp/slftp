unit rulesunit;

interface

uses
  Classes, pazo, slmasks, Contnrs, Generics.Collections;

type
  TRuleNode = class
    parent: TRuleNode;
    class function TakeThis(var s: String): boolean; virtual;
    procedure SetupChild(child: TRuleNode); virtual;
    constructor Create(parent: TRuleNode); virtual;
    class function Name: String; virtual; abstract;
    function Match(p: TPazo): boolean; virtual; abstract;
    function AsText: String; virtual; abstract;
    function AtConditionName: String; virtual; abstract;
  end;

  TCRuleNode = class of TRuleNode;

  TOperator = class(TRuleNode)
  end;

  TPrefixOperator = class(TOperator)
    child: TRuleNode;
    function AsText: String; override;
    destructor Destroy; override;
    procedure SetupChild(child: TRuleNode); override;
    function AtConditionName: String; override;
  end;

  TNotOperator = class(TPrefixOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TOpeningBracketOperator = class(TPrefixOperator)
    class function Name: String; override;
    function AsText: String; override;
    function Match(p: TPazo): boolean; override;
    destructor Destroy; override;
  end;

  TClosingBracketOperator = class(TOperator)
    class function Name: String; override;
  end;

  TInfixOperator = class(TOperator)
    left: TRuleNode;
    right: TRuleNode;

    function AsText: String; override;
    destructor Destroy; override;
    function AtConditionName: String; override;
  end;

  TOrOperator = class(TInfixOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TAndOperator = class(TInfixOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TOperand = class(TRuleNode)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
    function AtConditionName: String; override;
  end;

  TStringOperand = class(TOperand)
  private
    fValue: String;
    skipVerification: boolean;
  public
    function AsText: String; override;
    function Value: String;
    function FeedOperand(const s: String): boolean; virtual;
  end;

  TIntOperand = class(TOperand)
  private
    fValue: integer;
  public
    function AsText: String; override;
    function Value: integer;
    function FeedOperand(const s: String): boolean;
  end;

  TMaskOperand = class(TStringOperand)
  private
    maskValue: TslMask;
  public
    constructor Create(parent: TRuleNode); override;
    function AsText: String; override;
    function Value: TslMask;
    destructor Destroy; override;
    function FeedOperand(const s: String): boolean; override;
  end;

  TListOperand = class(TStringOperand)
  private
    listValue: TStringList;
    procedure Reparse;
  public
    constructor Create(parent: TRuleNode); override;
    function AsText: String; override;
    function Value: TStringList;
    destructor Destroy; override;
    function FeedOperand(const s: String): boolean; override;
  end;

  TCondition = class; //forward

  TConditionOperator = class(TOperator)
    condition: TCondition;
    operand: TOperand;
    function AsText: String; override;
    destructor Destroy; override;
    function FeedOperand(var s: String): boolean; virtual; abstract;
    function AtConditionName: String; override;
  end;

  TBooleanOperator = class(TConditionOperator)
    class function TakeThis(var s: String): boolean; override;
    class function Name: String; override;
    function GetSupplyValue(p: TPazo): boolean;
    function AsText: String; override;
    function Match(p: TPazo): boolean; override;
    function FeedOperand(var s: String): boolean; override;
  end;

  TStringOperator = class(TConditionOperator)
    function GetOperandValue: String;
    function GetSupplyValue(p: TPazo): String;
    function FeedOperand(var s: String): boolean; override;
  end;

  TMultiStringOperator = class(TConditionOperator)
  private
    re: TStringList;
  public
    constructor Create(parent: TRuleNode); override;
    destructor Destroy; override;
    function GetOperandValue: String;
    procedure GetSupplyValues(p: TPazo; re: TStringList);
    function FeedOperand(var s: String): boolean; override;
  end;

  TIntOperator = class(TConditionOperator)
    function GetOperandValue: integer;
    function GetSupplyValue(p: TPazo): integer;
    function FeedOperand(var s: String): boolean; override;
  end;

  TMaskOperator = class(TStringOperator)
    class function Name: String; override;
    function GetOperandValue: TslMask;
    function Match(p: TPazo): boolean; override;
    function FeedOperand(var s: String): boolean; override;
  end;

  TNotMaskOperator = class(TMaskOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TInOperator = class(TStringOperator)
    function GetOperandValue: TStringList;
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
    function FeedOperand(var s: String): boolean; override;
  end;

  TNotInOperator = class(TInOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TStringEqualOperator = class(TStringOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TStringNotEqualOperator = class(TStringEqualOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TMultiStringEqualOperator = class(TMultiStringOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TMultiStringNotEqualOperator = class(TMultiStringEqualOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TMultiInOperator = class(TMultiStringOperator)
    function GetOperandValue: TStringList;
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
    function FeedOperand(var s: String): boolean; override;
  end;

  TMultiNotInOperator = class(TMultiInOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TAtOperator = class(TMultiStringEqualOperator)
    class function Name: String; override;
    function AtConditionName: String; override;
  end;

  TIntEqualOperator = class(TIntOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TIntNotEqualOperator = class(TIntEqualOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TIntBiggerOrEqualThanOperator = class(TIntOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TIntBiggerThanOperator = class(TIntOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TIntLowerThanOperator = class(TIntOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TIntLowerOrEqualThanOperator = class(TIntOperator)
    class function Name: String; override;
    function Match(p: TPazo): boolean; override;
  end;

  TConditionOperatorClass = class of TConditionOperator;

  TCondition = class(TRuleNode)
    acceptedOperators: TClassList;

    function Verify(const s: String): boolean; virtual;
    function AsText: String; override;
    function Match(p: TPazo): boolean; override;
    class function Description: String; virtual; abstract;
    class function AcceptedOperatorsAsText: String;
    function TakesThisOperator(var op: String): TConditionOperatorClass;
    constructor Create(parent: TRuleNode); override;
    destructor Destroy; override;
  end;

  TStringCondition = class(TCondition)
  public
    constructor Create(parent: TRuleNode); override;
    function SupplyValue(r: TPazo): String; virtual; abstract;
  end;

  { Use this if you have a list of values to check and want to use in/notin and mask/notmask
  When using mask, values in list are then separated by a comma
  For instance, values that are a list of languages, like English,Japanese
  you can still use in/notin as in "if imdblanguages not in English then drop"
  but then you can also use a regex : "if imdblanguages !~ /^(Engl|Swed)ish.*/i then DROP" (i only want movies with main language being either English or Swedish )
  }
  TListCondition = class(TStringCondition)
    constructor Create(parent: TRuleNode); override;
    procedure SupplyValues(r: TPazo; re: TStringList); virtual; abstract;
    function SupplyValue(r: TPazo): String; override;
  end;

  TMultiStringCondition = class(TCondition)
    constructor Create(parent: TRuleNode); override;
    procedure SupplyValues(r: TPazo; re: TStringList); virtual; abstract;
  end;

  TIntCondition = class(TCondition)
    constructor Create(parent: TRuleNode); override;
    function SupplyValue(r: TPazo): integer; virtual; abstract;
  end;

  TBooleanCondition = class(TCondition)
    constructor Create(parent: TRuleNode); override;
    function SupplyValue(r: TPazo): boolean; virtual; abstract;
  end;

  TAtCondition = class(TMultiStringCondition)
    constructor Create(parent: TRuleNode); override;
  end;

  TConditionClass = class of TCondition;

  {
  @value(raDrop Rule says we should drop, so we will drop it)
  @value(raAllow Rule says we accept/allow it, so we will allow it)
  @value(raDontmatch No matching rule, will be dropped)
  }
  TRuleAction = (raDrop, raAllow, raDontmatch);

  TRule = class
    sitename: String;
    section: String;
    conditions: TRuleNode;
    action: TRuleAction;
    error: String;

    function Execute(r: TPazo): TRuleAction;

    function AsText(const includeSitesection: boolean): String;
    procedure Reparse(rule: String);
    constructor Create(const rule: String);
    destructor Destroy; override;
  end;

  {
  Used to give info about rules including its global rule ID.
  }
  TRuleWithID = class
    ID: integer;
    FRule: TRule;

    constructor Create(const aID: integer; const aRule: TRule);
  end;

procedure RulesRemove(const sitename, section: String);
procedure RulesSave;
procedure RulesStart;
procedure RulesReload;
function AddRule(const rule: String; var error: String): TPair<TRule, integer>;
function FireRuleSet(p: TPazo; ps: TPazoSite): TRuleAction;
function FireRules(p: TPazo; ps: TPazoSite): boolean;
procedure RulesInit;
procedure RulesUninit;
function RuleMod(const aID: integer; const aRule: string; out aMessage: string): boolean;
function RuleIns(const aID: integer; const aRule: string; out aMessage: string): boolean;
function RuleDel(const aID: integer; out aMessage: string): boolean;
function FindRules(const aSitename: string; const aSections: TStringList): TObjectList<TRuleWithID>;
function FindIrcRules(const sitename, section: string): TList<String>;
function RuleCopy(const aSrcSite, aDestSite, aSrcSection, aDestSection: string): string;
function GetRuleCount(const aRtpl: boolean): integer;

function FindConditionClassByName(const Name: String): TConditionClass;

var
  conditions: TClassList;

implementation

uses
  SysUtils, Math, DateUtils, IdGlobal, {$IFDEF MSWINDOWS}Windows,{$ENDIF} Types, configunit, sitesunit, mystrings, encinifile, debugunit,
  ruleconditions.common, ruleconditions.zeroday, ruleconditions.mp3, ruleconditions.tv, ruleconditions.imdb, ruleconditions.mvid, ruleconditions.nfo;

const
  dsection = 'rules';

type
  TPrefixOperatorClass = class of TPrefixOperator;
  TInfixOperatorClass = class of TInfixOperator;

var
  prefixops: TClassList;
  infixops: TClassList;
  rules: TDictionary<string, TDictionary<string, TObjectList<TRule>>>; //< key: sitename; inner key: section name
  rtpl: TDictionary<string, TDictionary<string, TObjectList<TRule>>>; //< pretty sure, we don't actually need this anymore
  split_site_data: boolean;

{ TInfixOperator }

function TInfixOperator.AsText: String;
begin
  try
    Result := left.AsText + ' ' + Name + ' ' + right.AsText;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TInfixOperator.AsText : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

function TInfixOperator.AtConditionName: String;
begin
  try
    Result := left.AtConditionName;
    if Result = '' then
      Result := right.AtConditionName;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TInfixOperator.AtConditionName : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

destructor TInfixOperator.Destroy;
begin
  if left <> nil then
  begin
    left.Free;
    left := nil;
  end;
  if right <> nil then
  begin
    right.Free;
    right := nil;
  end;
  inherited;
end;

{ TPrefixOperator }

function TPrefixOperator.AsText: String;
begin
  try
    Result := Name + ' ' + child.AsText;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TPrefixOperator.AsText : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

function TPrefixOperator.AtConditionName: String;
begin
  try
    Result := child.AtConditionName;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TPrefixOperator.AtConditionName : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

destructor TPrefixOperator.Destroy;
begin
  if child <> nil then
  begin
    child.Free;
    child := nil;
  end;
  inherited;
end;

procedure TPrefixOperator.SetupChild(child: TRuleNode);
begin
  self.child := child;
end;

{ TBracketOperator }

function TOpeningBracketOperator.AsText: String;
begin
  try
    Result := '( ' + child.AsText + ' )';
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TOpeningBracketOperator.AsText : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

destructor TOpeningBracketOperator.Destroy;
begin
  if child <> nil then
  begin
    child.Free;
    child := nil;
  end;
  inherited;
end;

function TOpeningBracketOperator.Match(p: TPazo): boolean;
begin
  try
    Result := child.Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TOpeningBracketOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TOpeningBracketOperator.Name: String;
begin
  Result := '(';
end;

{ TOrOperator }

function TOrOperator.Match(p: TPazo): boolean;
begin
  try
    Result := left.Match(p) or right.Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TOrOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TOrOperator.Name: String;
begin
  Result := '||';
end;

{ TAndOperator }

function TAndOperator.Match(p: TPazo): boolean;
begin
  try
    Result := left.Match(p) and right.Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TAndOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TAndOperator.Name: String;
begin
  Result := '&&';
end;

{ TNoOperator }

function TBooleanOperator.AsText: String;
begin
  try
    Result := condition.AsText;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TBooleanOperator.AsText : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

function TBooleanOperator.FeedOperand(var s: String): boolean;
begin
  Result := False; // ez specialis, nem kell neki semmilyen operandus
end;

function TBooleanOperator.GetSupplyValue(p: TPazo): boolean;
begin
  try
    Result := TBooleanCondition(condition).SupplyValue(p);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TBooleanOperator.GetSupplyValue : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

function TBooleanOperator.Match(p: TPazo): boolean;
begin
  try
    Result := GetSupplyValue(p);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TBooleanOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TBooleanOperator.Name: String;
begin
  Result := '';
end;

{ TInOperator }

function TInOperator.FeedOperand(var s: String): boolean;
begin
  if operand = nil then
    operand := TListOperand.Create(self);

  Result := TListOperand(operand).FeedOperand(s);
  if Result then
    s := '';
end;

function TInOperator.GetOperandValue: TStringList;
begin
  try
    Result := TListOperand(operand).Value;
  except
    on e: Exception do
    begin
      Result := nil;
      Debug(dpError, dsection, 'TInOperator.GetOperandValue : %s', [e.Message]);
    end;
  end;
end;

function TInOperator.Match(p: TPazo): boolean;
begin
  try
    Result := GetOperandValue.IndexOf(GetSupplyValue(p)) <> -1;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TInOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TInOperator.Name: String;
begin
  Result := 'in';
end;

{ TNotInOperator }

function TNotInOperator.Match(p: TPazo): boolean;
begin
  try
    Result := not inherited Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TNotInOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TNotInOperator.Name: String;
begin
  Result := 'notin';
end;

{ TStringEqualOperator }

function TStringEqualOperator.Match(p: TPazo): boolean;
begin
  try
    Result := AnsiSameText(GetOperandValue, GetSupplyValue(p));
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TStringEqualOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TStringEqualOperator.Name: String;
begin
  Result := '=';
end;

{ TStringNotEqualOperator }

function TStringNotEqualOperator.Match(p: TPazo): boolean;
begin
  try
    Result := not inherited Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TStringNotEqualOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TStringNotEqualOperator.Name: String;
begin
  Result := '!=';
end;

{ TIntEqualOperator }

function TIntEqualOperator.Match(p: TPazo): boolean;
var
  vr: integer;
begin
  try
    vr := CompareValue(GetSupplyValue(p), GetOperandValue);
    if vr = 0 then
      Result := True
    else
      Result := False;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TIntEqualOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TIntEqualOperator.Name: String;
begin
  Result := '=';
end;

{ TIntNotEqualOperator }

function TIntNotEqualOperator.Match(p: TPazo): boolean;
var
  vr: integer;
begin
  try
    vr := CompareValue(GetSupplyValue(p), GetOperandValue);
    if vr <> 0 then
      Result := True
    else
      Result := False;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TIntNotEqualOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TIntNotEqualOperator.Name: String;
begin
  Result := '!=';
end;

{ TIntBiggerOrEqualThanOperator }

function TIntBiggerOrEqualThanOperator.Match(p: TPazo): boolean;
var
  vr: integer;
begin
  try
    vr := CompareValue(GetSupplyValue(p), GetOperandValue);
    if ((vr = 0) or (vr = 1)) then
      Result := True
    else
      Result := False;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TIntBiggerOrEqualThanOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TIntBiggerOrEqualThanOperator.Name: String;
begin
  Result := '>=';
end;

{ TIntBiggerThanOperator }

function TIntBiggerThanOperator.Match(p: TPazo): boolean;
var
  vr: integer;
begin
  try
    vr := CompareValue(GetSupplyValue(p), GetOperandValue);
    if vr = 1 then
      Result := True
    else
      Result := False;

  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TIntBiggerThanOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TIntBiggerThanOperator.Name: String;
begin
  Result := '>';
end;

{ TIntLowerThanOperator }

function TIntLowerThanOperator.Match(p: TPazo): boolean;
var
  vr: integer;
begin
  try
    vr := CompareValue(GetSupplyValue(p), GetOperandValue);
    if vr = -1 then
      Result := True
    else
      Result := False;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TIntLowerThanOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TIntLowerThanOperator.Name: String;
begin
  Result := '<';
end;

{ TIntLowerOrEqualThanOperator }

function TIntLowerOrEqualThanOperator.Match(p: TPazo): boolean;
var
  vr: integer;
begin
  try
    vr := CompareValue(GetSupplyValue(p), GetOperandValue);
    if ((vr = 0) or (vr = -1)) then
      Result := True
    else
      Result := False;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TIntLowerOrEqualThanOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TIntLowerOrEqualThanOperator.Name: String;
begin
  Result := '<=';
end;

{ TCondition }

class function TCondition.AcceptedOperatorsAsText: String;
var
  i: integer;
  c: TCondition;
begin
  c := self.Create(nil);
  try
    Result := '';
    for i := 0 to c.acceptedOperators.Count - 1 do
      Result := Result + TConditionOperatorClass(c.acceptedOperators[i]).Name + ' ';

    Result := Trim(Result);
  finally
    c.Free;
  end;
end;

function TCondition.AsText: String;
begin
  Result := Name;
end;

constructor TCondition.Create(parent: TRuleNode);
begin
  inherited;
  acceptedOperators := TClassList.Create;
end;

destructor TCondition.Destroy;
begin
  acceptedOperators.Free;
  inherited;
end;

function TCondition.Verify(const s: String): boolean;
begin
  Result := True;
end;

function TCondition.Match(p: TPazo): boolean;
begin
  Result := False; // exception
end;

function TCondition.TakesThisOperator(var op: String): TConditionOperatorClass;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to acceptedOperators.Count - 1 do
    if TConditionOperatorClass(acceptedOperators[i]).TakeThis(op) then
    begin
      Result := TConditionOperatorClass(acceptedOperators[i]);
      exit;
    end;
end;

{ TMaskOperator }

function TMaskOperator.FeedOperand(var s: String): boolean;
begin
  if operand = nil then
    operand := TMaskOperand.Create(self);

  Result := TMaskOperand(operand).FeedOperand(s);
  if Result then
    s := '';
end;

function TMaskOperator.GetOperandValue: TslMask;
begin
  try
    Result := TMaskOperand(operand).Value;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TMaskOperator.GetOperandValue : %s', [e.Message]);
      Result := nil;
    end;
  end;
end;

function TMaskOperator.Match(p: TPazo): boolean;
begin
  try
    Result := GetOperandValue.Matches(GetSupplyValue(p));
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TMaskOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TMaskOperator.Name: String;
begin
  Result := '=~';
end;

{ TNotMaskOperator }

function TNotMaskOperator.Match(p: TPazo): boolean;
begin
  try
    Result := not inherited Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TNotMaskOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TNotMaskOperator.Name: String;
begin
  Result := '!~';
end;

{ TRuleNode }

constructor TRuleNode.Create(parent: TRuleNode);
begin
  inherited Create;
  self.parent := parent;
  if parent <> nil then
    parent.SetupChild(self);
end;

class function TRuleNode.TakeThis(var s: String): boolean;
begin
  Result := False;
  if s = Name then
  begin
    Result := True;
    s := '';
  end;
end;

{ TStringOperand }

procedure TRuleNode.SetupChild(child: TRuleNode);
begin
  // nothing here.
end;

{ TStringOperand }

function TStringOperand.AsText: String;
begin
  Result := Value;
end;

function TStringOperand.FeedOperand(const s: String): boolean;
begin
  if s = '' then
  begin
    Result := True;
    exit;
  end;

  if not skipVerification then
  begin
    Result := TConditionOperator(parent).condition.Verify(s);
    if not Result then
      exit;
  end
  else
    Result := True;

  if fValue <> '' then
    fValue := fValue + ' ' + s
  else
    fValue := s;
end;

function TStringOperand.Value: String;
begin
  Result := fValue;
end;

{ TIntOperand }

function TIntOperand.AsText: String;
begin
  Result := IntToStr(Value);
end;

function TIntOperand.FeedOperand(const s: String): boolean;
begin
  Result := False;
  fValue := StrToIntDef(s, -123717283);
  if fValue <> -123717283 then
    Result := True; // integer nem bovitheto szavankent
end;

function TIntOperand.Value: integer;
begin
  Result := fValue;
end;

{ TListOperand }

function TListOperand.AsText: String;
var
  i: integer;
begin
  Result := '';
  try
    for i := 0 to listValue.Count - 1 do
    begin
      Result := Result + listValue[i];
      if (i <> listValue.Count - 1) then
        Result := Result + ', ';
    end;
  except
    Result := '';
  end;
end;

constructor TListOperand.Create(parent: TRuleNode);
begin
  inherited Create(parent);
  listValue := TStringList.Create;
  listValue.CaseSensitive := False;

  Reparse;
end;

destructor TListOperand.Destroy;
begin
  listValue.Free;
  inherited;
end;

function TListOperand.FeedOperand(const s: String): boolean;
var
  l: integer;
begin
  Result := False;
  try
    if s = '' then
      exit;
    l := length(s);
    if s[l] = ',' then
    begin
      Result := inherited FeedOperand(Copy(s, 1, l - 1));
      if Result then
        fValue := fValue + ',';
    end
    else
      Result := inherited FeedOperand(s);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TListOperand.FeedOperand : %s', [e.Message]);
      fValue := '';
      Result := False;
    end;
  end;

  Reparse;
end;

procedure TListOperand.Reparse;
var
  s, fs: String;
  operand_read: integer;
begin
  listValue.Clear;
  try
    fs := fValue;
    operand_read := 0;
    while (True) do
    begin
      if (operand_read > 100) then
      begin
        Debug(dpError, dsection, '[ERROR] TListOperand.Reparse count break', []);
        break;
      end;
      Inc(operand_read);
      s := Trim(Fetch(fs, ',', True, False));
      if s = '' then
        Break;
      if s <> '' then
      begin
        listValue.Add(s);
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TListOperand.Reparse : %s', [e.Message]);
      listValue.Clear;
    end;
  end;
end;

function TListOperand.Value: TStringList;
begin
  Result := listValue;
end;

{ TMaskOperand }

function TMaskOperand.AsText: String;
begin
  Result := fValue;
end;

constructor TMaskOperand.Create(parent: TRuleNode);
begin
  inherited;
  skipVerification := True;
end;

destructor TMaskOperand.Destroy;
begin
  if maskValue <> nil then
    maskValue.Free;
  inherited;
end;

function TMaskOperand.FeedOperand(const s: String): boolean;
begin
  Result := inherited FeedOperand(s);
  if maskValue <> nil then
    maskValue.Free;
  maskValue := TslMask.Create(fValue);
end;

function TMaskOperand.Value: TslMask;
begin
  Result := maskValue;
end;

{ TStringOperator }

function TStringOperator.FeedOperand(var s: String): boolean;
begin
  if operand = nil then
    operand := TStringOperand.Create(self);

  Result := TStringOperand(operand).FeedOperand(s);
  if Result then
    s := '';
end;

function TStringOperator.GetOperandValue: String;
begin
  try
    Result := TStringOperand(operand).Value;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TStringOperator.GetOperandValue : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

function TStringOperator.GetSupplyValue(p: TPazo): String;
begin
  try
    Result := TStringCondition(condition).SupplyValue(p);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TStringOperator.GetSupplyValue : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

{ TMultiStringOperator }

constructor TMultiStringOperator.Create(parent: TRuleNode);
begin
  inherited;

  re := TStringList.Create;
  re.CaseSensitive := False;
end;

destructor TMultiStringOperator.Destroy;
begin
  re.Free;

  inherited;
end;

function TMultiStringOperator.FeedOperand(var s: String): boolean;
begin
  if operand = nil then
    operand := TStringOperand.Create(self);

  Result := TStringOperand(operand).FeedOperand(s);
  if Result then
    s := '';
end;

function TMultiStringOperator.GetOperandValue: String;
begin
  try
    Result := TStringOperand(operand).Value;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TMultiStringOperator.GetOperandValue : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

procedure TMultiStringOperator.GetSupplyValues(p: TPazo; re: TStringList);
begin
  try
    if (condition is TListCondition) then
      TListCondition(condition).SupplyValues(p, re)
    else
      TMultiStringCondition(condition).SupplyValues(p, re);
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TMultiStringOperator.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TIntOperator }

function TIntOperator.FeedOperand(var s: String): boolean;
begin
  if operand = nil then
    operand := TIntOperand.Create(self);

  Result := TIntOperand(operand).FeedOperand(s);
  if Result then
    s := '';
end;

function TIntOperator.GetOperandValue: integer;
begin
  try
    Result := TIntOperand(operand).Value;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TIntOperator.GetOperandValue : %s', [e.Message]);
      Result := 0;
    end;
  end;
end;

function TIntOperator.GetSupplyValue(p: TPazo): integer;
begin
  try
    Result := TIntCondition(condition).SupplyValue(p);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TIntOperator.GetSupplyValue : %s', [e.Message]);
      Result := 0;
    end;
  end;
end;

{ TIntCondition }

constructor TIntCondition.Create(parent: TRuleNode);
begin
  inherited;

  acceptedOperators.Add(TIntEqualOperator);
  acceptedOperators.Add(TIntNotEqualOperator);
  acceptedOperators.Add(TIntBiggerOrEqualThanOperator);
  acceptedOperators.Add(TIntBiggerThanOperator);
  acceptedOperators.Add(TIntLowerThanOperator);
  acceptedOperators.Add(TIntLowerOrEqualThanOperator);
end;

{ TStringCondition }

constructor TStringCondition.Create(parent: TRuleNode);
begin
  inherited;

  acceptedOperators.Add(TStringEqualOperator);
  acceptedOperators.Add(TStringNotEqualOperator);
  acceptedOperators.Add(TMaskOperator);
  acceptedOperators.Add(TNotMaskOperator);
  acceptedOperators.Add(TInOperator);
  acceptedOperators.Add(TNotInOperator);
end;

{ TBooleanCondition }

constructor TBooleanCondition.Create(parent: TRuleNode);
begin
  inherited;

  acceptedOperators.Add(TBooleanOperator);
end;

{ TConditionOperator }

function TConditionOperator.AsText: String;
begin
  Result := condition.AsText + ' ' + Name + ' ' + operand.AsText;
end;

function TConditionOperator.AtConditionName: String;
begin
  Result := '';
end;

destructor TConditionOperator.Destroy;
begin
  if condition <> nil then
  begin
    condition.Free;
    condition := nil;
  end;
  if operand <> nil then
  begin
    operand.Free;
    operand := nil;
  end;
  inherited;
end;

{ TNotOperator }

function TNotOperator.Match(p: TPazo): boolean;
begin
  try
    Result := not child.Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TNotOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TNotOperator.Name: String;
begin
  Result := 'not';
end;

{ TOperand }

function TOperand.AtConditionName: String;
begin
  Result := '';
end;

function TOperand.Match(p: TPazo): boolean;
begin
  Result := False; // exception
end;

class function TOperand.Name: String;
begin
  Result := 'operand';
end;

class function TBooleanOperator.TakeThis(var s: String): boolean;
begin
  Result := True;
  // we are not resetting s, it needs further processing
end;

{ TSuffixOperator }

class function TClosingBracketOperator.Name: String;
begin
  Result := ')';
end;

function mySpeedComparer(List: TStringList; Index1, Index2: integer): integer;
begin
  try
    Result :=
      CompareValue(StrToIntDef(list.ValueFromIndex[index2], 0),
      StrToIntDef(list.ValueFromIndex[index1], 0));
  except
    Result := 0;
  end;
end;

function FireRuleSetB(p: TPazo; ps: TPazoSite; const sitenametomatch, sectiontomatch: String): TRuleAction;
var
  ra: TRuleAction;
  fRulesForSite: TDictionary<string, TObjectList<TRule>>;
  fRules: TObjectList<TRule>;
  fRule: TRule;
begin
  Result := raDontmatch;
  ra := Result;
  try
    if rtpl.TryGetValue(sitenametomatch, fRulesForSite) and fRulesForSite.TryGetValue(sectiontomatch, fRules) then
    begin
      for fRule in fRules do
      begin
        try
          ra := fRule.Execute(p);
        except
          on e: Exception do
          begin
            Debug(dpError, dsection, Format('[EXCEPTION] FireRuleSetB(rtpl) r.Execute: %s, %s', [e.Message, fRule.AsText(True)]));
            Result := raDontmatch;
            exit;
          end;
        end;

        if ra = raDrop then
        begin
          ps.reason := fRule.AsText(True);
          Result := raDrop;
          exit;
        end;

        if ra = raAllow then
        begin
          ps.reason := fRule.AsText(True);
          Result := raAllow;
          exit;
        end;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] FireRuleSetB rtpl: %s', [e.Message]));
      Result := raDontmatch;
      exit;
    end;
  end;

  try
    if rules.TryGetValue(sitenametomatch, fRulesForSite) and fRulesForSite.TryGetValue(sectiontomatch, fRules) then
    begin
      for fRule in fRules do
      begin
        try
          ra := fRule.Execute(p);
        except
          on e: Exception do
          begin
            Debug(dpError, dsection, Format('[EXCEPTION] FireRuleSetB(rules) r.Execute: %s, %s', [e.Message, fRule.AsText(True)]));
            Result := raDontmatch;
            exit;
          end;
        end;

        if ra = raDrop then
        begin
          ps.reason := fRule.AsText(True);
          Result := raDrop;
          exit;
        end;

        if ra = raAllow then
        begin
          ps.reason := fRule.AsText(True);
          Result := raAllow;
          exit;
        end;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] FireRuleSetB rules: %s', [e.Message]));
      Result := raDontmatch;
      exit;
    end;
  end;
end;

function FireRuleSet(p: TPazo; ps: TPazoSite): TRuleAction;
begin

  try
    // first of all, we have the full generic rules
    Result := FireRuleSetB(p, ps, '*', '*');
    if Result <> raDontMatch then
    begin
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] FireRuleSetB * *: %s', [e.Message]));
      Result := raDontmatch;
      exit;
    end;
  end;

  try
    // let's look at section global rules
    Result := FireRuleSetB(p, ps, '*', p.rls.section);
    if Result <> raDontMatch then
    begin
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] FireRuleSetB * section: %s', [e.Message]));
      Result := raDontmatch;
      exit;
    end;
  end;

  try
    // check out the site's global rules
    Result := FireRuleSetB(p, ps, ps.Name, '*');
    if Result <> raDontMatch then
    begin
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] FireRuleSetB site *: %s', [e.Message]));
      Result := raDontmatch;
      exit;
    end;
  end;

  try
    // let's check the section's rules
    Result := FireRuleSetB(p, ps, ps.Name, p.rls.section);
    if Result <> raDontMatch then
    begin
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] FireRuleSetB site section: %s', [e.Message]));
      Result := raDontmatch;
      exit;
    end;
  end;

  // we're going to drop it, there was no matching rule
  Result := raDontMatch;
  if ps.reason = '' then
    ps.reason := 'No matching rule';

end;

function CalculateRank(const aDestSite: TSite; const aSpeedFrom: integer; const aSection: string; const aIsPre: boolean): integer;
var
  fCalculatedRank: integer;
begin
  //reduce speed stats weight - multiply ranks by 10, then the speedstats can't change the rank, but still change order within the same rank
  if (aDestSite.ReducedSpeedstatWeight) then
    fCalculatedRank := aSpeedFrom + aDestSite.GetRank(aSection) * 10
  else
    fCalculatedRank := aSpeedFrom * aDestSite.GetRank(aSection); //normal calculation

  if (aIsPre) then
    fCalculatedRank := Result + 100;

  Result := fCalculatedRank;
end;

function FireRules(p: TPazo; ps: TPazoSite): boolean;
var
  dstps: TPazoSite;
  y: TStringList;
  i: integer;
  ps_s, dstps_s: TSite;
begin
  Result := False;

  if (not Assigned(ps) or (ps = nil)) then
    exit;

  if ps.error then
    exit;

  ps_s := FindSiteByName('', ps.Name);
  if ps_s = nil then
    exit;
  if ps_s.PermDown then
    exit;
  if not (ps_s.WorkingStatus in [sstUnknown, sstUp]) then
    exit;

  p.srcsite := ps.Name;
  Debug(dpSpam, dsection, '-> ' + Format('%s: %s %s', [ps.Name, p.rls.section, p.rls.rlsname]));

  y := TStringList.Create;
  try
    y.Assign(ps.speed_from);

    for i := 0 to y.Count - 1 do
    begin
      try
        if i > y.Count then
          Break;
      except
        Break;
      end;

      try
        dstps := p.FindSite(y.Names[i]);
        if dstps = nil then
          Continue;

        if (dstps.Name <> ps.Name) then
        begin
          if (dstps.StatusRealPreOrShouldPre) then
          begin
            if (dstps.reason = '') then
              dstps.reason := 'Affil';
            Continue;
          end;

          if dstps.error then
            Continue;

          dstps_s := FindSiteByName('', dstps.Name);
          if dstps_s = nil then
            Continue;

          if (not (dstps_s.WorkingStatus in [sstUnknown, sstUp])) or (dstps_s.PermDown) then
          begin
            if (dstps.reason = '') then
              dstps.reason := 'Down';
            Continue;
          end;

          p.dstsite := dstps.Name;
          // i'm allowed to e ...
          if ((dstps.status in [rssAllowed]) or (FireRuleSet(p, dstps) = raAllow)) then
          begin
            Result := ps.AddDestination(dstps, CalculateRank(dstps_s, StrToIntDef(y.ValueFromIndex[i], 1), p.rls.section, ps.status in [rssShouldPre, rssRealPre]));
          end;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, dsection, Format('[EXCEPTION] FireRules loop: %s', [e.Message]));
          Result := False;
          Break;
        end;
      end;
    end;
    Debug(dpSpam, dsection, '<- ' + Format('%s: %s %s', [ps.Name, p.rls.section, p.rls.rlsname]));
  finally
    y.Free;
  end;
end;

procedure RulesSave;
var
  i: integer;
  fEncStringlist: TEncStringlist;
  fRulesPath: String;
  fRulesBySite: TDictionary<string, TStringList>;
  fSiteRules: TPair<string, TStringList>;
  fRule: TRule;
  fRulesPerSiteKVP: TPair<String, TDictionary<string, TObjectList<TRule>>>;
  fRulesPerSectionKVP: TPair<String, TObjectList<TRule>>;
begin
  if split_site_data then
  begin
    fRulesPath := ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim;
    fRulesBySite := TDictionary<string, TStringList>.Create;
    try
      for fRulesPerSiteKVP in rules do
      begin
        for fRulesPerSectionKVP in fRulesPerSiteKVP.Value do
        begin
          for fRule in fRulesPerSectionKVP.Value do
          begin
            if not fRulesBySite.ContainsKey(fRule.sitename) then
              fRulesBySite.Add(fRule.sitename, TStringList.Create);

            fRulesBySite[fRule.sitename].Add(fRule.AsText(True));
          end;
        end;
      end;

      for fSiteRules in fRulesBySite do
      begin
        if fSiteRules.Key = '*' then
          fSiteRules.Value.SaveToFile(fRulesPath + getAdminSiteName + '.rtpl')
        else
          fSiteRules.Value.SaveToFile(fRulesPath + fSiteRules.Key + '.rtpl');

        fSiteRules.Value.Free;
      end;
    finally
      fRulesBySite.Free;
    end;
  end
  else
  begin
    fEncStringlist := TEncStringlist.Create(passphrase);
    try
      for fRulesPerSiteKVP in rules do
      begin
        for fRulesPerSectionKVP in fRulesPerSiteKVP.Value do
        begin
          for fRule in fRulesPerSectionKVP.Value do
            fEncStringlist.Add(fRule.AsText(True));
        end;
      end;
      fEncStringlist.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.rules');
    finally
      fEncStringlist.Free;
    end;
  end;
end;

procedure RulesRemove(const sitename, section: String);
var
  fRulesPerSite: TDictionary<String, TObjectList<TRule>>;
  fRulesPerSectionKVP: TPair<String, TObjectList<TRule>>;
  fKeysToDelete: TList<String>;
  fKey: String;
begin
  if rules.TryGetValue(sitename, fRulesPerSite) then
  begin
    fKeysToDelete := TList<String>.Create;
    try
      for fRulesPerSectionKVP in fRulesPerSite do
      begin
        if (fRulesPerSectionKVP.Value.Count > 0)
          and ((section = '') or (fRulesPerSectionKVP.Key = section)) then
            fKeysToDelete.Add(fRulesPerSectionKVP.Key);
      end;

      for fKey in fKeysToDelete do
      begin
        fRulesPerSite[fKey].Free;
        fRulesPerSite.Remove(fKey);
      end;

      if fRulesPerSite.Count = 0 then
      begin
        rules.Remove(sitename);
      end;
    finally
      fKeysToDelete.Free;
    end;

    RulesSave;
  end;
end;

{ Returns the global ID for the given rule }
function GetGlobalIdForRule(const aRule: TRule): integer;
var
  fRulesPerSite: TPair<String, TDictionary<string, TObjectList<TRule>>>;
  fRulesPerSection: TPair<String, TObjectList<TRule>>;
  fRule: TRule;
begin
  Result := 0;

  // iterate all sites/sections and count the rules until we find the given one
  for fRulesPerSite in rules do
  begin
    for fRulesPerSection in fRulesPerSite.Value do
    begin
      for fRule in fRulesPerSection.Value do
      begin
        if fRule = aRule then
          exit;

        Result := Result + 1;
      end;
    end;
  end;
end;

{ Creates a TRule object from the given rule string and adds it to the list. Returns the TRule object and its global rule ID }
function DoAddRule(const rule: String; var error: String; const aNotAddToRtpl: boolean): TPair<TRule, integer>;
var
  r: TRule;
  fRulesPerSite: TDictionary<string, TObjectList<TRule>>;
begin
  error := '';
  Result := TPair<TRule, integer>.Create(nil, -1);

  r := TRule.Create(rule);
  if r.error <> '' then
  begin
    error := r.error;
    r.Free;
  end
  else
  begin
    if r <> nil then
    begin
      Result.Key := r;
      if aNotAddToRtpl then
      begin
        if not rules.ContainsKey(r.sitename) then
          rules.add(r.sitename, TDictionary<string, TObjectList<TRule>>.Create);

        fRulesPerSite := rules[r.sitename];
        if not fRulesPerSite.ContainsKey(r.section) then
        begin
          fRulesPerSite.Add(r.section, TObjectList<TRule>.Create);
        end;

        fRulesPerSite[r.section].Add(r);
        Result.Value := GetGlobalIdForRule(r);
      end
      else
      begin
        if not rtpl.ContainsKey(r.sitename) then
          rtpl.add(r.sitename, TDictionary<string, TObjectList<TRule>>.Create);

        fRulesPerSite := rtpl[r.sitename];
        if not fRulesPerSite.ContainsKey(r.section) then
        begin
          fRulesPerSite.Add(r.section, TObjectList<TRule>.Create);
        end;

        fRulesPerSite[r.section].Add(r);
        // I think we don't need the ID for the rtpl case
        Result.Value := -1;
      end;
    end
  end;
end;

function AddRule(const rule: String; var error: String): TPair<TRule, integer>;
begin
  Result := DoAddRule(rule, error, True);
  RulesSave;
end;

function RuleMod(const aID: integer; const aRule: string; out aMessage: string): boolean;
var
  fNewRule, fOldRule: TRule;
  fOldRuleText: string;
  fLocalID: integer;
  fRulesPerSite: TDictionary<string, TObjectList<TRule>>;
  fRulesPerSection: TObjectList<TRule>;
begin
  Result := False;

  fNewRule := TRule.Create(aRule);
  if fNewRule.error <> '' then
  begin
    aMessage := fNewRule.error;
    fNewRule.Free;
    exit;
  end;

  if aID < 0 then
  begin
    aMessage := Format('Invalid rule ID for site %s, section %s', [fNewRule.sitename, fNewRule.section]);
    exit;
  end;

  if not rules.TryGetValue(fNewRule.sitename, fRulesPerSite) or not fRulesPerSite.TryGetValue(fNewRule.section, fRulesPerSection) then
  begin
    aMessage := 'Cannot change section or site via rulemod. Please use ruledel and ruleadd/ruleins';
    exit;
  end;

  fLocalID := -1;
  for fOldRule in fRulesPerSection do
  begin
    if GetGlobalIdForRule(fOldRule) = aID then
    begin
      fLocalID := fRulesPerSection.IndexOf(fOldRule);
      break;
    end;
  end;

  if fLocalID = -1 then
  begin
    aMessage := 'Cannot change section or site via rulemod. Please use ruledel and ruleadd/ruleins';
    exit;
  end;

  // the old rule object will be freed when removing it from the list, so store its text here.
  fOldRuleText := fOldRule.AsText(True);
  fRulesPerSection.Delete(fLocalID);
  fRulesPerSection.Insert(fLocalID, fNewRule);
  RulesSave;

  aMessage := Format('<b>Modified<b>: %d %s <u><b>to</b></u> %s', [aID, fOldRuleText, fNewRule.AsText(True)]);
  Result := True;
end;

function RuleIns(const aID: integer; const aRule: string; out aMessage: string): boolean;
var
  fNewRule, fOldRule: TRule;
  fLocalID: integer;
  fRulesPerSite: TDictionary<string, TObjectList<TRule>>;
begin
  Result := False;

  fNewRule := TRule.Create(aRule);
  if fNewRule.error <> '' then
  begin
    aMessage := fNewRule.error;
    fNewRule.Free;
    exit;
  end;

  if aID < 0 then
  begin
    aMessage := Format('Invalid rule ID for site %s, section %s', [fNewRule.sitename, fNewRule.section]);
    exit;
  end;

  if not rules.TryGetValue(fNewRule.sitename, fRulesPerSite) then
  begin
    fRulesPerSite := TDictionary<string, TObjectList<TRule>>.Create;
    rules.add(fNewRule.sitename, fRulesPerSite);
  end;

  if not fRulesPerSite.ContainsKey(fNewRule.section) then
  begin
    fRulesPerSite.Add(fNewRule.section, TObjectList<TRule>.Create);
  end;

  fLocalID := -1;
  if (fRulesPerSite[fNewRule.section].Count = 0) or
    (GetGlobalIdForRule(fRulesPerSite[fNewRule.section][0]) > aID) then
  begin
    // no rules yet for this site and section. Put at first position.
    fLocalID := 0;
  end
  else if GetGlobalIdForRule(fRulesPerSite[fNewRule.section][fRulesPerSite[fNewRule.section].Count - 1]) < aID then
  begin
    // the highest rule of this site and section has a lower global ID as the one given. Put the new rule at the end.
    fLocalID := fRulesPerSite[fNewRule.section].Count;
  end
  else
  begin
    // find the ID within the site and section's list (local ID)
    for fOldRule in fRulesPerSite[fNewRule.section] do
    begin
      if GetGlobalIdForRule(fOldRule) = aID then
      begin
        fLocalID := fRulesPerSite[fNewRule.section].IndexOf(fOldRule);
        break;
      end;
    end;
  end;

  fRulesPerSite[fNewRule.section].Insert(fLocalID, fNewRule);
  RulesSave;

  if aID <> GetGlobalIdForRule(fNewRule) then
    aMessage := Format('<b>Inserted (ADJUSTED RULE ID!)<b>: %d %s', [GetGlobalIdForRule(fNewRule), fNewRule.AsText(True)])
  else
    aMessage := Format('<b>Inserted<b>: %d %s', [aID, fNewRule.AsText(True)]);

  Result := True;
end;

function RuleDel(const aID: integer; out aMessage: string): boolean;
var
  fRulesPerSite: TPair<String, TDictionary<string, TObjectList<TRule>>>;
  fRulesPerSection: TPair<String, TObjectList<TRule>>;
  fRule: TRule;
  fGlobalIndex: integer;
begin
  Result := False;
  fGlobalIndex := 0;

  if aID < 0 then
  begin
    aMessage := 'Invalid rule ID';
    exit;
  end;

  for fRulesPerSite in rules do
  begin
    for fRulesPerSection in fRulesPerSite.Value do
    begin
      for fRule in fRulesPerSection.Value do
      begin
        if fGlobalIndex = aID then
        begin
          aMessage := Format('<c4><b>Deleted</b></c>: <b>%d</b> %s', [aID, fRule.AsText(true)]);
          fRulesPerSection.Value.Delete(fRulesPerSection.Value.IndexOf(fRule));
          RulesSave;
          Result := True;
          exit;
        end;

        fGlobalIndex := fGlobalIndex + 1;
      end;
    end;
  end;

  aMessage := 'Invalid rule ID';
end;

function FindRules(const aSitename: string; const aSections: TStringList): TObjectList<TRuleWithID>;
var
  fRulesPerSite: TPair<String, TDictionary<string, TObjectList<TRule>>>;
  fRulesPerSection: TPair<String, TObjectList<TRule>>;
  fRule: TRule;
  fGlobalIndex: integer;
begin
  Result := TObjectList<TRuleWithID>.Create(true);
  fGlobalIndex := 0;

  for fRulesPerSite in rules do
  begin
    for fRulesPerSection in fRulesPerSite.Value do
    begin
      for fRule in fRulesPerSection.Value do
      begin
        if ((aSitename = '*') or (fRule.sitename = aSitename)) and
          ((aSections.Count = 0) or (aSections.IndexOf(fRule.section) <> -1)) then
        begin
          Result.Add(TRuleWithID.Create(fGlobalIndex, fRule));
        end;

        fGlobalIndex := fGlobalIndex + 1;
      end;
    end;
  end;
end;

function FindIrcRules(const sitename, section: string): TList<String>;
var
  fRulesPerSite: TPair<String, TDictionary<string, TObjectList<TRule>>>;
  fRulesPerSection: TPair<String, TObjectList<TRule>>;
  r: TRule;
  i: integer;
begin
  Result := TList<String>.Create();

  // display global rules
  if (((sitename <> '*') or (section <> '*')) or ((sitename = '*') and (section = '*'))) then
  begin
    for fRulesPerSite in rtpl do
    begin
      for fRulesPerSection in fRulesPerSite.Value do
      begin
        for i := 0 to fRulesPerSection.Value.Count - 1 do
        begin
          r := TRule(fRulesPerSection.Value[i]);
          if ((r.sitename = '*') and (r.section = '*')) then
          begin
            Result.Add(Format('rtpl-%d %s', [GetGlobalIdForRule(r), r.AsText(True)]));
          end;
        end;
      end;
    end;
    for fRulesPerSite in rules do
    begin
      for fRulesPerSection in fRulesPerSite.Value do
      begin
        for i := 0 to fRulesPerSection.Value.Count - 1 do
        begin
          r := TRule(fRulesPerSection.Value[i]);
          if ((r.sitename = '*') and (r.section = '*')) then
          begin
            Result.Add(Format('rule-%d %s', [GetGlobalIdForRule(r), r.AsText(True)]));
          end;
        end;
      end;
    end;
  end;

  // display global section rules
  if ((sitename <> '*') or ((sitename = '*') and (section <> '*'))) then
  begin
    for fRulesPerSite in rtpl do
    begin
      for fRulesPerSection in fRulesPerSite.Value do
      begin
        for i := 0 to fRulesPerSection.Value.Count - 1 do
        begin
          r := TRule(fRulesPerSection.Value[i]);
          if ((r.sitename = '*') and (r.section = section)) then
          begin
            Result.Add(Format('rtpl-%d %s', [GetGlobalIdForRule(r), r.AsText(True)]));
          end;
        end;
      end;
    end;
    for fRulesPerSite in rules do
    begin
      for fRulesPerSection in fRulesPerSite.Value do
      begin
        for i := 0 to fRulesPerSection.Value.Count - 1 do
        begin
          r := TRule(fRulesPerSection.Value[i]);
          if ((r.sitename = '*') and (r.section = section)) then
          begin
            Result.Add(Format('rule-%d %s', [GetGlobalIdForRule(r), r.AsText(True)]));
          end;
        end;
      end;
    end;

    // display global site rules
    if ((section <> '*') or ((sitename <> '*') and (section = '*'))) then
    begin
      for fRulesPerSite in rtpl do
      begin
        for fRulesPerSection in fRulesPerSite.Value do
        begin
          for i := 0 to fRulesPerSection.Value.Count - 1 do
          begin
            r := TRule(fRulesPerSection.Value[i]);
            if ((r.sitename = sitename) and (r.section = '*')) then
            begin
              Result.Add(Format('rtpl-%d %s', [GetGlobalIdForRule(r), r.AsText(True)]));
            end;
          end;
        end;
      end;
      for fRulesPerSite in rules do
      begin
        for fRulesPerSection in fRulesPerSite.Value do
        begin
          for i := 0 to fRulesPerSection.Value.Count - 1 do
          begin
            r := TRule(fRulesPerSection.Value[i]);
            if ((r.sitename = sitename) and (r.section = '*')) then
            begin
              Result.Add(Format('rule-%d %s', [GetGlobalIdForRule(r), r.AsText(True)]));
            end;
          end;
        end;
      end;
    end;

    // display site section rules
    if ((sitename <> '*') and (section <> '*')) then
    begin
      for fRulesPerSite in rtpl do
      begin
        for fRulesPerSection in fRulesPerSite.Value do
        begin
          for i := 0 to fRulesPerSection.Value.Count - 1 do
          begin
            r := TRule(fRulesPerSection.Value[i]);
            if ((r.sitename = sitename) and (r.section = section)) then
            begin
              Result.Add(Format('rtpl-%d %s', [GetGlobalIdForRule(r), r.AsText(True)]));
            end;
          end;
        end;
      end;
      for fRulesPerSite in rules do
      begin
        for fRulesPerSection in fRulesPerSite.Value do
        begin
          for i := 0 to fRulesPerSection.Value.Count - 1 do
          begin
            r := TRule(fRulesPerSection.Value[i]);
            if ((r.sitename = sitename) and (r.section = section)) then
            begin
              Result.Add(Format('rule-%d %s', [GetGlobalIdForRule(r), r.AsText(True)]));
            end;
          end;
        end;
      end;
    end;
  end;
end;

function RuleCopy(const aSrcSite, aDestSite, aSrcSection, aDestSection: string): string;
var
  fRule: TRule;
  fRuleString, fError: String;
  fRulesPerSite: TDictionary<string, TObjectList<TRule>>;
begin
  fError := '';
  Result := '';

  if rules.TryGetValue(aSrcSite, fRulesPerSite) and fRulesPerSite.ContainsKey(aSrcSection) then
  begin
    for fRule in fRulesPerSite[aSrcSection] do
    begin
      fRuleString := aDestSite + ' ' + aDestSection + ' ' + fRule.AsText(False);
      DoAddRule(fRuleString, fError, True);
      if fError <> '' then
        Result := fError;
    end;
  end;

  RulesSave;
end;

function GetRuleCount(const aRtpl: boolean): integer;
var
  fDict: TDictionary<string, TDictionary<string, TObjectList<TRule>>>;
  fRulesPerSite: TPair<String, TDictionary<string, TObjectList<TRule>>>;
  fRulesPerSection: TPair<String, TObjectList<TRule>>;
begin
  Result := 0;

  if aRtpl then
    fDict := rtpl
  else
    fDict := rules;

  for fRulesPerSite in fDict do
    for fRulesPerSection in fRulesPerSite.Value do
      Result := Result + fRulesPerSection.Value.Count;
end;

procedure RulesReload;
var
  fst: TStringList;
  i: integer;
  error: String;
  intFound: integer;
  SearchRec: TSearchRec;
  rule_line, rules_path: String;
  fRulesPerSite: TPair<String, TDictionary<string, TObjectList<TRule>>>;
  fRulesPerSection: TPair<String, TObjectList<TRule>>;
begin
  rules_path := ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim;

  if split_site_data then
  begin
    for fRulesPerSite in rules do
    begin
      for fRulesPerSection in fRulesPerSite.Value do
      begin
        fRulesPerSection.Value.Free;
      end;
      fRulesPerSite.Value.Free;
    end;

    FreeAndNil(rules);
    rules := TDictionary<string, TDictionary<string, TObjectList<TRule>>>.Create;
  end
  else
  begin
    for fRulesPerSite in rtpl do
    begin
      for fRulesPerSection in fRulesPerSite.Value do
      begin
        fRulesPerSection.Value.Free;
      end;
      fRulesPerSite.Value.Free;
    end;

    FreeAndNil(rtpl);
    rtpl := TDictionary<string, TDictionary<string, TObjectList<TRule>>>.Create;
  end;

  intFound := FindFirst(rules_path + '*.rtpl', faAnyFile, SearchRec);
  while intFound = 0 do
  begin
    fst := TStringList.Create();
    try
      fst.LoadFromFile(rules_path + SearchRec.Name);
      for i := 0 to fst.Count - 1 do
      begin
        rule_line := Trim(fst[i]);
        if ((rule_line = '') or (rule_line[1] = '#')) then
          Continue;

        DoAddRule(rule_line, error, split_site_data);
        if error <> '' then
        begin
          Debug(dpError, dsection, '[ERROR] ' + error + ' loading ' + fst[i]);
        end;
      end;
    finally
      fst.Free;
    end;

    intFound := FindNext(SearchRec);
  end;

{$IFDEF MSWINDOWS}
  SysUtils.FindClose(SearchRec);
{$ELSE}
  FindClose(SearchRec);
{$ENDIF}
end;

procedure RulesStart;
var
  f: TEncStringlist;
  i: integer;
  error: String;
  S: String;
begin
  split_site_data := config.ReadBool('sites', 'split_site_data', False);

  // load rules (rtpl)
  RulesReload;

  // load normal rules
  f := TEncStringlist.Create(passphrase);
  try
    f.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.rules');

    for i := 0 to f.Count - 1 do
    begin
      DoAddRule(f[i], error, True);
      if error <> '' then
      begin
        Debug(dpError, dsection, '[ERROR] ' + error + ' loading ' + f[i]);
      end;
    end;
  finally
    f.Free;
  end;

  if split_site_data then
  begin
    // convert to split format
    S := ExtractFilePath(ParamStr(0)) + 'slftp.rules';
    if FileExists(S) then
        DeleteFile({$IFDEF UNICODE}PChar(S){$ELSE}PAnsiChar(S){$ENDIF});

    // force saving in new split format
    RulesSave;
  end;
end;

procedure RulesInit;
begin
  rules := TDictionary<string, TDictionary<string, TObjectList<TRule>>>.Create;
  rtpl := TDictionary<string, TDictionary<string, TObjectList<TRule>>>.Create;

  conditions := TClassList.Create;
  conditions.Add(TConditionReleaseName);
  conditions.Add(TConditionSection);
  conditions.Add(TConditionInternal);
  conditions.Add(TConditionAge);
  conditions.Add(TConditionComplete);
  conditions.Add(TConditionNotComplete);
  conditions.Add(TConditionPre);
  conditions.Add(TConditionAllowed);
  conditions.Add(TConditionNotAllowed);
  conditions.Add(TConditionGroup);
  conditions.Add(TConditionFake);
  conditions.Add(TConditionForeign);
  conditions.Add(TConditionLanguage);
  conditions.Add(TConditionYear);
  conditions.Add(TConditionCurrentYear);
  conditions.Add(TConditionTag);
  conditions.Add(TConditionDisks);
  conditions.Add(TConditionKnownGroup);
  conditions.Add(TConditionUnKnownGroup);
  conditions.Add(TConditionSource);
  conditions.Add(TConditionDestination);
  conditions.Add(TConditionCompleteSource);
  conditions.Add(TConditionNewdirSource);
  conditions.Add(TConditionNuked);
  conditions.Add(TConditionAutofollow);
  conditions.Add(TConditionPred);

  conditions.Add(TCondition0daySource);

  conditions.Add(TConditionMP3Genre);
  conditions.Add(TConditionMP3Year);
  conditions.Add(TConditionMP3CurrentYear);
  conditions.Add(TConditionMP3Language);
  conditions.Add(TConditionMP3Foreign);
  conditions.Add(TConditionMP3Source);
  conditions.Add(TConditionMP3Live);
  conditions.Add(TConditionMP3Type);
  conditions.Add(TConditionMP3Bootleg);
  conditions.Add(TConditionMP3NumDisks);
  conditions.Add(TConditionMP3VA);

  conditions.Add(TConditionNfoGenreMask);

  conditions.Add(TConditionTVLookupDone);
  conditions.Add(TConditionTVShowName);
  conditions.Add(TConditionTVTag);
  conditions.Add(TConditionTVPremierYear);
  conditions.Add(TConditionTVCountry);
  conditions.Add(TConditionTVLanguage);
  conditions.Add(TConditionTVClassification);
  conditions.Add(TConditionTVScripted);
  conditions.Add(TConditionTVGenres);
  conditions.Add(TConditionTVNetwork);
  conditions.Add(TConditionTVRuntime);
  conditions.Add(TConditionTVEndedYear);
  conditions.Add(TConditionTVRunning);
  conditions.Add(TConditionTVStatus);
  conditions.Add(TConditionTVCurrentSeason);
  conditions.Add(TConditionTVCurrentEpisode);
  conditions.Add(TConditionTVCurrentOnAir);
  conditions.Add(TConditionTVDailyShow);
  conditions.Add(TConditionTVRating);

  conditions.Add(TConditionIMDBLookupDone);
  conditions.Add(TConditionIMDBYear);
  conditions.Add(TConditionIMDBCurrentYear);
  conditions.Add(TConditionIMDBLanguages);
  conditions.Add(TConditionIMDBCountries);
  conditions.Add(TConditionIMDBGenres);
  conditions.Add(TConditionIMDBScreens);
  conditions.Add(TConditionIMDBStv);
  conditions.Add(TConditionIMDBRating);
  conditions.Add(TConditionIMDBWide);
  conditions.Add(TConditionIMDBfestival);
  conditions.Add(TConditionIMDBldt);
  conditions.Add(TConditionIMDBVotes);
  conditions.Add(TConditionIMDBCineyear);
  conditions.Add(TConditionIMDBCurrentCineyear);
  conditions.Add(TConditionIMDBType);

  conditions.Add(TConditionMVIDLookupDone);
  conditions.Add(TConditionMVIDGenre);
  conditions.Add(TConditionMVIDFiles);
  conditions.Add(TConditionMVIDYear);
  conditions.Add(TConditionMVIDCurrentYear);
  conditions.Add(TConditionMVIDLanguage);
  conditions.Add(TConditionMVIDPAL);
  conditions.Add(TConditionMVIDNTSC);
  conditions.Add(TConditionMVIDVA);
  conditions.Add(TConditionMVIDLive);

  conditions.Add(TConditionDefault);

  prefixops := TClassList.Create;
  prefixops.Add(TNotOperator);
  prefixops.Add(TOpeningBracketOperator);

  infixops := TClassList.Create;
  infixops.Add(TAndOperator);
  infixops.Add(TOrOperator);
end;

procedure RulesUninit;
begin
  Debug(dpSpam, dsection, 'Uninit1');
  conditions.Free;
  prefixops.Free;
  infixops.Free;
  rules.Free;
  rtpl.Free;
  Debug(dpSpam, dsection, 'Uninit2');
end;

{ TAtOperator }

function TAtOperator.AtConditionName: String;
begin
  try
    Result := GetOperandValue;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TAtOperator.AtConditionName : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

class function TAtOperator.Name: String;
begin
  Result := '@';
end;

function InArray(var Name: String; elements: TClassList): TCRuleNode;
var
  i: integer;
begin
  Result := nil;
  try
    for i := 0 to elements.Count - 1 do
      if TCRuleNode(elements[i]).TakeThis(Name) then
      begin
        Result := TCRuleNode(elements[i]);
        exit;
      end;
  except
    Result := nil;
  end;
end;

function ParseRule(rule: String; var error: String): TRuleNode;
type
  TMitVarunk = (mvFelteteltVagyPrefixet, mvOperatort, mvInfixOrSuffix,
    mvOperandus, mvOperandusOrInfixOrSuffix, mvInfix);
var
  s: String;
  top: TRuleNode;
  mv: TMitVarunk;
  cr: TCRuleNode;
  c: TCondition;
  cco: TConditionOperatorClass;
  co: TConditionOperator;
  ifo: TInfixOperator;
  rule_read: integer;

  function TopRight: boolean;
  begin
    Result := True;
    while ((top.parent <> nil) and
      (top.parent is TPrefixOperator) and
      (not (top.parent is TOpeningBracketOperator))) do
      top := top.parent;

    if ((top.parent <> nil) and
      (top.parent is TInfixOperator)) then
    begin
      TInfixOperator(top.parent).right := top;
      top := top.parent;
    end;
  end;

  function infixorsuffix: boolean;
  begin
    Result := False;
    if TClosingBracketOperator.TakeThis(s) then
    begin
      if not TopRight then
        exit;

      if not (top.parent is TOpeningBracketOperator) then
      begin
        error := 'Syntax error, unexpected closing bracket';
        top.Free;
        exit;
      end
      else if TOpeningBracketOperator(top.parent).child = nil then
      begin
        error := 'Syntax error, empty parentheses';
        top.Free;
        exit;
      end
      else
      begin
        top := top.parent;
        mv := mvInfix;
        Result := True;
        exit;
      end;
    end;

    cr := InArray(s, infixops);
    if cr <> nil then
    begin
      if not TopRight then
        exit;

      ifo := TInfixOperator(cr.Create(top.parent));
      ifo.left := top;
      top.parent := ifo;
      top := ifo;
      mv := mvFelteteltVagyPrefixet;
      Result := True;
      exit;
    end;
  end;

  function AddOperator: boolean;
  begin
    Result := False;
    c := TCondition(top);
    cco := c.TakesThisOperator(s);
    if cco = nil then
    begin
      error := c.Name + ' doesnt take operator ' + s;
      top.Free;
      exit;
    end;

    co := cco.Create(top.parent);
    co.condition := c;
    c.parent := co;
    top := co;

    if cco = TBooleanOperator then
    begin
      if not TopRight then
        exit;
      mv := mvInfixOrSuffix;
    end
    else
      mv := mvOperandus;
    Result := True;
  end;

begin
  Result := nil;
  co := nil;
  top := nil;
  mv := mvFelteteltVagyPrefixet;
  rule_read := 0;

  while (True) do
  begin
    Inc(rule_read);
    if (rule_read > 250) then
    begin
      debugunit.Debug(dpError, dsection, '[iNFO] ParseRule count break', []);
      break;
    end;

    s := Fetch(rule, ' ', True, False);
    if s = '' then
      Break;

    while (s <> '') do
    begin
      case mv of
        mvFelteteltVagyPrefixet:
          begin
            cr := InArray(s, prefixops);
            if cr <> nil then
            begin
              top := cr.Create(top);
              Continue;
            end;

            cr := InArray(s, conditions);
            if cr <> nil then
            begin
              top := cr.Create(top);
              mv := mvOperatort;
              Continue;
            end;

            error := 'Syntax error, expecting prefix operator or condition name, got: ' + s;
            if top <> nil then
              top.Free;
            exit;
          end;
        mvOperatort:
          begin
            if not AddOperator then
              exit;
          end;
        mvOperandus:
          begin
            if not co.FeedOperand(s) then
            begin
              error := 'Condition ' + co.condition.Name + ' doesnt take operand: ' + s;
              top.Free;
              exit;
            end;
            mv := mvOperandusOrInfixOrSuffix;
          end;
        mvInfixOrSuffix:
          begin
            if infixorsuffix() then
              Continue;
            if error <> '' then
              exit;

            error := 'Infix or suffix operator expected, got: ' + s;
            top.Free;
            exit;

          end;
        mvOperandusOrInfixOrSuffix:
          begin
            if infixorsuffix() then
              Continue;
            if error <> '' then
              exit;

            // most mar csak etetni lehet
            if not TConditionOperator(top).FeedOperand(s) then
            begin
              error := 'Condition ' + TConditionOperator(top).condition.Name +
                ' doesnt take operand: ' + s;
              top.Free;
              exit;
            end;
          end;
        mvInfix:
          begin
            if infixorsuffix then
              Continue;
            if error <> '' then
              exit;

            error := 'Syntax error, infix/suffix operator expected, got: ' + s;
            top.Free;
            exit;
          end;
      end;
    end;
  end;

  if top = nil then
  begin
    error := 'No rules specified';
    exit;
  end;

  if ((top is TBooleanCondition) and (mv = mvOperatort)) then
    AddOperator
  else
  begin

    if (top is TCondition) then
    begin
      error := 'Operator for ' + top.Name + ' not specified';
      top.Free;
      exit;
    end;

    if ((top is TConditionOperator) and (TConditionOperator(top).operand = nil)) then
    begin
      error := 'Operand for ' + top.Name + ' not specified';
      top.Free;
      exit;
    end;

  end;

  if ((top is TInfixOperator) and (TInfixOperator(top).right = nil)) then
  begin
    top.Free;
    error := 'Syntax error, right side of an infix operator is not specified';
    exit;
  end;

  while (top.parent <> nil) do
  begin
    if top.parent is TOpeningBracketOperator then
    begin
      top.Free;
      error := 'Check parentheses';
      exit;
    end;
    if not TopRight then
      exit;
    if ((top <> nil) and (top.parent <> nil)) then
      top := top.parent;
  end;

  Result := top;
end;

{ TRule }

function TRule.AsText(const includeSitesection: boolean): String;
begin
  Result := '';

  if includeSitesection then
    Result := sitename + ' ' + section + ' ';

  Result := Result + 'if ' + conditions.AsText;

  Result := Result + ' then ';

  if action = raDrop then
    Result := Result + 'DROP'
  else if action = raDontmatch then
    Result := Result + 'Dont Match'
  else
    Result := Result + 'ALLOW';
end;

constructor TRule.Create(const rule: String);
begin
  error := '';
  reparse(rule);
end;

constructor TRuleWithID.Create(const aID: integer; const aRule: TRule);
begin
  self.ID := aID;
  self.FRule := aRule;
end;

destructor TRule.Destroy;
begin
  conditions.Free;
  inherited;
end;

function TRule.Execute(r: TPazo): TRuleAction;
begin
  Result := raDontmatch;

  try
    if not conditions.Match(r) then
      exit;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TRule.Execute : %s', [e.Message]);
      Result := raDontmatch;
      exit;
    end;
  end;

  // kulonben az alap akcio
  try
    Result := action;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TRule.Execute : %s', [e.Message]);
      Result := raDontmatch;
      exit;
    end;
  end;
end;

procedure TRule.Reparse(rule: String);
var
  i: integer;
  ifstr, thenstr, actionstr, conditionstr: String;
  isnot: boolean;
begin
  sitename := UpperCase(SubString(rule, ' ', 1));
  section := UpperCase(SubString(rule, ' ', 2));

  if sitename = '' then
  begin
    error := 'Sitename is invalid';
    exit;
  end;

  if section = '' then
  begin
    error := 'Section is invalid';
    exit;
  end;

  rule := Copy(rule, Length(sitename) + Length(section) + 3, 1000);
  ifstr := LowerCase(SubString(rule, ' ', 1));
  if ifstr = 'if' then
    isnot := False
  else if ifstr = 'ifnot' then
    isnot := True
  else
  begin
    error := 'Rule must start with if/ifnot';
    exit;
  end;

  i := Count(' ', rule);
  if i < 3 then
  begin
    error := 'Rule is too short?';
    exit;
  end;

  thenstr := LowerCase(SubString(rule, ' ', i));
  actionstr := UpperCase(SubString(rule, ' ', i + 1));
  if thenstr <> 'then' then
  begin
    error := 'then missing';
    exit;
  end;

  if actionstr = 'DROP' then
    action := raDrop
  else if actionstr = 'ACCEPT' then
    action := raAllow
  else if actionstr = 'ALLOW' then
    action := raAllow
  else
  begin
    error := 'Rule must end with ALLOW/DROP';
    exit;
  end;

  if conditions <> nil then
    conditions.Free;

  conditionstr := Copy(rule, Length(ifstr) + 2, 1000);
  //  conditionstr := lowercase(conditionstr);
  conditionstr := Trim(Copy(conditionstr, 1, Length(conditionstr) -
    Length(actionstr) - Length(thenstr) - 1));

  if isnot then
    conditionstr := 'not ( ' + conditionstr + ' )';

  conditions := ParseRule(conditionstr, error);
end;

{ TListCondition }

constructor TListCondition.Create(parent: TRuleNode);
begin
  inherited;
  acceptedOperators.Clear;
  acceptedOperators.Add(TMultiStringEqualOperator);
  acceptedOperators.Add(TMultiStringNotEqualOperator);
  acceptedOperators.Add(TMultiInOperator);
  acceptedOperators.Add(TMultiNotInOperator);
  acceptedOperators.Add(TMaskOperator);
  acceptedOperators.Add(TNotMaskOperator);
end;

function TListCondition.SupplyValue(r: TPazo): String;
var
  strList : TStringList;
begin
  strList := TStringList.Create;
  try
    SupplyValues(r, strList);
    strList.Delimiter := ',';
    strList.StrictDelimiter := true;
    Result := strList.DelimitedText;
  finally
    strList.Free;
  end;
end;

{ TMultiStringCondition }

constructor TMultiStringCondition.Create(parent: TRuleNode);
begin
  inherited;

  acceptedOperators.Add(TMultiStringEqualOperator);
  acceptedOperators.Add(TMultiStringNotEqualOperator);
  acceptedOperators.Add(TMultiInOperator);
  acceptedOperators.Add(TMultiNotInOperator);
end;

{ TAtCondition }

constructor TAtCondition.Create(parent: TRuleNode);
begin
  inherited;

  acceptedOperators.Clear;
  acceptedOperators.Add(TAtOperator);
end;

{ TMultiStringEqualOperator }

function TMultiStringEqualOperator.Match(p: TPazo): boolean;
begin
  try
    re.Clear;
    GetSupplyValues(p, re);
    Result := re.IndexOf(GetOperandValue) = 0;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TMultiStringEqualOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TMultiStringEqualOperator.Name: String;
begin
  Result := '=';
end;

{ TMultiStringNotEqualOperator }

function TMultiStringNotEqualOperator.Match(p: TPazo): boolean;
begin
  try
    Result := not inherited Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TMultiStringNotEqualOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TMultiStringNotEqualOperator.Name: String;
begin
  Result := '!=';
end;

{ TMultiNotInOperator }

function TMultiNotInOperator.Match(p: TPazo): boolean;
begin
  try
    Result := not inherited Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TMultiNotInOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TMultiNotInOperator.Name: String;
begin
  Result := 'notin';
end;

{ TMultiInOperator }

function TMultiInOperator.FeedOperand(var s: String): boolean;
begin
  if operand = nil then
    operand := TListOperand.Create(self);

  Result := TListOperand(operand).FeedOperand(s);
  if Result then
    s := '';
end;

function TMultiInOperator.GetOperandValue: TStringList;
begin
  try
    Result := TListOperand(operand).Value;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TMultiInOperator.GetOperandValue : %s', [e.Message]);
      Result := nil;
    end;
  end;
end;

function TMultiInOperator.Match(p: TPazo): boolean;
var
  i: integer;
  lista: TStringList;
begin
  try
    re.Clear;
    GetSupplyValues(p, re);
    lista := GetOperandValue;
    Result := True;
    for i := lista.Count - 1 downto 0 do
    begin
      if re.IndexOf(lista[i]) <> -1 then
        exit;
    end;
    Result := False;
  except
    on e: Exception do
    begin
      Debug(dpError, dsection, 'TMultiInOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TMultiInOperator.Name: String;
begin
  Result := 'in';
end;

function FindConditionClassByName(const Name: String): TConditionClass;
var
  i: integer;
begin
  Result := nil;
  try
    for i := 0 to conditions.Count - 1 do
    begin
      if TConditionClass(conditions[i]).Name = Name then
      begin
        Result := TConditionClass(conditions[i]);
        break;
      end;
    end;
  except
    Result := nil;
  end;
end;

end.
