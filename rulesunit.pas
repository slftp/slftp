unit rulesunit;

interface

uses kb, slmasks, Classes, pazo, Contnrs;

type
  TRuleNode = class
    parent: TRuleNode;
    class function TakeThis(var s: string): Boolean; virtual;
    procedure SetupChild(child: TRuleNode); virtual;
    constructor Create(parent: TRuleNode); virtual;
    class function Name: string; virtual; abstract;
    function Match(p: TPazo): Boolean; virtual; abstract;
    function AsText: string; virtual; abstract;
    function AtConditionName: string; virtual; abstract;
  end;

  TCRuleNode = class of TRuleNode;

  TOperator = class(TRuleNode)
  end;

  TPrefixOperator = class(TOperator)
    child: TRuleNode;
    function AsText: string; override;
    destructor Destroy; override;
    procedure SetupChild(child: TRuleNode); override;
    function AtConditionName: string; override;    
  end;
  TNotOperator = class(TPrefixOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;
  TOpeningBracketOperator = class(TPrefixOperator)
    class function Name: string; override;
    function AsText: string; override;
    function Match(p: TPazo): Boolean; override;
    destructor Destroy; override;
  end;
  TClosingBracketOperator = class(TOperator)
    class function Name: string; override;
  end;


  TInfixOperator = class(TOperator)
    left: TRuleNode;
    right: TRuleNode;

    function AsText: string; override;
    destructor Destroy; override;
    function AtConditionName: string; override;
  end;
  TOrOperator = class(TInfixOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;
  TAndOperator = class(TInfixOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;


  TOperand = class(TRuleNode)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
    function AtConditionName: string; override;
  end;
  TStringOperand = class(TOperand)
  private
    fValue: string;
    nelegyenhitelesites: Boolean;
  public
    function AsText: string; override;
    function Value: string;
    function FeedOperand(const s: string): Boolean; virtual;
  end;
  TIntOperand = class(TOperand)
  private
    fValue: Integer;
  public
    function AsText: string; override;
    function Value: Integer;
    function FeedOperand(const s: string): Boolean;
  end;
  TMaskOperand = class(TStringOperand)
  private
    maskValue: TslMask;
  public
    constructor Create(parent: TRuleNode); override;
    function AsText: string; override;
    function Value: TslMask;
    destructor Destroy; override;
    function FeedOperand(const s: string): Boolean; override;
  end;
  TListOperand = class(TStringOperand)
  private
    listValue: TStringList;
    procedure Reparse;
  public
    constructor Create(parent: TRuleNode); override;
    function AsText: string; override;
    function Value: TStringList;
    destructor Destroy; override;
    function FeedOperand(const s: string): Boolean; override;
  end;

  TCondition = class; //forward
  TConditionOperator = class(TOperator)
    condition: TCondition;
    operand: TOperand;
    function AsText: string; override;
    destructor Destroy; override;
    function FeedOperand(var s: string): Boolean; virtual; abstract;
    function AtConditionName: string; override;
  end;
  TBooleanOperator = class(TConditionOperator)
    class function TakeThis(var s: string): Boolean; override;
    class function Name: string; override;
    function GetSupplyValue(p: TPazo): Boolean;
    function AsText: string; override;
    function Match(p: TPazo): Boolean; override;
    function FeedOperand(var s: string): Boolean; override;
  end;
  TStringOperator = class(TConditionOperator)
    function GetOperandValue: string;
    function GetSupplyValue(p: TPazo): string;
    function FeedOperand(var s: string): Boolean; override;
  end;
  TMultiStringOperator = class(TConditionOperator)
  private
    re: TStringList;
  public
    constructor Create(parent: TRuleNode); override;
    destructor Destroy; override;
    function GetOperandValue: string;
    procedure GetSupplyValues(p: TPazo; re: TStringList);
    function FeedOperand(var s: string): Boolean; override;
  end;
  TIntOperator = class(TConditionOperator)
    function GetOperandValue: Integer;
    function GetSupplyValue(p: TPazo): Integer;
    function FeedOperand(var s: string): Boolean; override;
  end;

  TMaskOperator = class(TStringOperator)
    class function Name: string; override;
    function GetOperandValue: TslMask;
    function Match(p: TPazo): Boolean; override;
    function FeedOperand(var s: string): Boolean; override;
  end;
  TNotMaskOperator = class(TMaskOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;

  TInOperator = class(TStringOperator)
    function GetOperandValue: TStringList;
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
    function FeedOperand(var s: string): Boolean; override;
  end;
  TNotInOperator = class(TInOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;
  TStringEqualOperator = class(TStringOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;
  TStringNotEqualOperator = class(TStringEqualOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;

  TMultiStringEqualOperator = class(TMultiStringOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;
  TMultiStringNotEqualOperator = class(TMultiStringEqualOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;
  TMultiInOperator = class(TMultiStringOperator)
    function GetOperandValue: TStringList;
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
    function FeedOperand(var s: string): Boolean; override;
  end;
  TMultiNotInOperator = class(TMultiInOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;

  TAtOperator = class(TMultiStringEqualOperator)
    class function Name: string; override;
    function AtConditionName: string; override;
  end;
  TIntEqualOperator = class(TIntOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;
  TIntNotEqualOperator = class(TIntEqualOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;
  TIntBiggerOrEqualThanOperator = class(TIntOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;
  TIntBiggerThanOperator = class(TIntOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;
  TIntLowerThanOperator = class(TIntOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;
  TIntLowerOrEqualThanOperator = class(TIntOperator)
    class function Name: string; override;
    function Match(p: TPazo): Boolean; override;
  end;


  TConditionOperatorClass = class of TConditionOperator;

  TCondition = class(TRuleNode)
    acceptedOperators: TClassList;

    function Hitelesit(const s: string): Boolean; virtual;
    function AsText: string; override;
    function Match(p: TPazo): Boolean; override;
    class function Description: string; virtual; abstract;
    class function AcceptedOperatorsAsText: string;
    function TakesThisOperator(var op: string): TConditionOperatorClass;
    constructor Create(parent: TRuleNode); override;
    destructor Destroy; override;
  end;
  TStringCondition = class(TCondition)
  public
    constructor Create(parent: TRuleNode); override;
    function SupplyValue(r: TPazo): string; virtual; abstract;
  end;
  TMultiStringCondition= class(TCondition)
    constructor Create(parent: TRuleNode); override;
    procedure SupplyValues(r: TPazo; re: TStringList); virtual; abstract;
  end;
  TIntCondition = class(TCondition)
    constructor Create(parent: TRuleNode); override;
    function SupplyValue(r: TPazo): Integer; virtual; abstract;
  end;
  TBooleanCondition = class(TCondition)
    constructor Create(parent: TRuleNode); override;
    function SupplyValue(r: TPazo): Boolean; virtual; abstract;
  end;
  TAtCondition = class(TMultiStringCondition)
    constructor Create(parent: TRuleNode); override;
  end;


  TConditionClass = class of TCondition;

  TRuleAction = (raDrop, raAllow, raDontmatch);
  TRule = class
    sitename: string;
    section: string;
    conditions: TRuleNode;
    action: TRuleAction;
    error: string;

    function Execute(r: TPazo): TRuleAction;

    function AsText(includeSitesection: Boolean): string;
    procedure Reparse(rule: string);
    constructor Create(rule: string);
    destructor Destroy; override;
  end;


procedure RulesRemove(sitename, section: string);
procedure RulesSave;
procedure RulesStart;
procedure RulesReload;
procedure RulesLoad(action, filename: string);
function AddRule(rule: string; var error: string): TRule;
procedure RulesOrder(p: TPazo);
function FireRuleSet(p: TPazo; ps: TPazoSite): TRuleAction;
function FireRules(p: TPazo; ps: TPazoSite): Boolean;
procedure RulesInit;
procedure RulesUninit;

function FindConditionClassByName(name:string):TConditionClass;

var rules: TObjectList;
    rtpl: TObjectList;
    conditions: TClassList;


implementation

uses SysUtils, Math, sitesunit, queueunit, mystrings, encinifile, debugunit,
configunit, knowngroups, DateUtils
{$IFDEF MSWINDOWS},Windows{$ENDIF}
;

const dsection = 'rules';


type
  TPrefixOperatorClass = class of TPrefixOperator;
  TInfixOperatorClass = class of TInfixOperator;

  TConditionReleaseName = class(TStringCondition)
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionSection = class(TStringCondition)
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionInternal = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionAge = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
(*
  TConditionPretime = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionPretimeFound = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
*)
  TConditionComplete = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionNotComplete = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionPre = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionAllowed = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionNotAllowed = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionGroup = class(TStringCondition)
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionFake = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionForeign = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionLanguage = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionYear = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;

  TConditionKnownGroup = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionUnKnownGroup = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionSource = class(TStringCondition)
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionDestination = class(TStringCondition)
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;

  TConditionCompleteSource = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionNewdirSource = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;

  TConditionNuked = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;

  TCondition0daySource = class(TStringCondition)
    function Hitelesit(const s: string): Boolean; override;
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;

  TConditionTag = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionDisks = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;

  TConditionAutofollow = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;

  TConditionPred = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;

(*###      MP3       ###*)
  TConditionMP3Genre = class(TStringCondition)
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMP3Year = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMP3Language = class(TStringCondition)
    function Hitelesit(const s: string): Boolean; override;
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMP3Foreign = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMP3Source = class(TStringCondition)
    function Hitelesit(const s: string): Boolean; override;
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMP3Live = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMP3Type = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMP3Bootleg = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMP3NumDisks = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMP3VA = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
(*###      NFO       ###*)
  TConditionNfoMGenre = class(TStringCondition)
    function SupplyValue(r: TPazo): string; override;
    constructor Create(parent: TRuleNode); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
(*###      IMDB       ###*)
  TConditionIMDBYear = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionIMDBLanguages = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionIMDBCountries = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionIMDBGenres = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionIMDBScreens = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionIMDBStv = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionIMDBRating = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionIMDBVotes = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionIMDBldt = class(TBooleanCondition)
   function SupplyValue(r: TPazo): Boolean; override;
   class function Name: string; override;
   class function Description: string; override;
  end;
  TConditionIMDBWide = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionIMDBfestival = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionIMDBCineyear = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;

  TConditionTVShowName = class(TStringCondition)
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionTVtag = class(TStringCondition)
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionTVPremierYear = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionTVCountry = class(TStringCondition)
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionTVClassification = class(TStringCondition)
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionTVScripted = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionTVGenres = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionTVNetwork = class(TStringCondition)
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionTVRuntime = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionTVEndedYear = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
    TConditionTVRunning = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;

    TConditionTVStatus = class(TStringCondition)
    function SupplyValue(r: TPazo): string; override;
    class function Name: string; override;
    class function Description: string; override;
  end;

  {###      MVID    ###}
  TConditionMVIDGenre = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMVIDFiles = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMVIDYear = class(TIntCondition)
    function SupplyValue(r: TPazo): Integer; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMVIDVA = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMVIDPAL = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  TConditionMVIDNTSC = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
 TConditionMVIDLive = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;

  TConditionMVIDLanguage = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: string; override;
    class function Description: string; override;
  end;



  TConditionDefault = class(TBooleanCondition)
    function SupplyValue(r: TPazo): Boolean; override;
    class function Name: string; override;
    class function Description: string; override;
  end;
  // NE FELEJTSD EL LENT ADDOLNI A LISTABA !!!

var
  prefixops: TClassList;
  infixops: TClassList;


{ TInfixOperator }

function TInfixOperator.AsText: string;
begin
  try
    Result:= left.AsText + ' '+ Name + ' '+ right.AsText;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TInfixOperator.AsText : %s', [e.Message]);
      Result:= '';
    end;
  end;
end;

function TInfixOperator.AtConditionName: string;
begin
  try
    Result:= left.AtConditionName;
    if Result = '' then Result:= right.AtConditionName;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TInfixOperator.AtConditionName : %s', [e.Message]);
      Result:= '';
    end;
  end;
end;

destructor TInfixOperator.Destroy;
begin
  if left <> nil then
  begin
    left.Free;
    left:= nil;
  end;
  if right <> nil then
  begin
    right.Free;
    right:= nil;
  end;
  inherited;
end;

{ TPrefixOperator }

function TPrefixOperator.AsText: string;
begin
  try
    Result:= Name + ' ' + child.AsText;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TPrefixOperator.AsText : %s', [e.Message]);
      Result:= '';
    end;
  end;
end;

function TPrefixOperator.AtConditionName: string;
begin
  try
    Result:= child.AtConditionName;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TPrefixOperator.AtConditionName : %s', [e.Message]);
      Result:= '';
    end;
  end;
end;

destructor TPrefixOperator.Destroy;
begin
  if child <> nil then
  begin
    child.Free;
    child:= nil;
  end;
  inherited;
end;

procedure TPrefixOperator.SetupChild(child: TRuleNode);
begin
  self.child:= child;
end;

{ TBracketOperator }

function TOpeningBracketOperator.AsText: string;
begin
  try
    Result:= '( ' + child.AsText + ' )';
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TOpeningBracketOperator.AsText : %s', [e.Message]);
      Result:= '';
    end;
  end;
end;

destructor TOpeningBracketOperator.Destroy;
begin
  if child <> nil then
  begin
    child.Free;
    child:= nil;
  end;
  inherited;
end;

function TOpeningBracketOperator.Match(p: TPazo): Boolean;
begin
  try
    Result:= child.Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TOpeningBracketOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TOpeningBracketOperator.Name: string;
begin
  Result:= '(';
end;


{ TOrOperator }

function TOrOperator.Match(p: TPazo): Boolean;
begin
  try
    Result:= left.Match(p) or right.Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TOrOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TOrOperator.Name: string;
begin
  Result:= '||';
end;

{ TAndOperator }

function TAndOperator.Match(p: TPazo): Boolean;
begin
  try
    Result:= left.Match(p) and right.Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TAndOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TAndOperator.Name: string;
begin
  Result:= '&&';
end;

{ TNoOperator }


function TBooleanOperator.AsText: string;
begin
  try
    Result:= condition.AsText;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TBooleanOperator.AsText : %s', [e.Message]);
      Result:= '';
    end;
  end;
end;


function TBooleanOperator.FeedOperand(var s: string): Boolean;
begin
  Result:= False; // ez specialis, nem kell neki semmilyen operandus
end;

function TBooleanOperator.GetSupplyValue(p: TPazo): Boolean;
begin
  try
    Result:= TBooleanCondition(condition).SupplyValue(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TBooleanOperator.GetSupplyValue : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

function TBooleanOperator.Match(p: TPazo): Boolean;
begin
  try
    Result:= GetSupplyValue(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TBooleanOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TBooleanOperator.Name: string;
begin
  Result:= '';
end;

{ TInOperator }

function TInOperator.FeedOperand(var s: string): Boolean;
begin
  if operand = nil then
    operand:= TListOperand.Create(self);

  Result:= TListOperand(operand).FeedOperand(s);
  if Result then
    s:= '';
end;

function TInOperator.GetOperandValue: TStringList;
begin
  try
    Result:= TListOperand(operand).Value;
  except
    on e: Exception do
    begin
      result:=nil;
      Debug(dpError, 'rules', 'TInOperator.GetOperandValue : %s', [e.Message]);
    end;
  end;
end;

function TInOperator.Match(p: TPazo): Boolean;
begin
  try
    Result:= GetOperandValue.IndexOf(GetSupplyValue(p)) <> -1;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TInOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TInOperator.Name: string;
begin
  Result:= 'in';
end;

{ TNotInOperator }

function TNotInOperator.Match(p: TPazo): Boolean;
begin
  try
    Result:= not inherited Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TNotInOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TNotInOperator.Name: string;
begin
  Result:= 'notin';
end;


{ TStringEqualOperator }

function TStringEqualOperator.Match(p: TPazo): Boolean;
begin
  try
    Result:= AnsiSameText( GetOperandValue, GetSupplyValue(p) );
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TStringEqualOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TStringEqualOperator.Name: string;
begin
  Result:= '=';
end;

{ TStringNotEqualOperator }

function TStringNotEqualOperator.Match(p: TPazo): Boolean;
begin
  try
    Result:= not inherited Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TStringNotEqualOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TStringNotEqualOperator.Name: string;
begin
  Result:= '!=';
end;

{ TIntEqualOperator }

function TIntEqualOperator.Match(p: TPazo): Boolean;
var vr:integer;
begin
 try
  vr:=CompareValue(GetSupplyValue(p),GetOperandValue);
  if vr = 0 then
   Result:= True else Result:= False;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TIntEqualOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TIntEqualOperator.Name: string;
begin
  Result:= '=';
end;

{ TIntNotEqualOperator }

function TIntNotEqualOperator.Match(p: TPazo): Boolean;
var vr:integer;
begin
 try
  vr:=CompareValue(GetSupplyValue(p),GetOperandValue);
  if vr <> 0 then
   Result:= True else Result:= False;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TIntNotEqualOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TIntNotEqualOperator.Name: string;
begin
  Result:= '!=';
end;

{ TIntBiggerOrEqualThanOperator }

function TIntBiggerOrEqualThanOperator.Match(p: TPazo): Boolean;
var vr:integer;
begin
 try
  vr:=CompareValue(GetSupplyValue(p),GetOperandValue);
  if ((vr = 0) or (vr = 1))  then
   Result:= True else Result:= False;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TIntBiggerOrEqualThanOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TIntBiggerOrEqualThanOperator.Name: string;
begin
  Result:= '>=';
end;

{ TIntBiggerThanOperator }

function TIntBiggerThanOperator.Match(p: TPazo): Boolean;
var vr:integer;
begin
 try
  vr:=CompareValue(GetSupplyValue(p),GetOperandValue);
  if vr = 1  then
   Result:= True else Result:= False;

  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TIntBiggerThanOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TIntBiggerThanOperator.Name: string;
begin
  Result:= '>';
end;

{ TIntLowerThanOperator }

function TIntLowerThanOperator.Match(p: TPazo): Boolean;
var vr:integer;
begin
 try
  vr:=CompareValue(GetSupplyValue(p),GetOperandValue);
  if vr =  -1 then
   Result:= True else Result:= False;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TIntLowerThanOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TIntLowerThanOperator.Name: string;
begin
  Result:= '<';
end;

{ TIntLowerOrEqualThanOperator }

function TIntLowerOrEqualThanOperator.Match(p: TPazo): Boolean;
var vr:integer;
begin
 try
  vr:=CompareValue(GetSupplyValue(p),GetOperandValue);
  if ((vr = 0) or (vr = -1))  then
   Result:= True else Result:= False;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TIntLowerOrEqualThanOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TIntLowerOrEqualThanOperator.Name: string;
begin
  Result:= '<=';
end;


{ TCondition }

class function TCondition.AcceptedOperatorsAsText: string;
var i: Integer;
    c: TCondition;
begin
  c:= self.Create(nil);
  Result:= '';
  for i:= 0 to c.acceptedOperators.Count -1 do
    Result:= Result + TConditionOperatorClass(c.acceptedOperators[i]).Name + ' ';

  Result:= trim(Result);
  c.Free;
end;

function TCondition.AsText: string;
begin
  Result:= Name;
end;

constructor TCondition.Create(parent: TRuleNode);
begin
  inherited;
  acceptedOperators:= TClassList.Create;
end;

destructor TCondition.Destroy;
begin
  acceptedOperators.Free;
  inherited;
end;

function TCondition.Hitelesit(const s: string): Boolean;
begin
  Result:= True;
end;

function TCondition.Match(p: TPazo): Boolean;
begin
  Result:= False; // exception
end;

function TCondition.TakesThisOperator(var op: string): TConditionOperatorClass;
var i: Integer;
begin
  Result:= nil;
  for i:= 0 to acceptedOperators.Count -1 do
    if TConditionOperatorClass(acceptedOperators[i]).TakeThis(op) then
    begin
      Result:= TConditionOperatorClass(acceptedOperators[i]);
      exit;
    end;
end;


{ TMaskOperator }

function TMaskOperator.FeedOperand(var s: string): Boolean;
begin
  if operand = nil then
    operand:= TMaskOperand.Create(self);

  Result:= TMaskOperand(operand).FeedOperand(s);
  if Result then
    s:= '';
end;

function TMaskOperator.GetOperandValue: TslMask;
begin
  try
    Result:= TMaskOperand(operand).value;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TMaskOperator.GetOperandValue : %s', [e.Message]);
      result:=nil;
    end;
  end;
end;


function TMaskOperator.Match(p: TPazo): Boolean;
begin
  try
    Result:= GetOperandValue.Matches(GetSupplyValue(p));
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TMaskOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TMaskOperator.Name: string;
begin
  Result:= '=~';
end;

{ TNotMaskOperator }

function TNotMaskOperator.Match(p: TPazo): Boolean;
begin
  try
    Result:= not inherited Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TNotMaskOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TNotMaskOperator.Name: string;
begin
  Result:= '!~';
end;

{ TRuleNode }

constructor TRuleNode.Create(parent: TRuleNode);
begin
  inherited Create;
  self.parent:= parent;
  if parent <> nil then
    parent.SetupChild(self);
end;


class function TRuleNode.TakeThis(var s: string): Boolean;
begin
  Result:= False;
  if s = Name then
  begin
    Result:= True;
    s:= '';
  end;
end;


{ TStringOperand }


procedure TRuleNode.SetupChild(child: TRuleNode);
begin
// nothing here.
end;

{ TStringOperand }

function TStringOperand.AsText: string;
begin
  Result:= Value;
end;


function TStringOperand.FeedOperand(const s: string): Boolean;
begin
  if s = '' then
  begin
    Result:= True;
    exit;
  end;
  
  if not nelegyenhitelesites then
  begin
    Result:= TConditionOperator(parent).condition.hitelesit(s);
    if not Result then exit;
  end else
    Result:= True;

  if fValue <> '' then
    fValue:= fValue + ' '+s
  else
    fValue:= s;
end;

function TStringOperand.Value: string;
begin
  Result:= fValue;
end;

{ TIntOperand }

function TIntOperand.AsText: string;
begin
  Result:= IntToStr(Value);
end;


function TIntOperand.FeedOperand(const s: string): Boolean;
begin
  Result:= False;
  fValue:= StrToIntDef(s, -123717283);
  if fValue  <> -123717283 then
    Result:= True; // integer nem bovitheto szavankent
end;

function TIntOperand.Value: Integer;
begin
  Result:= fValue;
end;

{ TListOperand }

function TListOperand.AsText: string;
var i: Integer;
begin
  Result:= '';
  try
    for i:= 0 to listValue.Count -1 do
    begin
      Result:= Result+ listValue[i];
      if (i <> listValue.Count -1) then
        Result:= Result +', ';
    end;
  except
    Result:= '';
  end;
end;

constructor TListOperand.Create(parent: TRuleNode);
begin
  inherited Create(parent);
  listValue:= TStringList.Create;
  listValue.CaseSensitive:= False;

  Reparse;
end;

destructor TListOperand.Destroy;
begin
  listValue.Free;
  inherited;
end;

function TListOperand.FeedOperand(const s: string): Boolean;
var l: Integer;
begin
  Result:= False;
  try
    if s = '' then exit;
    l:= length(s);
    if s[l] = ',' then
    begin
      Result:= inherited FeedOperand(Copy(s, 1, l-1));
      if Result then
        fValue:= fValue+',';
    end else
      Result:= inherited FeedOperand(s);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TListOperand.FeedOperand : %s', [e.Message]);
      fValue:= '';
      Result:= false;
    end;
  end;

  Reparse;
end;

procedure TListOperand.Reparse;
var s, fs: string;
    operand_read: Integer;
begin
  listValue.Clear;
  try
    fs:= fValue;
    operand_read:= 0;
    while(true)do
    begin
      if (operand_read > 100) then
      begin
        debugunit.Debug(dpError, 'rules', '[iNFO] TListOperand.Reparse count break', []);
        break;
      end;
      inc(operand_read);
      s:= Trim(Fetch(fs, ','));
      if s = '' then Break;
      if s <> '' then
      begin
        listValue.Add(s);
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TListOperand.Reparse : %s', [e.Message]);
      listValue.Clear;
    end;
  end;
end;

function TListOperand.Value: TStringList;
begin
  Result:= listValue;
end;

{ TMaskOperand }

function TMaskOperand.AsText: string;
begin
  Result:= fValue;
end;


constructor TMaskOperand.Create(parent: TRuleNode);
begin
  inherited;
  nelegyenhitelesites:= True;
end;

destructor TMaskOperand.Destroy;
begin
  if maskValue <> nil then
    maskValue.Free;
  inherited;
end;

function TMaskOperand.FeedOperand(const s: string): Boolean;
begin
  Result:= inherited FeedOperand(s);
  if maskValue <> nil then
    maskValue.Free;
  maskValue:= TslMask.Create(fValue);
end;

function TMaskOperand.Value: TslMask;
begin
  Result:= maskValue;
end;

{ TStringOperator }

function TStringOperator.FeedOperand(var s: string): Boolean;
begin
  if operand = nil then
    operand:= TStringOperand.Create(self);

  Result:= TStringOperand(operand).FeedOperand(s);
  if Result then
    s:= '';
end;

function TStringOperator.GetOperandValue: string;
begin
  try
    Result:= TStringOperand(operand).Value;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TStringOperator.GetOperandValue : %s', [e.Message]);
      Result:= '';
    end;
  end;
end;

function TStringOperator.GetSupplyValue(p: TPazo): string;
begin
  try
    Result:= TStringCondition(condition).SupplyValue(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TStringOperator.GetSupplyValue : %s', [e.Message]);
      Result:= '';
    end;
  end;
end;

{ TMultiStringOperator }

constructor TMultiStringOperator.Create(parent: TRuleNode);
begin
  inherited;

  re:= TStringList.Create;
  re.CaseSensitive:= False;
end;

destructor TMultiStringOperator.Destroy;
begin
  re.Free;

  inherited;
end;

function TMultiStringOperator.FeedOperand(var s: string): Boolean;
begin
  if operand = nil then
    operand:= TStringOperand.Create(self);

  Result:= TStringOperand(operand).FeedOperand(s);
  if Result then
    s:= '';
end;

function TMultiStringOperator.GetOperandValue: string;
begin
  try
    Result:= TStringOperand(operand).Value;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TMultiStringOperator.GetOperandValue : %s', [e.Message]);
      Result:= '';
    end;
  end;
end;

procedure TMultiStringOperator.GetSupplyValues(p: TPazo; re: TStringList);
begin
  try
    TMultiStringCondition(condition).SupplyValues(p, re);
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TMultiStringOperator.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TIntOperator }

function TIntOperator.FeedOperand(var s: string): Boolean;
begin
  if operand = nil then
    operand:= TIntOperand.Create(self);

  Result:= TIntOperand(operand).FeedOperand(s);
  if Result then
    s:= '';
end;

function TIntOperator.GetOperandValue: Integer;
begin
  try
    Result:= TIntOperand(operand).Value;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TIntOperator.GetOperandValue : %s', [e.Message]);
      Result:= 0;
    end;
  end;
end;

function TIntOperator.GetSupplyValue(p: TPazo): Integer;
begin
  try
    Result:= TIntCondition(condition).SupplyValue(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TIntOperator.GetSupplyValue : %s', [e.Message]);
      Result:= 0;
    end;
  end;
end;

{ TIntCondition }

constructor TIntCondition.Create(parent: TRuleNode);
begin
  inherited;

  acceptedOperators.Add( TIntEqualOperator );
  acceptedOperators.Add( TIntNotEqualOperator );
  acceptedOperators.Add( TIntBiggerOrEqualThanOperator );
  acceptedOperators.Add( TIntBiggerThanOperator );
  acceptedOperators.Add( TIntLowerThanOperator );
  acceptedOperators.Add( TIntLowerOrEqualThanOperator );

end;


{ TStringCondition }

constructor TStringCondition.Create(parent: TRuleNode);
begin
  inherited ;

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

function TConditionOperator.AsText: string;
begin
  Result:= condition.AsText + ' ' + Name + ' ' + operand.AsText;
end;


function TConditionOperator.AtConditionName: string;
begin
  Result:= '';
end;

destructor TConditionOperator.Destroy;
begin
  if condition <> nil then
  begin
    condition.Free;
    condition:= nil;
  end;
  if operand <> nil then
  begin
    operand.Free;
    operand:= nil;
  end;
  inherited;
end;

{ TNotOperator }
function TNotOperator.Match(p: TPazo): Boolean;
begin
  try
    Result:= not child.Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TNotOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TNotOperator.Name: string;
begin
  Result:= 'not';
end;

{ TOperand }


function TOperand.AtConditionName: string;
begin
  Result:= '';
end;

function TOperand.Match(p: TPazo): Boolean;
begin
  Result:= False; // exception
end;

class function TOperand.Name: string;
begin
  Result:= 'operand';
end;


class function TBooleanOperator.TakeThis(var s: string): Boolean;
begin
  Result:= True;
  // we are not resetting s, it needs further processing
end;

{ TSuffixOperator }

class function TClosingBracketOperator.Name: string;
begin
  Result:= ')';
end;


function mySpeedComparer(List: TStringList; Index1, Index2: Integer): Integer;
begin
  try
    Result:=
      CompareValue(
        StrToIntDef(list.ValueFromIndex[index2],0),
        StrToIntDef(list.ValueFromIndex[index1],0)
      );
  except
    Result:= 0;
  end;
end;

function FireRuleSetB(p: TPazo; ps: TPazoSite; sitenametomatch, sectiontomatch: string): TRuleAction;
var i: Integer;
    ra: TRuleAction;
begin
  Result:= raDontmatch;
  ra:=result;
  try
    for i:= 0 to rtpl.Count -1 do
    begin
      try
        if ((TRule(rtpl[i]).sitename = sitenametomatch) and (TRule(rtpl[i]).section = sectiontomatch)) then
        begin
          try
            ra:= TRule(rtpl[i]).Execute(p);
          except
            on e: Exception do
            begin
              Debug(dpError, 'rules', Format('[EXCEPTION] FireRuleSetB(rtpl) r.Execute: %s, %s', [e.Message, TRule(rtpl[i]).AsText(True)]));
              Result:= raDontmatch;
              exit;
            end;
          end;
          if ra = raDrop then
          begin
            ps.reason:= TRule(rtpl[i]).AsText(True);
            Result:= raDrop;
            exit;
          end;

          if ra = raAllow then
          begin
            ps.reason:= TRule(rtpl[i]).AsText(True);
            Result:= raAllow;
            exit;
          end;
        end;
      except
        Break;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] FireRuleSetB rtpl: %s', [e.Message]));
      Result:= raDontmatch;
      exit;
    end;
  end;

  try
    for i:= 0 to rules.Count -1 do
    begin
      try
        if ((TRule(rules[i]).sitename = sitenametomatch) and (TRule(rules[i]).section = sectiontomatch)) then
        begin
          try
            ra:= TRule(rules[i]).Execute(p);
          except
            on e: Exception do
            begin
              Debug(dpError, 'rules', Format('[EXCEPTION] FireRuleSetB(rules) r.Execute: %s %s', [e.Message, TRule(rules[i]).AsText(True)]));
              Result:= raDontmatch;
              exit;
            end;
          end;
          if ra = raDrop then
          begin
            ps.reason:= TRule(rules[i]).AsText(True);
            Result:= raDrop;
            exit;
          end;

          if ra = raAllow then
          begin
            ps.reason:= TRule(rules[i]).AsText(True);
            Result:= raAllow;
            exit;
          end;
        end;
      except
        Break;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] FireRuleSetB rules: %s', [e.Message]));
      Result:= raDontmatch;
      exit;
    end;
  end;
end;

function FireRuleSet(p: TPazo; ps: TPazoSite): TRuleAction;
begin

  try
    Result:= FireRuleSetB(p, ps, '*', '*'); // eloszor megnezzuk a teljesen generic ruleokat    = first watch a completely generic ruleokat
    if Result <> raDontMatch then begin
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] FireRuleSetB * *: %s', [e.Message]));
      Result:= raDontmatch;
      exit;
    end;
  end;

  try
    Result:= FireRuleSetB(p, ps, '*', p.rls.section); // most megnezzuk a sectionre globalis ruleokat = Now look at the sectional globalis ruleokat
    if Result <> raDontMatch then begin
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] FireRuleSetB * section: %s', [e.Message]));
      Result:= raDontmatch;
      exit;
    end;
  end;

  try
    Result:= FireRuleSetB(p, ps, ps.name, '*'); // most megnezzuk a site globalis rulejait = Now look at the site globalis rulejait
    if Result <> raDontMatch then begin
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] FireRuleSetB site *: %s', [e.Message]));
      Result:= raDontmatch;
      exit;
    end;
  end;

  try
    Result:= FireRuleSetB(p, ps, ps.name, p.rls.section); // most megnezzuk a section rulejait. = Now look at the section rulejait.
    if Result <> raDontMatch then begin
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] FireRuleSetB site section: %s', [e.Message]));
      Result:= raDontmatch;
      exit;
    end;
  end;

  // egyebkent droppoljuk, nem volt matching rule.  = I use droppoljuk, there was no matching rule.
  //Result:= raDrop;
  Result:= raDontMatch;
  if ps.reason = '' then
    ps.reason:= 'No matching rule';

end;

function FireRules(p: TPazo; ps: TPazoSite): Boolean;
var dstps: TPazoSite;
    y: TStringList;
    i: Integer;
    ps_s, dstps_s: TSite;
begin
  Result:= False;

  if (not Assigned(ps) or (ps = nil)) then exit;

  if ps.error then exit;

  ps_s:= FindSiteByName('', ps.name);
  if ps_s = nil then exit;
  if ps_s.working = sstDown then exit;

  p.srcsite:= ps.name;
  Debug(dpSpam, 'rules', '-> '+Format('%s: %s %s', [ps.name, p.rls.section, p.rls.rlsname]));

  y:= TStringList.Create;
  //sitesdat.ReadSectionValues('speed-from-'+ps.Name, y);
  y.Assign(ps.speed_from);
(*
  try
    y.CustomSort(myspeedcomparer);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] FireRules CustomSort: %s', [e.Message]));
    end;
  end;
*)
  for i:= 0 to y.Count -1 do
  begin
    try if i > y.Count then Break; except Break; end;
    try
      dstps:= p.FindSite(y.Names[i]);
      if dstps = nil then Continue;

      if (dstps.name <> ps.name) then
      begin
        if (dstps.AllPre) then
        begin
          if (dstps.reason = '') then
            dstps.reason:= 'Affil';
          Continue;
        end;

        if dstps.error then Continue;

        dstps_s:= FindSiteByName('', dstps.name);
        if dstps_s = nil then Continue;

        if dstps_s.working = sstDown then
        begin
          if (dstps.reason = '') then
            dstps.reason:= 'Down';
          Continue;
        end;

        p.dstsite:= dstps.name;
        // aztan hogy allowed e...
        if ((dstps.status in [rssAllowed]) or (FireRuleSet(p, dstps) = raAllow)) then
        begin
          if (ps.status in [rssShouldPre, rssRealPre]) then
          begin
            if ps.AddDestination(dstps, (StrToIntDef(y.ValueFromIndex[i], 1) * dstps_s.GetRank(p.rls.section)) + 100 ) then
              Result:= True;
          end else begin
            if ps.AddDestination(dstps, StrToIntDef(y.ValueFromIndex[i], 1) * dstps_s.GetRank(p.rls.section) ) then
              Result:= True;
          end;
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, 'rules', Format('[EXCEPTION] FireRules loop: %s', [e.Message]));
        Result:= False;
        Break;
      end;
    end;
  end;
  Debug(dpSpam, 'rules', '<- '+Format('%s: %s %s', [ps.name, p.rls.section, p.rls.rlsname]));
  y.Free;
end;

procedure RulesOrder(p: TPazo);
var x: TStringList;
    i, j: Integer;
    r: TRule;
    s: string;
    fositeIndex, aktsiteIndex: Integer;
begin
  x:= TStringList.Create;
  for i:= 0 to p.sites.Count -1 do
  begin
    try
      x.Add(TPazoSite(p.sites[i]).name);
    except
      break;
    end;
  end;

  for i:= 0 to x.Count -1 do
  begin
    fositeIndex:= p.sites.IndexOf(p.FindSite(x[i]));
    for j:= 0 to rtpl.Count-1 do
    begin
      r:= TRule(rtpl[j]);
      if ((r.sitename = x[i]) and (r.section = p.rls.section)) then
      begin
        s:= r.conditions.AtConditionName;
        if s <> '' then
        begin
            aktsiteIndex:= p.sites.IndexOf(p.FindSite(s));
            if (aktsiteIndex > fositeIndex) then
            begin
              p.sites.Move(aktsiteIndex, fositeIndex);
              fositeIndex:= fositeIndex + 1;
              // meg kell nezni egyezik e ezzel...
              // fositeIndex:= p.sites.IndexOf(p.FindSite(x[i]));
            end;
        end;
      end;
    end;

    for j:= 0 to rules.Count-1 do
    begin
      r:= TRule(rules[j]);
      if ((r.sitename = x[i]) and (r.section = p.rls.section)) then
      begin
        s:= r.conditions.AtConditionName;
        if s <> '' then
        begin
            aktsiteIndex:= p.sites.IndexOf(p.FindSite(s));
            if (aktsiteIndex > fositeIndex) then
            begin
              p.sites.Move(aktsiteIndex, fositeIndex);
              fositeIndex:= fositeIndex + 1;
              // meg kell nezni egyezik e ezzel...
              // fositeIndex:= p.sites.IndexOf(p.FindSite(x[i]));
            end;
        end;
      end;
    end;
  end;
  x.Free;
end;

procedure RulesSave;
var i: Integer;
    f: TEncStringlist;
begin
  f:= TEncStringlist.Create(passphrase);
  try
    for i:= 0 to rules.Count -1 do
      f.Add(TRule(rules[i]).AsText(True));
    f.SaveToFile(ExtractFilePath(ParamStr(0))+'slftp.rules');
  finally
    f.Free;
  end;
end;

procedure RulesRemove(sitename, section: string);
var i: Integer;
    r: TRule;
begin
  i:= 0;
  while (i < rules.Count) do
  begin
    r:= TRule(rules[i]);
    if ((r.sitename = sitename) and ((section='')or(r.section= section)))then
    begin
      rules.Remove(r);
      dec(i);
    end;
    inc(i);
  end;
end;


function AddRule(rule: string; var error: string): TRule;
var r: TRule;
begin
  Result:= nil;

  r:= TRule.Create(rule);
  if r.error <> '' then
  begin
    error:= r.error;
    r.Free;
  end else
    Result:= r;
end;

procedure RulesLoad(action, filename: string);
var fst: TStringList;
    i: Integer;
    r: TRule;
    error: string;
begin
  if (UpperCase(action) = 'REPLACE') then
  begin
    rules.Free;
    rules:= TObjectList.Create;
  end;

  fst:= TStringList.Create();
  try
    fst.LoadFromFile(ExtractFilePath(ParamStr(0))+filename);
    for i:= 0 to fst.Count -1 do
    begin
      r:= AddRule(fst[i], error);
      if r <> nil then
      begin
        rules.Add(r);
      end else begin
        Debug(dpError, 'rules', '[ERROR] '+error+' loading '+fst[i]);
      end;
    end;
  finally
    fst.Free;
  end;
end;

procedure RulesReload();
var fst: TStringList;
    i: Integer;
    r: TRule;
    error: string;
    intFound: Integer;
    SearchRec: TSearchRec;
    rules_path: String;
begin
  {$IFDEF MSWINDOWS}
  rules_path:= ExtractFilePath(ParamStr(0))+'rtpl'+'\';
  {$ELSE}
  rules_path:= ExtractFilePath(ParamStr(0))+'rtpl'+'/';
  {$ENDIF}

  FreeAndNil(rtpl);
  rtpl:= TObjectList.Create;

  intFound := FindFirst(rules_path + '*.rtpl', faAnyFile, SearchRec);
  while intFound = 0 do
  begin
    fst:= TStringList.Create();
    try
      fst.LoadFromFile(rules_path + SearchRec.Name);
      for i:= 0 to fst.Count -1 do
      begin
        if (Trim(fst[i]) = '') then Continue;
        
        r:= AddRule(fst[i], error);
        if r <> nil then
        begin
          rtpl.Add(r);
        end else begin
          Debug(dpError, 'rules', '[ERROR] '+error+' loading '+fst[i]);
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
var f: TEncStringlist;
    i: Integer;
    r: TRule;
    error: string;
begin
  // load rules tpl
  RulesReload();
  
  //beparszoljuk a szabalyokat
  f:= TEncStringlist.Create(passphrase);
  try
    f.LoadFromFile(ExtractFilePath(ParamStr(0))+'slftp.rules');

    for i:= 0 to f.Count -1 do
    begin
      r:= AddRule(f[i], error);
      if r <> nil then
        rules.Add(r);
    end;

  finally
    f.Free;
  end;
end;

procedure RulesInit;
begin
  rules:= TObjectList.Create;
  rtpl:= TObjectList.Create;

  conditions:= TClassList.Create;
  conditions.Add(TConditionReleaseName);
  conditions.Add(TConditionSection);  
  conditions.Add(TConditionInternal);
  conditions.Add(TConditionAge);
//  conditions.Add(TConditionPretime);
//  conditions.Add(TConditionPretimeFound);  
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
  conditions.Add(TConditionMP3Language);
  conditions.Add(TConditionMP3Foreign);
  conditions.Add(TConditionMP3Source);
  conditions.Add(TConditionMP3Live);
  conditions.Add(TConditionMP3Type);
  conditions.Add(TConditionMP3Bootleg);
  conditions.Add(TConditionMP3NumDisks);
  conditions.Add(TConditionMP3VA);

  conditions.Add(TConditionNfoMGenre);

  conditions.Add(TConditionTVShowName);
  conditions.Add(TConditionTVTag);
  conditions.Add(TConditionTVPremierYear);
  conditions.Add(TConditionTVCountry);
  conditions.Add(TConditionTVClassification);  
  conditions.Add(TConditionTVScripted);
  conditions.Add(TConditionTVGenres);
  conditions.Add(TConditionTVNetwork);
  conditions.Add(TConditionTVRuntime);
  conditions.Add(TConditionTVEndedYear);
  conditions.Add(TConditionTVRunning);
  conditions.Add(TConditionTVStatus);

  conditions.Add(TConditionIMDBYear);
  conditions.Add(TConditionIMDBLanguages);
  conditions.Add(TConditionIMDBCountries);
  conditions.Add(TConditionIMDBGenres);
  conditions.Add(TConditionIMDBScreens);
  conditions.Add(TConditionIMDBStv);
  conditions.Add(TConditionIMDBRating);
  conditions.Add(TConditionIMDBWide);
  conditions.Add(TConditionIMDBfestival);
  conditions.Add(TConditionIMDBldt);
  conditions.Add(TConditionIMDBCineyear);

  conditions.Add(TConditionIMDBVotes);


  conditions.Add(TConditionMVIDGenre);
  conditions.Add(TConditionMVIDFiles);
  conditions.Add(TConditionMVIDYear);
  conditions.Add(TConditionMVIDLanguage);
  conditions.Add(TConditionMVIDPAL);
  conditions.Add(TConditionMVIDNTSC);
  conditions.Add(TConditionMVIDVA);
  conditions.Add(TConditionMVIDLive);


  conditions.Add(TConditionDefault);


  prefixops:= TClassList.Create;
  prefixops.Add(TNotOperator);
  prefixops.Add(TOpeningBracketOperator);

  infixops:= TClassList.Create;
  infixops.Add(TAndOperator);
  infixops.Add(TOrOperator);

end;
procedure RulesUninit;
begin
  Debug(dpSpam, 'rules', 'Uninit1');
  conditions.Free;
  prefixops.Free;
  infixops.Free;
  rules.Free;
  rtpl.Free;
  Debug(dpSpam, 'rules', 'Uninit2');
end;



{ TConditionReleaseName }

class function TConditionReleaseName.Description: string;
begin
  Result:=          'Returns with the name of the release.'+#13#10;
  Result:= Result + 'Example: if releasename =~ *-keygen* then DROP'+#13#10;
end;

function TConditionReleaseName.SupplyValue(r: TPazo): string;
begin
  try
    Result:= r.rls.rlsname;
  except
    Result:= '';
  end;
end;

class function TConditionReleaseName.Name: string;
begin
  Result:= 'releasename';
end;

{ TConditionSection }

class function TConditionSection.Description: string;
begin
  Result:=          'Returns with the section name of the release.'+#13#10;
  Result:= Result + 'if section = TV then DROP'+#13#10;
end;

function TConditionSection.SupplyValue(r: TPazo): string;
begin
  try
    Result:= r.rls.section;
  except
    Result:= '';
  end;
end;

class function TConditionSection.Name: string;
begin
  Result:= 'section';
end;

{ TConditionNotComplete }

class function TConditionNotComplete.Description: string;
begin
  Result:=          'Returns with the list of the sites where the release is not yet complete.'+#13#10;
  Result:= Result + 'This is the negated complete condition. It is true if the release is not notAllowed at the specified site and it is not pre or complete.'+#13#10;
  Result:= Result + 'For eg, you can add this rule on a slow 100mbit site: if notcomplete @ GBITSITE1 && source = GBITSITE2 then DROP'+#13#10;
  Result:= Result + 'With this rule, 100mbit site will be started racing after gbitsite1 is finished.'+#13#10;
end;

procedure TConditionNotComplete.SupplyValues(r: TPazo; re: TStringList);
var ps: TPazoSite;
    i: Integer;
begin
  try
    for i:= 0 to r.sites.Count-1 do
    begin
      if i > r.sites.Count then Break;
      try
        ps:= TPazoSite(r.sites[i]);
        if ((ps.status <> rssNotAllowed) and (not ps.Complete)) then
          re.Add(ps.name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionNotComplete.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionNotComplete.Name: string;
begin
  Result:= 'notcomplete';
end;

{ TConditionComplete }

class function TConditionComplete.Description: string;
begin
  Result:=          'Returns with the list of the sites, where the release is complete.'+#13#10;
  Result:= Result + 'if complete @ SITENAME && source != SITENAME then DROP'+#13#10;
end;

procedure TConditionComplete.SupplyValues(r: TPazo; re: TStringList);
var ps: TPazoSite;
    i: Integer;
begin
  try
    for i:= 0 to r.sites.Count-1 do
    begin
      if i > r.sites.Count then Break;
      try
        ps:= TPazoSite(r.sites[i]);
        if ps.Complete then
          re.Add(ps.name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionComplete.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionComplete.Name: string;
begin
  Result:= 'complete';
end;


{ TConditionPre }

class function TConditionPre.Description: string;
begin
  Result:=          'Returns true, if the release is pred on the specified site.'+#13#10;
  Result:= Result + 'If you add this rule on a slow site, pres wont be sent there til gbitsite is incomplete: '+#13#10;
  Result:= Result + 'if pre @ AFFILSITE && incomplete @ GBITSITE then DROP'+#13#10;
end;

procedure TConditionPre.SupplyValues(r: TPazo; re: TStringList);
var ps: TPazoSite;
    i: Integer;
begin
  try
    for i:= 0 to r.sites.Count-1 do
    begin
      if i > r.sites.Count then Break;
      try
        ps:= TPazoSite(r.sites[i]);
        if ps.status = rssRealPre then
          re.Add(ps.name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionPre.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionPre.Name: string;
begin
  Result:= 'pre';
end;

{ TConditionAt }

function TAtOperator.AtConditionName: string;
begin
  try
    Result:= GetOperandValue;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TAtOperator.AtConditionName : %s', [e.Message]);
      Result:= '';
    end;
  end;
end;

class function TAtOperator.Name: string;
begin
  Result:= '@';
end;

{ TConditionNotAllowed }

class function TConditionNotAllowed.Description: string;
begin
  Result:=          'Returns true, if the release is not allowed on the specified site.'+#13#10;
  Result:= Result + 'Example: notallowed @ SITENAME'+#13#10;
end;

procedure TConditionNotAllowed.SupplyValues(r: TPazo; re: TStringList);
var ps: TPazoSite;
    i: Integer;
begin
  try
    for i:= 0 to r.sites.Count-1 do
    begin
      if i > r.sites.Count then Break;
      try
        ps:= TPazoSite(r.sites[i]);
        if ps.status = rssNotAllowed then
          re.Add(ps.name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionNotAllowed.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionNotAllowed.Name: string;
begin
  Result:= 'notallowed';
end;

{ TConditionAllowed }

class function TConditionAllowed.Description: string;
begin
  Result:=          'Returns true, if the release is not notallowed on the specified site.'+#13#10;
  Result:= Result + 'Example: allowed @ SITENAME'+#13#10;
end;

procedure TConditionAllowed.SupplyValues(r: TPazo; re: TStringList);
var ps: TPazoSite;
    i: Integer;
begin
  try
    for i:= 0 to r.sites.Count-1 do
    begin
      if i > r.sites.Count then Break;
      try
        ps:= TPazoSite(r.sites[i]);
        if ps.status = rssAllowed then
          re.Add(ps.name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionAllowed.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionAllowed.Name: string;
begin
  Result:= 'allowed';
end;

{ TConditionGroup }

class function TConditionGroup.Description: string;
begin
  Result:=          'Returns with the groupname.'+#13#10;
  Result:= Result + 'Example: group in GRP1, GRP2, GRP3'+#13#10;
end;

function TConditionGroup.SupplyValue(r: TPazo): string;
begin
  try
    Result:= r.rls.groupname;
  except
    Result:= '';
  end;
end;

class function TConditionGroup.Name: string;
begin
  Result:= 'group';
end;

{ TConditionKnownGroup }

class function TConditionKnownGroup.Description: string;
begin
  Result:=          'Returns true, if the groupname is known (you can set the list in slftp.ini if i remember well).'+#13#10;
  Result:= Result + 'Note: If no known groups is set for a section in slftp.knowngroups, this condition wont match at all!'+#13#10;
  Result:= Result + 'Example: !ruleadd * * ifnot knowngroup then DROP'+#13#10;
end;

function TConditionKnownGroup.SupplyValue(r: TPazo): Boolean;
begin
  try
    Result:= r.rls.knowngroup = grp_known;
  except
    Result:= false;
  end;
end;

class function TConditionKnownGroup.Name: string;
begin
  Result:= 'knowngroup';
end;

{ TConditionUnKnownGroup }

class function TConditionUnKnownGroup.Description: string;
begin
  Result:=          'Returns true, if the groupname is not known.'+#13#10;
  Result:= Result + 'Note: If no known groups is set for a section in slftp.knowngroups, this condition wont match at all!'+#13#10;
  Result:= Result + 'Example: !ruleadd * * if unknowngroup then DROP'+#13#10;
end;

function TConditionUnKnownGroup.SupplyValue(r: TPazo): Boolean;
begin
  try
    Result:= r.rls.knowngroup = grp_unknown;
  except
    Result:= false;
  end;
end;

class function TConditionUnKnownGroup.Name: string;
begin
  Result:= 'unknowngroup';
end;

{ TConditionSource }

class function TConditionSource.Description: string;
begin
  Result:=          'Returns with the name of the source site, which is currently fireing the ruleset.'+#13#10;
  Result:=          'You can use this function to setup static routing.'+#13#10;
  Result:= Result + 'For eg, we dont want to race SITE1''s MP3 section from SITE2 (but we want everything else):'+#13#10;
  Result:= Result + 'Example: !ruleadd SITE1 MP3 if source = SITE2 then DROP'+#13#10;
end;

function TConditionSource.SupplyValue(r: TPazo): string;
begin
  Result:= r.srcsite;
end;

class function TConditionSource.Name: string;
begin
  Result:= 'source';
end;

{ TConditionDestination }

class function TConditionDestination.Description: string;
begin
  Result:=          'Returns with the name of the destination site.'+#13#10;
  Result:= Result + 'You can use this function to make exceptions in generic rules.'+#13#10;
  Result:= Result + 'For eg, we want to drop stuffs detected as fake except on a few dumps: '+#13#10;
  Result:= Result + '!ruleadd * * if fake && destination notin DUMP1, DUMP2 then DROP'+#13#10;
end;

function TConditionDestination.SupplyValue(r: TPazo): string;
begin
  Result:= r.dstsite;
end;

class function TConditionDestination.Name: string;
begin
  Result:= 'destination';
end;

{ TConditionNewdir }

class function TConditionNewdirSource.Description: string;
begin
  Result:=          'Returns true, if source is newdir (not pre or complete).'+#13#10;
  Result:= Result + 'Example: if newdirsource then DROP'+#13#10;
end;

function TConditionNewdirSource.SupplyValue(r: TPazo): Boolean;
var x: TPazoSite;
begin
  Result:= False;
  try
    x:= r.FindSite(r.srcsite);
    if (x <> nil) then
      Result:= x.status = rssAllowed;
  except
    Result:= false;
  end;
end;

class function TConditionNewdirSource.Name: string;
begin
  Result:= 'newdirsource';
end;

{ TConditionMP3Genre }

class function TConditionMP3Genre.Description: string;
begin
  Result:=          'Returns with the mp3 genre.'+#13#10;
  Result:= Result + 'Example: if mp3genre =~ *Metal* then ALLOW'+#13#10;
end;

function TConditionMP3Genre.SupplyValue(r: TPazo): string;
begin
  Result:= '';
  try
    if r.rls is TMP3Release then
      Result:= TMP3Release(r.rls).mp3genre;
  except
    Result:= '';
  end;
end;

class function TConditionMP3Genre.Name: string;
begin
  Result:= 'mp3genre';
end;

{ TConditionMP3EYear }

class function TConditionMP3Year.Description: string;
begin
  Result:=          'Returns with the mp3 year'+#13#10;
  Result:= Result + 'Example: if mp3year < 2009 then DROP'+#13#10;
end;

function TConditionMP3Year.SupplyValue(r: TPazo): Integer;
begin
  Result:= 0;
  try
    if (r.rls is TMP3Release) then
      Result:= TMP3Release(r.rls).mp3year;
  except
    Result:= 0;
  end;
end;

class function TConditionMP3Year.Name: string;
begin
  Result:= 'mp3year';
end;

{ TConditionInternal }

class function TConditionInternal.Description: string;
begin
  Result:=          'Returns true, if the release is tagged as internal.'+#13#10;
  Result:= Result + 'Example: !ruleadd * * if internal && destination notin DUMP1, DUMP2 then DROP'+#13#10;
end;

function TConditionInternal.SupplyValue(r: TPazo): Boolean;
begin
  try
    Result:= r.rls.internal;
  except
    Result:= false;
  end;
end;

class function TConditionInternal.Name: string;
begin
  Result:= 'internal';
end;

{ TConditionMP3Language }

class function TConditionMP3Language.Description: string;
begin
  Result:=          'Returns with mp3 rip''s language tag. Language is EN by default'+#13#10;
  Result:= Result + 'Example: if mp3language != EN then DROP'+#13#10;
end;

function TConditionMP3Language.SupplyValue(r: TPazo): string;
begin
  Result:= '';
  try
    if (r.rls is TMP3Release) then
      Result:= Uppercase(TMP3Release(r.rls).mp3lng);
//    Result:= TMP3Release(r.rls).mp3lng;
  except
    Result:= '';
  end;
end;

class function TConditionMP3Language.Name: string;
begin
  Result:= 'mp3language';
end;


function TConditionMP3Language.Hitelesit(const s: string): Boolean;
begin
  try
    Result:= ((AnsiSameText(s, 'EN')) or (mp3languages.IndexOf(s) <> -1));
  except
    Result:= false;
  end;
end;

{ TConditionMP3Foreign }

class function TConditionMP3Foreign.Description: string;
begin
  Result:=          'Returns true, if the mp3 rip''s language is not EN.'+#13#10;
  Result:= Result + 'Example: if mp3foreign then DROP'+#13#10;
end;

function TConditionMP3Foreign.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  try
    if (r.rls is TMP3Release) then
      Result:= TMP3Release(r.rls).mp3lng <> 'EN';
  except
    Result:= false;
  end;
end;

class function TConditionMP3Foreign.Name: string;
begin
  Result:= 'mp3foreign';
end;

{ TConditionMP3Source }

class function TConditionMP3Source.Description: string;
begin
  Result:=          'Returns with the mp3 rip''s source.'+#13#10;
  Result:= Result + 'Example: if not ( mp3source in CD, CDR, DVD, VINYL ) then DROP'+#13#10;
end;

function TConditionMP3Source.SupplyValue(r: TPazo): string;
begin
  Result:= '';
  try
    if (r.rls is TMP3Release) then
      Result:= TMP3Release(r.rls).mp3source;
  except
    Result:= '';
  end;
end;

class function TConditionMP3Source.Name: string;
begin
  Result:= 'mp3source';
end;


function TConditionMP3Source.Hitelesit(const s: string): Boolean;
begin
  try
    Result:= mp3sources.IndexOfName(s) <> -1;
  except
    Result:= false;
  end;
end;

{ TConditionMP3Live }

class function TConditionMP3Live.Description: string;
begin
  Result:=          'Returns true, if the mp3 rip''s source is a live source. (You can define live source tags in slftp.ini i think)'+#13#10;
  Result:= Result + 'Example: if mp3live then DROP'+#13#10;
end;

function TConditionMP3Live.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  try
    if (r.rls is TMP3Release) then
      with TMP3Release(r.rls) do
        Result:= ((mp3source = 'LIVE') or (mp3type('LIVE')));
  except
    Result:= false;
  end;
end;

class function TConditionMP3Live.Name: string;
begin
  Result:= 'mp3live';
end;

{ TConditionDefault }

class function TConditionDefault.Description: string;
begin
  Result:= 'This condition simple matches anything, you can use it for default policy.'+#13#10;
  Result:= Result + 'If there is no matching rule then no action is taken which is same as DROP by default.' +#13#10;
  Result:= Result + 'Example: if default then ALLOW'+#13#10;
end;

function TConditionDefault.SupplyValue(r: TPazo): Boolean;
begin
  Result:= True;
end;

class function TConditionDefault.Name: string;
begin
  Result:= 'default';
end;

{ TConditionMP3Type }

class function TConditionMP3Type.Description: string;
begin
  Result:= 'Returns with the mp3 rip''s types.'+#13#10;
  Result:= Result + 'Example: if mp3type in Bootleg, Demo then DROP';
end;

procedure TConditionMP3Type.SupplyValues(r: TPazo; re: TStringList);
var mp: TMP3Release;
begin
  try
    if r.rls is TMP3Release then
    begin
      mp:= TMP3Release(r.rls);
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
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionMP3Type.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionMP3Type.Name: string;
begin
  Result:= 'mp3type';
end;

{ TConditionMP3Bootleg }

class function TConditionMP3Bootleg.Description: string;
begin
  Result:= 'Returns true if the mp3 rip is bootleg.';
end;

function TConditionMP3Bootleg.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  try
    if (r.rls is TMP3Release) then
      Result:= TMP3Release(r.rls).bootleg;
  except
    Result:= false;
  end;
end;

class function TConditionMP3Bootleg.Name: string;
begin
  Result:= 'mp3bootleg';
end;

{ TConditionMP3LtNumCDs }

class function TConditionMP3NumDisks.Description: string;
begin
  Result:= 'Returns with the number of disks of an mp3 release'+#13#10;
  Result:= REsult + 'We drop rips more than 2 DVD''s and everything else if more than 4 (cd''s):'+#13#10;
  Result:= REsult + 'if mp3numdisks > 2 && mp3source = DVD then DROP'+#13#10;
  Result:= REsult + 'if mp3numdisks > 4  then DROP'+#13#10;
end;

function TConditionMP3NumDisks.SupplyValue(r: TPazo): Integer;
begin
  Result:= 1;
  try
    if r.rls is TMP3Release then
      Result:= TMP3Release(r.rls).Numdisks;
  except
    Result:= 1;
  end;
end;

class function TConditionMP3NumDisks.Name: string;
begin
  Result:= 'mp3numdisks';
end;

{ TConditionMP3VA }

class function TConditionMP3VA.Description: string;
begin
  Result:= 'Returns true if the mp3 rip is a compilation. (VA)';
end;

function TConditionMP3VA.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  try
    if (r.rls is TMP3Release) then
      Result:= TMP3Release(r.rls).mp3_va;
  except
    Result:= false;
  end;
end;

class function TConditionMP3VA.Name: string;
begin
   Result:= 'mp3va';
end;

{ TConditionAgeGt }

class function TConditionAge.Description: string;
begin
  Result:= 'This is useful for filtering old stuffs scanned by autodirlist in a not dated directory.'+ #13#10;
  Result:= Result + 'It expects the parameter in seconds. Example: if age > 86400 then DROP'+#13#10;
end;

function TConditionAge.SupplyValue(r: TPazo): Integer;
begin
  try
    Result:= r.Age;
  except
    Result:= 0;
  end;
end;

class function TConditionAge.Name: string;
begin
  Result:= 'age';
end;


{ TConditionNfoMGenre }

class function TConditionNfoMGenre.Description: string;
begin
  Result:= 'Checks for genre parsed from the nfo file. As its a stupid textfile, use masks.'+ #13#10;
  Result:= Result + 'Note: nfogenre is for mdvdr/mv sections! For mp3 stuffs use mp3genre condition.'+ #13#10;
  Result:= Result + 'Genre string contains latin alphabet only, all other chars are replaced to spaces!'+ #13#10;
  Result:= Result + 'Example: if nfogenre =~ *Hip*Hop* then ALLOW'+#13#10;
end;

function TConditionNfoMGenre.SupplyValue(r: TPazo): string;
begin
  Result:= '';
  try
    if r.rls is TNFORelease then
      Result:= TNFORelease(r.rls).nfogenre;
  except
    Result:= '';
  end;
end;

class function TConditionNfoMGenre.Name: string;
begin
  Result:= 'nfogenre';
end;


constructor TConditionNfoMGenre.Create(parent: TRuleNode);
begin
  inherited;
  acceptedOperators.Clear;
  acceptedOperators.Add(TMaskOperator);
end;

{ TConditionFake }

class function TConditionFake.Description: string;
begin
  Result:= 'Returns true if the release was recognized as fake by the fakechecker.'+#13#10;
end;

function TConditionFake.SupplyValue(r: TPazo): Boolean;
begin
  try
    Result:= r.rls.Fake;
  except
    Result:= false;
  end;
end;

class function TConditionFake.Name: string;
begin
  Result:= 'fake';
end;

{ TConditionTVShowName }

class function TConditionTVShowName.Description: string;
begin
  Result:= 'Returns with the tvshow''s name. This is the part before the S01E01 or 1x12 tag.';
end;

function TConditionTVShowName.SupplyValue(r: TPazo): string;
begin
  Result:= '';
  try
    if r.rls is TTVRelease then
      Result:= TTVRelease(r.rls).showname;
  except
    Result:= '';
  end;
end;

class function TConditionTVShowName.Name: string;
begin
  Result:= 'tvshowname';
end;

{ TConditionForeign }

class function TConditionForeign.Description: string;
begin
  Result:= 'Returns true, if the releasename has a language tag (other than english).'+#13#10;
  Result:= Result + 'Note: as mp3 rips have 2 letter codes, use mp3foreign and mp3language conditions.'+#13#10;
end;

function TConditionForeign.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;

  try
    if r.rls is TMP3Release then exit;
    if r.rls.languages.Count = 0 then exit;
    if (r.rls.languages[0] = 'English') then exit;
    Result:= True;
  except
    Result:= False;
  end;
end;

class function TConditionForeign.Name: string;
begin
  Result:= 'foreign';
end;

{ TConditionLanguageA }

class function TConditionLanguage.Description: string;
begin
  Result:= 'Returns with the recognized language tags of the release.'+#13#10;
  Result:= Result + 'Note: as mp3 rips have 2 letter codes, use mp3foreign and mp3language conditions.'+#13#10;
  Result:= Result + 'ifnot language in English, Slowenian, Chinese then DROP'+#13#10;
end;

procedure TConditionLanguage.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    re.Assign(r.rls.languages);
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionLanguage.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionLanguage.Name: string;
begin
  Result:= 'language';
end;


function InArray(var name: string; elements: TClassList): TCRuleNode;
var i: Integer;
begin
  Result:= nil;
  try
    for i:= 0 to elements.Count -1 do
      if TCRuleNode(elements[i]).TakeThis( name ) then
      begin
        Result:= TCRuleNode(elements[i]);
        exit;
      end;
  except
    Result:= nil;
  end;
end;

function ParseRule(rule: string; var error: string): TRuleNode;
type TMitVarunk = (mvFelteteltVagyPrefixet, mvOperatort, mvInfixOrSuffix, mvOperandus, mvOperandusOrInfixOrSuffix, mvInfix);
var s: string;
    top: TRuleNode;
    mv: TMitVarunk;
    cr: TCRuleNode;
    c: TCondition;
    cco: TConditionOperatorClass;
    co: TConditionOperator;
    ifo: TInfixOperator;
    rule_read: Integer;

    function TopRight: Boolean;
    begin
      Result:= True;
              while (
                  (top.parent <> nil)
                  and
                  (top.parent is TPrefixOperator)
                  and
                  (not (top.parent is TOpeningBracketOperator))
                 ) do
                top:= top.parent;

              if (
                  (top.parent <> nil)
                  and
                  (top.parent is TInfixOperator)
                 ) then
              begin
                TInfixOperator(top.parent).right:= top;
                top:= top.parent;
              end;
    end;
    function infixorsuffix: Boolean;
    begin
      Result:= False;
            if TClosingBracketOperator.TakeThis(s) then
            begin
              if not TopRight then exit;

              if not (top.parent is TOpeningBracketOperator) then
              begin
                error:= 'Syntax error, unexpected closing bracket';
                top.Free;
                exit;
              end else
              if TOpeningBracketOperator(top.parent).child = nil then
              begin
                error:= 'Syntax error, empty parentheses';
                top.Free;
                exit;
              end
              else
              begin
                top:= top.parent;
                mv:= mvInfix;
                Result:= True;
                exit; 
              end;
            end;

            cr:= InArray(s, infixops);
            if cr <> nil then
            begin
              if not TopRight then exit;

              ifo:= TInfixOperator(cr.Create(top.parent));
              ifo.left:= top;
              top.parent:= ifo;
              top:= ifo;
              mv:= mvFelteteltVagyPrefixet;
              Result:= True;
              exit;
            end;
    end;
    function AddOperator: Boolean;
    begin
      Result:= False;
            c:= TCondition(top);
            cco:= c.TakesThisOperator(s);
            if cco = nil then
            begin
              error:= c.Name+' doesnt take operator '+s;
              top.Free;
              exit;
            end;

            co:= cco.Create(top.parent);
            co.condition:= c;
            c.parent:= co;
            top:= co;

            if cco = TBooleanOperator then
            begin
              if not TopRight then exit;
              mv:= mvInfixOrSuffix;
            end
            else
              mv:= mvOperandus;
      Result:= True;
    end;
begin
  Result:= nil;
  co:= nil;
  top:= nil;
  mv:= mvFelteteltVagyPrefixet;
  rule_read:= 0;
  
  while(true) do
  begin
    Inc(rule_read);
    if (rule_read > 250) then
    begin
      debugunit.Debug(dpError, 'rules', '[iNFO] ParseRule count break', []);
      break;
    end;
    
    s:= Fetch(rule, ' ');
    if s = '' then Break;

    while (s <> '') do
    begin
      case mv of
        mvFelteteltVagyPrefixet:
          begin
            cr:= InArray(s, prefixops);
            if cr <> nil then
            begin
              top:= cr.Create(top);
              Continue;
            end;

            cr:= InArray(s, conditions);
            if cr <> nil then
            begin
              top:= cr.Create(top);
              mv:= mvOperatort;
              Continue;
            end;

            error:= 'Syntax error, expecting prefix operator or condition name, got: '+s;
            if top <> nil then top.Free;
            exit;
          end;
        mvOperatort:
          begin
            if not AddOperator then exit;
          end;
        mvOperandus:
          begin
            if not co.FeedOperand(s) then
            begin
              error:= 'Condition '+co.condition.Name+' doesnt take operand: '+s;
              top.Free;
              exit; 
            end;
            mv:= mvOperandusOrInfixOrSuffix;
          end;
        mvInfixOrSuffix:
          begin
            if infixorsuffix() then Continue;
            if error <> '' then exit;


            error:= 'Infix or suffix operator expected, got: '+s;
            top.Free;
            exit;

          end;
        mvOperandusOrInfixOrSuffix:
          begin
            if infixorsuffix() then Continue;
            if error <> '' then exit;

            // most mar csak etetni lehet
            if not TConditionOperator(top).FeedOperand(s) then
            begin
              error:= 'Condition '+TConditionOperator(top).condition.Name+' doesnt take operand: '+s;
              top.Free;
              exit;
            end;
          end;
        mvInfix:
          begin
            if infixorsuffix then Continue;
            if error <> '' then exit;

              error:= 'Syntax error, infix/suffix operator expected, got: '+s;
              top.Free;
              exit;
          end;
      end;
    end;
  end;

  if top = nil then
  begin
    error:= 'No rules specified';
    exit;
  end;

  if ((top is TBooleanCondition) and (mv = mvOperatort)) then
    AddOperator
  else
  begin

    if (top is TCondition) then
    begin
      error:= 'Operator for '+top.Name+' not specified';
      top.Free;
      exit;
    end;
  
    if ((top is TConditionOperator) and (TConditionOperator(top).operand = nil)) then
    begin
      error:= 'Operand for '+top.Name+' not specified';
      top.Free;
      exit;
    end;
  
  end;

  if ((top is TInfixOperator) and (TInfixOperator(top).right = nil)) then
  begin
    top.Free;
    error:= 'Syntax error, right side of an infix operator is not specified';
    exit;
  end;

  while (top.parent <> nil) do
  begin
    if top.parent is TOpeningBracketOperator then
    begin
      top.Free;
      error:= 'Check parentheses';
      exit;
    end;
    if not TopRight then exit;
    if ((top <> nil) and (top.parent <> nil)) then
      top:= top.parent;
  end;


  Result:= top;
end;



{ TRule }

function TRule.AsText(includeSitesection: Boolean): string;
begin
  Result:= '';
  if includeSitesection then
    Result:= sitename +' '+section+' ';
  Result:= Result+ 'if '+ conditions.AsText;

  Result:= Result + ' then ';
  if action = raDrop then
    Result:= Result + 'DROP'
  else if action = raDontmatch then
    Result:= Result + 'Dont Match'
  else
    Result:= Result + 'ALLOW';  
end;

constructor TRule.Create(rule: string);
begin
  error:= '';
  reparse(rule);
end;

destructor TRule.Destroy;
begin
  conditions.Free;
  inherited;
end;

function TRule.Execute(r: TPazo): TRuleAction;
begin
  Result:= raDontmatch;

  try
    if not conditions.Match(r) then exit;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TRule.Execute : %s', [e.Message]);
      Result:= raDontmatch;
      exit;
    end;
  end;

  // kulonben az alap akcio
  try
    Result:= action;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TRule.Execute : %s', [e.Message]);
      Result:= raDontmatch;
      exit;
    end;
  end;
end;

procedure TRule.Reparse(rule: string);
var i: Integer;
    ifstr, thenstr, actionstr, conditionstr: string;
    isnot: Boolean;
begin
  sitename:= UpperCase(SubString(rule, ' ', 1));
  section:= UpperCase(SubString(rule, ' ', 2));

  if sitename = '' then
  begin
    error:= 'Sitename is invalid';
    exit;
  end;

  if section = '' then
  begin
    error:= 'Section is invalid';
    exit;
  end;

  rule:= Copy(rule, Length(sitename)+Length(section)+3, 1000);
  ifstr:= LowerCase(SubString(rule, ' ', 1));
  if ifstr = 'if' then
    isnot:= False
  else
  if ifstr = 'ifnot' then
    isnot:= True
  else
  begin
    error:= 'Rule must start with if/ifnot';
    exit;
  end;

  i:= Count(' ', rule);
  if i < 3 then
  begin
    error:= 'Rule is too short?';
    exit;
  end;

  thenstr:= LowerCase(SubString(rule, ' ',i));
  actionstr:= UpperCase(SubString(rule, ' ',i+1));
  if thenstr <> 'then' then
  begin
    error:= 'then missing';
    exit;
  end;

  if actionstr = 'DROP' then
    action:= raDrop
  else
  if actionstr = 'ACCEPT' then
    action:= raAllow
  else
  if actionstr = 'ALLOW' then
    action:= raAllow
  else
  begin
    error:= 'Rule must end with ALLOW/DROP';
    exit;
  end;

  if conditions <> nil then
    conditions.Free;

  conditionstr:= Copy(rule, Length(ifstr)+2, 1000);
  conditionstr:= Trim(Copy(conditionstr, 1, Length(conditionstr)-Length(actionstr)-Length(thenstr)-1));

  if isnot then
    conditionstr:= 'not ( '+conditionstr+' )';

  conditions:= ParseRule(conditionstr, error);
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

function TMultiStringEqualOperator.Match(p: TPazo): Boolean;
begin
  try
    re.Clear;
    GetSupplyValues(p, re);
    Result:= re.IndexOf(GetOperandValue) = 0;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TMultiStringEqualOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TMultiStringEqualOperator.Name: string;
begin
  Result:= '=';
end;

{ TMultiStringNotEqualOperator }

function TMultiStringNotEqualOperator.Match(p: TPazo): Boolean;
begin
  try
    Result:= not inherited Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TMultiStringNotEqualOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TMultiStringNotEqualOperator.Name: string;
begin
  Result:= '!=';
end;

{ TMultiNotInOperator }

function TMultiNotInOperator.Match(p: TPazo): Boolean;
begin
  try
    Result:= not inherited Match(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TMultiNotInOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TMultiNotInOperator.Name: string;
begin
  Result:= 'notin';
end;

{ TMultiInOperator }

function TMultiInOperator.FeedOperand(var s: string): Boolean;
begin
  if operand = nil then
    operand:= TListOperand.Create(self);

  Result:= TListOperand(operand).FeedOperand(s);
  if Result then
    s:= '';
end;

function TMultiInOperator.GetOperandValue: TStringList;
begin
  try
    Result:= TListOperand(operand).Value;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TMultiInOperator.GetOperandValue : %s', [e.Message]);
          result:=nil;
    end;
  end;
end;

function TMultiInOperator.Match(p: TPazo): Boolean;
var i: Integer;
    lista: TStringList;
begin
  try
    re.Clear;
    GetSupplyValues(p, re);
    lista:= GetOperandValue;
    Result:= True;
    for i:= lista.Count -1 downto 0 do
    begin
      if re.IndexOf(lista[i]) <> -1 then exit;
    end;
    Result:= False;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TMultiInOperator.Match : %s', [e.Message]);
      Result:= false;
    end;
  end;
end;

class function TMultiInOperator.Name: string;
begin
  Result:= 'in';
end;

{ TConditionCompleteSource }

class function TConditionCompleteSource.Description: string;
begin
  Result:= 'Returns true, if the source site is complete. You can use this rule to fill some sites from complete source only.';
end;

class function TConditionCompleteSource.Name: string;
begin
  Result:= 'completesource';
end;

function TConditionCompleteSource.SupplyValue(r: TPazo): Boolean;
var x: TPazoSite;
begin
  Result:= False;
  try
    x:= r.FindSite(r.srcsite);
    if (x <> nil) then
      Result:= x.Complete;
  except
    Result:= false;
  end;
end;


{ TConditionYear }

class function TConditionYear.Description: string;
begin
  Result:= 'Returns with the year tag of the release name. Returns with zero if year tag is not present.';
end;

class function TConditionYear.Name: string;
begin
  Result:= 'year';
end;

function TConditionYear.SupplyValue(r: TPazo): Integer;
begin
  try
    Result:= r.rls.year;
  except
    Result:= 0;
  end;
end;

{ TCondition0daySource }

class function TCondition0daySource.Description: string;
begin
  Result:=          'Returns with the 0day rip''s source/OS.'+#13#10;
  Result:= Result + 'Example: if not ( 0daysource in WIN, LINUX ) then DROP'+#13#10;
end;

function TCondition0daySource.Hitelesit(const s: string): Boolean;
begin
  try
    Result:= nulldaysources.IndexOfName(s) <> -1;
  except
    Result:= false;
  end;
end;

class function TCondition0daySource.Name: string;
begin
  Result:= '0daysource';
end;

function TCondition0daySource.SupplyValue(r: TPazo): string;
begin
  Result:= '';
  try
    if (r.rls is T0dayRelease) then
      Result:= T0dayRelease(r.rls).nulldaysource;
  except
    Result:= '';
  end;
end;


{ TConditionAutofollow }

class function TConditionAutofollow.Description: string;
begin
  Result:= 'Returns true, if there was already an irc announce on the site (mean: somebody started to send the rip there).'+#13#10;
end;

class function TConditionAutofollow.Name: string;
begin
  Result:= 'autofollow';
end;

function TConditionAutofollow.SupplyValue(r: TPazo): Boolean;
var ps: TPazoSite;
begin
  Result:= False;
  try
    ps:= r.FindSite( r.dstsite );
    if ps <> nil then
    begin
      Result:= ps.ircevent;
      if ((Result) and (not ps.Complete)) then
        ps.firesourcesinstead:= True;
    end;
  except
    Result:= false;
  end;
end;

{ TConditionNuked }

class function TConditionNuked.Description: string;
begin
  Result:= 'Returns true, if the rlz was nuked on the site.'+#13#10;
  Result:= Result + 'ex: !ruleadd * * if nuked then DROP';
end;

class function TConditionNuked.Name: string;
begin
  Result:= 'nuked';
end;

function TConditionNuked.SupplyValue(r: TPazo): Boolean;
var ps: TPazoSite;
begin
  Result:= False;
  try
    ps:= r.FindSite( r.dstsite );
    if ps <> nil then
    begin
      if ps.status = rssNuked then
        Result:= True;
    end;
  except
    Result:= false;
  end;
end;

{ TConditionPred }

class function TConditionPred.Description: string;
begin
  Result:= 'Returns true, if there was a pre on any site.'+#13#10;
end;

class function TConditionPred.Name: string;
begin
  Result:= 'pred';
end;

function TConditionPred.SupplyValue(r: TPazo): Boolean;
begin
  try
    Result:= r.rls.pred;
  except
    Result:= false;
  end;
end;

{ TConditionDisks }

class function TConditionDisks.Description: string;
begin
  Result:= 'Returns with the number of disks (for eg 3 for Foobar.2008.NTSC.3DiSC.MDVDR-GRP'+#13#10;
  Result:= 'NOTE: this is NOT for MP3 rips.';
end;

class function TConditionDisks.Name: string;
begin
  Result:= 'discs';
end;

function TConditionDisks.SupplyValue(r: TPazo): Integer;
begin
  try
    Result:= r.rls.disks;
  except
    Result:= 1;
  end;
end;
(*
{ TConditionPretime }

class function TConditionPretime.Description: string;
begin
  Result:= 'This is useful for anyone who needs to abide by certain pretime rules on a site.'+ #13#10;
  Result:= Result + 'It expects the parameter in seconds. Example: if pretime > 86400 && not pretimenotfound then DROP'+#13#10;
end;

function TConditionPretime.SupplyValue(r: TPazo): Integer;
begin

  Result:= SecondsBetween(Now, r.rls.pretime);
end;

class function TConditionPretime.Name: string;
begin
  Result:= 'pretime';
end;

{ TConditionPretimeFound }

class function TConditionPretimeFound.Description: string;
begin
  Result:= 'This is useful for anyone who needs to abide by certain pretime rules on a site.'+ #13#10;
  Result:= Result + 'It expects the parameter as boolean. Example: if pretime > 86400 && not pretimefound then DROP'+#13#10;
end;

function TConditionPretimeFound.SupplyValue(r: TPazo): Boolean;
begin

  Result:= r.rls.pretimefound;// SecondsBetween(Now, r.rls.pretime);
end;

class function TConditionPretimeFound.Name: string;
begin
  Result:= 'pretimefound';
end;

*)
{ TConditionTVPremierYear }

class function TConditionTVPremierYear.Description: string;
begin
  Result:= 'Returns with the year of premier (based on tvrage). Returns zero if lookup is not ready yet.';
end;

class function TConditionTVPremierYear.Name: string;
begin
  Result:= 'tvpremieryear';
end;

function TConditionTVPremierYear.SupplyValue(r: TPazo): Integer;
begin
  Result:= 0;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result:= TTVRelease(r.rls).premier_year;
    end;
  except
    Result:= 0;
  end;
end;

{ TConditionTVCountry }

class function TConditionTVCountry.Description: string;
begin
  Result:= 'Returns with the Country field parsed from tvrage.';
end;

class function TConditionTVCountry.Name: string;
begin
  Result:= 'tvcountry';
end;

function TConditionTVCountry.SupplyValue(r: TPazo): string;
begin
  Result:= '';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result:= TTVRelease(r.rls).country;
    end;
  except
    Result:= '';
  end;
end;

{ TConditionTVClassication }
class function TConditionTVClassification.Description: string;
begin
  Result:= 'Returns with the Classification field parsed from tvrage.';
end;

class function TConditionTVClassification.Name: string;
begin
  Result:= 'tvclassification';
end;

function TConditionTVClassification.SupplyValue(r: TPazo): string;
begin
  Result:='';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result:= TTVRelease(r.rls).classification;
    end;
  except
    Result:= '';
  end;
end;


{ TConditionTVScripted }

class function TConditionTVScripted.Description: string;
begin
  Result:= 'Returns with the Classification field parsed from tvrage. Returns false if lookup is not ready yet.';
end;

class function TConditionTVScripted.Name: string;
begin
  Result:= 'tvscripted';
end;

function TConditionTVScripted.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result:= TTVRelease(r.rls).scripted;
    end;
  except
    Result:= false;
  end;
end;

{ TConditionTVGenres }

class function TConditionTVGenres.Description: string;
begin
  Result:= 'Returns with the Genres field parsed from tvrage.';
end;

class function TConditionTVGenres.Name: string;
begin
  Result:= 'tvgenres';
end;

procedure TConditionTVGenres.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        re.Assign(TTVRelease(r.rls).genres);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionTVGenres.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TConditionTVNetwork }

class function TConditionTVNetwork.Description: string;
begin
  Result:= 'Returns with the Network field parsed from tvrage.';
end;

class function TConditionTVNetwork.Name: string;
begin
  Result:= 'tvnetwork';
end;

function TConditionTVNetwork.SupplyValue(r: TPazo): string;
begin
  Result:= '';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result:= TTVRelease(r.rls).network;
    end;
  except
    Result:= '';
  end;
end;

{ TConditionTVRuntime }

class function TConditionTVRuntime.Description: string;
begin
  Result:= 'Returns with the Runtime field parsed from tvrage. Returns 0 if lookup is not ready yet';
end;

class function TConditionTVRuntime.Name: string;
begin
  Result:= 'tvruntime';
end;

function TConditionTVRuntime.SupplyValue(r: TPazo): Integer;
begin
  Result:= 0;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result:= TTVRelease(r.rls).runtime;
    end;
  except
    Result:= 0;
  end;
end;


{ TConditionTVEndedYear }

class function TConditionTVEndedYear.Description: string;
begin
  Result:= 'Returns with the end year field parsed from tvrage. Returns 0 if lookup is not ready yet';
end;

class function TConditionTVEndedYear.Name: string;
begin
  Result:= 'tvendedyear';
end;

function TConditionTVEndedYear.SupplyValue(r: TPazo): Integer;
begin
  Result:= 0;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result:= TTVRelease(r.rls).ended_year;
    end;
  except
    Result:= 0;
  end;
end;

{ TConditionTVStatus }
class function TConditionTVStatus.Description: string;
begin
  Result:= 'Returns with the Status field parsed from tvrage.';//+#13#00;
end;

class function TConditionTVStatus.Name: string;
begin
  Result:= 'tvstatus';
end;

function TConditionTVStatus.SupplyValue(r: TPazo): string;
begin
  Result:='';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result:= TTVRelease(r.rls).status;
    end;
  except
    Result:= '';
  end;
end;

{ TConditionTVRunning }
class function TConditionTVRunning.Description: string;
begin
  Result:= 'Returns with the Status field parsed from tvrage. Returns false if lookup is not ready yet.';//+#13#10;
end;

class function TConditionTVRunning.Name: string;
begin
  Result:= 'tvrunning';
end;

function TConditionTVRunning.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result:= TTVRelease(r.rls).running;
    end;
  except
    Result:= false;
  end;
end;


{ TConditionTag }

class function TConditionTag.Description: string;
begin
  Result:= 'You can use tag condition to check if a specified tag exists in the releasename.'+#13#10;
  Result:= 'Basicly, tag is a shorthand to *.tag.*, *.tag-*, *-tag.*, *-tag-*'+#13#10;
  Result:= '!ruleadd HQ PS2 if tag = NTSC then DROP'+#13#10;
  Result:= '!ruleadd HQ TV if not tag in HDTV, PDTV then DROP';//+#13#10;
end;

class function TConditionTag.Name: string;
begin
  Result:= 'tag';
end;

procedure TConditionTag.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    re.Assign(r.rls.tags);
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionTag.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TConditionTVtag }

class function TConditionTVtag.Description: string;
begin
  Result:= 'Returns the "tv tag" of the release name. Check slftp.ini, [kb] tvtags.';
end;

class function TConditionTVtag.Name: string;
begin
  Result:= 'tvtag';
end;

function TConditionTVtag.SupplyValue(r: TPazo): string;
begin
  Result:= '';
  try
    if r.rls is TTvRelease then
      Result:= TTvRelease(r.rls).tvtag;
  except
    Result:= '';
  end;
end;



{ TConditionIMDBYear }

class function TConditionIMDBYear.Description: string;
begin
  Result:= 'Returns with the year of the movie''s release date. Returns zero if IMDB lookup is not yet ready.';//+#13#10;
end;

class function TConditionIMDBYear.Name: string;
begin
  Result:= 'imdbyear';
end;

function TConditionIMDBYear.SupplyValue(r: TPazo): Integer;
begin
  Result:= 0;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result:= TImdbRelease(r.rls).imdb_year;
    end;
  except
    Result:= 0;
  end;
end;

{ TConditionIMDBLanguages }

class function TConditionIMDBLanguages.Description: string;
begin
  Result:= 'Returns with the list of the movie''s languages.';
end;

class function TConditionIMDBLanguages.Name: string;
begin
  Result:= 'imdblanguages';
end;

procedure TConditionIMDBLanguages.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        re.Assign(TImdbRelease(r.rls).imdb_languages);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionIMDBLanguages.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TConditionIMDBCountries }

class function TConditionIMDBCountries.Description: string;
begin
  Result:= 'Returns with the list of the countries which cooperated in recording the movie.';
end;

class function TConditionIMDBCountries.Name: string;
begin
  Result:= 'imdbcountries';
end;

procedure TConditionIMDBCountries.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        re.Assign(TImdbRelease(r.rls).imdb_countries);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionIMDBCountries.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TConditionIMDBGenres }

class function TConditionIMDBGenres.Description: string;
begin
  Result:= 'Returns with the list of the movie''s genres.';
end;

class function TConditionIMDBGenres.Name: string;
begin
  Result:= 'imdbgenre';
end;

procedure TConditionIMDBGenres.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        re.Assign(TImdbRelease(r.rls).imdb_genres);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionIMDBGenres.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TConditionIMDBScreens }

class function TConditionIMDBScreens.Description: string;
begin
  Result:= 'Returns with the number of opening screens of the movie.';
end;

class function TConditionIMDBScreens.Name: string;
begin
  Result:= 'imdbscreens';
end;

function TConditionIMDBScreens.SupplyValue(r: TPazo): Integer;
begin
  Result:= 0;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result:= TImdbRelease(r.rls).imdb_screens;
    end;
  except
    Result:= 0;
  end;
end;

{ TConditionIMDBRating }

class function TConditionIMDBRating.Description: string;
begin
  Result:= 'Returns with the current IMDB rating of the movie MULTIPLIED by ten. (so max score is 100, min is 0)';
end;

class function TConditionIMDBRating.Name: string;
begin
  Result:= 'imdbrating';
end;

function TConditionIMDBRating.SupplyValue(r: TPazo): Integer;
begin
  Result:= 0;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result:= TImdbRelease(r.rls).imdb_rating;
    end;
  except
    Result:= 0;
  end;
end;

{ TConditionIMDBVotes }

class function TConditionIMDBVotes.Description: string;
begin
  Result:= 'Returns with the number of votes of the movie.';
end;

class function TConditionIMDBVotes.Name: string;
begin
  Result:= 'imdbvotes';
end;

function TConditionIMDBVotes.SupplyValue(r: TPazo): Integer;
begin
  Result:= 0;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result:= TImdbRelease(r.rls).imdb_votes;
    end;
  except
    Result:= 0;
  end;
end;

class function TConditionIMDBWide.Description: String;
begin
  Result:= 'Returns with the a boolean for Wide. Returns zero if IMDB lookup is not yet ready.';//+#13#10;
end;

class function TConditionIMDBWide.Name: string;
begin
  Result:= 'imdbwide';
end;

function TConditionIMDBWide.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result:= TImdbRelease(r.rls).imdb_wide;
    end;
  except
    Result:= false;
  end;
end;


class function TConditionIMDBldt.Description: String;
begin
  Result:= 'Returns with the a boolean for Limited. Returns zero if IMDB lookup is not yet ready.';//+#13#10;
end;

class function TConditionIMDBldt.Name: string;
begin
  Result:= 'imdblimited';
end;

function TConditionIMDBldt.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result:= TImdbRelease(r.rls).imdb_ldt;
    end;
  except
    Result:= false;
  end;
end;

class function TConditionIMDBFestival.Description: String;
begin
  Result:= 'Returns with the a boolean for Festival. Returns zero if IMDB lookup is not yet ready.';//+#13#10;
end;

class function TConditionIMDBFestival.Name: string;
begin
  Result:= 'imdbfestival';
end;

function TConditionIMDBFestival.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result:= TImdbRelease(r.rls).imdb_festival;
    end;
  except
    Result:= false;
  end;
end;

{ TConditionIMDBStv }

class function TConditionIMDBStv.Description: string;
begin
Result:='Returns true, if the movie is STV (Read Countries from slftp.ini [kb])';//+#13#10;
end;

class function TConditionIMDBStv.Name: string;
begin
  Result:= 'imdbstv';
end;

function TConditionIMDBStv.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result:= TImdbRelease(r.rls).imdb_stvm;
    end;
  except
    Result:= false;
  end;
end;


{ TConditionIMDBCineyear }

class function TConditionIMDBCineyear.Description: string;
begin
  Result:= 'Returns the Screeing year of the movie.';
end;

class function TConditionIMDBCineyear.Name: string;
begin
  Result:= 'imdbcineyear';
end;

function TConditionIMDBCineyear.SupplyValue(r: TPazo): Integer;
begin
  Result:= 0;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result:= TImdbRelease(r.rls).CineYear;
    end;
  except
    Result:= 0;
  end;
end;



(*
class function TConditionIMDBGenres.Description: string;
begin
  Result:= 'Returns with the list of the movie''s genres.';
end;

class function TConditionIMDBGenres.Name: string;
begin
  Result:= 'imdbgenre';
end;

procedure TConditionIMDBGenres.SupplyValues(r: TPazo; re: TStringList);
begin
  if r.rls is TIMDBRelease then
    re.Assign(TImdbRelease(r.rls).imdb_genres);
end;

*)
{ TConditionMVIDGenre }


class function TConditionMVIDGenre.Description: string;
begin
  Result:= 'Returns the Genre parsed from nfo.'+#13#10;
  //  Result:= Result + '' +#13#00;
end;

class function TConditionMVIDGenre.Name: string;
begin
  Result:= 'mvidgenre';
end;

procedure TConditionMVIDGenre.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    if r.rls is TMVIDRelease then
      re.Assign(TMvidRelease(r.rls).mvid_Genre);
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionMVIDGenre.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TConditionMVIDLanguage }

class function TConditionMVIDLanguage.Description: string;
begin
  Result:= 'Returns the Language parsed from nfo.'+#13#10;
  //  Result:= Result + '' +#13#00;
end;

class function TConditionMVIDLanguage.Name: string;
begin
  Result:= 'mvidlanguage';
end;

procedure TConditionMVIDLanguage.SupplyValues(r: TPazo; re: TStringList);
begin
  try
  if r.rls is TMVIDRelease then
  re.Assign(TMvidRelease(r.rls).Languages);
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionMVIDLanguage.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;


{ TConditionMVIDFiles }

class function TConditionMVIDFiles.Description: string;
begin
  Result:= 'Returns the number of files parsed from sfv. (yes ";" will not count)';//+#13#10;
  //  Result:= Result + '' +#13#00;
end;

class function TConditionMVIDFiles.Name: string;
begin
  Result:= 'mvidfiles';
end;

function TConditionMVIDFiles.SupplyValue(r: TPazo): Integer;
begin
  Result:= -1;
  if r.rls is TMVIDRelease then
    Result:= TMVIDRelease(r.rls).FileCount;
end;

{ TConditionMVIDYear }

class function TConditionMVIDYear.Description: string;
begin
  Result:= 'Returns with the year of the MVID  release date. Returns zero if NFO lookup is not yet ready.';//+#13#10;
  //  Result:= Result + '' +#13#00;
end;

class function TConditionMVIDYear.Name: string;
begin
  Result:= 'mvidyear';
end;

function TConditionMVIDYear.SupplyValue(r: TPazo): Integer;
begin
  Result:= 0;
  if r.rls is TMVIDRelease then
    Result:= TMVIDRelease(r.rls).mvid_year;
end;

{ TConditionMVIDVA }

class function TConditionMVIDVA.Description: String;
begin
  Result:= 'Returns true if release is Various Artists (VA)';//+#13#10;
  //  Result:= Result + '' +#13#00;
end;

class function TConditionMVIDVA.Name: string;
begin
  Result:= 'mvidva';
end;

function TConditionMVIDVA.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  if r.rls is TMVIDRelease then
    Result:= TMVIDRelease(r.rls).mvid_va;
end;

{ TConditionMVIDPAL }

class function TConditionMVIDPAL.Description: String;
begin
  Result:= 'Returns with the a boolean for PAL region.';//+#13#10;
  //  Result:= Result + '' +#13#10;
end;

class function TConditionMVIDPAL.Name: string;
begin
  Result:= 'mvidpal';
end;

function TConditionMVIDPAL.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  if r.rls is TMVIDRelease then
    Result:= TMVIDRelease(r.rls).mvid_pal;
end;

{ TConditionMVIDNTSC }

class function TConditionMVIDNTSC.Description: String;
begin
  Result:= 'Returns with the a boolean for NTSC region.';//+#13#10;
  //  Result:= Result + '' +#13#10;
end;

class function TConditionMVIDNTSC.Name: string;
begin
  Result:= 'mvidntsc';
end;

function TConditionMVIDNTSC.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  if r.rls is TMVIDRelease then
    Result:= TMVIDRelease(r.rls).mvid_ntsc;
end;

{ TConditionMVIDLIVE }

class function TConditionMVIDLIVE.Description: String;
begin
  Result:= 'Returns with the a boolean for LIVE type.';//+#13#10;
  //  Result:= Result + '' +#13#00;
end;

class function TConditionMVIDLIVE.Name: string;
begin
  Result:= 'mvidlive';
end;

function TConditionMVIDLIVE.SupplyValue(r: TPazo): Boolean;
begin
  Result:= False;
  if r.rls is TMVIDRelease then
    Result:= TMVIDRelease(r.rls).mvid_live;
end;



function FindConditionClassByName(name:string):TConditionClass;
var i: Integer;
begin
  result:=nil;
  try
    for i:= 0 to conditions.Count -1 do
    begin
      if TConditionClass(conditions[i]).name = name then
      begin
        result:=TConditionClass(conditions[i]);
        break;
      end;
    end;
  except
    Result:= nil;
  end;
end;

end.
