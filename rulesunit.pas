unit rulesunit;

interface

uses kb, slmasks, Classes, pazo, Contnrs;

type
  TRuleNode = class
    parent: TRuleNode;
    class function TakeThis(var s: AnsiString): boolean; virtual;
    procedure SetupChild(child: TRuleNode); virtual;
    constructor Create(parent: TRuleNode); virtual;
    class function Name: AnsiString; virtual; abstract;
    function Match(p: TPazo): boolean; virtual; abstract;
    function AsText: AnsiString; virtual; abstract;
    function AtConditionName: AnsiString; virtual; abstract;
  end;

  TCRuleNode = class of TRuleNode;

  TOperator = class(TRuleNode)
  end;

  TPrefixOperator = class(TOperator)
    child: TRuleNode;
    function AsText: AnsiString; override;
    destructor Destroy; override;
    procedure SetupChild(child: TRuleNode); override;
    function AtConditionName: AnsiString; override;
  end;

  TNotOperator = class(TPrefixOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TOpeningBracketOperator = class(TPrefixOperator)
    class function Name: AnsiString; override;
    function AsText: AnsiString; override;
    function Match(p: TPazo): boolean; override;
    destructor Destroy; override;
  end;

  TClosingBracketOperator = class(TOperator)
    class function Name: AnsiString; override;
  end;

  TInfixOperator = class(TOperator)
    left: TRuleNode;
    right: TRuleNode;

    function AsText: AnsiString; override;
    destructor Destroy; override;
    function AtConditionName: AnsiString; override;
  end;

  TOrOperator = class(TInfixOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TAndOperator = class(TInfixOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TOperand = class(TRuleNode)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
    function AtConditionName: AnsiString; override;
  end;

  TStringOperand = class(TOperand)
  private
    fValue: AnsiString;
    nelegyenhitelesites: boolean;
  public
    function AsText: AnsiString; override;
    function Value: AnsiString;
    function FeedOperand(const s: AnsiString): boolean; virtual;
  end;

  TIntOperand = class(TOperand)
  private
    fValue: integer;
  public
    function AsText: AnsiString; override;
    function Value: integer;
    function FeedOperand(const s: AnsiString): boolean;
  end;

  TMaskOperand = class(TStringOperand)
  private
    maskValue: TslMask;
  public
    constructor Create(parent: TRuleNode); override;
    function AsText: AnsiString; override;
    function Value: TslMask;
    destructor Destroy; override;
    function FeedOperand(const s: AnsiString): boolean; override;
  end;

  TListOperand = class(TStringOperand)
  private
    listValue: TStringList;
    procedure Reparse;
  public
    constructor Create(parent: TRuleNode); override;
    function AsText: AnsiString; override;
    function Value: TStringList;
    destructor Destroy; override;
    function FeedOperand(const s: AnsiString): boolean; override;
  end;

  TCondition = class; //forward

  TConditionOperator = class(TOperator)
    condition: TCondition;
    operand: TOperand;
    function AsText: AnsiString; override;
    destructor Destroy; override;
    function FeedOperand(var s: AnsiString): boolean; virtual; abstract;
    function AtConditionName: AnsiString; override;
  end;

  TBooleanOperator = class(TConditionOperator)
    class function TakeThis(var s: AnsiString): boolean; override;
    class function Name: AnsiString; override;
    function GetSupplyValue(p: TPazo): boolean;
    function AsText: AnsiString; override;
    function Match(p: TPazo): boolean; override;
    function FeedOperand(var s: AnsiString): boolean; override;
  end;

  TStringOperator = class(TConditionOperator)
    function GetOperandValue: AnsiString;
    function GetSupplyValue(p: TPazo): AnsiString;
    function FeedOperand(var s: AnsiString): boolean; override;
  end;

  TMultiStringOperator = class(TConditionOperator)
  private
    re: TStringList;
  public
    constructor Create(parent: TRuleNode); override;
    destructor Destroy; override;
    function GetOperandValue: AnsiString;
    procedure GetSupplyValues(p: TPazo; re: TStringList);
    function FeedOperand(var s: AnsiString): boolean; override;
  end;

  TIntOperator = class(TConditionOperator)
    function GetOperandValue: integer;
    function GetSupplyValue(p: TPazo): integer;
    function FeedOperand(var s: AnsiString): boolean; override;
  end;

  TMaskOperator = class(TStringOperator)
    class function Name: AnsiString; override;
    function GetOperandValue: TslMask;
    function Match(p: TPazo): boolean; override;
    function FeedOperand(var s: AnsiString): boolean; override;
  end;

  TNotMaskOperator = class(TMaskOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TInOperator = class(TStringOperator)
    function GetOperandValue: TStringList;
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
    function FeedOperand(var s: AnsiString): boolean; override;
  end;

  TNotInOperator = class(TInOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TStringEqualOperator = class(TStringOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TStringNotEqualOperator = class(TStringEqualOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TMultiStringEqualOperator = class(TMultiStringOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TMultiStringNotEqualOperator = class(TMultiStringEqualOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TMultiInOperator = class(TMultiStringOperator)
    function GetOperandValue: TStringList;
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
    function FeedOperand(var s: AnsiString): boolean; override;
  end;

  TMultiNotInOperator = class(TMultiInOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TAtOperator = class(TMultiStringEqualOperator)
    class function Name: AnsiString; override;
    function AtConditionName: AnsiString; override;
  end;

  TIntEqualOperator = class(TIntOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TIntNotEqualOperator = class(TIntEqualOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TIntBiggerOrEqualThanOperator = class(TIntOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TIntBiggerThanOperator = class(TIntOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TIntLowerThanOperator = class(TIntOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TIntLowerOrEqualThanOperator = class(TIntOperator)
    class function Name: AnsiString; override;
    function Match(p: TPazo): boolean; override;
  end;

  TConditionOperatorClass = class of TConditionOperator;

  TCondition = class(TRuleNode)
    acceptedOperators: TClassList;

    function Hitelesit(const s: AnsiString): boolean; virtual;
    function AsText: AnsiString; override;
    function Match(p: TPazo): boolean; override;
    class function Description: AnsiString; virtual; abstract;
    class function AcceptedOperatorsAsText: AnsiString;
    function TakesThisOperator(var op: AnsiString): TConditionOperatorClass;
    constructor Create(parent: TRuleNode); override;
    destructor Destroy; override;
  end;

  TStringCondition = class(TCondition)
  public
    constructor Create(parent: TRuleNode); override;
    function SupplyValue(r: TPazo): AnsiString; virtual; abstract;
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
    function SupplyValue(r: TPazo): AnsiString; override;
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

  TRuleAction = (raDrop, raAllow, raDontmatch);

  TRule = class
    sitename: AnsiString;
    section: AnsiString;
    conditions: TRuleNode;
    action: TRuleAction;
    error: AnsiString;

    function Execute(r: TPazo): TRuleAction;

    function AsText(const includeSitesection: boolean): AnsiString;
    procedure Reparse(rule: AnsiString);
    constructor Create(const rule: AnsiString);
    destructor Destroy; override;
  end;

procedure RulesRemove(const sitename, section: AnsiString);
procedure RulesSave;
procedure RulesStart;
procedure RulesReload;
procedure RulesLoad(const action, filename: AnsiString);
function AddRule(const rule: AnsiString; var error: AnsiString): TRule;
procedure RulesOrder(p: TPazo);
function FireRuleSet(p: TPazo; ps: TPazoSite): TRuleAction;
function FireRules(p: TPazo; ps: TPazoSite): boolean;
procedure RulesInit;
procedure RulesUninit;

function FindConditionClassByName(const Name: AnsiString): TConditionClass;

var
  rules: TObjectList;
  rtpl: TObjectList;
  conditions: TClassList;

implementation

uses
  SysUtils, Math, sitesunit, queueunit, mystrings, encinifile, debugunit, configunit,
  knowngroups, DateUtils {$IFDEF MSWINDOWS}, Windows{$ENDIF};

const
  dsection = 'rules';

{$I rulesunit.inc}

type
  TPrefixOperatorClass = class of TPrefixOperator;
  TInfixOperatorClass = class of TInfixOperator;

  TConditionReleaseName = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionSection = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionInternal = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionAge = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
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
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionNotComplete = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionPre = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionAllowed = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionNotAllowed = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionGroup = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionFake = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionForeign = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionLanguage = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionYear = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionKnownGroup = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionUnKnownGroup = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionSource = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionDestination = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionCompleteSource = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionNewdirSource = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionNuked = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TCondition0daySource = class(TStringCondition)
    function Hitelesit(const s: AnsiString): boolean; override;
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTag = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionDisks = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionAutofollow = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionPred = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  (*###      MP3       ###*)
  TConditionMP3Genre = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMP3Year = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMP3Language = class(TStringCondition)
    function Hitelesit(const s: AnsiString): boolean; override;
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMP3Foreign = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMP3Source = class(TStringCondition)
    function Hitelesit(const s: AnsiString): boolean; override;
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMP3Live = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMP3Type = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMP3Bootleg = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMP3NumDisks = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMP3VA = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  (*###      NFO       ###*)
  TConditionNfoMGenre = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    constructor Create(parent: TRuleNode); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  (*###      IMDB       ###*)
  TConditionIMDBYear = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionIMDBLanguages = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionIMDBCountries = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionIMDBGenres = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionIMDBScreens = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionIMDBStv = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionIMDBRating = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionIMDBVotes = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionIMDBldt = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionIMDBWide = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionIMDBfestival = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionIMDBCineyear = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVShowName = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVtag = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVPremierYear = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVCountry = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVLanguage = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVClassification = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVScripted = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVGenres = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVNetwork = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVRuntime = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVEndedYear = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVRunning = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVStatus = class(TStringCondition)
    function SupplyValue(r: TPazo): AnsiString; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVCurrentSeason = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVCurrentEpisiode = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVCurrentOnAir = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionTVDailyShow = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  {###      MVID    ###}
  TConditionMVIDGenre = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMVIDFiles = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMVIDYear = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMVIDVA = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMVIDPAL = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMVIDNTSC = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMVIDLive = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionMVIDLanguage = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;

  TConditionDefault = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: AnsiString; override;
    class function Description: AnsiString; override;
  end;
  // NE FELEJTSD EL LENT ADDOLNI A LISTABA !!!

var
  prefixops: TClassList;
  infixops: TClassList;

  { TInfixOperator }

function TInfixOperator.AsText: AnsiString;
begin
  try
    Result := left.AsText + ' ' + Name + ' ' + right.AsText;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TInfixOperator.AsText : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

function TInfixOperator.AtConditionName: AnsiString;
begin
  try
    Result := left.AtConditionName;
    if Result = '' then
      Result := right.AtConditionName;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TInfixOperator.AtConditionName : %s', [e.Message]);
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

function TPrefixOperator.AsText: AnsiString;
begin
  try
    Result := Name + ' ' + child.AsText;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TPrefixOperator.AsText : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

function TPrefixOperator.AtConditionName: AnsiString;
begin
  try
    Result := child.AtConditionName;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TPrefixOperator.AtConditionName : %s', [e.Message]);
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

function TOpeningBracketOperator.AsText: AnsiString;
begin
  try
    Result := '( ' + child.AsText + ' )';
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TOpeningBracketOperator.AsText : %s', [e.Message]);
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
      Debug(dpError, 'rules', 'TOpeningBracketOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TOpeningBracketOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TOrOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TOrOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TAndOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TAndOperator.Name: AnsiString;
begin
  Result := '&&';
end;

{ TNoOperator }

function TBooleanOperator.AsText: AnsiString;
begin
  try
    Result := condition.AsText;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TBooleanOperator.AsText : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

function TBooleanOperator.FeedOperand(var s: AnsiString): boolean;
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
      Debug(dpError, 'rules', 'TBooleanOperator.GetSupplyValue : %s', [e.Message]);
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
      Debug(dpError, 'rules', 'TBooleanOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TBooleanOperator.Name: AnsiString;
begin
  Result := '';
end;

{ TInOperator }

function TInOperator.FeedOperand(var s: AnsiString): boolean;
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
      Debug(dpError, 'rules', 'TInOperator.GetOperandValue : %s', [e.Message]);
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
      Debug(dpError, 'rules', 'TInOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TInOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TNotInOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TNotInOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TStringEqualOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TStringEqualOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TStringNotEqualOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TStringNotEqualOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TIntEqualOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TIntEqualOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TIntNotEqualOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TIntNotEqualOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TIntBiggerOrEqualThanOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TIntBiggerOrEqualThanOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TIntBiggerThanOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TIntBiggerThanOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TIntLowerThanOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TIntLowerThanOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TIntLowerOrEqualThanOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TIntLowerOrEqualThanOperator.Name: AnsiString;
begin
  Result := '<=';
end;

{ TCondition }

class function TCondition.AcceptedOperatorsAsText: AnsiString;
var
  i: integer;
  c: TCondition;
begin
  c := self.Create(nil);
  try
    Result := '';
    for i := 0 to c.acceptedOperators.Count - 1 do
      Result := Result + TConditionOperatorClass(c.acceptedOperators[i]).Name + ' ';

    Result := trim(Result);

  finally
    c.Free;
  end;
end;

function TCondition.AsText: AnsiString;
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

function TCondition.Hitelesit(const s: AnsiString): boolean;
begin
  Result := True;
end;

function TCondition.Match(p: TPazo): boolean;
begin
  Result := False; // exception
end;

function TCondition.TakesThisOperator(var op: AnsiString): TConditionOperatorClass;
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

function TMaskOperator.FeedOperand(var s: AnsiString): boolean;
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
      Debug(dpError, 'rules', 'TMaskOperator.GetOperandValue : %s', [e.Message]);
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
      Debug(dpError, 'rules', 'TMaskOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TMaskOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TNotMaskOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TNotMaskOperator.Name: AnsiString;
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

class function TRuleNode.TakeThis(var s: AnsiString): boolean;
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

function TStringOperand.AsText: AnsiString;
begin
  Result := Value;
end;

function TStringOperand.FeedOperand(const s: AnsiString): boolean;
begin
  if s = '' then
  begin
    Result := True;
    exit;
  end;

  if not nelegyenhitelesites then
  begin
    Result := TConditionOperator(parent).condition.hitelesit(s);
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

function TStringOperand.Value: AnsiString;
begin
  Result := fValue;
end;

{ TIntOperand }

function TIntOperand.AsText: AnsiString;
begin
  Result := IntToStr(Value);
end;

function TIntOperand.FeedOperand(const s: AnsiString): boolean;
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

function TListOperand.AsText: AnsiString;
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

function TListOperand.FeedOperand(const s: AnsiString): boolean;
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
      Debug(dpError, 'rules', 'TListOperand.FeedOperand : %s', [e.Message]);
      fValue := '';
      Result := False;
    end;
  end;

  Reparse;
end;

procedure TListOperand.Reparse;
var
  s, fs: AnsiString;
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
        debugunit.Debug(dpError, 'rules', '[ERROR] TListOperand.Reparse count break', []);
        break;
      end;
      Inc(operand_read);
      s := Trim(Fetch(fs, ','));
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
      Debug(dpError, 'rules', 'TListOperand.Reparse : %s', [e.Message]);
      listValue.Clear;
    end;
  end;
end;

function TListOperand.Value: TStringList;
begin
  Result := listValue;
end;

{ TMaskOperand }

function TMaskOperand.AsText: AnsiString;
begin
  Result := fValue;
end;

constructor TMaskOperand.Create(parent: TRuleNode);
begin
  inherited;
  nelegyenhitelesites := True;
end;

destructor TMaskOperand.Destroy;
begin
  if maskValue <> nil then
    maskValue.Free;
  inherited;
end;

function TMaskOperand.FeedOperand(const s: AnsiString): boolean;
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

function TStringOperator.FeedOperand(var s: AnsiString): boolean;
begin
  if operand = nil then
    operand := TStringOperand.Create(self);

  Result := TStringOperand(operand).FeedOperand(s);
  if Result then
    s := '';
end;

function TStringOperator.GetOperandValue: AnsiString;
begin
  try
    Result := TStringOperand(operand).Value;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TStringOperator.GetOperandValue : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

function TStringOperator.GetSupplyValue(p: TPazo): AnsiString;
begin
  try
    Result := TStringCondition(condition).SupplyValue(p);
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TStringOperator.GetSupplyValue : %s', [e.Message]);
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

function TMultiStringOperator.FeedOperand(var s: AnsiString): boolean;
begin
  if operand = nil then
    operand := TStringOperand.Create(self);

  Result := TStringOperand(operand).FeedOperand(s);
  if Result then
    s := '';
end;

function TMultiStringOperator.GetOperandValue: AnsiString;
begin
  try
    Result := TStringOperand(operand).Value;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TMultiStringOperator.GetOperandValue : %s', [e.Message]);
      Result := '';
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
      Debug(dpError, 'rules', Format(
        '[EXCEPTION] TMultiStringOperator.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TIntOperator }

function TIntOperator.FeedOperand(var s: AnsiString): boolean;
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
      Debug(dpError, 'rules', 'TIntOperator.GetOperandValue : %s', [e.Message]);
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
      Debug(dpError, 'rules', 'TIntOperator.GetSupplyValue : %s', [e.Message]);
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

function TConditionOperator.AsText: AnsiString;
begin
  Result := condition.AsText + ' ' + Name + ' ' + operand.AsText;
end;

function TConditionOperator.AtConditionName: AnsiString;
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
      Debug(dpError, 'rules', 'TNotOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TNotOperator.Name: AnsiString;
begin
  Result := 'not';
end;

{ TOperand }

function TOperand.AtConditionName: AnsiString;
begin
  Result := '';
end;

function TOperand.Match(p: TPazo): boolean;
begin
  Result := False; // exception
end;

class function TOperand.Name: AnsiString;
begin
  Result := 'operand';
end;

class function TBooleanOperator.TakeThis(var s: AnsiString): boolean;
begin
  Result := True;
  // we are not resetting s, it needs further processing
end;

{ TSuffixOperator }

class function TClosingBracketOperator.Name: AnsiString;
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

function FireRuleSetB(p: TPazo; ps: TPazoSite;
  sitenametomatch, sectiontomatch: AnsiString): TRuleAction;
var
  i: integer;
  ra: TRuleAction;
begin
  Result := raDontmatch;
  ra := Result;
  try
    for i := 0 to rtpl.Count - 1 do
    begin
      try
        if ((TRule(rtpl[i]).sitename = sitenametomatch) and
          (TRule(rtpl[i]).section = sectiontomatch)) then
        begin
          try
            ra := TRule(rtpl[i]).Execute(p);
          except
            on e: Exception do
            begin
              Debug(dpError, 'rules',
                Format('[EXCEPTION] FireRuleSetB(rtpl) r.Execute: %s, %s',
                [e.Message, TRule(rtpl[i]).AsText(True)]));
              Result := raDontmatch;
              exit;
            end;
          end;
          if ra = raDrop then
          begin
            ps.reason := TRule(rtpl[i]).AsText(True);
            Result := raDrop;
            exit;
          end;

          if ra = raAllow then
          begin
            ps.reason := TRule(rtpl[i]).AsText(True);
            Result := raAllow;
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
      Result := raDontmatch;
      exit;
    end;
  end;

  try
    for i := 0 to rules.Count - 1 do
    begin
      try
        if ((TRule(rules[i]).sitename = sitenametomatch) and
          (TRule(rules[i]).section = sectiontomatch)) then
        begin
          try
            ra := TRule(rules[i]).Execute(p);
          except
            on e: Exception do
            begin
              Debug(dpError, 'rules',
                Format('[EXCEPTION] FireRuleSetB(rules) r.Execute: %s %s',
                [e.Message, TRule(rules[i]).AsText(True)]));
              Result := raDontmatch;
              exit;
            end;
          end;
          if ra = raDrop then
          begin
            ps.reason := TRule(rules[i]).AsText(True);
            Result := raDrop;
            exit;
          end;

          if ra = raAllow then
          begin
            ps.reason := TRule(rules[i]).AsText(True);
            Result := raAllow;
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
      Result := raDontmatch;
      exit;
    end;
  end;
end;

function FireRuleSet(p: TPazo; ps: TPazoSite): TRuleAction;
begin

  try
    Result := FireRuleSetB(p, ps, '*', '*');
    // eloszor megnezzuk a teljesen generic ruleokat    = first watch a completely generic ruleokat
    if Result <> raDontMatch then
    begin
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] FireRuleSetB * *: %s', [e.Message]));
      Result := raDontmatch;
      exit;
    end;
  end;

  try
    Result := FireRuleSetB(p, ps, '*', p.rls.section);
    // most megnezzuk a sectionre globalis ruleokat = Now look at the sectional globalis ruleokat
    if Result <> raDontMatch then
    begin
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] FireRuleSetB * section: %s',
        [e.Message]));
      Result := raDontmatch;
      exit;
    end;
  end;

  try
    Result := FireRuleSetB(p, ps, ps.Name, '*');
    // most megnezzuk a site globalis rulejait = Now look at the site globalis rulejait
    if Result <> raDontMatch then
    begin
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] FireRuleSetB site *: %s',
        [e.Message]));
      Result := raDontmatch;
      exit;
    end;
  end;

  try
    Result := FireRuleSetB(p, ps, ps.Name, p.rls.section);
    // most megnezzuk a section rulejait. = Now look at the section rulejait.
    if Result <> raDontMatch then
    begin
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] FireRuleSetB site section: %s',
        [e.Message]));
      Result := raDontmatch;
      exit;
    end;
  end;

  // egyebkent droppoljuk, nem volt matching rule.  = I use droppoljuk, there was no matching rule.
  //Result:= raDrop;
  Result := raDontMatch;
  if ps.reason = '' then
    ps.reason := 'No matching rule';

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
  if ps_s.working = sstDown then
    exit;
  if ps_s.PermDown then
    exit;

  p.srcsite := ps.Name;
  Debug(dpSpam, 'rules', '-> ' + Format('%s: %s %s',
    [ps.Name, p.rls.section, p.rls.rlsname]));

  y := TStringList.Create;
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

        if (dstps_s.working = sstDown) or (dstps_s.PermDown) then
        begin
          if (dstps.reason = '') then
            dstps.reason := 'Down';
          Continue;
        end;

        p.dstsite := dstps.Name;
        // aztan hogy allowed e...
        if ((dstps.status in [rssAllowed]) or (FireRuleSet(p, dstps) = raAllow)) then
        begin
          if (ps.status in [rssShouldPre, rssRealPre]) then
          begin
            if ps.AddDestination(dstps, (StrToIntDef(y.ValueFromIndex[i], 1) *
              dstps_s.GetRank(p.rls.section)) + 100) then
              Result := True;
          end
          else
          begin
            if ps.AddDestination(dstps, StrToIntDef(y.ValueFromIndex[i], 1) *
              dstps_s.GetRank(p.rls.section)) then
              Result := True;
          end;
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, 'rules', Format('[EXCEPTION] FireRules loop: %s', [e.Message]));
        Result := False;
        Break;
      end;
    end;
  end;
  Debug(dpSpam, 'rules', '<- ' + Format('%s: %s %s',
    [ps.Name, p.rls.section, p.rls.rlsname]));
  y.Free;
end;

procedure RulesOrder(p: TPazo);
var
  x: TStringList;
  i, j: integer;
  r: TRule;
  s: AnsiString;
  fositeIndex, aktsiteIndex: integer;
begin
  x := TStringList.Create;
  for i := 0 to p.sites.Count - 1 do
  begin
    try
      x.Add(TPazoSite(p.sites[i]).Name);
    except
      break;
    end;
  end;

  for i := 0 to x.Count - 1 do
  begin
    fositeIndex := p.sites.IndexOf(p.FindSite(x[i]));
    for j := 0 to rtpl.Count - 1 do
    begin
      r := TRule(rtpl[j]);
      if ((r.sitename = x[i]) and (r.section = p.rls.section)) then
      begin
        s := r.conditions.AtConditionName;
        if s <> '' then
        begin
          aktsiteIndex := p.sites.IndexOf(p.FindSite(s));
          if (aktsiteIndex > fositeIndex) then
          begin
            p.sites.Move(aktsiteIndex, fositeIndex);
            fositeIndex := fositeIndex + 1;
            // meg kell nezni egyezik e ezzel...
            // fositeIndex:= p.sites.IndexOf(p.FindSite(x[i]));
          end;
        end;
      end;
    end;

    for j := 0 to rules.Count - 1 do
    begin
      r := TRule(rules[j]);
      if ((r.sitename = x[i]) and (r.section = p.rls.section)) then
      begin
        s := r.conditions.AtConditionName;
        if s <> '' then
        begin
          aktsiteIndex := p.sites.IndexOf(p.FindSite(s));
          if (aktsiteIndex > fositeIndex) then
          begin
            p.sites.Move(aktsiteIndex, fositeIndex);
            fositeIndex := fositeIndex + 1;
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
var
  i: integer;
  f: TEncStringlist;
  a_i: integer;
  a_j: integer;
  a_sitename: AnsiString;
  a_rules_path: AnsiString;
  a_sites_done: TStringList;
  a_siterules: TStringList;
  a_r: TRule;
begin
  if (config.ReadBool('sites', 'split_site_data', False)) then
  begin
    a_sites_done := TStringList.Create;

    try
      a_rules_path := ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim;
      for a_i := 0 to rules.Count - 1 do
      begin
        a_r := TRule(rules[a_i]);
        a_sitename := a_r.sitename;
        if (a_sites_done.IndexOf(a_sitename) <> -1) or (a_sitename = '*') then
          continue;
        a_siterules := TStringList.Create;

        try
          for a_j := a_i to rules.Count - 1 do
          begin
            a_r := TRule(rules[a_j]);
            if a_r.sitename <> a_sitename then
              continue;
            a_siterules.Add(a_r.AsText(True));
          end;

          a_siterules.SaveToFile(a_rules_path + a_sitename + '.rtpl');
          a_sites_done.Add(a_sitename);

        finally
          a_siterules.Free;
        end;
      end;

    finally
      a_sites_done.Free;
    end;
  end
  else
  begin
    f := TEncStringlist.Create(passphrase);
    try
      for i := 0 to rules.Count - 1 do
        f.Add(TRule(rules[i]).AsText(True));
      f.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.rules');
    finally
      f.Free;
    end;
  end;
end;

procedure RulesRemove(const sitename, section: AnsiString);
var
  i: integer;
  r: TRule;
begin
  i := 0;

  while (i < rules.Count) do
  begin
    r := TRule(rules[i]);
    if ((r.sitename = sitename) and ((section = '') or (r.section = section))) then
    begin
      rules.Remove(r);
      Dec(i);
    end;

    Inc(i);
  end;
end;

function AddRule(const rule: AnsiString; var error: AnsiString): TRule;
var
  r: TRule;
begin
  Result := nil;

  r := TRule.Create(rule);
  if r.error <> '' then
  begin
    error := r.error;
    r.Free;
  end
  else
    Result := r;
end;

procedure RulesLoad(const action, filename: AnsiString);
var
  fst: TStringList;
  i: integer;
  r: TRule;
  error: AnsiString;
begin
  if (UpperCase(action) = 'REPLACE') then
  begin
    rules.Free;
    rules := TObjectList.Create;
  end;

  fst := TStringList.Create();
  try
    fst.LoadFromFile(ExtractFilePath(ParamStr(0)) + filename);
    for i := 0 to fst.Count - 1 do
    begin
      r := AddRule(fst[i], error);
      if r <> nil then
      begin
        rules.Add(r);
      end
      else
      begin
        Debug(dpError, 'rules', '[ERROR] ' + error + ' loading ' + fst[i]);
      end;
    end;
  finally
    fst.Free;
  end;
end;

procedure RulesReload();
var
  fst: TStringList;
  i: integer;
  r: TRule;
  error: AnsiString;
  intFound: integer;
  SearchRec: TSearchRec;
  rule_line, rules_path: AnsiString;
  split_site_data: boolean;
begin
  rules_path := ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim;
  split_site_data := config.ReadBool('sites', 'split_site_data', False);
  if (split_site_data) then
  begin
    FreeAndNil(rules);
    rules := TObjectList.Create;
  end
  else
  begin
    FreeAndNil(rtpl);
    rtpl := TObjectList.Create;
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

        r := AddRule(rule_line, error);
        if r <> nil then
        begin
          if split_site_data then
          begin
            rules.Add(r);
          end
          else
          begin
            rtpl.Add(r);
          end;
        end
        else
        begin
          Debug(dpError, 'rules', '[ERROR] ' + error + ' loading ' + fst[i]);
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
  r: TRule;
  error: AnsiString;
  S: AnsiString;
begin
  // load rules tpl
  RulesReload();

  //beparszoljuk a szabalyokat
  f := TEncStringlist.Create(passphrase);
  try
    f.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.rules');

    for i := 0 to f.Count - 1 do
    begin
      r := AddRule(f[i], error);
      if r <> nil then
        rules.Add(r);
    end;

  finally
    f.Free;
  end;

  if (config.ReadBool('sites', 'split_site_data', False)) then
  begin
    S := ExtractFilePath(ParamStr(0)) + 'slftp.rules'; // convert to split format
    if FileExists(S) then
      DeleteFile(PAnsiChar(S));
    RulesSave; // force saving in new split format
  end;
end;

procedure RulesInit;
begin
  rules := TObjectList.Create;
  rtpl := TObjectList.Create;

  conditions := TClassList.Create;
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
  conditions.Add(TConditionTVCurrentEpisiode);
  conditions.Add(TConditionTVCurrentOnAir);
  conditions.Add(TConditionTVDailyShow);

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

  prefixops := TClassList.Create;
  prefixops.Add(TNotOperator);
  prefixops.Add(TOpeningBracketOperator);

  infixops := TClassList.Create;
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

class function TConditionReleaseName.Description: AnsiString;
begin
result:=ReleaseNameDescription;
end;

function TConditionReleaseName.SupplyValue(r: TPazo): AnsiString;
begin
  try
    Result := r.rls.rlsname;
  except
    Result := '';
  end;
end;

class function TConditionReleaseName.Name: AnsiString;
begin
  Result := 'releasename';
end;

{ TConditionSection }

class function TConditionSection.Description: AnsiString;
begin
result:=SectionDescription;
end;

function TConditionSection.SupplyValue(r: TPazo): AnsiString;
begin
  try
    Result := r.rls.section;
  except
    Result := '';
  end;
end;

class function TConditionSection.Name: AnsiString;
begin
  Result := 'section';
end;

{ TConditionNotComplete }

class function TConditionNotComplete.Description: AnsiString;
begin
  Result := NotCompleteDescription;
end;

procedure TConditionNotComplete.SupplyValues(r: TPazo; re: TStringList);
var
  ps: TPazoSite;
  i: integer;
begin
  try
    for i := 0 to r.sites.Count - 1 do
    begin
      if i > r.sites.Count then
        Break;
      try
        ps := TPazoSite(r.sites[i]);
        if ((ps.status <> rssNotAllowed) and (not ps.Complete)) then
          re.Add(ps.Name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format(
        '[EXCEPTION] TConditionNotComplete.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionNotComplete.Name: AnsiString;
begin
  Result := 'notcomplete';
end;

{ TConditionComplete }

class function TConditionComplete.Description: AnsiString;
begin
  Result := CompleteDescription;
end;

procedure TConditionComplete.SupplyValues(r: TPazo; re: TStringList);
var
  ps: TPazoSite;
  i: integer;
begin
  try
    for i := 0 to r.sites.Count - 1 do
    begin
      if i > r.sites.Count then
        Break;
      try
        ps := TPazoSite(r.sites[i]);
        if ps.Complete then
          re.Add(ps.Name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format(
        '[EXCEPTION] TConditionComplete.GetSupplyValues: %s',
        [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionComplete.Name: AnsiString;
begin
  Result := 'complete';
end;

{ TConditionPre }

class function TConditionPre.Description: AnsiString;
begin
  Result := PreDescription;
end;

procedure TConditionPre.SupplyValues(r: TPazo; re: TStringList);
var
  ps: TPazoSite;
  i: integer;
begin
  try
    for i := 0 to r.sites.Count - 1 do
    begin
      if i > r.sites.Count then
        Break;
      try
        ps := TPazoSite(r.sites[i]);
        if ps.status = rssRealPre then
          re.Add(ps.Name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionPre.GetSupplyValues: %s',
        [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionPre.Name: AnsiString;
begin
  Result := 'pre';
end;

{ TConditionAt }

function TAtOperator.AtConditionName: AnsiString;
begin
  try
    Result := GetOperandValue;
  except
    on e: Exception do
    begin
      Debug(dpError, 'rules', 'TAtOperator.AtConditionName : %s', [e.Message]);
      Result := '';
    end;
  end;
end;

class function TAtOperator.Name: AnsiString;
begin
  Result := '@';
end;

{ TConditionNotAllowed }

class function TConditionNotAllowed.Description: AnsiString;
begin
  Result := NotAllowedDescription;
end;

procedure TConditionNotAllowed.SupplyValues(r: TPazo; re: TStringList);
var
  ps: TPazoSite;
  i: integer;
begin
  try
    for i := 0 to r.sites.Count - 1 do
    begin
      if i > r.sites.Count then
        Break;
      try
        ps := TPazoSite(r.sites[i]);
        if ps.status = rssNotAllowed then
          re.Add(ps.Name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format(
        '[EXCEPTION] TConditionNotAllowed.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionNotAllowed.Name: AnsiString;
begin
  Result := 'notallowed';
end;

{ TConditionAllowed }

class function TConditionAllowed.Description: AnsiString;
begin
  Result := AllowedDescription;
end;

procedure TConditionAllowed.SupplyValues(r: TPazo; re: TStringList);
var
  ps: TPazoSite;
  i: integer;
begin
  try
    for i := 0 to r.sites.Count - 1 do
    begin
      if i > r.sites.Count then
        Break;
      try
        ps := TPazoSite(r.sites[i]);
        if ps.status = rssAllowed then
          re.Add(ps.Name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionAllowed.GetSupplyValues: %s',
        [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionAllowed.Name: AnsiString;
begin
  Result := 'allowed';
end;

{ TConditionGroup }

class function TConditionGroup.Description: AnsiString;
begin
  Result := GroupDescription;
end;

function TConditionGroup.SupplyValue(r: TPazo): AnsiString;
begin
  try
    Result := r.rls.groupname;
  except
    Result := '';
  end;
end;

class function TConditionGroup.Name: AnsiString;
begin
  Result := 'group';
end;

{ TConditionKnownGroup }

class function TConditionKnownGroup.Description: AnsiString;
begin
  Result := KnownGroupDescription;
end;

function TConditionKnownGroup.SupplyValue(r: TPazo): boolean;
begin
  try
    Result := r.rls.knowngroup = grp_known;
  except
    Result := False;
  end;
end;

class function TConditionKnownGroup.Name: AnsiString;
begin
  Result := 'knowngroup';
end;

{ TConditionUnKnownGroup }

class function TConditionUnKnownGroup.Description: AnsiString;
begin
  Result := UnKnownGroupDescription;
end;

function TConditionUnKnownGroup.SupplyValue(r: TPazo): boolean;
begin
  try
    Result := r.rls.knowngroup = grp_unknown;
  except
    Result := False;
  end;
end;

class function TConditionUnKnownGroup.Name: AnsiString;
begin
  Result := 'unknowngroup';
end;

{ TConditionSource }

class function TConditionSource.Description: AnsiString;
begin
  Result := SourceDescription;
end;

function TConditionSource.SupplyValue(r: TPazo): AnsiString;
begin
  Result := r.srcsite;
end;

class function TConditionSource.Name: AnsiString;
begin
  Result := 'source';
end;

{ TConditionDestination }

class function TConditionDestination.Description: AnsiString;
begin
  Result := DestinationDescription;
end;

function TConditionDestination.SupplyValue(r: TPazo): AnsiString;
begin
  Result := r.dstsite;
end;

class function TConditionDestination.Name: AnsiString;
begin
  Result := 'destination';
end;

{ TConditionNewdir }

class function TConditionNewdirSource.Description: AnsiString;
begin
  Result := NewdirSourceDescription;
end;

function TConditionNewdirSource.SupplyValue(r: TPazo): boolean;
var
  x: TPazoSite;
begin
  Result := False;
  try
    x := r.FindSite(r.srcsite);
    if (x <> nil) then
      Result := x.status = rssAllowed;
  except
    Result := False;
  end;
end;

class function TConditionNewdirSource.Name: AnsiString;
begin
  Result := 'newdirsource';
end;

{ TConditionMP3Genre }

class function TConditionMP3Genre.Description: AnsiString;
begin
  Result := 'Returns with the mp3 genre.' + #13#10;
  Result := Result + 'Example: if mp3genre =~ *Metal* then ALLOW' + #13#10;
end;

function TConditionMP3Genre.SupplyValue(r: TPazo): AnsiString;
begin
  Result := '';
  try
    if r.rls is TMP3Release then
      Result := TMP3Release(r.rls).mp3genre;
  except
    Result := '';
  end;
end;

class function TConditionMP3Genre.Name: AnsiString;
begin
  Result := 'mp3genre';
end;

{ TConditionMP3EYear }

class function TConditionMP3Year.Description: AnsiString;
begin
  Result := 'Returns with the mp3 year' + #13#10;
  Result := Result + 'Example: if mp3year < 2009 then DROP' + #13#10;
end;

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

class function TConditionMP3Year.Name: AnsiString;
begin
  Result := 'mp3year';
end;

{ TConditionInternal }

class function TConditionInternal.Description: AnsiString;
begin
  Result := 'Returns true, if the release is tagged as internal.' + #13#10;
  Result := Result +
    'Example: !ruleadd * * if internal && destination notin DUMP1, DUMP2 then DROP' + #13#10;
end;

function TConditionInternal.SupplyValue(r: TPazo): boolean;
begin
  try
    Result := r.rls.internal;
  except
    Result := False;
  end;
end;

class function TConditionInternal.Name: AnsiString;
begin
  Result := 'internal';
end;

{ TConditionMP3Language }

class function TConditionMP3Language.Description: AnsiString;
begin
  Result := 'Returns with mp3 rip''s language tag. Language is EN by default' +
    #13#10;
  Result := Result + 'Example: if mp3language != EN then DROP' + #13#10;
end;

function TConditionMP3Language.SupplyValue(r: TPazo): AnsiString;
begin
  Result := '';
  try
    if (r.rls is TMP3Release) then
      Result := Uppercase(TMP3Release(r.rls).mp3lng);
    //    Result:= TMP3Release(r.rls).mp3lng;
  except
    Result := '';
  end;
end;

class function TConditionMP3Language.Name: AnsiString;
begin
  Result := 'mp3language';
end;

function TConditionMP3Language.Hitelesit(const s: AnsiString): boolean;
begin
  try
    Result := ((AnsiSameText(s, 'EN')) or (mp3languages.IndexOf(s) <> -1));
  except
    Result := False;
  end;
end;

{ TConditionMP3Foreign }

class function TConditionMP3Foreign.Description: AnsiString;
begin
  Result := 'Returns true, if the mp3 rip''s language is not EN.' + #13#10;
  Result := Result + 'Example: if mp3foreign then DROP' + #13#10;
end;

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

class function TConditionMP3Foreign.Name: AnsiString;
begin
  Result := 'mp3foreign';
end;

{ TConditionMP3Source }

class function TConditionMP3Source.Description: AnsiString;
begin
  Result := 'Returns with the mp3 rip''s source.' + #13#10;
  Result := Result + 'Example: if not ( mp3source in CD, CDR, DVD, VINYL ) then DROP' +
    #13#10;
end;

function TConditionMP3Source.SupplyValue(r: TPazo): AnsiString;
begin
  Result := '';
  try
    if (r.rls is TMP3Release) then
      Result := TMP3Release(r.rls).mp3source;
  except
    Result := '';
  end;
end;

class function TConditionMP3Source.Name: AnsiString;
begin
  Result := 'mp3source';
end;

function TConditionMP3Source.Hitelesit(const s: AnsiString): boolean;
begin
  try
    Result := mp3sources.IndexOfName(s) <> -1;
  except
    Result := False;
  end;
end;

{ TConditionMP3Live }

class function TConditionMP3Live.Description: AnsiString;
begin
  Result :=
    'Returns true, if the mp3 rip''s source is a live source. (You can define live source tags in slftp.ini i think)'
    +
    #13#10;
  Result := Result + 'Example: if mp3live then DROP' + #13#10;
end;

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

class function TConditionMP3Live.Name: AnsiString;
begin
  Result := 'mp3live';
end;

{ TConditionDefault }

class function TConditionDefault.Description: AnsiString;
begin
  Result := 'This condition simple matches anything, you can use it for default policy.' +
    #13#10;
  Result := Result +
    'If there is no matching rule then no action is taken which is same as DROP by default.'
    +
    #13#10;
  Result := Result + 'Example: if default then ALLOW' + #13#10;
end;

function TConditionDefault.SupplyValue(r: TPazo): boolean;
begin
  Result := True;
end;

class function TConditionDefault.Name: AnsiString;
begin
  Result := 'default';
end;

{ TConditionMP3Type }

class function TConditionMP3Type.Description: AnsiString;
begin
  Result := 'Returns with the mp3 rip''s types.' + #13#10;
  Result := Result + 'Example: if mp3type in Bootleg, Demo then DROP';
end;

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
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionMP3Type.GetSupplyValues: %s',
        [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionMP3Type.Name: AnsiString;
begin
  Result := 'mp3type';
end;

{ TConditionMP3Bootleg }

class function TConditionMP3Bootleg.Description: AnsiString;
begin
  Result := 'Returns true if the mp3 rip is bootleg.';
end;

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

class function TConditionMP3Bootleg.Name: AnsiString;
begin
  Result := 'mp3bootleg';
end;

{ TConditionMP3LtNumCDs }

class function TConditionMP3NumDisks.Description: AnsiString;
begin
  Result := 'Returns with the number of disks of an mp3 release' + #13#10;
  Result := Result +
    'We drop rips more than 2 DVD''s and everything else if more than 4 (cd''s):' + #13#10;
  Result := Result + 'if mp3numdisks > 2 && mp3source = DVD then DROP' + #13#10;
  Result := Result + 'if mp3numdisks > 4  then DROP' + #13#10;
end;

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

class function TConditionMP3NumDisks.Name: AnsiString;
begin
  Result := 'mp3numdisks';
end;

{ TConditionMP3VA }

class function TConditionMP3VA.Description: AnsiString;
begin
  Result := 'Returns true if the mp3 rip is a compilation. (VA)';
end;

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

class function TConditionMP3VA.Name: AnsiString;
begin
  Result := 'mp3va';
end;

{ TConditionAgeGt }

class function TConditionAge.Description: AnsiString;
begin
  Result :=
    'This is useful for filtering old stuffs scanned by autodirlist in a not dated directory.'
    +
    #13#10;
  Result := Result + 'It expects the parameter in seconds. Example: if age > 86400 then DROP'
    + #13#10;
end;

function TConditionAge.SupplyValue(r: TPazo): integer;
begin
  try
    Result := r.Age;
  except
    Result := 0;
  end;
end;

class function TConditionAge.Name: AnsiString;
begin
  Result := 'age';
end;

{ TConditionNfoMGenre }

class function TConditionNfoMGenre.Description: AnsiString;
begin
  Result := 'Checks for genre parsed from the nfo file. As its a stupid textfile, use masks.'
    +
    #13#10;
  Result := Result +
    'Note: nfogenre is for mdvdr/mv sections! For mp3 stuffs use mp3genre condition.' +
    #13#10;
  Result := Result +
    'Genre string contains latin alphabet only, all other chars are replaced to spaces!' +
    #13#10;
  Result := Result + 'Example: if nfogenre =~ *Hip*Hop* then ALLOW' + #13#10;
end;

function TConditionNfoMGenre.SupplyValue(r: TPazo): AnsiString;
begin
  Result := '';
  try
    if r.rls is TNFORelease then
      Result := TNFORelease(r.rls).nfogenre;
  except
    Result := '';
  end;
end;

class function TConditionNfoMGenre.Name: AnsiString;
begin
  Result := 'nfogenre';
end;

constructor TConditionNfoMGenre.Create(parent: TRuleNode);
begin
  inherited;
  acceptedOperators.Clear;
  acceptedOperators.Add(TMaskOperator);
end;

{ TConditionFake }

class function TConditionFake.Description: AnsiString;
begin
  Result := FakeDescription;
end;

function TConditionFake.SupplyValue(r: TPazo): boolean;
begin
  try
    Result := r.rls.Fake;
  except
    Result := False;
  end;
end;

class function TConditionFake.Name: AnsiString;
begin
  Result := 'fake';
end;

{ TConditionTVShowName }

class function TConditionTVShowName.Description: AnsiString;
begin
  Result := TVShowNameDescription;
end;

function TConditionTVShowName.SupplyValue(r: TPazo): AnsiString;
begin
  Result := '';
  try
    if r.rls is TTVRelease then
      Result := TTVRelease(r.rls).showname;
  except
    Result := '';
  end;
end;

class function TConditionTVShowName.Name: AnsiString;
begin
  Result := 'tvshowname';
end;

{ TConditionForeign }

class function TConditionForeign.Description: AnsiString;
begin
  Result :=ForeignDescription;
end;

function TConditionForeign.SupplyValue(r: TPazo): boolean;
begin
  Result := False;

  try
    if r.rls is TMP3Release then
      exit;
    if r.rls.languages.Count = 0 then
      exit;
    if (r.rls.languages[0] = 'English') then
      exit;
    Result := True;
  except
    Result := False;
  end;
end;

class function TConditionForeign.Name: AnsiString;
begin
  Result := 'foreign';
end;

{ TConditionLanguageA }

class function TConditionLanguage.Description: AnsiString;
begin
  Result := LanguageDescription;
end;

procedure TConditionLanguage.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    re.Assign(r.rls.languages);
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format(
        '[EXCEPTION] TConditionLanguage.GetSupplyValues: %s',
        [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionLanguage.Name: AnsiString;
begin
  Result := 'language';
end;

function InArray(var Name: AnsiString; elements: TClassList): TCRuleNode;
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

function ParseRule(rule: AnsiString; var error: AnsiString): TRuleNode;
type
  TMitVarunk = (mvFelteteltVagyPrefixet, mvOperatort, mvInfixOrSuffix,
    mvOperandus, mvOperandusOrInfixOrSuffix, mvInfix);
var
  s: AnsiString;
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
      debugunit.Debug(dpError, 'rules', '[iNFO] ParseRule count break', []);
      break;
    end;

    s := Fetch(rule, ' ');
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

function TRule.AsText(const includeSitesection: boolean): AnsiString;
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

constructor TRule.Create(const rule: AnsiString);
begin
  error := '';
  reparse(rule);
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
      Debug(dpError, 'rules', 'TRule.Execute : %s', [e.Message]);
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
      Debug(dpError, 'rules', 'TRule.Execute : %s', [e.Message]);
      Result := raDontmatch;
      exit;
    end;
  end;
end;

procedure TRule.Reparse(rule: AnsiString);
var
  i: integer;
  ifstr, thenstr, actionstr, conditionstr: AnsiString;
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
  
  acceptedOperators.Add(TMultiStringEqualOperator);
  acceptedOperators.Add(TMultiStringNotEqualOperator);
  acceptedOperators.Add(TMultiInOperator);
  acceptedOperators.Add(TMultiNotInOperator);
end;

function TListCondition.SupplyValue(r: TPazo): AnsiString;
var strList : TStringList;
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
      Debug(dpError, 'rules', 'TMultiStringEqualOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TMultiStringEqualOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TMultiStringNotEqualOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TMultiStringNotEqualOperator.Name: AnsiString;
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
      Debug(dpError, 'rules', 'TMultiNotInOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TMultiNotInOperator.Name: AnsiString;
begin
  Result := 'notin';
end;

{ TMultiInOperator }

function TMultiInOperator.FeedOperand(var s: AnsiString): boolean;
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
      Debug(dpError, 'rules', 'TMultiInOperator.GetOperandValue : %s', [e.Message]);
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
      Debug(dpError, 'rules', 'TMultiInOperator.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TMultiInOperator.Name: AnsiString;
begin
  Result := 'in';
end;

{ TConditionCompleteSource }

class function TConditionCompleteSource.Description: AnsiString;
begin
  Result := CompleteSourceDescription;

end;

class function TConditionCompleteSource.Name: AnsiString;
begin
  Result := 'completesource';
end;

function TConditionCompleteSource.SupplyValue(r: TPazo): boolean;
var
  x: TPazoSite;
begin
  Result := False;
  try
    x := r.FindSite(r.srcsite);
    if (x <> nil) then
      Result := x.Complete;
  except
    Result := False;
  end;
end;

{ TConditionYear }

class function TConditionYear.Description: AnsiString;
begin
  Result := YearDescription;

end;

class function TConditionYear.Name: AnsiString;
begin
  Result := 'year';
end;

function TConditionYear.SupplyValue(r: TPazo): integer;
begin
  try
    Result := r.rls.year;
  except
    Result := 0;
  end;
end;

{ TCondition0daySource }

class function TCondition0daySource.Description: AnsiString;
begin
  Result := zerodaySourceDescription;
end;

function TCondition0daySource.Hitelesit(const s: AnsiString): boolean;
begin
  try
    Result := nulldaysources.IndexOfName(s) <> -1;
  except
    Result := False;
  end;
end;

class function TCondition0daySource.Name: AnsiString;
begin
  Result := '0daysource';
end;

function TCondition0daySource.SupplyValue(r: TPazo): AnsiString;
begin
  Result := '';
  try
    if (r.rls is T0dayRelease) then
      Result := T0dayRelease(r.rls).nulldaysource;
  except
    Result := '';
  end;
end;


{ TConditionAutofollow }
class function TConditionAutofollow.Description: AnsiString;
begin
  Result := AutofollowDescription;
end;

class function TConditionAutofollow.Name: AnsiString;
begin
  Result := 'autofollow';
end;

function TConditionAutofollow.SupplyValue(r: TPazo): boolean;
var
  ps: TPazoSite;
begin
  Result := False;
  try
    ps := r.FindSite(r.dstsite);
    if ps <> nil then
    begin
      Result := ps.ircevent;
      if ((Result) and (not ps.Complete)) then
        ps.firesourcesinstead := True;
    end;
  except
    Result := False;
  end;
end;


{ TConditionNuked }
class function TConditionNuked.Description: AnsiString;
begin
  Result := NukeDescription;
end;

class function TConditionNuked.Name: AnsiString;
begin
  Result := 'nuked';
end;

function TConditionNuked.SupplyValue(r: TPazo): boolean;
var
  ps: TPazoSite;
begin
  Result := False;
  try
    ps := r.FindSite(r.dstsite);
    if ps <> nil then
    begin
      if ps.status = rssNuked then
        Result := True;
    end;
  except
    Result := False;
  end;
end;

{ TConditionPred }
class function TConditionPred.Description: AnsiString;
begin
  Result := PredDescription;
end;

class function TConditionPred.Name: AnsiString;
begin
  Result := 'pred';
end;

function TConditionPred.SupplyValue(r: TPazo): boolean;
begin
  try
    Result := r.rls.PredOnAnySite;
  except
    Result := False;
  end;
end;

{ TConditionDisks }
class function TConditionDisks.Description: AnsiString;
begin
  Result := DisksDescription;
end;

class function TConditionDisks.Name: AnsiString;
begin
  Result := 'discs';
end;

function TConditionDisks.SupplyValue(r: TPazo): integer;
begin
  try
    Result := r.rls.disks;
  except
    Result := 1;
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

class function TConditionTVPremierYear.Description: AnsiString;
begin
  Result :=  TVPremierYearDescription;
end;

class function TConditionTVPremierYear.Name: AnsiString;
begin
  Result := 'tvpremieryear';
end;

function TConditionTVPremierYear.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).premier_year;
    end;
  except
    Result := 0;
  end;
end;

{ TConditionTVCountry }

class function TConditionTVCountry.Description: AnsiString;
begin
  Result := TVCountryDescription;
end;

class function TConditionTVCountry.Name: AnsiString;
begin
  Result := 'tvcountry';
end;

function TConditionTVCountry.SupplyValue(r: TPazo): AnsiString;
begin
  Result := '';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).country;
    end;
  except
    Result := '';
  end;
end;

{ TConditionTVLanguage }

class function TConditionTVLanguage.Description: AnsiString;
begin
  Result := TVLanguageDescription;
end;

class function TConditionTVLanguage.Name: AnsiString;
begin
  Result := 'tvlanguage';
end;

function TConditionTVLanguage.SupplyValue(r: TPazo): AnsiString;
begin
  Result := '';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).tvlanguage;
    end;
  except
    Result := '';
  end;
end;


{ TConditionTVClassication }

class function TConditionTVClassification.Description: AnsiString;
begin
  Result := TVClassificationDescription;
end;

class function TConditionTVClassification.Name: AnsiString;
begin
  Result := 'tvclassification';
end;

function TConditionTVClassification.SupplyValue(r: TPazo): AnsiString;
begin
  Result := '';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := lowercase(TTVRelease(r.rls).classification);
    end;
  except
    Result := '';
  end;
end;

{ TConditionTVScripted }

class function TConditionTVScripted.Description: AnsiString;
begin
  Result :=
    TVScriptedDescription;
end;

class function TConditionTVScripted.Name: AnsiString;
begin
  Result := 'tvscripted';
end;

function TConditionTVScripted.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).scripted;
    end;
  except
    Result := False;
  end;
end;

{ TConditionTVGenres }

class function TConditionTVGenres.Description: AnsiString;
begin
  Result := TVGenresDescription;
end;

class function TConditionTVGenres.Name: AnsiString;
begin
  Result := 'tvgenres';
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
      Debug(dpError, 'rules', Format(
        '[EXCEPTION] TConditionTVGenres.GetSupplyValues: %s',
        [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TConditionTVNetwork }

class function TConditionTVNetwork.Description: AnsiString;
begin
  Result := TVNetworkDescription;
end;

class function TConditionTVNetwork.Name: AnsiString;
begin
  Result := 'tvnetwork';
end;

function TConditionTVNetwork.SupplyValue(r: TPazo): AnsiString;
begin
  Result := '';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).network;
    end;
  except
    Result := '';
  end;
end;

{ TConditionTVRuntime }

class function TConditionTVRuntime.Description: AnsiString;
begin
  Result := TVRuntimeDescription;
end;

class function TConditionTVRuntime.Name: AnsiString;
begin
  Result := 'tvruntime';
end;

function TConditionTVRuntime.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).runtime;
    end;
  except
    Result := 0;
  end;
end;

{ TConditionTVEndedYear }

class function TConditionTVEndedYear.Description: AnsiString;
begin
  Result :=  TVEndedYearDescription;
end;

class function TConditionTVEndedYear.Name: AnsiString;
begin
  Result := 'tvendedyear';
end;

function TConditionTVEndedYear.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).ended_year;
    end;
  except
    Result := 0;
  end;
end;

{ TConditionTVStatus }

class function TConditionTVStatus.Description: AnsiString;
begin
  Result := TVStatusDescription;
end;

class function TConditionTVStatus.Name: AnsiString;
begin
  Result := 'tvstatus';
end;

function TConditionTVStatus.SupplyValue(r: TPazo): AnsiString;
begin
  Result := '';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).status;
    end;
  except
    Result := '';
  end;
end;

{ TConditionTVRunning }

class function TConditionTVRunning.Description: AnsiString;
begin
  Result := TVRunningDescription;
end;

class function TConditionTVRunning.Name: AnsiString;
begin
  Result := 'tvrunning';
end;

function TConditionTVRunning.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).running;
    end;
  except
    Result := False;
  end;
end;


{ TConditionTVDailyShow }

class function TConditionTVDailyShow.Description: AnsiString;
begin
  Result := TVDailyShowDescription;
end;

class function TConditionTVDailyShow.Name: AnsiString;
begin
  Result := 'tvdaily';
end;

function TConditionTVDailyShow.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).daily;
    end;
  except
    Result := False;
  end;
end;

{ TConditionTVCurrentEpisode }

class function TConditionTVCurrentEpisiode.Description: AnsiString;
begin
  Result := TVCurrentEpisiodeDescription;
end;

class function TConditionTVCurrentEpisiode.Name: AnsiString;
begin
  Result := 'tvcurrentep';
end;

function TConditionTVCurrentEpisiode.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).currentepisode;
    end;
  except
    Result := False;
  end;
end;

{ TConditionTVCurrent }

class function TConditionTVCurrentOnAir.Description: AnsiString;
begin
  Result := TVCurrentOnAirDescription;
end;

class function TConditionTVCurrentOnAir.Name: AnsiString;
begin
  Result := 'tvcurrent';
end;

function TConditionTVCurrentOnAir.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).currentair;
    end;
  except
    Result := False;
  end;
end;

{ TConditionTVCurrentSeason }

class function TConditionTVCurrentSeason.Description: AnsiString;
begin
  Result := TVCurrentSeasonDescription;
end;

class function TConditionTVCurrentSeason.Name: AnsiString;
begin
  Result := 'tvcurrentseason';
end;

function TConditionTVCurrentSeason.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).currentseason;
    end;
  except
    Result := False;
  end;
end;

{ TConditionTag }
class function TConditionTag.Description: AnsiString;
begin
  result := TagDescription;
end;

class function TConditionTag.Name: AnsiString;
begin
  Result := 'tag';
end;

procedure TConditionTag.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    re.Assign(r.rls.tags);
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format('[EXCEPTION] TConditionTag.GetSupplyValues: %s',
        [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TConditionTVtag }
class function TConditionTVtag.Description: AnsiString;
begin
  Result := TVtagDescription;
end;

class function TConditionTVtag.Name: AnsiString;
begin
  Result := 'tvtag';
end;

function TConditionTVtag.SupplyValue(r: TPazo): AnsiString;
begin
  Result := '';
  try
    if r.rls is TTvRelease then
      Result := TTvRelease(r.rls).tvtag;
  except
    Result := '';
  end;
end;

{ TConditionIMDBYear }
class function TConditionIMDBYear.Description: AnsiString;
begin
  Result := IMDBYearDescription;
end;

class function TConditionIMDBYear.Name: AnsiString;
begin
  Result := 'imdbyear';
end;

function TConditionIMDBYear.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result := TImdbRelease(r.rls).imdb_year;
    end;
  except
    Result := 0;
  end;
end;

{ TConditionIMDBLanguages }

class function TConditionIMDBLanguages.Description: AnsiString;
begin
  Result := 'Returns with the list of the movie''s languages.';
end;

class function TConditionIMDBLanguages.Name: AnsiString;
begin
  Result := 'imdblanguages';
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
      Debug(dpError, 'rules', Format(
        '[EXCEPTION] TConditionIMDBLanguages.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TConditionIMDBCountries }

class function TConditionIMDBCountries.Description: AnsiString;
begin
  Result := 'Returns with the list of the countries which cooperated in recording the movie.';
end;

class function TConditionIMDBCountries.Name: AnsiString;
begin
  Result := 'imdbcountries';
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
      Debug(dpError, 'rules', Format(
        '[EXCEPTION] TConditionIMDBCountries.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TConditionIMDBGenres }

class function TConditionIMDBGenres.Description: AnsiString;
begin
  Result := 'Returns with the list of the movie''s genres.';
end;

class function TConditionIMDBGenres.Name: AnsiString;
begin
  Result := 'imdbgenre';
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
      Debug(dpError, 'rules', Format(
        '[EXCEPTION] TConditionIMDBGenres.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TConditionIMDBScreens }

class function TConditionIMDBScreens.Description: AnsiString;
begin
  Result := IMDBScreensDescription;
end;

class function TConditionIMDBScreens.Name: AnsiString;
begin
  Result := 'imdbscreens';
end;

function TConditionIMDBScreens.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result := TImdbRelease(r.rls).imdb_screens;
    end;
  except
    Result := 0;
  end;
end;

{ TConditionIMDBRating }
class function TConditionIMDBRating.Description: AnsiString;
begin
  Result := IMDBRatingDescription;
end;

class function TConditionIMDBRating.Name: AnsiString;
begin
  Result := 'imdbrating';
end;

function TConditionIMDBRating.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result := TImdbRelease(r.rls).imdb_rating;
    end;
  except
    Result := 0;
  end;
end;

{ TConditionIMDBVotes }
class function TConditionIMDBVotes.Description: AnsiString;
begin
  Result := IMDBVotesDescription;
end;

class function TConditionIMDBVotes.Name: AnsiString;
begin
  Result := 'imdbvotes';
end;

function TConditionIMDBVotes.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result := TImdbRelease(r.rls).imdb_votes;
    end;
  except
    Result := 0;
  end;
end;

class function TConditionIMDBWide.Description: AnsiString;
begin
  Result := IMDBWideDescription;
end;

class function TConditionIMDBWide.Name: AnsiString;
begin
  Result := 'imdbwide';
end;

function TConditionIMDBWide.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result := TImdbRelease(r.rls).imdb_wide;
    end;
  except
    Result := False;
  end;
end;

class function TConditionIMDBldt.Description: AnsiString;
begin
  Result := IMDBLimitedDescription;
end;

class function TConditionIMDBldt.Name: AnsiString;
begin
  Result := 'imdblimited';
end;

function TConditionIMDBldt.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result := TImdbRelease(r.rls).imdb_ldt;
    end;
  except
    Result := False;
  end;
end;

class function TConditionIMDBFestival.Description: AnsiString;
begin
  Result := IMDBFestivalDescription;
end;

class function TConditionIMDBFestival.Name: AnsiString;
begin
  Result := 'imdbfestival';
end;

function TConditionIMDBFestival.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result := TImdbRelease(r.rls).imdb_festival;
    end;
  except
    Result := False;
  end;
end;

{ TConditionIMDBStv }
class function TConditionIMDBStv.Description: AnsiString;
begin
  Result := IMDBSTVDescription;
end;

class function TConditionIMDBStv.Name: AnsiString;
begin
  Result := 'imdbstv';
end;

function TConditionIMDBStv.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result := TImdbRelease(r.rls).imdb_stvm;  //maybe rename this some more clear; stvm and stvs aren't clear yet
    end;
  except
    Result := False;
  end;
end;

{ TConditionIMDBCineyear }
class function TConditionIMDBCineyear.Description: AnsiString;
begin
  Result := IMDBCineYearDescription;
end;

class function TConditionIMDBCineyear.Name: AnsiString;
begin
  Result := 'imdbcineyear';
end;

function TConditionIMDBCineyear.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  try
    if r.rls is TIMDBRelease then
    begin
      if TImdbRelease(r.rls).imdb_id <> '' then
        Result := TImdbRelease(r.rls).CineYear;
    end;
  except
    Result := 0;
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

class function TConditionMVIDGenre.Description: AnsiString;
begin
  Result := 'Returns the Genre parsed from nfo.' + #13#10;
  //  Result:= Result + '' +#13#00;
end;

class function TConditionMVIDGenre.Name: AnsiString;
begin
  Result := 'mvidgenre';
end;

procedure TConditionMVIDGenre.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    if r.rls is TMVIDRelease then
      re.Assign(TMvidRelease(r.rls).mvid_Genre);
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format(
        '[EXCEPTION] TConditionMVIDGenre.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TConditionMVIDLanguage }

class function TConditionMVIDLanguage.Description: AnsiString;
begin
  Result := 'Returns the Language parsed from nfo.' + #13#10;
  //  Result:= Result + '' +#13#00;
end;

class function TConditionMVIDLanguage.Name: AnsiString;
begin
  Result := 'mvidlanguage';
end;

procedure TConditionMVIDLanguage.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    if r.rls is TMVIDRelease then
      re.Assign(TMvidRelease(r.rls).Languages);
  except
    on E: Exception do
    begin
      Debug(dpError, 'rules', Format(
        '[EXCEPTION] TConditionMVIDLanguage.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

{ TConditionMVIDFiles }

class function TConditionMVIDFiles.Description: AnsiString;
begin
  Result := 'Returns the number of files parsed from sfv. (yes ";" will not count)';
  //+#13#10;
  //  Result:= Result + '' +#13#00;
end;

class function TConditionMVIDFiles.Name: AnsiString;
begin
  Result := 'mvidfiles';
end;

function TConditionMVIDFiles.SupplyValue(r: TPazo): integer;
begin
  Result := -1;
  if r.rls is TMVIDRelease then
    Result := TMVIDRelease(r.rls).FileCount;
end;

{ TConditionMVIDYear }

class function TConditionMVIDYear.Description: AnsiString;
begin
  Result :=
    'Returns with the year of the MVID  release date. Returns zero if NFO lookup is not yet ready.';
  //+#13#10;
  //  Result:= Result + '' +#13#00;
end;

class function TConditionMVIDYear.Name: AnsiString;
begin
  Result := 'mvidyear';
end;

function TConditionMVIDYear.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  if r.rls is TMVIDRelease then
    Result := TMVIDRelease(r.rls).mvid_year;
end;

{ TConditionMVIDVA }

class function TConditionMVIDVA.Description: AnsiString;
begin
  Result := 'Returns true if release is Various Artists (VA)'; //+#13#10;
  //  Result:= Result + '' +#13#00;
end;

class function TConditionMVIDVA.Name: AnsiString;
begin
  Result := 'mvidva';
end;

function TConditionMVIDVA.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  if r.rls is TMVIDRelease then
    Result := TMVIDRelease(r.rls).mvid_va;
end;

{ TConditionMVIDPAL }

class function TConditionMVIDPAL.Description: AnsiString;
begin
  Result := 'Returns with the a boolean for PAL region.'; //+#13#10;
  //  Result:= Result + '' +#13#10;
end;

class function TConditionMVIDPAL.Name: AnsiString;
begin
  Result := 'mvidpal';
end;

function TConditionMVIDPAL.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  if r.rls is TMVIDRelease then
    Result := TMVIDRelease(r.rls).mvid_pal;
end;

{ TConditionMVIDNTSC }

class function TConditionMVIDNTSC.Description: AnsiString;
begin
  Result := 'Returns with the a boolean for NTSC region.'; //+#13#10;
  //  Result:= Result + '' +#13#10;
end;

class function TConditionMVIDNTSC.Name: AnsiString;
begin
  Result := 'mvidntsc';
end;

function TConditionMVIDNTSC.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  if r.rls is TMVIDRelease then
    Result := TMVIDRelease(r.rls).mvid_ntsc;
end;

{ TConditionMVIDLIVE }

class function TConditionMVIDLIVE.Description: AnsiString;
begin
  Result := 'Returns with the a boolean for LIVE type.'; //+#13#10;
  //  Result:= Result + '' +#13#00;
end;

class function TConditionMVIDLIVE.Name: AnsiString;
begin
  Result := 'mvidlive';
end;

function TConditionMVIDLIVE.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  if r.rls is TMVIDRelease then
    Result := TMVIDRelease(r.rls).mvid_live;
end;

function FindConditionClassByName(const Name: AnsiString): TConditionClass;
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

