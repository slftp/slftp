unit mysqlutilunit;

interface

uses slmysql2,sysutils,classes,syncobjs,kb, regexpr;

procedure nWoMYSQLInit;
procedure nWoMYSQLUNinit;
procedure nWoMYSQLStart;

type

PPDBStats = ^PDBStats;
PDBStats = ^TDBStatsRec;
PPDupeResults = ^PDupeResults;
PDupeResults = ^TDupeResultsRec;

TDupeResultsRec = record
rlsname:string;
section:string;
ctime:string;
nukeres:string;
o_ctime:string;
fixxed_ctime:string;
AddedDT:TDateTime;
O_AddedDT:TDateTime;
end;
TDBStatsRec = record
Count:integer;
Nukes:integer;
FirstAdded:TDatetime;
FirstTimeStamp:longint;
FirstAgo:String;
end;


TNWOMYSQL = class
procedure OnConnectorTerminate(Sender: TObject);
private

//DupeResult:array of PDupeResultsRec;
function GetMYSQLHost:string;
function GetMYSQLPort:Integer;
function GetMYSQLUser:string;
function GetMYSQLPassw:string;
function GetMYSQLDBName:string;
function GetMYSQLTableName:string;
procedure SetMYSQLHost(value:string);
procedure SetMYSQLPort(value:integer);
procedure SetMYSQLUser(value:string);
procedure SetMYSQLPassw(value:string);
procedure SetMYSQLDBName(value:string);
procedure SetMYSQLTableName(value:string);
function GetIDFieldName:string;
function GetIDFieldNumber:Integer;
function GetRLSNameFieldName:string;
function GetRLSNameFieldNumber:Integer;
function GetRLSDateFieldName:string;
function GetRLSDateFieldNumber:Integer;
function GetSectionFieldName:string;
function GetSectionFieldNumber:Integer;
function GetFilesFieldName:string;
function GetFilesFieldNumber:Integer;
function GetSizeFieldName:string;
function GetSizeFieldNumber:Integer;
function GetNukereasonFieldName:string;
function GetNukereasonFieldNumber:Integer;
//function GetGenreFieldName:string;
//function GetGenreFieldNumber:Integer;
function GetMYSQLConnectionStatus:string;
function GetNukecount:integer;
function GetFirstctime:Longint;
function GetRlscount:integer;
function GetMYSQLConnectedstatus:boolean;
//function GetDBStats:PDBStatsRec;
function GetTaskMYSQLStatus:boolean;
function MYSQLConnect:boolean;
function MYSQLDisconnect:boolean;
function MYSQLReconnect:boolean;
public
constructor Create;
property RLSCount:Integer read GetRlscount;
property Nukecount:Integer read GetNukecount;
property FirstCTime:Longint read GetFirstctime;
property StatusText:string read GetMYSQLConnectionStatus;
property Connected:Boolean read GetMYSQLConnectedstatus;
property Connect:boolean read MYSQLConnect;
property Disconnect:boolean read MYSQLDisconnect;
property Reconnect:boolean read MYSQLReconnect;
published
property MYSQLHost:string read GetMYSQLHost write SetMYSQLHost;
property MYSQLPort:integer read GetMYSQLPort write SetMYSQLPort;
property MYSQLUsername:string read GetMYSQLUser write SetMYSQLUser;
property MYSQLPassword:string read GetMYSQLPassw write SetMYSQLPassw;
property MYSQLDBName:string read GetMYSQLDBName write SetMYSQLDBName;
property MYSQLTableName:string read GetMYSQLTableName write SetMYSQLTableName;
property IDFieldName:String read GetIDFieldName;
property IDFieldNumber:Integer read GetIDFieldNumber;
property RLSNameFieldName:String read GetRLSNameFieldName;
property RLSNameFieldNumber:Integer read GetRLSNameFieldNumber;
property RLSDateFieldName:String read GetRLSDateFieldName;
property RLSDateFieldNumber:Integer read GetRLSDateFieldNumber;
property SectionFieldName:String read GetSectionFieldName;
property SectionFieldNumber:Integer read GetSectionFieldNumber;
property FilesFieldName:String read GetFilesFieldName;
property FilesFieldNumber:Integer read GetFilesFieldNumber;
property SizeFieldName:String read GetSizeFieldName;
property SizeFieldNumber:Integer read GetSizeFieldNumber;
property NukereasonFieldName:String read GetNukereasonFieldName;
property NukereasonFieldNumber:Integer read GetNukereasonFieldNumber;
//property GenreFieldName:String read GetGenreFieldName;
//property GenreFieldNumber:Integer read GetGenreFieldNumber;
//property OffSet:TSLOFFSET read foffset write foffset;
property Enabled:boolean read GetTaskMYSQLStatus;
end;

TMYSQLConnect = class(TThread)
connect_event:TEvent;
private
public
//constructor create(my:TNWOMYSQL);virtual;
constructor create(wait:boolean);virtual;
//constructor create(UseEvent:boolean;Netname,Channel:string);
protected
procedure Execute;override;
end;


(*
TSLOFFSET = class
private
foffregx:TRegExpr;
foffset:string;
fsoffset:string;
flioffset:longint;
fenabled:boolean;
function AddOffset:boolean;
function DelOffset:boolean;
function GetOffsetTrigger:string;
function ReadOffsetValue:string;

public
constructor Create;
destructor Destroy;override;
function NewCtime(oldctime:Int64):Int64;
property AddTime:Boolean read AddOffset;
property DelTime:Boolean read DelOffset;
property Enabled:boolean read fenabled;
property OffSetValue:Longint read flioffset;
property Trigger:string read GetOffsetTrigger;
end;
   *)








var
dupedb,sql:PMYSQL;
pretimepazo,mslsqle:TEvent;
nwosql:TNWOMYSQL = nil;
conecctor:TMYSQLConnect;
last_ping_check:TDateTime;
//offset:TSLOffset;

implementation

 uses configunit,sitesunit,irc,mystrings,Debugunit,DateUtils,Math;

const
sect:string = 'taskmysql';


{   TNWOMYSQL    }

constructor TNWOMYSQL.Create;
begin
mslsqle:=TEvent.Create(nil,False,False,'nWo_MYSQL_CONNECTION');
end;

procedure TNWOMYSQL.OnConnectorTerminate(Sender: TObject);
begin
irc_addAdmin(StatusText);
end;

function TNWOMYSQL.MYSQLConnect:boolean;
begin
result:=True;
conecctor:=TMYSQLConnect.create(false);
conecctor.FreeOnTerminate:=True;
conecctor.OnTerminate:=OnConnectorTerminate;
conecctor.Resume;
end;

function TNWOMYSQL.MYSQLDisconnect:boolean;
begin
slmysql2.mysql_close(dupedb);
irc_addadmin('Disconnected from MYSQLServer.');
result:=True;
end;

function TNWOMYSQL.MYSQLReconnect:boolean;
begin
MYSQLDisconnect;
MYSQLConnect;
result:=True;
end;

function TNWOMYSQL.GetMYSQLConnectionStatus:string;
begin
if mysql_ping(dupedb) <> 0 then result:='DOH! we are not connected!!!' else
result:='Looks like we are connected';
end;

function TNWOMYSQL.GetTaskMYSQLStatus:boolean;
begin
result:=config.ReadBool('taskmysql','enabled',False);
end;


function TNWOMYSQL.GetMYSQLConnectedstatus:boolean;
begin
if mysql_ping(dupedb) = 0 then Result:=True else Result:=False;
end;


function TNWOMYSQL.GetNukecount:integer;
var
err,cmd:PChar;
nukecount_res:PMYSQL_RES;
rows, rows1: integer;
row:array of pchar;
p:PPChar;
begin
Result:=-1;
//new(nukecount_res);
cmd:=Pchar(Format('SELECT COUNT(%s) as count FROM %s WHERE %s != ""',
[IDFieldName,MYSQLTableName,NukereasonFieldName]));
mysql_real_query(dupedb, pchar(cmd), Length(cmd)+$01);
nukecount_res:=mysql_store_result(dupedb);
FillChar(err, Sizeof(err), 0);
err:=mysql_error(dupedb);
if err <> '' then Debug(dpError, sect, err);
if nukecount_res = nil then exit;
rows:=mysql_num_rows(nukecount_res);
for rows1:=0 to rows-1 do begin
mysql_data_seek(nukecount_res, rows1);
p:=mysql_fetch_row(nukecount_res);
Pointer(row):= p;
if row[0] <> '' then result:=strtoint(row[0]);
end;
mysql_free_result(nukecount_res);
//dispose(nukecount_res);
end;

function TNWOMYSQL.GetRlscount:integer;
var
err,cmd:PChar;
rlscount_resu:PMYSQL_RES;
rows, rows1: integer;
row:array of pchar;
p:PPChar;
begin
result:=-1;
//new(rlscount_resu);
cmd:=Pchar(Format('SELECT COUNT(%s) as count FROM %s',[IDFieldName,MYSQLTableName]));
mysql_real_query(dupedb, pchar(cmd), Length(cmd)+$01);
rlscount_resu:=mysql_store_result(dupedb);
FillChar(err, Sizeof(err), 0);
err:=mysql_error(dupedb);
if err <> '' then Debug(dpError, sect, err);
if rlscount_resu = nil then exit;
rows:=mysql_num_rows(rlscount_resu);
for rows1:=0 to rows-1 do begin
mysql_data_seek(rlscount_resu, rows1);
p:=mysql_fetch_row(rlscount_resu);
Pointer(row):= p;
result:=strtoint(row[0]);
end;
mysql_free_result(rlscount_resu);
//dispose(rlscount_resu);
end;

function TNWOMYSQL.GetFirstctime:Longint;
var err,cmd:PChar;
Firstct_resu:PMYSQL_RES;
rows, rows1: integer;
row:array of pchar;
p:PPChar;
begin
result:=-1;
//new(Firstct_resu);
cmd:=PChar('SELECT * FROM '+MYSQLTableName+' WHERE '+RLSNameFieldName+' LIKE "%" ORDER BY '+RLSDateFieldName+' LIMIT 1');
mysql_real_query(dupedb, pchar(cmd), Length(cmd)+$01);
Firstct_resu:=mysql_store_result(dupedb);
FillChar(err, Sizeof(err), 0);
err:=mysql_error(dupedb);
if err <> '' then Debug(dpError, sect, err);
if Firstct_resu = nil then exit;
rows:=mysql_num_rows(Firstct_resu);
for rows1:=0 to rows-1 do begin
mysql_data_seek(Firstct_resu, rows1);
p:=mysql_fetch_row(Firstct_resu);
Pointer(row):= p;
Result:=strtoint(row[RLSDateFieldNumber]);
end;
mysql_free_result(Firstct_resu);
//dispose(Firstct_resu);
end;

(*
function TNWOMYSQL.GetDBStats:PDBStatsRec;
begin
//TDBStatsRec(result)
result.Count:=GetRlscount;
result.Nukes:=GetNukecount;
result.FirstTimeStamp:=GetFirstctime;
result.FirstAdded:=UnixToDateTime(GetFirstctime);
result.FirstAgo:= PretimeAsString(SecondsBetween(now,result.FirstAdded));
end;
  *)

function TNWOMYSQL.GetMYSQLHost:string;
begin
result:=sitesdat.ReadString('MYSQL','Host','127.0.0.1');
end;
procedure TNWOMYSQL.SetMYSQLHost(value: string);
begin
sitesdat.WriteString('MYSQL','Host',value);
sitesdat.UpdateFile;
end;

function TNWOMYSQL.GetMYSQLPort:integer;
begin
result:=sitesdat.ReadInteger('MYSQL','Port',3306);
end;
procedure TNWOMYSQL.SetMYSQLPort(value: Integer);
begin
sitesdat.WriteInteger('MYSQL','Port',value);
sitesdat.UpdateFile;
end;

function TNWOMYSQL.GetMYSQLUser:string;
begin
result:=sitesdat.ReadString('MYSQL','Username','root');
end;
procedure TNWOMYSQL.SetMYSQLUser(value: string);
begin
sitesdat.WriteString('MYSQL','Username',value);
sitesdat.UpdateFile;
end;

function TNWOMYSQL.GetMYSQLPassw:string;
begin
result:=sitesdat.ReadString('MYSQL','Password','root');
end;
procedure TNWOMYSQL.SetMYSQLPassw(value: string);
begin
sitesdat.WriteString('MYSQL','Password',value);
sitesdat.UpdateFile;
end;
function TNWOMYSQL.GetMYSQLDBName:string;
begin
result:=sitesdat.ReadString('MYSQL','dbname','dupedb');
end;
procedure TNWOMYSQL.SetMYSQLDBName(value: string);
begin
sitesdat.WriteString('MYSQL','dbname',value);
sitesdat.UpdateFile;
end;
function TNWOMYSQL.GetMYSQLTableName:string;
begin
result:=sitesdat.ReadString('MYSQL','tablename','dupetable');
end;
procedure TNWOMYSQL.SetMYSQLTableName(value: string);
begin
sitesdat.WriteString('MYSQL','tablename',value);
sitesdat.UpdateFile;
end;

function TNWOMYSQL.GetIDFieldName:string;
begin
result:=SubString(config.ReadString(sect,'id_field',''),';',1);
end;

function TNWOMYSQL.GetIDFieldNumber:Integer;
begin
result:=Strtoint(SubString(config.ReadString(sect,'id_field',''),';',2));
end;

function TNWOMYSQL.GetRLSNameFieldName:string;
begin
result:=SubString(config.ReadString(sect,'rlsname_field',''),';',1);
end;

function TNWOMYSQL.GetRLSNameFieldNumber:Integer;
begin
result:=StrToInt(SubString(config.ReadString(sect,'rlsname_field',''),';',2));
end;

function TNWOMYSQL.GetRLSDateFieldName:string;
begin
result:=SubString(config.ReadString(sect,'rlsdate_field',''),';',1);
end;

function TNWOMYSQL.GetRLSDateFieldNumber:Integer;
begin
result:=StrToInt(SubString(config.ReadString(sect,'rlsdate_field',''),';',2));
end;

function TNWOMYSQL.GetSectionFieldName:string;
begin
result:=SubString(config.ReadString(sect,'section_field',''),';',1);
end;

function TNWOMYSQL.GetSectionFieldNumber:Integer;
begin
result:=StrToInt(SubString(config.ReadString(sect,'section_field',''),';',2));
end;

function TNWOMYSQL.GetFilesFieldName:string;
begin
result:=SubString(config.ReadString(sect,'files_field',''),';',1);
end;

function TNWOMYSQL.GetFilesFieldNumber:Integer;
begin
result:=StrToInt(SubString(config.ReadString(sect,'files_field',''),';',2));
end;

function TNWOMYSQL.GetSizeFieldName:string;
begin
result:=SubString(config.ReadString(sect,'size_field',''),';',1);
end;

function TNWOMYSQL.GetSizeFieldNumber:Integer;
begin
result:=StrToInt(SubString(config.ReadString(sect,'size_field',''),';',2));
end;

function TNWOMYSQL.GetNukereasonFieldName:string;
begin
result:=SubString(config.ReadString(sect,'nukereason_filed',''),';',1);
end;

function TNWOMYSQL.GetNukereasonFieldNumber:Integer;
begin

result:=StrToInt(SubString(config.ReadString(sect,'nukereason_filed',''),';',2));
end;


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Connection Thread...
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


constructor TMYSQLConnect.create(wait:Boolean);
begin
inherited Create(wait);
end;

procedure TMYSQLConnect.Execute;
begin
dupedb:=slmysql2.mysql_real_connect(sql,pchar(nwosql.MYSQLHost),pchar(nwosql.MYSQLUsername),pchar(nwosql.MYSQLPassword),pchar(nwosql.MYSQLDBName),nwosql.MYSQLPort,nil,0);
if mysql_ping(dupedb) = 0 then irc_addadmin('Connected to MYSQL.db') else irc_addadmin('Cant connect to MYSQL.db');
mslsqle.SetEvent;
end;



procedure nWoMYSQLInit;
begin
//Debug(dpSpam, 'OFFSET', ' Init...');
//offset:=TSLOffset.Create;
(*
Debug(dpSpam, 'MYSQLUtils', ' Init...');
if config.ReadBool('taskmysql','enabled',False) then begin
Debug(dpSpam, 'MYSQLConnection', ' Init...');
//new(sql);
//new(dupedb);
//slmysql2.mysql_thread_init;
sql:=slmysql2.mysql_init(nil);
dupedb:=slmysql2.mysql_init(nil);
nWosql:=TNWOMYSQL.Create;
last_ping_check:=now;
end;
*)
end;

procedure nWoMYSQLStart;
begin
if config.ReadBool('taskmysql','enabled',False) then begin
Debug(dpSpam, 'MYSQLConnection', ' Start...');
nWosql.MYSQLConnect;
end;
end;

procedure nWoMYSQLUNinit;
begin
//Debug(dpSpam, 'OFFSET', 'Uninit1');
//offset.Free;
(*
if config.ReadBool('taskmysql','enabled',False) then begin
Debug(dpSpam, 'MYSQL', 'Uninit1');
//mysql_thread_end;
mysql_close(dupedb);
//dispose(sql);
//dispose(dupedb);
//mysql_close(sql);
sql:=nil;
dupedb:=nil;
nWosql.Free;
end;
Debug(dpSpam, 'MYSQL', 'Uninit2');
*)
end;




(*
constructor TSLOffset.Create;
var s:string;
i:integer;
begin
inherited Create;
foffset:=config.ReadString('offset','value','0');
fenabled:=True;
if foffset = '' then fenabled:=False;
if foffset = '0' then fenabled:=False;
foffregx:=TRegExpr.create;
foffregx.Expression:='(\d+)';
if foffregx.Exec(foffset) then
s:=foffregx.Match[0] else s:='0';
if s <> '0' then begin
i:=strtoint(s);
flioffset:=i * 3600;
end else flioffset:=0;
end;

destructor TSLOffset.Destroy;
begin
  foffregx.free;
  inherited Destroy;
end;

function TSLOffset.GetOffsetTrigger:string;
begin
if AddOffset then result:='+';
if DelOffset then result:='-';
end;

function TSLOffset.AddOffset:boolean;
begin
foffregx.Expression:='^\+';
result:=foffregx.exec(ReadOffsetValue);
end;

function TSLOffset.DelOffset:boolean;
begin
foffregx.Expression:='^\-';
result:=foffregx.exec(ReadOffsetValue);
end;

function TSLOffset.NewCtime(oldctime: Int64):Int64;
var s:string;i:integer;
begin
result:=oldctime;

foffregx.Expression:='(\d+)';
if foffregx.Exec(ReadOffsetValue) then
s:=foffregx.Match[0] else s:='0';
if s <> '0' then begin
i:=strtoint(s);
flioffset:=i * 3600;
end else flioffset:=0;


if AddTime then Inc(Result,OffSetValue);
if DelTime then Dec(Result,OffSetValue);
end;

function TSLOffset.ReadOffsetValue:string;
begin
result:= config.ReadString('offset','value','0');
end;
*)

end.
