unit slsqlite;

interface


type

  sqlite_int64 = int64;
  PPPChar = ^PPChar;
  Psqlite3  = Pointer;
  PPSqlite3 = ^PSqlite3;
  Psqlite3_context  = Pointer;
  Psqlite3_stmt  = Pointer;
  PPsqlite3_stmt = ^Psqlite3_stmt;
  Psqlite3_value  = Pointer;
  PPsqlite3_value  = ^Psqlite3_value;

 sqlite3_callback = function (_para1:pointer; _para2:longint; _para3:PPchar; _para4:PPchar):longint;cdecl;
  busy_handler_func = function (_para1:pointer; _para2:longint):longint;cdecl;
  sqlite3_set_authorizer_func = function (_para1:pointer; _para2:longint; _para3:PAnsiChar; _para4:PAnsiChar; _para5:PAnsiChar; _para6:PAnsiChar):longint;cdecl;
  sqlite3_trace_func = procedure (_para1:pointer; _para2:PAnsiChar);cdecl;
  sqlite3_progress_handler_func = function (_para1:pointer):longint;cdecl;
  sqlite3_commit_hook_func = function (_para1:pointer):longint;cdecl;
  bind_destructor_func = procedure (_para1:pointer);cdecl;
  create_function_step_func = procedure (_para1:Psqlite3_context; _para2:longint; _para3:PPsqlite3_value);cdecl;
  create_function_func_func = procedure (_para1:Psqlite3_context; _para2:longint; _para3:PPsqlite3_value);cdecl;
  create_function_final_func = procedure (_para1:Psqlite3_context);cdecl;
  sqlite3_set_auxdata_func = procedure (_para1:pointer);cdecl;
  sqlite3_result_func = procedure (_para1:pointer);cdecl;
  sqlite3_create_collation_func = function (_para1:pointer; _para2:longint; _para3:pointer; _para4:longint; _para5:pointer):longint;cdecl;
  sqlite3_collation_needed_func = procedure (_para1:pointer; _para2:Psqlite3; eTextRep:longint; _para4:PAnsiChar);cdecl;


  TslSqliteDB = class
  private
    fSQLite: Pointer;

    function Execute(stm: Psqlite3_stmt): boolean;
  public
    function column_count(stm: Psqlite3_stmt): Integer;
    function column_name(stm: Psqlite3_stmt; index: Integer): AnsiString;
    function column_int(stm: Psqlite3_stmt; index: Integer): Integer;
    function column_int64(stm: Psqlite3_stmt; index: Integer): Int64;
    function column_double(stm: Psqlite3_stmt; index: Integer): Double;
    function column_text(stm: Psqlite3_stmt; index: Integer): AnsiString;

    function Step(stm: Psqlite3_stmt): Boolean;

    function Open(const sql: AnsiString): Psqlite3_stmt; overload;
    function Open(const sql: AnsiString; const Args: array of const): Psqlite3_stmt; overload;
    function Open(stm: Psqlite3_stmt): Boolean; overload;
    function Open(stm: Psqlite3_stmt; const Args: array of const): Boolean; overload;

    function Reset(stm: PSqlite3_stmt): Boolean;
    function Bind(stm: Psqlite3_stmt; const Args: array of const): Boolean;

    function ExecSQL(stm: Psqlite3_stmt): Boolean; overload;
    function ExecSQL(stm: Psqlite3_stmt; const Args: array of const): Boolean; overload;
    function ExecSQL(const sql: AnsiString): Boolean; overload;
    function ExecSQL(const sql: AnsiString; const Args: array of const): Boolean; overload;

    function Close(var re: Psqlite3_stmt): Boolean;

    constructor Create(const filename: AnsiString; pragma: AnsiString);
    destructor Destroy; override;
  end;
  
Var
  sqlite3_close : function (_para1:Psqlite3):longint;cdecl;
  sqlite3_exec : function (_para1:Psqlite3; sql:PAnsiChar; _para3:sqlite3_callback; _para4:pointer; errmsg:PPchar):longint;cdecl;
  sqlite3_last_insert_rowid : function (_para1:Psqlite3):sqlite_int64;cdecl;
  sqlite3_changes : function (_para1:Psqlite3):longint;cdecl;
  sqlite3_total_changes : function (_para1:Psqlite3):longint;cdecl;
  sqlite3_interrupt : procedure (_para1:Psqlite3);cdecl;
  sqlite3_complete : function (sql:PAnsiChar):longint;cdecl;
  sqlite3_complete16 : function (sql:pointer):longint;cdecl;
  sqlite3_busy_handler : function (_para1:Psqlite3; _para2:busy_handler_func; _para3:pointer):longint;cdecl;
  sqlite3_busy_timeout : function (_para1:Psqlite3; ms:longint):longint;cdecl;
  sqlite3_get_table : function (_para1:Psqlite3; sql:PAnsiChar; resultp:PPPchar; nrow:Plongint; ncolumn:Plongint; errmsg:PPchar):longint;cdecl;
  sqlite3_free_table : procedure (result:PPchar);cdecl;
// Todo: see how translate sqlite3_mprintf, sqlite3_vmprintf, sqlite3_snprintf
//   sqlite3_mprintf : function (_para1:Pchar; args:array of const):Pchar;cdecl;
  sqlite3_mprintf : function (_para1:PAnsiChar):PAnsiChar;cdecl;
//  sqlite3_vmprintf : function (_para1:Pchar; _para2:va_list):Pchar;cdecl;
  sqlite3_free : procedure (z:PAnsiChar);cdecl;
//  sqlite3_snprintf : function (_para1:longint; _para2:Pchar; _para3:Pchar; args:array of const):Pchar;cdecl;
  sqlite3_snprintf : function (_para1:longint; _para2:PAnsiChar; _para3:PAnsiChar):PAnsiChar;cdecl;
  sqlite3_set_authorizer : function (_para1:Psqlite3; xAuth:sqlite3_set_authorizer_func; pUserData:pointer):longint;cdecl;
  sqlite3_trace : function (_para1:Psqlite3; xTrace:sqlite3_trace_func; _para3:pointer):pointer;cdecl;
  sqlite3_progress_handler : procedure (_para1:Psqlite3; _para2:longint; _para3:sqlite3_progress_handler_func; _para4:pointer);cdecl;
  sqlite3_commit_hook : function (_para1:Psqlite3; _para2:sqlite3_commit_hook_func; _para3:pointer):pointer;cdecl;
  sqlite3_open : function (filename:PAnsiChar; ppDb:PPsqlite3):longint;cdecl;
  sqlite3_open16 : function (filename:pointer; ppDb:PPsqlite3):longint;cdecl;
  sqlite3_errcode : function (db:Psqlite3):longint;cdecl;
  sqlite3_errmsg : function (_para1:Psqlite3):PAnsiChar;cdecl;
  sqlite3_errmsg16 : function (_para1:Psqlite3):pointer;cdecl;
  sqlite3_prepare : function (db:Psqlite3; zSql:PAnsiChar; nBytes:longint; var ppStmt:Psqlite3_stmt; pzTail:PPchar):longint;cdecl;
  sqlite3_prepare16 : function (db:Psqlite3; zSql:pointer; nBytes:longint; ppStmt:PPsqlite3_stmt; pzTail:Ppointer):longint;cdecl;
  sqlite3_bind_blob : function (_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; n:longint; _para5:bind_destructor_func):longint;cdecl;
  sqlite3_bind_double : function (_para1:Psqlite3_stmt; _para2:longint; _para3:double):longint;cdecl;
  sqlite3_bind_int : function (_para1:Psqlite3_stmt; _para2:longint; _para3:longint):longint;cdecl;
  sqlite3_bind_int64 : function (_para1:Psqlite3_stmt; _para2:longint; _para3:sqlite_int64):longint;cdecl;
  sqlite3_bind_null : function (_para1:Psqlite3_stmt; _para2:longint):longint;cdecl;
  sqlite3_bind_text : function (_para1:Psqlite3_stmt; _para2:longint; _para3:PAnsiChar; n:longint; _para5:bind_destructor_func):longint;cdecl;
  sqlite3_bind_text16 : function (_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; _para4:longint; _para5:bind_destructor_func):longint;cdecl;
//  sqlite3_bind_value : function (_para1:Psqlite3_stmt; _para2:longint; _para3:Psqlite3_value):longint;cdecl;
//These overloaded functions were introduced to allow the use of SQLITE_STATIC and SQLITE_TRANSIENT
//It's the c world man ;-)
  sqlite3_bind_blob1 : function (_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; n:longint; _para5:longint):longint;cdecl;
  sqlite3_bind_text1 : function (_para1:Psqlite3_stmt; _para2:longint; _para3:PAnsiChar; n:longint; _para5:longint):longint;cdecl;
  sqlite3_bind_text161 : function (_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; _para4:longint; _para5:longint):longint;cdecl;

  sqlite3_bind_parameter_count : function (_para1:Psqlite3_stmt):longint;cdecl;
  sqlite3_bind_parameter_name : function (_para1:Psqlite3_stmt; _para2:longint):PAnsiChar;cdecl;
  sqlite3_bind_parameter_index : function (_para1:Psqlite3_stmt; zName:PAnsiChar):longint;cdecl;
//  sqlite3_clear_bindings : function (_para1:Psqlite3_stmt):longint;cdecl;
  sqlite3_column_count : function (pStmt:Psqlite3_stmt):longint;cdecl;
  sqlite3_column_name : function (_para1:Psqlite3_stmt; _para2:longint):PAnsiChar;cdecl;
  sqlite3_column_name16 : function (_para1:Psqlite3_stmt; _para2:longint):pointer;cdecl;
  sqlite3_column_decltype : function (_para1:Psqlite3_stmt; i:longint):PAnsiChar;cdecl;
  sqlite3_column_decltype16 : function (_para1:Psqlite3_stmt; _para2:longint):pointer;cdecl;
  sqlite3_step : function (_para1:Psqlite3_stmt):longint;cdecl;
  sqlite3_data_count : function (pStmt:Psqlite3_stmt):longint;cdecl;

  sqlite3_clear_bindings: function (pStmt:Psqlite3_stmt):longint;cdecl;

  sqlite3_column_blob : function (_para1:Psqlite3_stmt; iCol:longint):pointer;cdecl;
  sqlite3_column_bytes : function (_para1:Psqlite3_stmt; iCol:longint):longint;cdecl;
  sqlite3_column_bytes16 : function (_para1:Psqlite3_stmt; iCol:longint):longint;cdecl;
  sqlite3_column_double : function (_para1:Psqlite3_stmt; iCol:longint):double;cdecl;
  sqlite3_column_int : function (_para1:Psqlite3_stmt; iCol:longint):longint;cdecl;
  sqlite3_column_int64 : function (_para1:Psqlite3_stmt; iCol:longint):sqlite_int64;cdecl;
  sqlite3_column_text : function (_para1:Psqlite3_stmt; iCol:longint):PAnsiChar;cdecl;
  sqlite3_column_text16 : function (_para1:Psqlite3_stmt; iCol:longint):pointer;cdecl;
  sqlite3_column_type : function (_para1:Psqlite3_stmt; iCol:longint):longint;cdecl;
  sqlite3_finalize : function (pStmt:Psqlite3_stmt):longint;cdecl;
  sqlite3_reset : function (pStmt:Psqlite3_stmt):longint;cdecl;
  sqlite3_create_function : function (_para1:Psqlite3; zFunctionName:PAnsiChar; nArg:longint; eTextRep:longint; _para5:pointer;        xFunc:create_function_func_func; xStep:create_function_step_func; xFinal:create_function_final_func):longint;cdecl;
  sqlite3_create_function16 : function (_para1:Psqlite3; zFunctionName:pointer; nArg:longint; eTextRep:longint; _para5:pointer;            xFunc:create_function_func_func; xStep:create_function_step_func; xFinal:create_function_final_func):longint;cdecl;
  sqlite3_aggregate_count : function (_para1:Psqlite3_context):longint;cdecl;
  sqlite3_value_blob : function (_para1:Psqlite3_value):pointer;cdecl;
  sqlite3_value_bytes : function (_para1:Psqlite3_value):longint;cdecl;
  sqlite3_value_bytes16 : function (_para1:Psqlite3_value):longint;cdecl;
  sqlite3_value_double : function (_para1:Psqlite3_value):double;cdecl;
  sqlite3_value_int : function (_para1:Psqlite3_value):longint;cdecl;
  sqlite3_value_int64 : function (_para1:Psqlite3_value):sqlite_int64;cdecl;
  sqlite3_value_text : function (_para1:Psqlite3_value):PAnsiChar;cdecl;
  sqlite3_value_text16 : function (_para1:Psqlite3_value):pointer;cdecl;
  sqlite3_value_text16le : function (_para1:Psqlite3_value):pointer;cdecl;
  sqlite3_value_text16be : function (_para1:Psqlite3_value):pointer;cdecl;
  sqlite3_value_type : function (_para1:Psqlite3_value):longint;cdecl;
  sqlite3_aggregate_context : function (_para1:Psqlite3_context; nBytes:longint):pointer;cdecl;
  sqlite3_user_data : function (_para1:Psqlite3_context):pointer;cdecl;
  sqlite3_get_auxdata : function (_para1:Psqlite3_context; _para2:longint):pointer;cdecl;
  sqlite3_set_auxdata : procedure (_para1:Psqlite3_context; _para2:longint; _para3:pointer; _para4:sqlite3_set_auxdata_func);cdecl;
  sqlite3_result_blob : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl;
  sqlite3_result_double : procedure (_para1:Psqlite3_context; _para2:double);cdecl;
  sqlite3_result_error : procedure (_para1:Psqlite3_context; _para2:PAnsiChar; _para3:longint);cdecl;
  sqlite3_result_error16 : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:longint);cdecl;
  sqlite3_result_int : procedure (_para1:Psqlite3_context; _para2:longint);cdecl;
  sqlite3_result_int64 : procedure (_para1:Psqlite3_context; _para2:sqlite_int64);cdecl;
  sqlite3_result_null : procedure (_para1:Psqlite3_context);cdecl;
  sqlite3_result_text : procedure (_para1:Psqlite3_context; _para2:PAnsiChar; _para3:longint; _para4:sqlite3_result_func);cdecl;
  sqlite3_result_text16 : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl;
  sqlite3_result_text16le : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl;
  sqlite3_result_text16be : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl;
  sqlite3_result_value : procedure (_para1:Psqlite3_context; _para2:Psqlite3_value);cdecl;     
  sqlite3_create_collation : function (_para1:Psqlite3; zName:PAnsiChar; eTextRep:longint; _para4:pointer; xCompare:sqlite3_create_collation_func):longint;cdecl;
  sqlite3_create_collation16 : function (_para1:Psqlite3; zName:PAnsiChar; eTextRep:longint; _para4:pointer; xCompare:sqlite3_create_collation_func):longint;cdecl;
  sqlite3_collation_needed : function (_para1:Psqlite3; _para2:pointer; _para3:sqlite3_collation_needed_func):longint;cdecl;
  sqlite3_collation_needed16 : function (_para1:Psqlite3; _para2:pointer; _para3:sqlite3_collation_needed_func):longint;cdecl;
  sqlite3_libversion: function : PAnsiChar;cdecl;
  sqlite3_initialize: function : Longint;cdecl;
  sqlite3_shutdown: function : Longint;cdecl;

// Not published functions
  sqlite3_libversion_number : Function :longint;cdecl;
//  sqlite3_key : function (db:Psqlite3; pKey:pointer; nKey:longint):longint;cdecl;
//  sqlite3_rekey : function (db:Psqlite3; pKey:pointer; nKey:longint):longint;cdecl;
//  sqlite3_sleep : function (_para1:longint):longint;cdecl;
//  sqlite3_expired : function (_para1:Psqlite3_stmt):longint;cdecl;
//function sqlite3_global_recover:longint;cdecl;

function slSqliteVersion: AnsiString;

var slsqlite_inited: Boolean = False;
    slsqlite_error: AnsiString = '';

implementation

uses
  SysUtils,
  mystrings,
{$IFDEF FPC}
  dynlibs
{$ELSE}
  {$IFDEF MSWINDOWS}
  Windows
  {$ELSE}
  Libc
  {$ENDIF}
{$ENDIF}
;

const
  {$IFDEF LINUX}
  slSqlite_libsqlite_name         = 'libsqlite3.so.0'; {Do not localize}
  {$ELSE}
  slSqlite_libsqlite_name         = 'sqlite3.dll';  {Do not localize}
  {$ENDIF}

const

  //sqlite_exec and sqlite_step return values
  SQLITE_OK         = 0;
  SQLITE_ERROR      = 1;
  SQLITE_INTERNAL   = 2;
  SQLITE_PERM       = 3;
  SQLITE_ABORT      = 4;
  SQLITE_BUSY       = 5;
  SQLITE_LOCKED     = 6;
  SQLITE_NOMEM      = 7;
  SQLITE_READONLY   = 8;
  SQLITE_INTERRUPT  = 9;
  SQLITE_IOERR      = 10;
  SQLITE_CORRUPT    = 11;
  SQLITE_NOTFOUND   = 12;
  SQLITE_FULL       = 13;
  SQLITE_CANTOPEN   = 14;
  SQLITE_PROTOCOL   = 15;
  SQLITE_EMPTY      = 16;
  SQLITE_SCHEMA     = 17;
  SQLITE_TOOBIG     = 18;
  SQLITE_CONSTRAINT = 19;
  SQLITE_MISMATCH   = 20;
  SQLITE_MISUSE     = 21;
  SQLITE_NOLFS      = 22;
  SQLITE_AUTH       = 23;
  SQLITE_FORMAT     = 24;
  SQLITE_RANGE      = 25;
  SQLITE_ROW        = 100;
  SQLITE_DONE       = 101;

  // values used in sqlite_set_authorizer to define what operations authorize
  SQLITE_COPY                = 0;
  SQLITE_CREATE_INDEX        = 1;
  SQLITE_CREATE_TABLE        = 2;
  SQLITE_CREATE_TEMP_INDEX   = 3;
  SQLITE_CREATE_TEMP_TABLE   = 4;
  SQLITE_CREATE_TEMP_TRIGGER = 5;
  SQLITE_CREATE_TEMP_VIEW    = 6;
  SQLITE_CREATE_TRIGGER      = 7;
  SQLITE_CREATE_VIEW         = 8;
  SQLITE_DELETE              = 9;
  SQLITE_DROP_INDEX          = 10;
  SQLITE_DROP_TABLE          = 11;
  SQLITE_DROP_TEMP_INDEX     = 12;
  SQLITE_DROP_TEMP_TABLE     = 13;
  SQLITE_DROP_TEMP_TRIGGER   = 14;
  SQLITE_DROP_TEMP_VIEW      = 15;
  SQLITE_DROP_TRIGGER        = 16;
  SQLITE_DROP_VIEW           = 17;
  SQLITE_INSERT              = 18;
  SQLITE_PRAGMA              = 19;
  SQLITE_READ                = 20;
  SQLITE_SELECT              = 21;
  SQLITE_TRANSACTION         = 22;
  SQLITE_UPDATE              =  23;
  
  //Return values of the authorizer function
  SQLITE_DENY                = 1;
  SQLITE_IGNORE              = 2;

  SQLITE_NUMERIC = -1;
  SQLITE_TEXT    = -2;
  SQLITE_ARGS    = -3;

Const
  SQLITE_STATIC    =  0;
  SQLITE_TRANSIENT =  -1;
var
  h_libsqlite    : Integer = 0;


function slSqlite_LoadProc(handle: Integer; const fnName: AnsiString; var fn: Pointer): Boolean;
var fceName: AnsiString;
begin
  Result:= False;
  FceName := fnName+#0;
{$IFDEF FPC}
  fn := GetProcAddress(handle, fceName);
{$ELSE}
  fn := GetProcAddress(handle, @fceName[1]);
{$ENDIF}
  if fn = nil then
  begin
    slsqlite_error:= 'Cannot load '+fnName
  end
  else
    Result:= True;
end;

procedure slSqliteInit;
begin
  if slsqlite_inited then exit;

{$IFDEF FPC}
  // Workaround that is requered under Linux
  if h_libsqlite = 0 then h_libsqlite := LoadLibrary(ExtractFilePath(ParamStr(0))+slSqlite_libsqlite_name);
  if h_libsqlite = 0 then h_libsqlite := LoadLibrary(slSqlite_libsqlite_name);
  if h_libsqlite = 0 then
  begin
    slsqlite_error:= 'Couldnt load libcrypto';
    exit;
  end;

{$ELSE}
  {$IFDEF LINUX}
  // Workaround that is requered under Linux
  if h_libsqlite = 0 then h_libsqlite := HMODULE(dlopen(PChar(ExtractFilePath(ParamStr(0))+slSqlite_libsqlite_name), RTLD_GLOBAL));
  if h_libsqlite = 0 then h_libsqlite := HMODULE(dlopen(slSqlite_libsqlite_name, RTLD_GLOBAL));
  if h_libsqlite = 0 then
  begin
    slsqlite_error:= 'Couldnt load libcrypto';
    exit;
  end;

  {$ELSE}
  if h_libsqlite = 0 then
    h_libsqlite := LoadLibrary(slSqlite_libsqlite_name);
  if h_libsqlite = 0 then
  begin
    slsqlite_error:= 'Couldnt load libsqlite: '+SysErrorMessage(GetLastError) ;
    exit;
  end;

  {$ENDIF}
{$ENDIF}


  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_close', @sqlite3_close) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_exec', @sqlite3_exec) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_last_insert_rowid', @sqlite3_last_insert_rowid) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_changes', @sqlite3_changes) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_total_changes', @sqlite3_total_changes) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_interrupt', @sqlite3_interrupt) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_complete', @sqlite3_complete) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_complete16', @sqlite3_complete16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_busy_handler', @sqlite3_busy_handler) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_busy_timeout', @sqlite3_busy_timeout) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_get_table', @sqlite3_get_table) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_free_table', @sqlite3_free_table) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_mprintf', @sqlite3_mprintf) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_free', @sqlite3_free) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_snprintf', @sqlite3_snprintf) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_set_authorizer', @sqlite3_set_authorizer) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_trace', @sqlite3_trace) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_progress_handler', @sqlite3_progress_handler) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_commit_hook', @sqlite3_commit_hook) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_open', @sqlite3_open) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_open16', @sqlite3_open16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_errcode', @sqlite3_errcode) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_errmsg', @sqlite3_errmsg) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_errmsg16', @sqlite3_errmsg16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_prepare', @sqlite3_prepare) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_prepare16', @sqlite3_prepare16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_bind_blob', @sqlite3_bind_blob) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_bind_double', @sqlite3_bind_double) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_bind_int', @sqlite3_bind_int) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_bind_int64', @sqlite3_bind_int64) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_bind_null', @sqlite3_bind_null) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_bind_text', @sqlite3_bind_text) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_bind_text16', @sqlite3_bind_text16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_bind_blob', @sqlite3_bind_blob) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_bind_text', @sqlite3_bind_text1) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_bind_text16', @sqlite3_bind_text161) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_bind_parameter_count', @sqlite3_bind_parameter_count) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_bind_parameter_name', @sqlite3_bind_parameter_name) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_bind_parameter_index', @sqlite3_bind_parameter_index) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_count', @sqlite3_column_count) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_name', @sqlite3_column_name) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_name16', @sqlite3_column_name16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_decltype', @sqlite3_column_decltype) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_decltype16', @sqlite3_column_decltype16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_step', @sqlite3_step) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_clear_bindings', @sqlite3_clear_bindings) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_data_count', @sqlite3_data_count) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_blob', @sqlite3_column_blob) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_bytes', @sqlite3_column_bytes) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_bytes16', @sqlite3_column_bytes16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_double', @sqlite3_column_double) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_int', @sqlite3_column_int) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_int64', @sqlite3_column_int64) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_text', @sqlite3_column_text) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_text16', @sqlite3_column_text16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_column_type', @sqlite3_column_type) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_finalize', @sqlite3_finalize) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_reset', @sqlite3_reset) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_create_function', @sqlite3_create_function) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_create_function16', @sqlite3_create_function16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_aggregate_count', @sqlite3_aggregate_count) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_value_blob', @sqlite3_value_blob) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_value_bytes', @sqlite3_value_bytes) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_value_bytes16', @sqlite3_value_bytes16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_value_double', @sqlite3_value_double) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_value_int', @sqlite3_value_int) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_value_int64', @sqlite3_value_int64) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_value_text', @sqlite3_value_text) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_value_text16', @sqlite3_value_text16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_value_text16le', @sqlite3_value_text16le) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_value_text16be', @sqlite3_value_text16be) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_value_type', @sqlite3_value_type) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_aggregate_context', @sqlite3_aggregate_context) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_user_data', @sqlite3_user_data) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_get_auxdata', @sqlite3_get_auxdata) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_set_auxdata', @sqlite3_set_auxdata) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_result_blob', @sqlite3_result_blob) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_result_double', @sqlite3_result_double) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_result_error', @sqlite3_result_error) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_result_error16', @sqlite3_result_error16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_result_int', @sqlite3_result_int) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_result_int64', @sqlite3_result_int64) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_result_null', @sqlite3_result_null) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_result_text', @sqlite3_result_text) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_result_text16', @sqlite3_result_text16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_result_text16le', @sqlite3_result_text16le) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_result_text16be', @sqlite3_result_text16be) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_result_value', @sqlite3_result_value) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_create_collation', @sqlite3_create_collation) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_create_collation16', @sqlite3_create_collation16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_collation_needed', @sqlite3_collation_needed) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_collation_needed16', @sqlite3_collation_needed16) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_libversion', @sqlite3_libversion) then exit;
  if not slSqlite_LoadProc(h_libsqlite, 'sqlite3_libversion_number', @sqlite3_libversion_number) then exit;
  slSqlite_LoadProc(h_libsqlite, 'sqlite3_initialize', @sqlite3_initialize);
  slSqlite_LoadProc(h_libsqlite, 'sqlite3_shutdown', @sqlite3_shutdown);

  if ((not Assigned(sqlite3_initialize)) or (0 = sqlite3_initialize())) then
    slsqlite_inited:= True;
end;

procedure slSqliteUnInit;
begin
  if not slsqlite_inited then exit;
  if Assigned(sqlite3_shutdown) then
    sqlite3_shutdown();
  if h_libsqlite > 0 then FreeLibrary(h_libsqlite);
  h_libsqlite := 0;

  slsqlite_inited:= False;
end;

function slSqliteVersion: AnsiString;
begin
  Result:= AnsiString(SQLite3_libVersion);
end;


{ TslSqliteDB }

function TslSqliteDB.Bind(stm: Psqlite3_stmt; const Args: array of const): Boolean;
var i: Integer;
    s: AnsiString;
    d: Double;
begin
  Result:= False;
  if 0 <> sqlite3_clear_bindings(stm) then exit;

  for I := 0 to High(Args) do
    with Args[I] do
      case VType of
        vtInteger: if 0 <> sqlite3_bind_int(stm, i+1, VInteger) then exit;
        vtInt64:   if 0 <>sqlite3_bind_int64(stm, i+1, VInt64^) then exit;
        vtBoolean: if 0 <> sqlite3_bind_int(stm, i+1, Integer(VBoolean)) then exit;
        vtString:
          begin
            s:= VString^;
            if 0 <> sqlite3_bind_text1(stm, i+1, PAnsiChar(s), length(s), SQLITE_STATIC) then exit;
          end;
        vtAnsiString:
          begin
            s:= AnsiString(VAnsiString);
            if 0 <> sqlite3_bind_text1(stm, i+1, PAnsiChar(s), length(s), SQLITE_STATIC) then exit;
          end;
        vtCurrency:
          begin
            s:= CurrToStr(VCurrency^);
            if 0 <> sqlite3_bind_text1(stm, i+1, PAnsiChar(s), length(s), SQLITE_STATIC) then exit;
          end;
        vtVariant:
          begin
            s:= AnsiString(VVariant^);
            if 0 <> sqlite3_bind_text1(stm, i+1, PAnsiChar(s), length(s), SQLITE_STATIC) then exit;
          end;
        vtChar:
          begin
            s:= VChar;
            if 0 <> sqlite3_bind_text1(stm, i+1, PAnsiChar(s), length(s), SQLITE_STATIC) then exit;
          end;
        vtObject,
        vtClass:
          begin
            s:= VObject.ClassName;
            if 0 <> sqlite3_bind_text1(stm, i+1, PAnsiChar(s), length(s), SQLITE_STATIC) then exit;
          end;
        vtPChar:
            if 0 <> sqlite3_bind_text1(stm, i+1, VPChar, StrLen(VPChar), SQLITE_STATIC) then exit;
        vtPointer:
            if 0 <> sqlite3_bind_null(stm, i+1) then exit;
        vtExtended:
          begin
            d:= VExtended^;
            if 0 <> sqlite3_bind_double(stm, i+1, d) then exit;
          end;
    end;

  Result:= True;
end;

constructor TslSqliteDB.Create(const filename: AnsiString; pragma: AnsiString);
var ss: AnsiString;
begin
 if 0 <> SQLite3_Open(PChar(filename), @fSQLite) then
   raise Exception.Create('Cant open sqlite');

 pragma:= Trim(pragma);
 while(true)do
 begin
   ss:= Fetch(pragma, ';');
   if ss= '' then Break;
   ExecSQL('PRAGMA '+ss);
 end;
end;

destructor TslSqliteDB.Destroy;
begin
  SQLite3_Close(fSQLite);

  inherited;
end;



function TslSqliteDB.ExecSQL(stm: Psqlite3_stmt;  const Args: array of const): Boolean;
begin

  Result:= False;

  if not Reset(stm) then exit;
  if not Bind(stm, Args) then exit;

  if sqlite3_step(stm) <> SQLITE_DONE then exit;

  Result:= True;
end;

function TslSqliteDB.ExecSQL(stm: Psqlite3_stmt): Boolean;
begin
  Result:= ExecSQL(stm, []);
end;

function TslSqliteDB.ExecSQL(const sql: AnsiString): Boolean;
begin
  Result:= ExecSQL(sql, []);
end;

function TslSqliteDB.ExecSQL(const sql: AnsiString;
  const Args: array of const): Boolean;
var re: Psqlite3_stmt;
begin
  Result:= False;

  re:= Open(sql, args);
  if re = nil then exit;

  if not execute(re) then
  begin
    Close(re);
    exit;
  end;

  Close(re);

  Result:= True;
end;

function TslSqliteDB.Close(var re: Psqlite3_stmt): Boolean;
begin
  Result:= False;
  if re <> nil then
  begin
    if 0 <> sqlite3_finalize(re) then exit;
    re:= nil;
  end;
  Result:= TRue;
end;

function TslSqliteDB.Step(stm: Psqlite3_stmt): Boolean;
begin
  Result:= False;
  if SQLITE_ROW <> sqlite3_step(stm) then exit;
  Result:= TRue;
end;




function TslSqliteDB.Open(const sql: AnsiString;
  const Args: array of const): Psqlite3_stmt;
var re: Psqlite3_stmt;
begin
  Result:= nil;

  if 0 <> sqlite3_prepare(fSQLite, PAnsiChar(sql), length(sql), re, nil) then exit;

  if not Bind(re, Args) then
  begin
    Close(re);
    exit;
  end;

  Result:= re;
end;

function TslSqliteDB.Open(const sql: AnsiString): Psqlite3_stmt;
begin
  Result:= Open(sql, []);
end;

function TslSqliteDB.column_count(stm: Psqlite3_stmt): Integer;
begin
  Result:= sqlite3_column_count(stm);
end;

function TslSqliteDB.column_name(stm: Psqlite3_stmt; index: Integer): AnsiString;
var c: PAnsiChar;
begin
  Result:= '';

  c:= sqlite3_column_name(stm, index);
  Result:= StrPas(c);
end;

function TslSqliteDB.column_int(stm: Psqlite3_stmt; index: Integer): Integer;
begin
  Result:= sqlite3_column_int(stm, index);
end;
function TslSqliteDB.column_int64(stm: Psqlite3_stmt; index: Integer): Int64;
begin
  Result:= sqlite3_column_int64(stm, index);
end;

function TslSqliteDB.column_double(stm: Psqlite3_stmt; index: Integer): Double;
begin
  Result:= sqlite3_column_double(stm, index);
end;

function TslSqliteDB.column_text(stm: Psqlite3_stmt; index: Integer): AnsiString;
begin
  Result:= StrPas(sqlite3_column_text(stm, index));
end;


function TslSqliteDB.Execute(stm: Psqlite3_stmt): boolean;
begin
  Result:= False;
  if sqlite3_step(stm) <> SQLITE_DONE then exit;
  REsult:= true;
end;

function TslSqliteDB.Reset(stm: PSqlite3_stmt): Boolean;
begin
  Result:= False;
  if 0 <> sqlite3_reset(stm) then exit;
  Result:= true;
end;

function TslSqliteDB.Open(stm: Psqlite3_stmt): Boolean;
begin
  Result:= Open(stm, []);
end;

function TslSqliteDB.Open(stm: Psqlite3_stmt;
  const Args: array of const): Boolean;
begin
  Result:= False;
  if not Reset(stm) then exit;
  if not Bind(stm, Args) then exit;
  Result:= True;
end;

initialization
  slSqliteInit;
finalization
  slSqliteUnInit;
end.
