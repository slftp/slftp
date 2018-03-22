unit slmysql2;

interface

uses Classes, ctypes, types;

{$IFDEF FPC}
{$MACRO on}
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$DEFINE extdecl:=stdcall}
  const
    mysqllib  = 'libmysql.dll';
    mysqlvlib = 'libmysql.dll';

{$ELSE}
  {$DEFINE extdecl:=cdecl}
  const
    sharedsuffix = 'so';
    mysqllib = 'libmysqlclient_r.'+sharedsuffix;
    //mysqlvlib = mysqllib+'.15';
    mysqlvlib = 'libmysqlclient_r.'+sharedsuffix;

{$ENDIF}

const
       CLIENT_LONG_PASSWORD = 1;           // new more secure passwords
       CLIENT_FOUND_ROWS = 2;              // Found instead of affected rows
       CLIENT_LONG_FLAG = 4;               // Get all column flags
       CLIENT_CONNECT_WITH_DB = 8;         // One can specify db on connect
       CLIENT_NO_SCHEMA = 16;              // Don't allow database.table.column
       CLIENT_COMPRESS = 32;               // Can use compression protocol
       CLIENT_ODBC = 64;                   // Odbc client
       CLIENT_LOCAL_FILES = 128;           // Can use LOAD DATA LOCAL
       CLIENT_IGNORE_SPACE = 256;          // Ignore spaces before '('
       CLIENT_PROTOCOL_41 = 512;           // New 4.1 protocol
       CLIENT_INTERACTIVE = 1024;          // This is an interactive client
       CLIENT_SSL = 2048;                  // Switch to SSL after handshake
       CLIENT_IGNORE_SIGPIPE = 4096;       // IGNORE sigpipes
       CLIENT_TRANSACTIONS = 8192;         // Client knows about transactions
       CLIENT_RESERVED = 16384;            // Old flag for 4.1 protocol
       CLIENT_SECURE_CONNECTION = 32768;   // New 4.1 authentication
       CLIENT_MULTI_STATEMENTS = 65536;    // Enable/disable multi-stmt support
       CLIENT_MULTI_RESULTS = 131072;      // Enable/disable multi-results
       CLIENT_REMEMBER_OPTIONS : cardinal = $80000000; // 1 shl 31;


type PMYSQL = Pointer;
     PMYSQL_RES = Pointer;
       my_bool = cchar;

       MYSQL_ROW = ppchar;

       enum_field_types = (MYSQL_TYPE_DECIMAL,MYSQL_TYPE_TINY,
         MYSQL_TYPE_SHORT,MYSQL_TYPE_LONG,MYSQL_TYPE_FLOAT,
         MYSQL_TYPE_DOUBLE,MYSQL_TYPE_NULL,
         MYSQL_TYPE_TIMESTAMP,MYSQL_TYPE_LONGLONG,
         MYSQL_TYPE_INT24,MYSQL_TYPE_DATE,MYSQL_TYPE_TIME,
         MYSQL_TYPE_DATETIME,MYSQL_TYPE_YEAR,
         MYSQL_TYPE_NEWDATE,
         MYSQL_TYPE_VARCHAR, MYSQL_TYPE_BIT, MYSQL_TYPE_NEWDECIMAL=246,
         MYSQL_TYPE_ENUM = 247,
         MYSQL_TYPE_SET = 248,MYSQL_TYPE_TINY_BLOB = 249,
         MYSQL_TYPE_MEDIUM_BLOB = 250,MYSQL_TYPE_LONG_BLOB = 251,
         MYSQL_TYPE_BLOB = 252,MYSQL_TYPE_VAR_STRING = 253,
         MYSQL_TYPE_STRING = 254,MYSQL_TYPE_GEOMETRY = 255
         );
       mysql_enum_shutdown_level = (SHUTDOWN_DEFAULT = 0,
         SHUTDOWN_WAIT_CONNECTIONS = 1, //MYSQL_SHUTDOWN_KILLABLE_CONNECT,     // wait for existing connections to finish
         SHUTDOWN_WAIT_TRANSACTIONS = 2, //MYSQL_SHUTDOWN_KILLABLE_TRANS,      // wait for existing trans to finish
         SHUTDOWN_WAIT_UPDATES = 8, //MYSQL_SHUTDOWN_KILLABLE_UPDATE,          // wait for existing updates to finish (=> no partial MyISAM update)
         SHUTDOWN_WAIT_ALL_BUFFERS = 16, //MYSQL_SHUTDOWN_KILLABLE_UPDATE shl 1,// flush InnoDB buffers and other storage engines' buffers
         SHUTDOWN_WAIT_CRITICAL_BUFFERS = 17, //(MYSQL_SHUTDOWN_KILLABLE_UPDATE shl 1)+1, // don't flush InnoDB buffers, flush other storage engines' buffers
         KILL_QUERY = 254,
         KILL_CONNECTION = 255
         );
       enum_mysql_set_option = (MYSQL_OPTION_MULTI_STATEMENTS_ON,
         MYSQL_OPTION_MULTI_STATEMENTS_OFF
         );
       mysql_option = (MYSQL_OPT_CONNECT_TIMEOUT,MYSQL_OPT_COMPRESS,
         MYSQL_OPT_NAMED_PIPE,MYSQL_INIT_COMMAND,
         MYSQL_READ_DEFAULT_FILE,MYSQL_READ_DEFAULT_GROUP,
         MYSQL_SET_CHARSET_DIR,MYSQL_SET_CHARSET_NAME,
         MYSQL_OPT_LOCAL_INFILE,MYSQL_OPT_PROTOCOL,
         MYSQL_SHARED_MEMORY_BASE_NAME,MYSQL_OPT_READ_TIMEOUT,
         MYSQL_OPT_WRITE_TIMEOUT,MYSQL_OPT_USE_RESULT,
         MYSQL_OPT_USE_REMOTE_CONNECTION,MYSQL_OPT_USE_EMBEDDED_CONNECTION,
         MYSQL_OPT_GUESS_CONNECTION,MYSQL_SET_CLIENT_IP,
         MYSQL_SECURE_AUTH
         ,MYSQL_REPORT_DATA_TRUNCATION, MYSQL_OPT_RECONNECT
         );

       Pst_mysql_rows = ^st_mysql_rows;
       st_mysql_rows = record
            next : Pst_mysql_rows;                    // list of rows
            data : MYSQL_ROW;
            length : culong;
         end;
       MYSQL_ROWS = st_mysql_rows;
       PMYSQL_ROWS = ^MYSQL_ROWS;

       PMYSQL_ROW_OFFSET = ^MYSQL_ROW_OFFSET;         // offset to current row
       MYSQL_ROW_OFFSET = MYSQL_ROWS;

       MYSQL_FIELD_OFFSET = cuint;

{$if defined(NO_CLIENT_LONG_LONG)}
       my_ulonglong = culong;
{$elseif defined(mswindows)}
       my_ulonglong = cint64;
{$else}
       my_ulonglong = culonglong;
{$ifend}

       Pst_mysql_field = ^st_mysql_field;
       st_mysql_field = record
            name : PAnsiChar;             // Name of column
            org_name : PAnsiChar;         // Original column name, if an alias
            table : PAnsiChar;            // Table of column if column was a field
            org_table : PAnsiChar;        // Org table name, if table was an alias
            db : PAnsiChar;               // Database for table
            catalog : PAnsiChar;          // Catalog for table
            def : PAnsiChar;              // Default value (set by mysql_list_fields)
            length : culong;          // Width of column (create length)
            max_length : culong;      // Max width for selected set

            name_length : cuint;
            org_name_length : cuint;
            table_length : cuint;
            org_table_length : cuint;
            db_length : cuint;
            catalog_length : cuint;
            def_length : cuint;

            flags : cuint;            // Div flags
            decimals : cuint;         // Number of decimals in field

            charsetnr : cuint;        // Character set
            ftype : enum_field_types; // Type of field. See mysql_com.h for types
         end;
       MYSQL_FIELD = st_mysql_field;
       PMYSQL_FIELD = ^MYSQL_FIELD;


    var
  {$IFDEF FPC}
      mysql_server_init: function (argc:cint; argv:PPchar; groups:PPchar):cint;extdecl;
      mysql_server_end: procedure ;extdecl;
      mysql_library_init: function (argc:cint; argv:PPchar; groups:PPchar):cint;extdecl;
      mysql_library_end: procedure ;extdecl;
      mysql_num_rows: function (res:PMYSQL_RES):my_ulonglong;extdecl;
      mysql_num_fields: function (res:PMYSQL_RES):cuint;extdecl;
      mysql_eof: function (res:PMYSQL_RES):my_bool;extdecl;
      mysql_fetch_field_direct: function (res:PMYSQL_RES; fieldnr:cuint):PMYSQL_FIELD;extdecl;
      mysql_fetch_fields: function (res:PMYSQL_RES):PMYSQL_FIELD;extdecl;
      mysql_row_tell: function (res:PMYSQL_RES):MYSQL_ROW_OFFSET;extdecl;
      mysql_field_tell: function (res:PMYSQL_RES):MYSQL_FIELD_OFFSET;extdecl;
      mysql_field_count: function (mysql:PMYSQL):cuint;extdecl;
      mysql_affected_rows: function (mysql:PMYSQL):my_ulonglong;extdecl;
      mysql_insert_id: function (mysql:PMYSQL):my_ulonglong;extdecl;
      mysql_errno: function (mysql:PMYSQL):cuint;extdecl;
      mysql_error: function (mysql:PMYSQL):PAnsiChar;extdecl;
      mysql_sqlstate: function (mysql:PMYSQL):PAnsiChar;extdecl;
      mysql_warning_count: function (mysql:PMYSQL):cuint;extdecl;
      mysql_info: function (mysql:PMYSQL):PAnsiChar;extdecl;
      mysql_thread_id: function (mysql:PMYSQL):culong;extdecl;
      mysql_character_set_name: function (mysql:PMYSQL):PAnsiChar;extdecl;
      mysql_set_character_set: function (mysql:PMYSQL; csname:PAnsiChar):cint;extdecl;
      mysql_init: function (mysql:PMYSQL):PMYSQL;extdecl;
      mysql_ssl_set: function (mysql:PMYSQL; key:PAnsiChar; cert:PAnsiChar; ca:PAnsiChar; capath:PAnsiChar;
                 cipher:PAnsiChar):my_bool;extdecl;
      mysql_change_user: function (mysql:PMYSQL; user:PAnsiChar; passwd:PAnsiChar; db:PAnsiChar):my_bool;extdecl;
      mysql_real_connect: function (mysql:PMYSQL; host:PAnsiChar; user:PAnsiChar; passwd:PAnsiChar; db:PAnsiChar;
                 port:cuint; unix_socket:PAnsiChar; clientflag:culong):PMYSQL;extdecl;
      mysql_select_db: function (mysql:PMYSQL; db:PAnsiChar):cint;extdecl;
      mysql_query: function (mysql:PMYSQL; q:PAnsiChar):cint;extdecl;
      mysql_send_query: function (mysql:PMYSQL; q:PAnsiChar; length:culong):cint;extdecl;
      mysql_real_query: function (mysql:PMYSQL; q:PAnsiChar; length:culong):cint;extdecl;
      mysql_store_result: function (mysql:PMYSQL):PMYSQL_RES;extdecl;
      mysql_use_result: function (mysql:PMYSQL):PMYSQL_RES;extdecl;

      mysql_shutdown: function (mysql:PMYSQL; shutdown_level:mysql_enum_shutdown_level):cint;extdecl;
      mysql_dump_debug_info: function (mysql:PMYSQL):cint;extdecl;
      mysql_refresh: function (mysql:PMYSQL; refresh_options:cuint):cint;extdecl;
      mysql_kill: function (mysql:PMYSQL; pid:culong):cint;extdecl;
      mysql_set_server_option: function (mysql:PMYSQL; option:enum_mysql_set_option):cint;extdecl;
      mysql_ping: function (mysql:PMYSQL):cint;extdecl;
      mysql_stat: function (mysql:PMYSQL):PAnsiChar;extdecl;
      mysql_get_server_info: function (mysql:PMYSQL):PAnsiChar;extdecl;
      mysql_get_client_info: function :PAnsiChar;extdecl;
      mysql_get_client_version: function :culong;extdecl;
      mysql_get_host_info: function (mysql:PMYSQL):PAnsiChar;extdecl;
      mysql_get_server_version: function (mysql:PMYSQL):culong;extdecl;
      mysql_get_proto_info: function (mysql:PMYSQL):cuint;extdecl;
      mysql_thread_safe:function: cuint;extdecl;

      mysql_list_dbs: function (mysql:PMYSQL; wild:PAnsiChar):PMYSQL_RES;extdecl;


      mysql_list_tables: function (mysql:PMYSQL; wild:PAnsiChar):PMYSQL_RES;extdecl;
      mysql_list_processes: function (mysql:PMYSQL):PMYSQL_RES;extdecl;
      mysql_options: function (mysql:PMYSQL; option:mysql_option; arg:PAnsiChar):cint;extdecl;
      mysql_free_result: procedure (result:PMYSQL_RES);extdecl;
      mysql_data_seek: procedure (result:PMYSQL_RES; offset:my_ulonglong);extdecl;
      mysql_row_seek: function (result:PMYSQL_RES; offset:MYSQL_ROW_OFFSET):MYSQL_ROW_OFFSET;extdecl;
      mysql_field_seek: function (result:PMYSQL_RES; offset:MYSQL_FIELD_OFFSET):MYSQL_FIELD_OFFSET;extdecl;
      mysql_fetch_row: function (result:PMYSQL_RES):MYSQL_ROW;extdecl;
      mysql_fetch_lengths: function (result:PMYSQL_RES):pculong;extdecl;
      mysql_fetch_field: function (result:PMYSQL_RES):PMYSQL_FIELD;extdecl;
      mysql_list_fields: function (mysql:PMYSQL; table:PAnsiChar; wild:PAnsiChar):PMYSQL_RES;extdecl;
      mysql_escape_string: function (fto:PAnsiChar; from:PAnsiChar; from_length:culong):culong;extdecl;
      mysql_hex_string: function (fto:PAnsiChar; from:PAnsiChar; from_length:culong):culong;extdecl;
      mysql_real_escape_string: function (mysql:PMYSQL; fto:PAnsiChar; from:PAnsiChar; length:culong):culong;extdecl;
      mysql_debug: procedure (debug:PAnsiChar);extdecl;

      mysql_rollback: function (mysql:PMYSQL):my_bool;extdecl;
      mysql_autocommit: function (mysql:PMYSQL; auto_mode:my_bool):my_bool;extdecl;
      mysql_commit: function (mysql:PMYSQL):my_bool;extdecl;
      mysql_more_results: function (mysql:PMYSQL):my_bool;extdecl;
      mysql_next_result: function (mysql:PMYSQL):cint;extdecl;
      mysql_close: procedure (sock:PMYSQL);extdecl;

      my_init : function :my_bool;extdecl;
      mysql_thread_init : function :my_bool;extdecl;
      mysql_thread_end : procedure ;extdecl;

    {$ELSE}
      {$IFDEF MSWINDOWS}
      mysql_server_init: function (argc:cint; argv:PPchar; groups:PPchar):cint;stdcall;
      mysql_server_end: procedure ;stdcall;
      mysql_library_init: function (argc:cint; argv:PPchar; groups:PPchar):cint;stdcall;
      mysql_library_end: procedure ;stdcall;
      mysql_num_rows: function (res:PMYSQL_RES):my_ulonglong;stdcall;
      mysql_num_fields: function (res:PMYSQL_RES):cuint;stdcall;
      mysql_eof: function (res:PMYSQL_RES):my_bool;stdcall;
      mysql_fetch_field_direct: function (res:PMYSQL_RES; fieldnr:cuint):PMYSQL_FIELD;stdcall;
      mysql_fetch_fields: function (res:PMYSQL_RES):PMYSQL_FIELD;stdcall;
      mysql_row_tell: function (res:PMYSQL_RES):MYSQL_ROW_OFFSET;stdcall;
      mysql_field_tell: function (res:PMYSQL_RES):MYSQL_FIELD_OFFSET;stdcall;
      mysql_field_count: function (mysql:PMYSQL):cuint;stdcall;
      mysql_affected_rows: function (mysql:PMYSQL):my_ulonglong;stdcall;
      mysql_insert_id: function (mysql:PMYSQL):my_ulonglong;stdcall;
      mysql_errno: function (mysql:PMYSQL):cuint;stdcall;
      mysql_error: function (mysql:PMYSQL):PAnsiChar;stdcall;
      mysql_sqlstate: function (mysql:PMYSQL):PAnsiChar;stdcall;
      mysql_warning_count: function (mysql:PMYSQL):cuint;stdcall;
      mysql_info: function (mysql:PMYSQL):PAnsiChar;stdcall;
      mysql_thread_id: function (mysql:PMYSQL):culong;stdcall;
      mysql_character_set_name: function (mysql:PMYSQL):PAnsiChar;stdcall;
      mysql_set_character_set: function (mysql:PMYSQL; csname:PAnsiChar):cint;stdcall;
      mysql_init: function (mysql:PMYSQL):PMYSQL;stdcall;
      mysql_ssl_set: function (mysql:PMYSQL; key:PAnsiChar; cert:PAnsiChar; ca:PAnsiChar; capath:PAnsiChar;
                 cipher:PAnsiChar):my_bool;stdcall;
      mysql_change_user: function (mysql:PMYSQL; user:PAnsiChar; passwd:PAnsiChar; db:PAnsiChar):my_bool;stdcall;
      mysql_real_connect: function (mysql:PMYSQL; host:PAnsiChar; user:PAnsiChar; passwd:PAnsiChar; db:PAnsiChar;
                 port:cuint; unix_socket:PAnsiChar; clientflag:culong):PMYSQL;stdcall;
      mysql_select_db: function (mysql:PMYSQL; db:PAnsiChar):cint;stdcall;
      mysql_query: function (mysql:PMYSQL; q:PAnsiChar):cint;stdcall;
      mysql_send_query: function (mysql:PMYSQL; q:PAnsiChar; length:culong):cint;stdcall;
      mysql_real_query: function (mysql:PMYSQL; q:PAnsiChar; length:culong):cint;stdcall;
      mysql_store_result: function (mysql:PMYSQL):PMYSQL_RES;stdcall;
      mysql_use_result: function (mysql:PMYSQL):PMYSQL_RES;stdcall;

      mysql_shutdown: function (mysql:PMYSQL; shutdown_level:mysql_enum_shutdown_level):cint;stdcall;
      mysql_dump_debug_info: function (mysql:PMYSQL):cint;stdcall;
      mysql_refresh: function (mysql:PMYSQL; refresh_options:cuint):cint;stdcall;
      mysql_kill: function (mysql:PMYSQL; pid:culong):cint;stdcall;
      mysql_set_server_option: function (mysql:PMYSQL; option:enum_mysql_set_option):cint;stdcall;
      mysql_ping: function (mysql:PMYSQL):cint;stdcall;
      mysql_stat: function (mysql:PMYSQL):PAnsiChar;stdcall;
      mysql_get_server_info: function (mysql:PMYSQL):PAnsiChar;stdcall;
      mysql_get_client_info: function :PAnsiChar;stdcall;
      mysql_get_client_version: function :culong;stdcall;
      mysql_get_host_info: function (mysql:PMYSQL):PAnsiChar;stdcall;
      mysql_get_server_version: function (mysql:PMYSQL):culong;stdcall;
      mysql_get_proto_info: function (mysql:PMYSQL):cuint;stdcall;
      mysql_thread_safe:function: cuint;stdcall;
      mysql_list_dbs: function (mysql:PMYSQL; wild:PAnsiChar):PMYSQL_RES;stdcall;

      mysql_list_tables: function (mysql:PMYSQL; wild:PAnsiChar):PMYSQL_RES;stdcall;
      mysql_list_processes: function (mysql:PMYSQL):PMYSQL_RES;stdcall;
      mysql_options: function (mysql:PMYSQL; option:mysql_option; arg:PAnsiChar):cint;stdcall;
      mysql_free_result: procedure (result:PMYSQL_RES);stdcall;
      mysql_data_seek: procedure (result:PMYSQL_RES; offset:my_ulonglong);stdcall;
      mysql_row_seek: function (result:PMYSQL_RES; offset:MYSQL_ROW_OFFSET):MYSQL_ROW_OFFSET;stdcall;
      mysql_field_seek: function (result:PMYSQL_RES; offset:MYSQL_FIELD_OFFSET):MYSQL_FIELD_OFFSET;stdcall;
      mysql_fetch_row: function (result:PMYSQL_RES):MYSQL_ROW;stdcall;
      mysql_fetch_lengths: function (result:PMYSQL_RES):pculong;stdcall;
      mysql_fetch_field: function (result:PMYSQL_RES):PMYSQL_FIELD;stdcall;
      mysql_list_fields: function (mysql:PMYSQL; table:PAnsiChar; wild:PAnsiChar):PMYSQL_RES;stdcall;
      mysql_escape_string: function (fto:PAnsiChar; from:PAnsiChar; from_length:culong):culong;stdcall;
      mysql_hex_string: function (fto:PAnsiChar; from:PAnsiChar; from_length:culong):culong;stdcall;
      mysql_real_escape_string: function (mysql:PMYSQL; fto:PAnsiChar; from:PAnsiChar; length:culong):culong;stdcall;
      mysql_debug: procedure (debug:PAnsiChar);stdcall;

      mysql_rollback: function (mysql:PMYSQL):my_bool;stdcall;
      mysql_autocommit: function (mysql:PMYSQL; auto_mode:my_bool):my_bool;stdcall;
      mysql_commit: function (mysql:PMYSQL):my_bool;stdcall;
      mysql_more_results: function (mysql:PMYSQL):my_bool;stdcall;
      mysql_next_result: function (mysql:PMYSQL):cint;stdcall;
      mysql_close: procedure (sock:PMYSQL);stdcall;

      my_init : function :my_bool;stdcall;
      mysql_thread_init : function :my_bool;stdcall;
      mysql_thread_end : procedure ;stdcall;

      {$ELSE}
      mysql_server_init: function (argc:cint; argv:PPchar; groups:PPchar):cint;cdecl;
      mysql_server_end: procedure ;cdecl;
      mysql_library_init: function (argc:cint; argv:PPchar; groups:PPchar):cint;cdecl;
      mysql_library_end: procedure ;cdecl;
      mysql_num_rows: function (res:PMYSQL_RES):my_ulonglong;cdecl;
      mysql_num_fields: function (res:PMYSQL_RES):cuint;cdecl;
      mysql_eof: function (res:PMYSQL_RES):my_bool;cdecl;
      mysql_fetch_field_direct: function (res:PMYSQL_RES; fieldnr:cuint):PMYSQL_FIELD;cdecl;
      mysql_fetch_fields: function (res:PMYSQL_RES):PMYSQL_FIELD;cdecl;
      mysql_row_tell: function (res:PMYSQL_RES):MYSQL_ROW_OFFSET;cdecl;
      mysql_field_tell: function (res:PMYSQL_RES):MYSQL_FIELD_OFFSET;cdecl;
      mysql_field_count: function (mysql:PMYSQL):cuint;cdecl;
      mysql_affected_rows: function (mysql:PMYSQL):my_ulonglong;cdecl;
      mysql_insert_id: function (mysql:PMYSQL):my_ulonglong;cdecl;
      mysql_errno: function (mysql:PMYSQL):cuint;cdecl;
      mysql_error: function (mysql:PMYSQL):PAnsiChar;cdecl;
      mysql_sqlstate: function (mysql:PMYSQL):PAnsiChar;cdecl;
      mysql_warning_count: function (mysql:PMYSQL):cuint;cdecl;
      mysql_info: function (mysql:PMYSQL):PAnsiChar;cdecl;
      mysql_thread_id: function (mysql:PMYSQL):culong;cdecl;
      mysql_character_set_name: function (mysql:PMYSQL):PAnsiChar;cdecl;
      mysql_set_character_set: function (mysql:PMYSQL; csname:PAnsiChar):cint;cdecl;
      mysql_init: function (mysql:PMYSQL):PMYSQL;cdecl;
      mysql_ssl_set: function (mysql:PMYSQL; key:PAnsiChar; cert:PAnsiChar; ca:PAnsiChar; capath:PAnsiChar;
                 cipher:PAnsiChar):my_bool;cdecl;
      mysql_change_user: function (mysql:PMYSQL; user:PAnsiChar; passwd:PAnsiChar; db:PAnsiChar):my_bool;cdecl;
      mysql_real_connect: function (mysql:PMYSQL; host:PAnsiChar; user:PAnsiChar; passwd:PAnsiChar; db:PAnsiChar;
                 port:cuint; unix_socket:PAnsiChar; clientflag:culong):PMYSQL;cdecl;
      mysql_select_db: function (mysql:PMYSQL; db:PAnsiChar):cint;cdecl;
      mysql_query: function (mysql:PMYSQL; q:PAnsiChar):cint;cdecl;
      mysql_send_query: function (mysql:PMYSQL; q:PAnsiChar; length:culong):cint;cdecl;
      mysql_real_query: function (mysql:PMYSQL; q:PAnsiChar; length:culong):cint;cdecl;
      mysql_store_result: function (mysql:PMYSQL):PMYSQL_RES;cdecl;
      mysql_use_result: function (mysql:PMYSQL):PMYSQL_RES;cdecl;

      mysql_shutdown: function (mysql:PMYSQL; shutdown_level:mysql_enum_shutdown_level):cint;cdecl;
      mysql_dump_debug_info: function (mysql:PMYSQL):cint;cdecl;
      mysql_refresh: function (mysql:PMYSQL; refresh_options:cuint):cint;cdecl;
      mysql_kill: function (mysql:PMYSQL; pid:culong):cint;cdecl;
      mysql_set_server_option: function (mysql:PMYSQL; option:enum_mysql_set_option):cint;cdecl;
      mysql_ping: function (mysql:PMYSQL):cint;cdecl;
      mysql_stat: function (mysql:PMYSQL):PAnsiChar;cdecl;
      mysql_get_server_info: function (mysql:PMYSQL):PAnsiChar;cdecl;
      mysql_get_client_info: function :PAnsiChar;cdecl;
      mysql_get_client_version: function :culong;cdecl;
      mysql_get_host_info: function (mysql:PMYSQL):PAnsiChar;cdecl;
      mysql_get_server_version: function (mysql:PMYSQL):culong;cdecl;
      mysql_get_proto_info: function (mysql:PMYSQL):cuint;cdecl;
      mysql_thread_safe:function: cuint;cdecl;
      mysql_list_dbs: function (mysql:PMYSQL; wild:PAnsiChar):PMYSQL_RES;cdecl;

      mysql_list_tables: function (mysql:PMYSQL; wild:PAnsiChar):PMYSQL_RES;cdecl;
      mysql_list_processes: function (mysql:PMYSQL):PMYSQL_RES;cdecl;
      mysql_options: function (mysql:PMYSQL; option:mysql_option; arg:PAnsiChar):cint;cdecl;
      mysql_free_result: procedure (result:PMYSQL_RES);cdecl;
      mysql_data_seek: procedure (result:PMYSQL_RES; offset:my_ulonglong);cdecl;
      mysql_row_seek: function (result:PMYSQL_RES; offset:MYSQL_ROW_OFFSET):MYSQL_ROW_OFFSET;cdecl;
      mysql_field_seek: function (result:PMYSQL_RES; offset:MYSQL_FIELD_OFFSET):MYSQL_FIELD_OFFSET;cdecl;
      mysql_fetch_row: function (result:PMYSQL_RES):MYSQL_ROW;cdecl;
      mysql_fetch_lengths: function (result:PMYSQL_RES):pculong;cdecl;
      mysql_fetch_field: function (result:PMYSQL_RES):PMYSQL_FIELD;cdecl;
      mysql_list_fields: function (mysql:PMYSQL; table:PAnsiChar; wild:PAnsiChar):PMYSQL_RES;cdecl;
      mysql_escape_string: function (fto:PAnsiChar; from:PAnsiChar; from_length:culong):culong;cdecl;
      mysql_hex_string: function (fto:PAnsiChar; from:PAnsiChar; from_length:culong):culong;cdecl;
      mysql_real_escape_string: function (mysql:PMYSQL; fto:PAnsiChar; from:PAnsiChar; length:culong):culong;cdecl;
      mysql_debug: procedure (debug:PAnsiChar);cdecl;

      mysql_rollback: function (mysql:PMYSQL):my_bool;cdecl;
      mysql_autocommit: function (mysql:PMYSQL; auto_mode:my_bool):my_bool;cdecl;
      mysql_commit: function (mysql:PMYSQL):my_bool;cdecl;
      mysql_more_results: function (mysql:PMYSQL):my_bool;cdecl;
      mysql_next_result: function (mysql:PMYSQL):cint;cdecl;
      mysql_close: procedure (sock:PMYSQL);cdecl;

      my_init : function :my_bool;cdecl;
      mysql_thread_init : function :my_bool;cdecl;
      mysql_thread_end : procedure ;cdecl;

      {$ENDIF}
    {$ENDIF}


function mqwo(m:PMYSQL; q: AnsiString): Boolean; overload;
function mqwo(m:PMYSQL; q: AnsiString; args: array of const): Boolean; overload;
function mygetrow(r: PMYSQL_RES; var res: TStringDynArray): Boolean;
function myquery(m:PMYSQL; q: AnsiString): PMYSQL_RES; overload;
function myquery(m:PMYSQL; q: AnsiString; args: array of const): PMYSQL_RES; overload;
function gcaa(m:PMYSQL; q: AnsiString; args: array of const; var res: TStringDynArray): Boolean; overload;
function gcaa(m:PMYSQL; q: AnsiString; args: array of const; res: TStringList): Boolean; overload;
function gc(m:PMYSQL; q: AnsiString; args: array of const): AnsiString;
function slmysql_info: AnsiString;
function slmysql_error(m: PMYSQL): AnsiString;

Function InitialiseMysql(Const LibraryName : AnsiString) : Boolean; overload;
Function InitialiseMysql : Boolean; overload;
Procedure ReleaseMysql;

var
  MysqlLibraryHandle : Integer = 0;
  slmysql_liberror : AnsiString = '';
  slmysql_LoadedLibrary : AnsiString = '';




implementation

uses
  VarRecUtils,
  SysUtils,
{$IFDEF FPC}
  dynlibs
{$ELSE}
  {$IFDEF MSWINDOWS}
  Windows
  {$ELSE}
  Libc
  {$ENDIF}
{$ENDIF}
  , debugunit
;

var
  RefCount : integer;

function slmysql_LoadProc(handle: Integer; const fnName: AnsiString; var fn: Pointer): Boolean;
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
    slmysql_liberror:= 'Cannot load '+fnName
  end
  else
    Result:= True;
end;

Function TryInitialiseMysql(Const LibraryName : AnsiString) : Integer;
begin
  Result := 0;

  if (RefCount=0) then
    begin
    {$IFDEF FPC}
    if (MysqlLibraryHandle = 0) then MysqlLibraryHandle := loadlibrary(PChar(ExtractFilePath(ParamStr(0))+LibraryName));
    if (MysqlLibraryHandle = 0) then MysqlLibraryHandle := loadlibrary(PChar(LibraryName));
    if (MysqlLibraryHandle = 0) then Exit;
    {$ELSE}
      {$IFDEF LINUX}
        if (MysqlLibraryHandle = 0) then MysqlLibraryHandle := loadlibrary(PChar(ExtractFilePath(ParamStr(0))+LibraryName));
        if (MysqlLibraryHandle = 0) then MysqlLibraryHandle := loadlibrary(PChar(LibraryName));
        if (MysqlLibraryHandle = 0) then Exit;
      {$ELSE}
        if (MysqlLibraryHandle = 0) then MysqlLibraryHandle := loadlibrary(PChar(LibraryName));
        if (MysqlLibraryHandle = 0) then MysqlLibraryHandle := loadlibrary(PChar(ExtractFilePath(ParamStr(0))+LibraryName));
        if (MysqlLibraryHandle = 0) then Exit;
      {$ENDIF}
    {$ENDIF}

    slmysql_LoadedLibrary:=LibraryName;
// Only the procedure that are given in the c-library documentation are loaded, to
// avoid problems with 'incomplete' libraries

    if not slmysql_LoadProc(MysqlLibraryHandle, 'my_init', @my_init) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_thread_init', @mysql_thread_init) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_thread_end', @mysql_thread_end) then exit;

    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_affected_rows', @mysql_affected_rows) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_autocommit', @mysql_autocommit) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_change_user', @mysql_change_user) then exit;
//    pointer(mysql_charset_name) := GetProcedureAddress(MysqlLibraryHandle,'mysql_charset_name');
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_close', @mysql_close) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_commit', @mysql_commit) then exit;
//    pointer(mysql_connect) := GetProcedureAddress(MysqlLibraryHandle,'mysql_connect');
//    pointer(mysql_create_db) := GetProcedureAddress(MysqlLibraryHandle,'mysql_create_db');
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_data_seek', @mysql_data_seek) then exit;
//    pointer(mysql_drop_db) := GetProcedureAddress(MysqlLibraryHandle,'mysql_drop_db');
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_debug', @mysql_debug) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_dump_debug_info', @mysql_dump_debug_info) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_eof', @mysql_eof) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_errno', @mysql_errno) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_error', @mysql_error) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_escape_string', @mysql_escape_string) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_fetch_field', @mysql_fetch_field) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_fetch_field_direct', @mysql_fetch_field_direct) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_fetch_fields', @mysql_fetch_fields) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_fetch_lengths', @mysql_fetch_lengths) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_fetch_row', @mysql_fetch_row) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_field_seek', @mysql_field_seek) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_field_count', @mysql_field_count) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_field_tell', @mysql_field_tell) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_free_result', @mysql_free_result) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_get_client_info', @mysql_get_client_info) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_get_client_version', @mysql_get_client_version) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_get_host_info', @mysql_get_host_info) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_get_server_version', @mysql_get_server_version) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_get_proto_info', @mysql_get_proto_info) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_get_server_info', @mysql_get_server_info) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_thread_safe', @mysql_thread_safe) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_info', @mysql_info) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_init', @mysql_init) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_insert_id', @mysql_insert_id) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_kill', @mysql_kill) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_list_dbs', @mysql_list_dbs) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_list_fields', @mysql_list_fields) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_list_processes', @mysql_list_processes) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_list_tables', @mysql_list_tables) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_more_results', @mysql_more_results) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_next_result', @mysql_next_result) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_num_fields', @mysql_num_fields) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_num_rows', @mysql_num_rows) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_options', @mysql_options) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_ping', @mysql_ping) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_query', @mysql_query) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_real_connect', @mysql_real_connect) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_real_escape_string', @mysql_real_escape_string) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_real_query', @mysql_real_query) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_refresh', @mysql_refresh) then exit;
//    pointer(mysql_reload) := GetProcedureAddress(MysqlLibraryHandle,'mysql_reload');
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_rollback', @mysql_rollback) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_row_seek', @mysql_row_seek) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_row_tell', @mysql_row_tell) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_select_db', @mysql_select_db) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_server_end', @mysql_server_end) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_server_init', @mysql_server_init) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_set_server_option', @mysql_set_server_option) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_sqlstate', @mysql_sqlstate) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_shutdown', @mysql_shutdown) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_stat', @mysql_stat) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_store_result', @mysql_store_result) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_thread_id', @mysql_thread_id) then exit;
//    pointer(mysql_thread_save) := GetProcedureAddress(MysqlLibraryHandle,'mysql_thread_save');
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_use_result', @mysql_use_result) then exit;
    if not slmysql_LoadProc(MysqlLibraryHandle, 'mysql_warning_count', @mysql_warning_count) then exit;

    slmysql_LoadProc(MysqlLibraryHandle, 'mysql_library_init', @mysql_library_init);
    slmysql_LoadProc(MysqlLibraryHandle, 'mysql_library_end', @mysql_library_end);

    if Assigned(mysql_library_init) then
      mysql_library_init(0, nil, nil)
    else
    if Assigned(mysql_server_init) then
      mysql_server_init(0, nil, nil);

    Inc(RefCount);
  end
  else
    inc(RefCount);
  Result:=RefCount;
end;


Function InitialiseMysql(Const LibraryName : AnsiString) : Boolean;
begin
  Result := False;
  if (TryInitialiseMysql(LibraryName) <> 0) then
    Result := True;
end;
Function InitialiseMysql : Boolean;
begin
  Result:= InitialiseMysql(mysqlvlib);
end;


Procedure ReleaseMysql;
begin
  if RefCount> 1 then
    Dec(RefCount)
  else
  begin
    if Assigned(mysql_library_end) then
      mysql_library_end()
    else
    if Assigned(mysql_server_end) then
      mysql_server_end();

    if FreeLibrary(MysqlLibraryHandle) then
    begin
      Dec(RefCount);
      MysqlLibraryHandle := 0;
      slmysql_LoadedLibrary:='';
    end;
  end;
end;


function mqwo(m:PMYSQL; q: AnsiString): Boolean; overload;
var mysql_err: AnsiString;
begin
  Result:= 0 = mysql_query(m, PAnsiChar(q));
  mysql_err:= mysql_error(m);
  if mysql_err <> '' then
  begin
    Debug(dpError, 'slmysql2', Format('[MySQL] mqwo: %s - %s', [q, mysql_err]));
  end;
end;

function mqwo(m:PMYSQL; q: AnsiString; args: array of const): Boolean; overload;
var newargs: TConstArray;
    i, j: Integer;
    s2, s: AnsiString;
begin
  SetLength(newargs, Length(args));
  try
    for i:= low(args) to high(args) do
    begin
      case Args[i].VType of
        vtString:
          begin
            s2:= args[i].VString^;
            j:= length(s2);
            SetLength(s, j*2+1);
            j:= mysql_real_escape_string(m, PAnsiChar(s), PAnsiChar(s2), j);
            newargs[i]:= CreateVarRecFromString(Copy(s, 1, j));
          end;
        vtAnsiString:
        begin
            s2:= AnsiString(args[i].VAnsiString);
            j:= length(s2);
            SetLength(s, j*2+1);
            j:= mysql_real_escape_string(m, PAnsiChar(s), PAnsiChar(s2), j);
            newargs[i]:= CreateVarRecFromString(Copy(s, 1, j));
        end;
        vtWideString:
          begin
            s2:= WideCharToString(args[i].VWideString);
            j:= length(s2);
            SetLength(s, j*2+1);
            j:= mysql_real_escape_string(m, PAnsiChar(s), PAnsiChar(s2), j);
            newargs[i]:= CreateVarRecFromString(Copy(s, 1, j));
          end;
        vtChar:
          begin
            SetLength(s, 3);
            s2:= args[i].VChar;
            j:= mysql_real_escape_string(m, PAnsiChar(s), PAnsiChar(s2), 1);
            newargs[i]:= CreateVarRecFromString(Copy(s, 1, j));
          end;
      else
        newargs[i]:= CopyVarRec(args[i]);
      end;
    end;
    Result:= mqwo(m, Format(q, newargs));
  finally
    FinalizeConstArray(newargs);
  end;
end;

function myquery(m:PMYSQL; q: AnsiString): PMYSQL_RES;
begin
  Result:= myquery(m, q, []);
end;
function myquery(m:PMYSQL; q: AnsiString; args: array of const): PMYSQL_RES;
begin
  Result:= nil;
  if not mqwo(m, q, args) then
    exit;
  Result:= mysql_store_result(m);
end;

function mygetrow(r: PMYSQL_RES; var res: TStringDynArray): Boolean;
var i, j: Integer;
    row: array of PAnsiChar;
    p: PPChar;
begin
  Pointer(row):= nil;
  Result:= False;

  p:= mysql_fetch_row(r);
  if p = nil then
    exit;

  j:=  mysql_num_fields(r);
  if j > 0 then
  begin
    SetLength(res, j);
    Pointer(row):= p;
    for i:= 0 to j -1 do
      res[i]:= StrPas(row[i]);
  end;

  Pointer(row):= nil;
  Result:= True;
end;
function gcaa(m:PMYSQL; q: AnsiString; args: array of const; var res: TStringDynArray): Boolean;
var r: PMYSQL_RES;
begin
  Result:= False;
  try
    SetLength(res, 0);
    r:= myquery(m, q, args);
    if r = nil then exit;
    Result:= True;

    mygetrow(r, res);

    mysql_free_result(r);
  except
    on E: Exception do
    begin
      Debug(dpError, 'slmysql2', Format('[EXCEPTION] gcaa : %s %s', [e.Message, q]));
      SetLength(res, 0);
      Result:= False;
    end;
  end;
end;

function gcaa(m:PMYSQL; q: AnsiString; args: array of const; res: TStringList): Boolean;
var r: PMYSQL_RES;
    re: TStringDynArray;
    mf: PMYSQL_FIELD;
    i: Integer;
begin
  Result:= False;
  try
    SetLength(re,0);
    res.Clear;
    r:= myquery(m, q, args);
    if r = nil then exit;
    Result:= True;

    mygetrow(r, re);

    mf:= mysql_fetch_fields(r);
    for i:= low(re) to high(re) do
    begin
      res.Values[ StrPas(mf^.name) ]:= re[i];
      inc(mf);
    end;

    mysql_free_result(r);
  except
    on E: Exception do
    begin
      Debug(dpError, 'slmysql2', Format('[EXCEPTION] gcaa : %s %s', [e.Message, q]));
      SetLength(re, 0);
      Result:= False;
    end;
  end;
end;


function gc(m:PMYSQL; q: AnsiString; args: array of const): AnsiString;
var row: TStringDynArray;
begin
  Result:= '';
  try
    gcaa(m, q, args, row);
    if length(row) > 0 then
      Result:= row[0]
  except
    on E: Exception do
    begin
      Debug(dpError, 'slmysql2', Format('[EXCEPTION] gc : %s %s', [e.Message, q]));
      Result:= '';
    end;
  end;
end;

function slmysql_info: AnsiString;
begin
  Result:= StrPas(mysql_get_client_info);
end;
function slmysql_error(m: PMYSQL): AnsiString;
begin
  Result:= StrPas(mysql_error(m));
end;


end.
