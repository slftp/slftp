unit mypthread;

interface

{$IFDEF MSWINDOWS}

const
  libpthreadmodulename = 'pthreadVC2.dll';


//!! Moved from below (time.h) to resolve dependency
{ POSIX.4 structure for a time value.  This is like a `struct timeval' but
   has nanoseconds instead of microseconds.  }
type
  timespec = {packed} record
    tv_sec: Longword;            { Seconds.  }
    tv_nsec: Longword;           { Nanoseconds.  }
  end;
  {$EXTERNALSYM timespec}
  TTimeSpec = timespec;
  {$EXTERNALSYM TTimeSpec}
  PTimeSpec = ^TTimeSpec;
  {$EXTERNALSYM PTimeSpec}

{ Fast locks (not abstract because mutexes and conditions aren't abstract). }
  _pthread_fastlock = {packed} record
    __status: Longint;          { "Free" or "taken" or head of waiting list }
    __spinlock: Integer;        { For compare-and-swap emulation }
  end;
  {$EXTERNALSYM _pthread_fastlock}
  TPthreadFastlock = _pthread_fastlock;
  {$EXTERNALSYM TPthreadFastlock}
  PPthreadFastlock = ^TPthreadFastlock;
  {$EXTERNALSYM PPthreadFastlock}

  _pthread_descr = Pointer;
  {$EXTERNALSYM _pthread_descr}
  
{ Conditions (not abstract because of PTHREAD_COND_INITIALIZER }
  pthread_cond_t = {packed} record
    __c_lock: _pthread_fastlock;     { Protect against concurrent access }
    __c_waiting: _pthread_descr;     { Threads waiting on this condition }
  end;
  {$EXTERNALSYM pthread_cond_t}
  TCondVar = pthread_cond_t;
  {$EXTERNALSYM TCondVar}
  PCondVar = ^TCondVar;
  {$EXTERNALSYM PCondVar}
  

{ Mutexes (not abstract because of PTHREAD_MUTEX_INITIALIZER).  }
{ (The layout is unnatural to maintain binary compatibility
    with earlier releases of LinuxThreads.) }
  TPthreadMutex = {packed} record
    __m_reserved: Integer;        { Reserved for future use }
    __m_count: Integer;           { Depth of recursive locking }
    __m_owner: _pthread_descr;    { Owner thread (if recursive or errcheck) }
    __m_kind: Integer;            { Mutex kind: fast, recursive or errcheck }
    __m_lock: _pthread_fastlock;     { Underlying fast lock }
  end;
  {$EXTERNALSYM TPthreadMutex}
  pthread_mutex_t = TPthreadMutex;
  {$EXTERNALSYM pthread_mutex_t}
  TRTLCriticalSection = TPthreadMutex;
  PRTLCriticalSection = ^TRTLCriticalSection;
  {$EXTERNALSYM PRTLCriticalSection}

{ Attribute for conditionally variables.  }
  pthread_condattr_t = {packed} record
    __dummy: Integer;
  end;
  {$EXTERNALSYM pthread_condattr_t}
  TPthreadCondattr = pthread_condattr_t;
  {$EXTERNALSYM TPthreadCondattr}
  PPthreadCondattr = ^TPthreadCondattr;
  {$EXTERNALSYM PPthreadCondattr}

{ Attribute for mutex.  }
  pthread_mutexattr_t = {packed} record
    __mutexkind: Integer;
  end;
  {$EXTERNALSYM pthread_mutexattr_t}
  TMutexAttribute = pthread_mutexattr_t;
  {$EXTERNALSYM TMutexAttribute}
  PMutexAttribute = ^TMutexAttribute;
  {$EXTERNALSYM PMutexAttribute}
  
{ Wait for condition variable COND to be signaled or broadcast until
   ABSTIME.  MUTEX is assumed to be locked before.  ABSTIME is an
   absolute time specification; zero is the beginning of the epoch
   (00:00:00 GMT, January 1, 1970). }
function pthread_cond_timedwait(var Cond: TCondVar;
  var Mutex: TRTLCriticalSection; const AbsTime: TTimeSpec): Integer; cdecl;
{$EXTERNALSYM pthread_cond_timedwait}
{ Wake up one thread waiting for condition variable COND.  }
function pthread_cond_signal(var Cond: TCondVar): Integer; cdecl;
{$EXTERNALSYM pthread_cond_signal}
{ Initialize condition variable COND using attributes ATTR, or use
   the default values if later is NULL.  }
function pthread_cond_init(var Cond: TCondVar;
  var CondAttr: TPthreadCondattr): Integer; cdecl; overload;
function pthread_cond_init(var Cond: TCondVar;
  CondAttr: PPthreadCondattr): Integer; cdecl; overload;
{$EXTERNALSYM pthread_cond_init}

{ Destroy condition variable COND.  }
function pthread_cond_destroy(var Cond: TCondVar): Integer; cdecl;
{$EXTERNALSYM pthread_cond_destroy}
{ Wait until lock for MUTEX becomes available and lock it.  }
function pthread_mutex_lock(var Mutex: TRTLCriticalSection): Integer; cdecl;
{$EXTERNALSYM pthread_mutex_lock}

{ Unlock MUTEX.  }
function pthread_mutex_unlock(var Mutex: TRTLCriticalSection): Integer; cdecl;
{$EXTERNALSYM pthread_mutex_unlock}

{ Destroy MUTEX.  }
function pthread_mutex_destroy(var Mutex: TRTLCriticalSection): Integer; cdecl;
{$EXTERNALSYM pthread_mutex_destroy}
{ Initialize MUTEX using attributes in *MUTEX_ATTR, or use the
   default values if later is NULL.  }
function pthread_mutex_init(var Mutex: TRTLCriticalSection;
  var Attr: TMutexAttribute): Integer; cdecl; overload;
function pthread_mutex_init(var Mutex: TRTLCriticalSection;
  Attr: PMutexAttribute): Integer; cdecl; overload;
{$EXTERNALSYM pthread_mutex_init}
{ Wait for condition variable COND to be signaled or broadcast.
   MUTEX is assumed to be locked before.  }
function pthread_cond_wait(var Cond: TCondVar;
  var Mutex: TRTLCriticalSection): Integer; cdecl;
{$EXTERNALSYM pthread_cond_wait}


procedure PThreadTest;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses Classes, SysUtils, DateUtils;

type
  TMyThread=class(TTHread)
    procedure Execute; override;
  end;

var m: TPthreadMutex;
    c: TCondVar;


(* We provide a wrapper around pthread_cond_timedwait
function pthread_cond_timedwait;        external libpthreadmodulename name 'pthread_cond_timedwait';
*)
function pthread_cond_destroy;          external libpthreadmodulename name 'pthread_cond_destroy';
function pthread_cond_init(var Cond: TCondVar; var CondAttr: TPthreadCondattr): Integer; external libpthreadmodulename name 'pthread_cond_init';
function pthread_cond_init(var Cond: TCondVar; CondAttr: PPthreadCondattr): Integer; external libpthreadmodulename name 'pthread_cond_init';
function pthread_cond_signal;           external libpthreadmodulename name 'pthread_cond_signal';
function pthread_mutex_destroy;         external libpthreadmodulename name 'pthread_mutex_destroy';
function pthread_mutex_init(var Mutex: TRTLCriticalSection; var Attr: TMutexAttribute): Integer; external libpthreadmodulename name 'pthread_mutex_init';
function pthread_mutex_init(var Mutex: TRTLCriticalSection; Attr: PMutexAttribute): Integer; external libpthreadmodulename name 'pthread_mutex_init';
function pthread_mutex_lock;            external libpthreadmodulename name 'pthread_mutex_lock';
function pthread_mutex_unlock;          external libpthreadmodulename name 'pthread_mutex_unlock';
function pthread_cond_wait;             external libpthreadmodulename name 'pthread_cond_wait';

function real_pthread_cond_timedwait(var Cond: TCondVar;
  var Mutex: TRTLCriticalSection; const AbsTime: TTimeSpec): Integer; cdecl;
  external libpthreadmodulename name 'pthread_cond_timedwait';

{ Because of an oddity in the implementation of pthread_cond_timedwait
  which modifies the FPU control word, protect the FPU control
  against any changes. }
function pthread_cond_timedwait(var Cond: TCondVar;
  var Mutex: TRTLCriticalSection; const AbsTime: TTimeSpec): Integer; cdecl;
var
  FpuCW: Word;
begin
  FpuCW := Get8087CW;
  try
    Result := real_pthread_cond_timedwait(Cond, Mutex, AbsTime);
  finally
    Set8087CW(FpuCW);
  end;
end;


procedure TMyThread.Execute;
var s: TTimeSpec;
begin
WriteLn(TimeToStr(Now)+' elotte');
    pthread_mutex_lock(m);
    s.tv_sec := DateTimeToUnix(now)+2;
    s.tv_nsec := 0;
    pthread_cond_timedwait(c, m, s);
WriteLn(TimeToStr(Now)+' utana1');
    pthread_mutex_unlock(m);
WriteLn(TimeToStr(Now)+' utana2');
end;

procedure PThreadTest;
begin
pthread_mutex_init(m, nil);
pthread_cond_init(c, nil);
TMyThread.Create(False);
Sleep(5000);
WriteLn(TimeToStr(Now)+' signal sent');
pthread_mutex_lock(m);
pthread_cond_signal(c);
pthread_mutex_unlock(m);
sleep(2000);
pthread_mutex_destroy(m);
pthread_cond_destroy(c);
end;
{$ENDIF}

end.
