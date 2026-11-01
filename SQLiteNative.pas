unit SQLiteNative;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils;
const SQLITE_OK=0; SQLITE_ROW=100; SQLITE_DONE=101;
type
  TUTF8FileStream = class(THandleStream)
  public
    constructor Create(const FileName: string; Writing: Boolean);
    destructor Destroy; override;
  end;
  ESQLiteError = class(Exception)
    Code: Integer;
    constructor CreateCode(ACode: Integer; const Detail: string);
  end;
  TSQLiteDB = class
  private
    FHandle: Pointer;
    FDeadline: QWord;
    procedure Check(Code: Integer);
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure Exec(const SQL: string);
    function Prepare(const SQL: string): Pointer;
    function Next(Stmt: Pointer): Boolean;
    procedure Done(Stmt: Pointer);
    procedure BindText(Stmt: Pointer; Index: Integer; const Value: string);
    function Text(Stmt: Pointer; Column: Integer): string;
    function Number(Stmt: Pointer; Column: Integer): Int64;
    function Scalar(const SQL: string): string;
    function LastID: Int64;
  end;
function UTF8PathExists(const Path: string): Boolean;
procedure EnsureUTF8Directory(const Path: string);
function DeleteUTF8File(const Path: string): Boolean;
function SQLiteVersion: string;
implementation
uses Windows;
{$L sqlite3.obj}
{$linklib libmingwex.a}
{$linklib libgcc.a}
{$linklib libkernel32_sqlite.a}
{$linklib libmsvcrt_sqlite.a}

function sqlite3_open_v2(Name:PAnsiChar;var DB:Pointer;Flags:LongInt;VFS:PAnsiChar):LongInt;cdecl;external;
function sqlite3_close(DB:Pointer):LongInt;cdecl;external;
function sqlite3_errmsg(DB:Pointer):PAnsiChar;cdecl;external;
function sqlite3_exec(DB:Pointer;SQL:PAnsiChar;Callback,Arg,Err:Pointer):LongInt;cdecl;external;
function sqlite3_prepare_v2(DB:Pointer;SQL:PAnsiChar;Count:LongInt;var Stmt:Pointer;Tail:Pointer):LongInt;cdecl;external;
function sqlite3_step(Stmt:Pointer):LongInt;cdecl;external;
function sqlite3_finalize(Stmt:Pointer):LongInt;cdecl;external;
function sqlite3_bind_text(Stmt:Pointer;Index:LongInt;Value:PAnsiChar;Count:LongInt;Destroy:Pointer):LongInt;cdecl;external;
function sqlite3_column_text(Stmt:Pointer;Index:LongInt):PAnsiChar;cdecl;external;
function sqlite3_column_bytes(Stmt:Pointer;Index:LongInt):LongInt;cdecl;external;
function sqlite3_column_int64(Stmt:Pointer;Index:LongInt):Int64;cdecl;external;
function sqlite3_last_insert_rowid(DB:Pointer):Int64;cdecl;external;
function sqlite3_busy_timeout(DB:Pointer;MS:LongInt):LongInt;cdecl;external;
procedure sqlite3_progress_handler(DB:Pointer;N:LongInt;Callback,Arg:Pointer);cdecl;external;
function sqlite3_libversion:PAnsiChar;cdecl;external;
constructor TUTF8FileStream.Create(const FileName: string; Writing: Boolean);
var H: THandle;
begin
  if Writing then H:=CreateFileW(PWideChar(UTF8Decode(FileName)),GENERIC_WRITE,FILE_SHARE_READ,nil,CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL,0)
  else H:=CreateFileW(PWideChar(UTF8Decode(FileName)),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
  if H=INVALID_HANDLE_VALUE then RaiseLastOSError;
  inherited Create(H);
end;
destructor TUTF8FileStream.Destroy;
begin CloseHandle(Handle);inherited Destroy;end;
function UTF8PathExists(const Path:string):Boolean;
begin Result:=GetFileAttributesW(PWideChar(UTF8Decode(Path)))<>INVALID_FILE_ATTRIBUTES;end;
procedure EnsureUTF8Directory(const Path:string);
begin
  if not CreateDirectoryW(PWideChar(UTF8Decode(Path)),nil) then
    if GetLastError<>ERROR_ALREADY_EXISTS then RaiseLastOSError;
  if (GetFileAttributesW(PWideChar(UTF8Decode(Path))) and FILE_ATTRIBUTE_DIRECTORY)=0 then
    raise EInOutError.Create('Path is not a directory: '+Path);
end;
function DeleteUTF8File(const Path:string):Boolean;
begin Result:=DeleteFileW(PWideChar(UTF8Decode(Path)));end;
constructor ESQLiteError.CreateCode(ACode:Integer;const Detail:string);
begin inherited Create(Detail);Code:=ACode;end;
function Progress(Arg:Pointer):LongInt;cdecl;
begin Result:=Ord(GetTickCount64>TSQLiteDB(Arg).FDeadline);end;
constructor TSQLiteDB.Create(const FileName:string);
var C:Integer;
begin
 inherited Create;
 C:=sqlite3_open_v2(PAnsiChar(FileName),FHandle,$00000002 or $00000004 or $00010000,nil);
 Check(C);
 sqlite3_busy_timeout(FHandle,1000);
 sqlite3_progress_handler(FHandle,1000,@Progress,Self);
end;
destructor TSQLiteDB.Destroy;
begin if FHandle<>nil then sqlite3_close(FHandle);inherited Destroy;end;
procedure TSQLiteDB.Check(Code:Integer);
begin if Code<>SQLITE_OK then raise ESQLiteError.CreateCode(Code,string(sqlite3_errmsg(FHandle)));end;
procedure TSQLiteDB.Exec(const SQL:string);
begin FDeadline:=GetTickCount64+10000;Check(sqlite3_exec(FHandle,PAnsiChar(SQL),nil,nil,nil));end;
function TSQLiteDB.Prepare(const SQL:string):Pointer;
begin FDeadline:=GetTickCount64+10000;Result:=nil;Check(sqlite3_prepare_v2(FHandle,PAnsiChar(SQL),Length(SQL),Result,nil));end;
function TSQLiteDB.Next(Stmt:Pointer):Boolean;
var C:Integer;
begin C:=sqlite3_step(Stmt);Result:=C=SQLITE_ROW;if not Result and(C<>SQLITE_DONE) then Check(C);end;
procedure TSQLiteDB.Done(Stmt:Pointer);
begin if Stmt<>nil then sqlite3_finalize(Stmt);end;
procedure TSQLiteDB.BindText(Stmt:Pointer;Index:Integer;const Value:string);
begin Check(sqlite3_bind_text(Stmt,Index,PAnsiChar(Value),Length(Value),Pointer(PtrInt(-1))));end;
function TSQLiteDB.Text(Stmt:Pointer;Column:Integer):string;
begin SetString(Result,sqlite3_column_text(Stmt,Column),sqlite3_column_bytes(Stmt,Column));end;
function TSQLiteDB.Number(Stmt:Pointer;Column:Integer):Int64;
begin Result:=sqlite3_column_int64(Stmt,Column);end;
function TSQLiteDB.Scalar(const SQL:string):string;
var S:Pointer;
begin S:=Prepare(SQL);try if Next(S) then Result:=Text(S,0) else Result:='';finally Done(S);end;end;
function TSQLiteDB.LastID:Int64;
begin Result:=sqlite3_last_insert_rowid(FHandle);end;
function SQLiteVersion:string;
begin Result:=string(sqlite3_libversion);end;
end.
