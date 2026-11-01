unit SessionHistory;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, SyncObjs, ConfigManager, SQLiteNative;
type
  THistoryNoticeKind = (hnBackupCreated, hnDisabled, hnReportFailed);
  THistoryNotice = record Kind: THistoryNoticeKind; Detail: string; end;
  THistoryNode = record ID: Integer; Name, Host: string; end;
  THistoryNodes = array of THistoryNode;
  THistoryJob = record
    IsConfig: Boolean; AtMS: Int64; Nodes: THistoryNodes;
    Node: THistoryNode; State: TNodeState; Success: Boolean; RTT: Integer;
  end;
  THistoryJobs = array of THistoryJob;
  TObservedTotals = record
    Node: THistoryNode;
    RowID, LastAt, Checks, Losses, Samples, LatencySum, LatencyMax: Int64;
    UpMS, DownMS, UnknownMS, Outages: Int64;
    State: TNodeState; Active: Boolean;
  end;
  TSessionHistory = class(TThread)
  private
    FLock: TRTLCriticalSection;
    FWake, FReady: TEvent;
    FQueue: THistoryJobs;
    FNotices: array of THistoryNotice;
    FRows: array of TObservedTotals;
    FDB: TSQLiteDB;
    FRoot, FDBPath, FReportPath, FReportLanguage: string;
    FLaunch: TDateTime;
    FStartMS, FEndMS, FMaxMS, FSessionID: Int64;
    FDBLock: THandle;
    FFailed, FFinishing, FMakeReport: Boolean;
    procedure Notice(Kind: THistoryNoticeKind; const Detail: string);
    procedure Disable(const Detail: string);
    procedure Enqueue(const Job: THistoryJob);
    procedure OpenDatabase;
    procedure CheckDatabase;
    procedure ArchiveCorrupt;
    procedure CreateSchema;
    function Intern(const TableName, ColumnName, Value: string): Int64;
    procedure ProcessJob(const Job: THistoryJob);
    procedure ProcessConfig(const Job: THistoryJob);
    procedure RecordState(var Row: TObservedTotals; State: TNodeState; AtMS: Int64; CountOutage: Boolean = True);
    procedure AccountTo(var Row: TObservedTotals; AtMS: Int64);
    procedure CloseRow(var Row: TObservedTotals; AtMS: Int64);
    procedure FlushTotals(AtMS: Int64);
  protected
    procedure Execute; override;
  public
    constructor Create(const Root: string; Started: TDateTime);
    destructor Destroy; override;
    procedure SetNodes(const Nodes: array of TNodeConfig; const ActiveIDs: array of Integer);
    procedure RecordProbe(const Node: TNodeConfig; Success: Boolean; At: TDateTime; RTT: Integer);
    function Finish(At: TDateTime; MakeReport: Boolean; const Language: string): string;
    function TakeNotice(out Item: THistoryNotice): Boolean;
    function WaitReady(TimeoutMS: Cardinal = 10000): Boolean;
    property SessionID: Int64 read FSessionID;
    property DatabasePath: string read FDBPath;
  end;
function HistoryTimeMS(Value: TDateTime): Int64;
implementation
uses Windows, DateUtils, SessionReport;
const HistoryApplicationID = 1347175748;
function HistoryTimeMS(Value: TDateTime): Int64;
begin Result := DateTimeToUnix(Value, False) * 1000 + MilliSecondOf(Value); end;
function Later(A,B: Int64): Int64;
begin if A>B then Result:=A else Result:=B;end;
constructor TSessionHistory.Create(const Root: string; Started: TDateTime);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  InitCriticalSection(FLock);
  FWake := TEvent.Create(nil, False, False, '');
  FReady := TEvent.Create(nil, True, False, '');
  // SysUtils.ExpandFileName converts Unicode paths through the Windows ANSI codepage.
  FRoot := Root;
  if ExtractFileDrive(FRoot) = '' then
    FRoot := IncludeTrailingPathDelimiter(GetCurrentDir) + FRoot;
  FRoot := IncludeTrailingPathDelimiter(FRoot);
  FDBPath := FRoot + 'history.sqlite'; FLaunch := Started;
  FStartMS := HistoryTimeMS(Started); FMaxMS := FStartMS;
  FDBLock := INVALID_HANDLE_VALUE;
  Start;
end;
destructor TSessionHistory.Destroy;
begin
  Finish(Now, False, 'english');
  FreeAndNil(FWake); FreeAndNil(FReady);
  DoneCriticalSection(FLock);
  inherited Destroy;
end;
procedure TSessionHistory.Notice(Kind: THistoryNoticeKind; const Detail: string);
var N: Integer;
begin
  EnterCriticalSection(FLock);
  try
    N:=Length(FNotices);
    if N>=8 then Exit;
    SetLength(FNotices,N+1);FNotices[N].Kind:=Kind;FNotices[N].Detail:=Detail;
  finally LeaveCriticalSection(FLock);end;
end;
procedure TSessionHistory.Disable(const Detail: string);
var First: Boolean;
begin
  EnterCriticalSection(FLock);
  try First:=not FFailed; FFailed:=True; FQueue:=nil;
  finally LeaveCriticalSection(FLock);end;
  if First then Notice(hnDisabled, Detail);
end;
function TSessionHistory.TakeNotice(out Item: THistoryNotice): Boolean;
var I:Integer;
begin
  EnterCriticalSection(FLock);
  try
    Result:=Length(FNotices)>0;
    if Result then
    begin
      Item:=FNotices[0];for I:=1 to High(FNotices) do FNotices[I-1]:=FNotices[I];
      SetLength(FNotices,Length(FNotices)-1);
    end;
  finally LeaveCriticalSection(FLock);end;
end;
function TSessionHistory.WaitReady(TimeoutMS:Cardinal):Boolean;
begin
  Result:=FReady.WaitFor(TimeoutMS)=wrSignaled;
  EnterCriticalSection(FLock);
  try Result:=Result and not FFailed;finally LeaveCriticalSection(FLock);end;
end;
procedure TSessionHistory.Enqueue(const Job: THistoryJob);
var N:Integer; Overflow:Boolean;
begin
  Overflow:=False;
  EnterCriticalSection(FLock);
  try
    if FFinishing or FFailed then Exit;
    N:=Length(FQueue);
    if N>=8192 then Overflow:=True
    else begin SetLength(FQueue,N+1);FQueue[N]:=Job;FWake.SetEvent;end;
  finally LeaveCriticalSection(FLock);end;
  if Overflow then Disable('History queue capacity exceeded');
end;
procedure TSessionHistory.SetNodes(const Nodes:array of TNodeConfig;const ActiveIDs:array of Integer);
var Job:THistoryJob;I,J,N:Integer;
begin
  Job:=Default(THistoryJob);Job.IsConfig:=True;Job.AtMS:=HistoryTimeMS(Now);
  for I:=0 to High(Nodes) do
    for J:=0 to High(ActiveIDs) do
      if Nodes[I].Id=ActiveIDs[J] then
      begin
        N:=Length(Job.Nodes);SetLength(Job.Nodes,N+1);
        Job.Nodes[N].ID:=Nodes[I].Id;Job.Nodes[N].Name:=Nodes[I].Name;Job.Nodes[N].Host:=Nodes[I].Host;Break;
      end;
  Enqueue(Job);
end;
procedure TSessionHistory.RecordProbe(const Node:TNodeConfig;Success:Boolean;At:TDateTime;RTT:Integer);
var Job:THistoryJob;
begin
  Job:=Default(THistoryJob);Job.AtMS:=HistoryTimeMS(At);
  Job.Node.ID:=Node.Id;Job.Node.Name:=Node.Name;Job.Node.Host:=Node.Host;
  Job.State:=Node.State;Job.Success:=Success;Job.RTT:=RTT;Enqueue(Job);
end;
function TSessionHistory.Finish(At:TDateTime;MakeReport:Boolean;const Language:string):string;
begin
  EnterCriticalSection(FLock);
  try
    if not FFinishing then
    begin
      FFinishing:=True;FEndMS:=HistoryTimeMS(At);FMakeReport:=MakeReport;FReportLanguage:=Language;FWake.SetEvent;
    end;
  finally LeaveCriticalSection(FLock);end;
  WaitFor;Result:=FReportPath;
end;
procedure TSessionHistory.CheckDatabase;
var S:Pointer; CheckResult,AppID,Version:string;
begin
  CheckResult:=FDB.Scalar('PRAGMA quick_check(1)');
  if CheckResult<>'ok' then raise ESQLiteError.CreateCode(11,CheckResult);
  AppID:=FDB.Scalar('PRAGMA application_id');Version:=FDB.Scalar('PRAGMA user_version');
  if (AppID='0') and (FDB.Scalar('SELECT count(*) FROM sqlite_schema WHERE type=''table''')='0') then Exit;
  if (AppID<>IntToStr(HistoryApplicationID)) or (Version<>'1') then
    raise Exception.Create('Unsupported history database schema');
  S:=FDB.Prepare('PRAGMA foreign_key_check');
  try if FDB.Next(S) then raise ESQLiteError.CreateCode(11,'History foreign key violation');
  finally FDB.Done(S);end;
end;
procedure TSessionHistory.ArchiveCorrupt;
const Suffixes:array[0..3]of string=('','-wal','-shm','-journal');
var Directory,Base,Source,Target:string;I,N:Integer;Input,Output:TUTF8FileStream;
begin
  FreeAndNil(FDB);
  Directory:=FRoot+'bak';EnsureUTF8Directory(Directory);
  Base:=IncludeTrailingPathDelimiter(Directory)+'history-corrupt-'+FormatDateTime('yyyy-mm-dd_hhnnss-zzz',FLaunch);
  N:=0;while UTF8PathExists(Base+'.sqlite') do begin Inc(N);Base:=Base+'-'+IntToStr(N);end;
  Base:=Base+'.sqlite';
  // Copy and flush every existing sidecar before removing any original file.
  for I:=0 to High(Suffixes) do
  begin
    Source:=FDBPath+Suffixes[I];Target:=Base+Suffixes[I];
    if not UTF8PathExists(Source) then Continue;
    Input:=TUTF8FileStream.Create(Source,False);
    try
      Output:=TUTF8FileStream.Create(Target,True);
      try Output.CopyFrom(Input,0);if Output.Size<>Input.Size then raise EInOutError.Create('Incomplete database backup');
        if not FlushFileBuffers(Output.Handle) then RaiseLastOSError;
      finally Output.Free;end;
    finally Input.Free;end;
  end;
  for I:=High(Suffixes) downto 0 do
  begin
    Source:=FDBPath+Suffixes[I];
    if UTF8PathExists(Source) and not DeleteUTF8File(Source) then RaiseLastOSError;
  end;
  Notice(hnBackupCreated,Base);
end;
procedure TSessionHistory.CreateSchema;
begin
  FDB.Exec('PRAGMA foreign_keys=ON; PRAGMA synchronous=FULL; PRAGMA journal_mode=DELETE; PRAGMA cache_size=-512; PRAGMA mmap_size=0;');
  if FDB.Scalar('PRAGMA application_id')='0' then FDB.Exec('PRAGMA auto_vacuum=INCREMENTAL;');
  FDB.Exec('BEGIN IMMEDIATE;');
  try
    FDB.Exec('CREATE TABLE IF NOT EXISTS sessions(id INTEGER PRIMARY KEY AUTOINCREMENT,started_ms INTEGER NOT NULL,ended_ms INTEGER);'+
      'CREATE TABLE IF NOT EXISTS hosts(id INTEGER PRIMARY KEY,address TEXT NOT NULL UNIQUE);'+
      'CREATE TABLE IF NOT EXISTS names(id INTEGER PRIMARY KEY,value TEXT NOT NULL UNIQUE);'+
      'CREATE TABLE IF NOT EXISTS session_nodes(id INTEGER PRIMARY KEY,session_id INTEGER NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,'+
      'source_id INTEGER NOT NULL,host_id INTEGER NOT NULL REFERENCES hosts(id),name_id INTEGER NOT NULL REFERENCES names(id),started_ms INTEGER NOT NULL,ended_ms INTEGER);'+
      'CREATE INDEX IF NOT EXISTS session_nodes_session ON session_nodes(session_id);'+
      'CREATE TABLE IF NOT EXISTS state_events(id INTEGER PRIMARY KEY,node_id INTEGER NOT NULL REFERENCES session_nodes(id) ON DELETE CASCADE,'+
      'at_ms INTEGER NOT NULL,state INTEGER NOT NULL CHECK(state BETWEEN 0 AND 2));'+
      'CREATE INDEX IF NOT EXISTS state_events_node_time ON state_events(node_id,at_ms,id);'+
      'CREATE TABLE IF NOT EXISTS node_totals(node_id INTEGER PRIMARY KEY REFERENCES session_nodes(id) ON DELETE CASCADE,'+
      'checks INTEGER NOT NULL DEFAULT 0,losses INTEGER NOT NULL DEFAULT 0,samples INTEGER NOT NULL DEFAULT 0,'+
      'latency_sum_ms INTEGER NOT NULL DEFAULT 0,latency_max_ms INTEGER NOT NULL DEFAULT 0,'+
      'up_ms INTEGER NOT NULL DEFAULT 0,down_ms INTEGER NOT NULL DEFAULT 0,unknown_ms INTEGER NOT NULL DEFAULT 0,outages INTEGER NOT NULL DEFAULT 0);'+
      'PRAGMA application_id=1347175748; PRAGMA user_version=1;');
    FDB.Exec('INSERT INTO sessions(started_ms) VALUES('+IntToStr(FStartMS)+');');FSessionID:=FDB.LastID;
    FDB.Exec('DELETE FROM sessions WHERE id NOT IN(SELECT id FROM sessions ORDER BY id DESC LIMIT 100);'+
      'DELETE FROM hosts WHERE NOT EXISTS(SELECT 1 FROM session_nodes WHERE host_id=hosts.id);'+
      'DELETE FROM names WHERE NOT EXISTS(SELECT 1 FROM session_nodes WHERE name_id=names.id);COMMIT;');
  except try FDB.Exec('ROLLBACK;');except end;raise;end;
  FDB.Exec('PRAGMA incremental_vacuum(128);');
end;
procedure TSessionHistory.OpenDatabase;
var Input:TUTF8FileStream; Header:AnsiString;
begin
  if not UTF8PathExists(FRoot) then raise EInOutError.Create('History directory does not exist');
  FDBLock:=CreateFileW(PWideChar(UTF8Decode(FDBPath+'.lock')),GENERIC_READ or GENERIC_WRITE,0,nil,OPEN_ALWAYS,FILE_ATTRIBUTE_NORMAL,0);
  if FDBLock=INVALID_HANDLE_VALUE then RaiseLastOSError;
  if UTF8PathExists(FDBPath) then
  begin
    Input:=TUTF8FileStream.Create(FDBPath,False);
    try
      Header:='';
      if Input.Size>=100 then begin SetLength(Header,16);Input.ReadBuffer(Header[1],16);end;
      if (Input.Size<>0) and(Header<>'SQLite format 3'#0) then
      begin Input.Free;Input:=nil;ArchiveCorrupt;end;
    finally Input.Free;end;
  end;
  try
    FDB:=TSQLiteDB.Create(FDBPath);CheckDatabase;
  except
    on E:ESQLiteError do
    begin
      if not ((E.Code and 255) in [11,26]) then raise;
      ArchiveCorrupt;FDB:=TSQLiteDB.Create(FDBPath);CheckDatabase;
    end;
  end;
  CreateSchema;
end;
function TSessionHistory.Intern(const TableName,ColumnName,Value:string):Int64;
var S:Pointer;
begin
  S:=FDB.Prepare('INSERT OR IGNORE INTO '+TableName+'('+ColumnName+') VALUES(?)');
  try FDB.BindText(S,1,Value);FDB.Next(S);finally FDB.Done(S);end;
  S:=FDB.Prepare('SELECT id FROM '+TableName+' WHERE '+ColumnName+'=?');
  try FDB.BindText(S,1,Value);if not FDB.Next(S) then raise Exception.Create('History dictionary lookup failed');Result:=FDB.Number(S,0);
  finally FDB.Done(S);end;
end;
procedure TSessionHistory.AccountTo(var Row:TObservedTotals;AtMS:Int64);
var Delta:Int64;
begin
  AtMS:=Later(AtMS,Row.LastAt);Delta:=AtMS-Row.LastAt;
  case Row.State of nsUp:Inc(Row.UpMS,Delta);nsDown:Inc(Row.DownMS,Delta);else Inc(Row.UnknownMS,Delta);end;
  Row.LastAt:=AtMS;
end;
procedure TSessionHistory.RecordState(var Row:TObservedTotals;State:TNodeState;AtMS:Int64;CountOutage:Boolean);
begin
  AtMS:=Later(AtMS,Row.LastAt);AccountTo(Row,AtMS);
  Row.State:=State;if CountOutage and (State=nsDown) then Inc(Row.Outages);
  FDB.Exec('INSERT INTO state_events(node_id,at_ms,state) VALUES('+IntToStr(Row.RowID)+','+IntToStr(AtMS)+','+IntToStr(Ord(State))+');');
end;
procedure TSessionHistory.CloseRow(var Row:TObservedTotals;AtMS:Int64);
begin
  AccountTo(Row,AtMS);Row.Active:=False;
  FDB.Exec('UPDATE session_nodes SET ended_ms='+IntToStr(Row.LastAt)+' WHERE id='+IntToStr(Row.RowID));
end;
procedure TSessionHistory.ProcessConfig(const Job:THistoryJob);
var I,J,N:Integer;Found:Boolean;HostID,NameID:Int64;
    PreviousStates:array of TNodeState;
begin
  // Preserve only continuous observations of the same target. Closed rows
  // from an earlier monitoring period must not restore a stale state.
  SetLength(PreviousStates,Length(Job.Nodes));
  for J:=0 to High(Job.Nodes) do
  begin
    PreviousStates[J]:=nsUnknown;
    for I:=0 to High(FRows) do
      if FRows[I].Active and (FRows[I].Node.ID=Job.Nodes[J].ID) and
        (FRows[I].Node.Host=Job.Nodes[J].Host) then
      begin
        PreviousStates[J]:=FRows[I].State;
        Break;
      end;
  end;
  for I:=0 to High(FRows) do
    if FRows[I].Active then
    begin
      Found:=False;
      for J:=0 to High(Job.Nodes) do
        if (FRows[I].Node.ID=Job.Nodes[J].ID) and (FRows[I].Node.Host=Job.Nodes[J].Host) and (FRows[I].Node.Name=Job.Nodes[J].Name) then Found:=True;
      if not Found then CloseRow(FRows[I],Job.AtMS);
    end;
  for I:=0 to High(Job.Nodes) do
  begin
    Found:=False;
    for J:=0 to High(FRows) do
      if FRows[J].Active and(FRows[J].Node.ID=Job.Nodes[I].ID) then Found:=True;
    if Found then Continue;
    HostID:=Intern('hosts','address',Job.Nodes[I].Host);NameID:=Intern('names','value',Job.Nodes[I].Name);
    FDB.Exec('INSERT INTO session_nodes(session_id,source_id,host_id,name_id,started_ms) VALUES('+
      IntToStr(FSessionID)+','+IntToStr(Job.Nodes[I].ID)+','+IntToStr(HostID)+','+IntToStr(NameID)+','+IntToStr(Job.AtMS)+');');
    N:=Length(FRows);SetLength(FRows,N+1);FRows[N].Node:=Job.Nodes[I];FRows[N].RowID:=FDB.LastID;
    FRows[N].Active:=True;FRows[N].LastAt:=Job.AtMS;FRows[N].State:=nsUnknown;
    FDB.Exec('INSERT INTO node_totals(node_id) VALUES('+IntToStr(FRows[N].RowID)+');');
    // Initial state of a name segment does not count as a new outage.
    RecordState(FRows[N],PreviousStates[I],Job.AtMS,False);
  end;
end;
procedure TSessionHistory.ProcessJob(const Job:THistoryJob);
var I:Integer;
begin
  FMaxMS:=Later(FMaxMS,Job.AtMS);
  if Job.IsConfig then begin ProcessConfig(Job);Exit;end;
  for I:=High(FRows) downto 0 do
    if FRows[I].Active and(FRows[I].Node.ID=Job.Node.ID) and(FRows[I].Node.Host=Job.Node.Host) and(FRows[I].Node.Name=Job.Node.Name) then
    begin
      Inc(FRows[I].Checks);
      if not Job.Success then Inc(FRows[I].Losses)
      else if Job.RTT>=0 then
      begin Inc(FRows[I].Samples);Inc(FRows[I].LatencySum,Job.RTT);FRows[I].LatencyMax:=Later(FRows[I].LatencyMax,Job.RTT);end;
      if FRows[I].State<>Job.State then RecordState(FRows[I],Job.State,Job.AtMS);
      Break;
    end;
end;
procedure TSessionHistory.FlushTotals(AtMS:Int64);
var I:Integer;
begin
  for I:=0 to High(FRows) do
  begin
    if FRows[I].Active then AccountTo(FRows[I],AtMS);
    with FRows[I] do
      FDB.Exec('UPDATE node_totals SET checks='+IntToStr(Checks)+',losses='+IntToStr(Losses)+',samples='+IntToStr(Samples)+
        ',latency_sum_ms='+IntToStr(LatencySum)+',latency_max_ms='+IntToStr(LatencyMax)+',up_ms='+IntToStr(UpMS)+
        ',down_ms='+IntToStr(DownMS)+',unknown_ms='+IntToStr(UnknownMS)+',outages='+IntToStr(Outages)+' WHERE node_id='+IntToStr(RowID));
  end;
end;
procedure TSessionHistory.Execute;
var Batch:THistoryJobs;Closing,Failed:Boolean;I:Integer;NextFlush:QWord;AtMS:Int64;
begin
  try
    try OpenDatabase;except on E:Exception do Disable(E.Message);end;
    FReady.SetEvent;NextFlush:=GetTickCount64+2000;
    repeat
      FWake.WaitFor(2000);
      EnterCriticalSection(FLock);
      try Batch:=FQueue;FQueue:=nil;Closing:=FFinishing;Failed:=FFailed;
      finally LeaveCriticalSection(FLock);end;
      if not Failed then
      begin
        try
          FDB.Exec('BEGIN IMMEDIATE;');
          try
            for I:=0 to High(Batch) do ProcessJob(Batch[I]);
            if Closing then
            begin
              FEndMS:=Later(Later(FEndMS,FStartMS),FMaxMS);
              for I:=0 to High(FRows) do if FRows[I].Active then CloseRow(FRows[I],FEndMS);
              FlushTotals(FEndMS);
              FDB.Exec('UPDATE sessions SET ended_ms='+IntToStr(FEndMS)+' WHERE id='+IntToStr(FSessionID));
            end
            else if GetTickCount64>=NextFlush then
            begin
              AtMS:=Later(HistoryTimeMS(Now),FMaxMS);FlushTotals(AtMS);FMaxMS:=AtMS;NextFlush:=GetTickCount64+2000;
            end;
            FDB.Exec('COMMIT;');
          except try FDB.Exec('ROLLBACK;');except end;raise;end;
        except on E:Exception do Disable(E.Message);end;
      end;
      Batch:=nil;
    until Closing;
    EnterCriticalSection(FLock);try Failed:=FFailed;finally LeaveCriticalSection(FLock);end;
    if FMakeReport and not Failed then
      try FReportPath:=GenerateSessionReport(FDB,FSessionID,FRoot,FReportLanguage);
      except on E:Exception do Notice(hnReportFailed,E.Message);end;
  finally
    FreeAndNil(FDB);
    if FDBLock<>INVALID_HANDLE_VALUE then CloseHandle(FDBLock);
    FReady.SetEvent;
  end;
end;
end.
