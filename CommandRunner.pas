unit CommandRunner;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, SyncObjs, ConfigManager;

type
  TCommandJob = record
    Node: TNodeConfig;
    GroupName: string;
  end;

  TCommandRunner = class(TThread)
  private
    FLock: TRTLCriticalSection;
    FWake: TEvent;
    FQueue: array of TCommandJob;
    procedure RunJob(const Job: TCommandJob);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(const Node: TNodeConfig; const GroupName: string);
  end;

// Windows quoting rules; macros are expanded after parsing into arguments.
procedure ParseCommand(const Command: string; Arguments: TStrings);
function ExpandCommandArgument(const Argument: string; const Job: TCommandJob): string;
function QuoteWindowsArgument(const Argument: string): string;

implementation

uses LogWriter, Windows, Process, ProcessUnicode;

{$push}
{$packrecords c}
type
  TCommandJobLimits = record
    ProcessTime, JobTime: Int64;
    Flags: DWORD;
    MinWorkingSet, MaxWorkingSet: PtrUInt;
    ActiveProcesses: DWORD;
    Affinity: PtrUInt;
    Priority, SchedulingClass: DWORD;
  end;
  TCommandExtendedLimits = record
    Basic: TCommandJobLimits;
    IoCounters: array[0..5] of QWord;
    ProcessMemory, JobMemory, PeakProcessMemory, PeakJobMemory: PtrUInt;
  end;
{$pop}

// Layout/API: Microsoft JOBOBJECT_EXTENDED_LIMIT_INFORMATION, information
// class 9. KILL_ON_JOB_CLOSE also covers script descendants on cancellation.
function CommandCreateJob(Attributes: Pointer; Name: PWideChar): THandle;
  stdcall; external 'kernel32.dll' name 'CreateJobObjectW';
function CommandSetJob(Job: THandle; InfoClass: Integer; Info: Pointer; Size: DWORD): BOOL;
  stdcall; external 'kernel32.dll' name 'SetInformationJobObject';
function CommandAssignJob(Job, ProcessHandle: THandle): BOOL;
  stdcall; external 'kernel32.dll' name 'AssignProcessToJobObject';

type TWideArgArray = array[0..65535] of PWideChar;
  PWideArgArray = ^TWideArgArray;
function CommandLineToArgvW(CommandLine: PWideChar; var Count: Integer): PWideArgArray;
  stdcall; external 'shell32.dll';

procedure ParseCommand(const Command: string; Arguments: TStrings);
var Argv: PWideArgArray; Count, I: Integer; Text: UnicodeString;
begin
  Arguments.Clear;
  Text := UTF8Decode(Trim(Command));
  if Text = '' then Exit;
  Count := 0;
  Argv := CommandLineToArgvW(PWideChar(Text), Count);
  if Argv = nil then RaiseLastOSError;
  try
    for I := 0 to Count - 1 do Arguments.Add(UTF8Encode(UnicodeString(Argv^[I])));
  finally
    LocalFree(HLOCAL(Argv));
  end;
end;

function QuoteWindowsArgument(const Argument: string): string;
var I, Slashes: Integer;
begin
  Result := '"';
  Slashes := 0;
  for I := 1 to Length(Argument) do
  begin
    if Argument[I] = '\' then Inc(Slashes)
    else
    begin
      if Argument[I] = '"' then
        Result := Result + StringOfChar('\', Slashes * 2 + 1) + '"'
      else
        Result := Result + StringOfChar('\', Slashes) + Argument[I];
      Slashes := 0;
    end;
  end;
  Result := Result + StringOfChar('\', Slashes * 2) + '"';
end;

function ExpandCommandArgument(const Argument: string; const Job: TCommandJob): string;
var Status: string;
begin
  case Job.Node.State of
    nsUp: Status := 'up';
    nsDown: Status := 'down';
  else Status := 'unknown';
  end;
  Result := StringReplace(Argument, '{host}', Job.Node.Host, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{name}', Job.Node.Name, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{status}', Status, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{group}', Job.GroupName, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '{ping}', IntToStr(Job.Node.LastPingMs), [rfReplaceAll, rfIgnoreCase]);
end;

constructor TCommandRunner.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  InitCriticalSection(FLock);
  FWake := TEvent.Create(nil, True, False, '');
  Start;
end;

destructor TCommandRunner.Destroy;
begin
  Terminate;
  if FWake <> nil then FWake.SetEvent;
  if Suspended then Start;
  WaitFor;
  FreeAndNil(FWake);
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TCommandRunner.Enqueue(const Node: TNodeConfig; const GroupName: string);
var Index: Integer; StateNode: TNodeConfig;
begin
  StateNode := Node;
  StateNode.Command := '';
  if Node.State = nsUp then StateNode.Command := Node.UpCommand;
  if Node.State = nsDown then StateNode.Command := Node.DownCommand;
  StateNode.UpCommand := '';
  StateNode.DownCommand := '';
  if Trim(StateNode.Command) <> '' then Enqueue(StateNode, GroupName);
  if Trim(Node.Command) = '' then Exit;
  EnterCriticalSection(FLock);
  try
    if Terminated then Exit;
    // Limit memory growth when an external program is slow.
    if Length(FQueue) >= 4096 then
    begin
      ApplicationLog('multiPingLed: command queue full');
      Exit;
    end;
    Index := Length(FQueue);
    SetLength(FQueue, Index + 1);
    FQueue[Index].Node := Node;
    FQueue[Index].GroupName := GroupName;
    FWake.SetEvent;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TCommandRunner.Execute;
var Job: TCommandJob; HaveJob: Boolean; I: Integer;
begin
  while not Terminated do
  begin
    EnterCriticalSection(FLock);
    try
      HaveJob := Length(FQueue) > 0;
      if HaveJob then
      begin
        Job := FQueue[0];
        for I := 1 to High(FQueue) do FQueue[I - 1] := FQueue[I];
        SetLength(FQueue, Length(FQueue) - 1);
      end;
      // Reset under the queue lock to avoid losing an enqueue signal.
      FWake.ResetEvent;
    finally
      LeaveCriticalSection(FLock);
    end;
    if HaveJob then
    begin
      try
        RunJob(Job);
      except
        on E: Exception do ApplicationLog('multiPingLed: command failed: ' + E.Message);
      end;
      Job := Default(TCommandJob);
    end
    else if not Terminated then FWake.WaitFor(INFINITE);
  end;
end;

procedure TCommandRunner.RunJob(const Job: TCommandJob);
var
  Args: TStringList;
  Proc: ProcessUnicode.TProcess;
  Ext, BatchLine: string;
  I, J: Integer;
  Deadline: QWord;
  JobHandle: THandle;
  Limits: TCommandExtendedLimits;
  Completed, Cancelled: Boolean;
begin
  Args := TStringList.Create;
  Proc := nil;
  JobHandle := 0;
  Completed := False;
  Cancelled := False;
  try
    ParseCommand(Job.Node.Command, Args);
    if (Args.Count = 0) or Terminated then Exit;
    for I := 0 to Args.Count - 1 do Args[I] := ExpandCommandArgument(Args[I], Job);
    Ext := LowerCase(ExtractFileExt(Args[0]));
    Proc := ProcessUnicode.TProcess.Create(nil);
    Proc.Options := [poRunSuspended, poNoConsole];
    Proc.ShowWindow := swoHide;
    if Ext = '.ps1' then
    begin
      Proc.Executable := 'powershell.exe';
      Proc.Parameters.Add('-NoProfile');
      Proc.Parameters.Add('-ExecutionPolicy');
      Proc.Parameters.Add('Bypass');
      Proc.Parameters.Add('-File');
      for I := 0 to Args.Count - 1 do
        Proc.Parameters.Add(UTF8Decode(QuoteWindowsArgument(Args[I])));
    end
    else if (Ext = '.cmd') or (Ext = '.bat') then
    begin
      // cmd.exe reparses its input: reject shell syntax rather than executing
      // host/name/group metadata as commands. Only script + literal args.
      BatchLine := '';
      for I := 0 to Args.Count - 1 do
      begin
        for J := 1 to Length(Args[I]) do
          if Args[I][J] in ['"', '&', '|', '<', '>', '^', '%', '!', #10, #13] then
            raise EArgumentException.Create('Unsafe character in batch argument');
        if I > 0 then BatchLine := BatchLine + ' ';
        BatchLine := BatchLine + '"' + Args[I] + '"';
      end;
      Proc.Executable := 'cmd.exe';
      Proc.Parameters.Add('/D');
      Proc.Parameters.Add('/S');
      Proc.Parameters.Add('/C');
      Proc.Parameters.Add(UTF8Decode('"' + BatchLine + '"'));
    end
    else
    begin
      Proc.Executable := UTF8Decode(Args[0]);
      for I := 1 to Args.Count - 1 do
        Proc.Parameters.Add(UTF8Decode(QuoteWindowsArgument(Args[I])));
    end;
    if Terminated then Exit;
    JobHandle := CommandCreateJob(nil, nil);
    if JobHandle = 0 then RaiseLastOSError;
    Limits := Default(TCommandExtendedLimits);
    Limits.Basic.Flags := $00002000; // JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
    if not CommandSetJob(JobHandle, 9, @Limits, SizeOf(Limits)) then RaiseLastOSError;
    Proc.Execute;
    if not CommandAssignJob(JobHandle, Proc.ProcessHandle) then
    begin
      Proc.Terminate(1);
      RaiseLastOSError;
    end;
    Proc.Resume;
    Deadline := GetTickCount64 + 30000;
    while Proc.Running do
    begin
      if Terminated or (GetTickCount64 >= Deadline) then
      begin
        Cancelled := True;
        Break;
      end;
      Sleep(20);
    end;
    Completed := not Cancelled;
  finally
    if JobHandle <> 0 then
    begin
      // Successful scripts may intentionally launch independent applications.
      // Retain kill-on-close for cancellation, timeout, or an exception only.
      if Completed then
      begin
        Limits.Basic.Flags := 0;
        CommandSetJob(JobHandle, 9, @Limits, SizeOf(Limits));
      end;
      CloseHandle(JobHandle);
      if (Proc <> nil) and not Completed then
        WaitForSingleObject(Proc.ProcessHandle, 1000);
    end;
    Proc.Free;
    Args.Free;
  end;
end;

end.
