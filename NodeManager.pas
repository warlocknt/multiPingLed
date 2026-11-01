unit NodeManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ConfigManager, PingHelper, SyncObjs, CommandRunner, SessionHistory;

type
  TIntArray = array of Integer;
  TNodeArray = array of TNodeConfig;

  TPingFunction = function(const Host: string; TimeoutMs: Integer): TPingResult;
  TPingThread = class;

  { TNodeManager }

  TNodeManager = class
  private
    FNodes: TNodeArray;
    FPingThreads: TFPList;
    FCriticalSection: TRTLCriticalSection;
    FOnStateChange: TNotifyEvent;
    FEventSection: TRTLCriticalSection;
    FPingFunction: TPingFunction;
    FCommands: TCommandRunner;
    FHistory: TSessionHistory;
    procedure SetOnStateChange(Value: TNotifyEvent);
    function GetOnStateChange: TNotifyEvent;
    function FindThreadIndexByNodeId(Id: Integer): Integer;
  public
    procedure SetHistory(History: TSessionHistory);
    procedure StopAllThreads;
    constructor Create(APingFunction: TPingFunction = nil);
    destructor Destroy; override;
    procedure ApplyConfig(const Nodes: TNodeArray; ActiveIds:TIntArray);
    function GetNodes: TNodeArray;
    function GetNodeById(Id: Integer): TNodeConfig;
    function IdInArray(Id: Integer; const Arr: TIntArray): Boolean;
    procedure StartThreadInternal(Index: Integer);
    procedure RecordProbeResult(Id: Integer; Success: Boolean; PingTime: TDateTime; PingMs: Integer);
    procedure UpdateNodeState(Id: Integer; NewState: TNodeState);
    procedure UpdateNodeResult(Id: Integer; NewState: TNodeState; PingTime: TDateTime; PingMs: Integer);
    function ActiveThreadCount: Integer;
    procedure UpdateNodePingTime(Id: Integer; PingTime: TDateTime; PingMs: Integer);
    procedure ExecuteNodeCommand(const Node: TNodeConfig; const GroupName: string); virtual;
    procedure MarkModified;
    property OnStateChange: TNotifyEvent read GetOnStateChange write SetOnStateChange;
  end;
  
  TPingThread = class(TThread)
  private
    FNodeManager: TNodeManager;
    FNodeId: Integer;
    FHost: string;
    FIntervalMs: Integer;
    FTimeoutMs: Integer;
    FWakeEvent: TEvent;
    procedure DoPing;

  protected
    procedure Execute; override;
  public
    constructor Create(ANodeManager: TNodeManager; const NodeInfo: TNodeConfig);
    destructor Destroy; override;
    procedure RequestStop;
  end;

implementation

uses LogWriter, Windows;

procedure NodeDebugLog(const Msg: string);
begin
  ApplicationLog('multiPingLed[NodeMgr]: ' + Msg);
end;

constructor TNodeManager.Create(APingFunction: TPingFunction);
begin
  inherited Create;
  FPingThreads := TFPList.Create;
  InitCriticalSection(FCriticalSection);
  InitCriticalSection(FEventSection);
  FPingFunction := APingFunction;
  if not Assigned(FPingFunction) then FPingFunction := @PingHost;
  FCommands := TCommandRunner.Create;
  SetLength(FNodes, 0);
end;

destructor TNodeManager.Destroy;
begin
  // StopAllThreads сам возьмёт критическую секцию
  OnStateChange := nil;
  SetHistory(nil);
  StopAllThreads;
  FreeAndNil(FCommands);
  FPingThreads.Free;
  DoneCriticalSection(FEventSection);
  DoneCriticalsection(FCriticalSection);
  inherited Destroy;
end;

procedure TNodeManager.SetHistory(History: TSessionHistory);
begin
  EnterCriticalSection(FEventSection);
  try FHistory := History;
  finally LeaveCriticalSection(FEventSection); end;
end;

procedure TNodeManager.StopAllThreads;
var Threads: array of TPingThread; I: Integer;
begin
  Threads := nil;
  EnterCriticalSection(FCriticalSection);
  try
    SetLength(Threads, FPingThreads.Count);
    for I := 0 to High(Threads) do
    begin
      Threads[I] := TPingThread(FPingThreads[I]);
      Threads[I].RequestStop;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
  // Called by the owner; workers never synchronize with the GUI.
  for I := 0 to High(Threads) do
  begin
    Threads[I].WaitFor;
    Threads[I].Free;
  end;
  EnterCriticalSection(FCriticalSection);
  try
    FPingThreads.Clear;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TNodeManager.FindThreadIndexByNodeId(Id: Integer): Integer;
var
  I: Integer;
  Thread: TPingThread;
begin
  Result := -1;
  // Вызывается ТОЛЬКО из ApplyConfig, который уже захватил FCriticalSection
  for I := 0 to FPingThreads.Count - 1 do
  begin
    Thread := TPingThread(FPingThreads[I]);
    if Thread.FNodeId = Id then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

procedure TNodeManager.ApplyConfig(const Nodes: TNodeArray; ActiveIds: TIntArray);
var
  Stopping: array of TPingThread;
  NewNodes: TNodeArray;
  Thread: TPingThread;
  I, J: Integer;
  Keep: Boolean;
begin
  // ApplyConfig/StopAllThreads are serialized by the owning main thread.
  Stopping := nil;
  NewNodes := Copy(Nodes, 0, Length(Nodes));
  EnterCriticalSection(FCriticalSection);
  try
    for I := FPingThreads.Count - 1 downto 0 do
    begin
      Thread := TPingThread(FPingThreads[I]);
      Keep := False;
      for J := 0 to High(NewNodes) do
        if (NewNodes[J].Id = Thread.FNodeId) and IdInArray(Thread.FNodeId, ActiveIds) then
        begin
          Keep := (Thread.FHost = NewNodes[J].Host) and
            (Thread.FIntervalMs = NewNodes[J].IntervalMs) and
            (Thread.FTimeoutMs = NewNodes[J].TimeoutMs) and not Thread.Finished;
          Break;
        end;
      if not Keep then
      begin
        Thread.RequestStop;
        SetLength(Stopping, Length(Stopping) + 1);
        Stopping[High(Stopping)] := Thread;
        FPingThreads.Delete(I);
      end;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
  // Retain ownership of removed workers until they actually exit.
  for I := 0 to High(Stopping) do
  begin
    Stopping[I].WaitFor;
    Stopping[I].Free;
  end;
  EnterCriticalSection(FEventSection);
  try
    EnterCriticalSection(FCriticalSection);
    try
      for I := 0 to High(NewNodes) do
      begin
        NewNodes[I].State := nsUnknown;
        NewNodes[I].LastPingTime := 0;
        NewNodes[I].LastPingMs := -1;
        NewNodes[I].LastStateChange := 0;
        NewNodes[I].ConsecutiveFailures := 0;
        NewNodes[I].ProbeCount := 0;
        NewNodes[I].ProbeIndex := 0;
        NewNodes[I].ProbeLosses := 0;
        FillChar(NewNodes[I].ProbeHistory, SizeOf(NewNodes[I].ProbeHistory), 0);
        for J := 0 to High(FNodes) do
          if (NewNodes[I].Id = FNodes[J].Id) and (NewNodes[I].Host = FNodes[J].Host) then
          begin
            NewNodes[I].State := FNodes[J].State;
            NewNodes[I].LastPingTime := FNodes[J].LastPingTime;
            NewNodes[I].LastPingMs := FNodes[J].LastPingMs;
            NewNodes[I].LastStateChange := FNodes[J].LastStateChange;
            NewNodes[I].ConsecutiveFailures := FNodes[J].ConsecutiveFailures;
            NewNodes[I].ProbeCount := FNodes[J].ProbeCount;
            NewNodes[I].ProbeIndex := FNodes[J].ProbeIndex;
            NewNodes[I].ProbeLosses := FNodes[J].ProbeLosses;
            NewNodes[I].ProbeHistory := FNodes[J].ProbeHistory;
            Break;
          end;
      end;
      FNodes := NewNodes;
      for I := 0 to High(FNodes) do
        if IdInArray(FNodes[I].Id, ActiveIds) and
          (FindThreadIndexByNodeId(FNodes[I].Id) < 0) then StartThreadInternal(I);
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
  finally
    LeaveCriticalSection(FEventSection);
  end;
end;

function TNodeManager.GetNodes: TNodeArray;
begin
  EnterCriticalSection(FCriticalSection);
  try
    Result := Copy(FNodes, 0, Length(FNodes));
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TNodeManager.GetNodeById(Id: Integer): TNodeConfig;
var
  I: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    for I := 0 to High(FNodes) do
    begin
      if FNodes[I].Id = Id then
      begin
        Result := FNodes[I];
        Exit;
      end;
    end;
    Result := Default(TNodeConfig);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TNodeManager.MarkModified;
begin
  // ApplyConfig always reconciles worker parameters and active IDs.
end;

function TNodeManager.IdInArray(Id: Integer; const Arr: TIntArray): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(Arr) do
    if Arr[I] = Id then
      Exit(True);
end;

procedure TNodeManager.StartThreadInternal(Index: Integer);
var Thread: TPingThread;
begin
  if (Index < 0) or (Index > High(FNodes)) then Exit;
  Thread := TPingThread.Create(Self, FNodes[Index]);
  try
    FPingThreads.Add(Thread);
    Thread.Start;
  except
    FPingThreads.Remove(Thread);
    Thread.Free;
    raise;
  end;
end;

procedure TNodeManager.RecordProbeResult(Id: Integer; Success: Boolean;
  PingTime: TDateTime; PingMs: Integer);
var I, Threshold: Integer; ConfirmedState: TNodeState; Found: Boolean;
begin
  EnterCriticalSection(FEventSection);
  try
    Found := False;
    ConfirmedState := nsUnknown;
    EnterCriticalSection(FCriticalSection);
    try
      for I := 0 to High(FNodes) do
        if FNodes[I].Id = Id then
        begin
          Found := True;
          with FNodes[I] do
          begin
            if ProbeCount = Length(ProbeHistory) then
            begin
              if ProbeHistory[ProbeIndex] then Dec(ProbeLosses);
            end
            else Inc(ProbeCount);
            ProbeHistory[ProbeIndex] := not Success;
            if not Success then Inc(ProbeLosses);
            ProbeIndex := (ProbeIndex + 1) mod Length(ProbeHistory);
            Threshold := FailThreshold;
            if Threshold <= 0 then Threshold := 3;
            if Success then
            begin
              ConsecutiveFailures := 0;
              ConfirmedState := nsUp;
            end
            else
            begin
              if ConsecutiveFailures < 20 then Inc(ConsecutiveFailures);
              ConfirmedState := FNodes[I].State;
              if ConsecutiveFailures >= Threshold then ConfirmedState := nsDown;
            end;
          end;
          Break;
        end;
    finally LeaveCriticalSection(FCriticalSection); end;
    if Found then
    begin
      UpdateNodeResult(Id, ConfirmedState, PingTime, PingMs);
      if FHistory <> nil then FHistory.RecordProbe(GetNodeById(Id), Success, PingTime, PingMs);
    end;
  finally LeaveCriticalSection(FEventSection); end;
end;

procedure TNodeManager.UpdateNodeState(Id: Integer; NewState: TNodeState);
begin
  UpdateNodeResult(Id, NewState, Now, -1);
end;

procedure TNodeManager.UpdateNodeResult(Id: Integer; NewState: TNodeState;
  PingTime: TDateTime; PingMs: Integer);
var I: Integer; Changed: Boolean;
begin
  // Serialize transitions and callbacks, including subscriber detachment.
  EnterCriticalSection(FEventSection);
  try
    Changed := False;
    EnterCriticalSection(FCriticalSection);
    try
      for I := 0 to High(FNodes) do
        if FNodes[I].Id = Id then
        begin
          Changed := FNodes[I].State <> NewState;
          if Changed then FNodes[I].LastStateChange := PingTime;
          FNodes[I].State := NewState;
          FNodes[I].LastPingTime := PingTime;
          FNodes[I].LastPingMs := PingMs;
          Break;
        end;
    finally
      LeaveCriticalSection(FCriticalSection);
    end;
    if Changed and Assigned(FOnStateChange) then FOnStateChange(Self);
  finally
    LeaveCriticalSection(FEventSection);
  end;
end;

procedure TNodeManager.UpdateNodePingTime(Id: Integer; PingTime: TDateTime; PingMs: Integer);
var
  I: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    for I := 0 to High(FNodes) do
    begin
      if FNodes[I].Id = Id then
      begin
        FNodes[I].LastPingTime := PingTime;
        FNodes[I].LastPingMs := PingMs;
        Break;
      end;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TNodeManager.ExecuteNodeCommand(const Node: TNodeConfig; const GroupName: string);
begin
  FCommands.Enqueue(Node, GroupName);
end;

procedure TNodeManager.SetOnStateChange(Value: TNotifyEvent);
begin
  EnterCriticalSection(FEventSection);
  try
    FOnStateChange := Value;
    if Assigned(FOnStateChange) then FOnStateChange(Self);
  finally
    LeaveCriticalSection(FEventSection);
  end;
end;

function TNodeManager.GetOnStateChange: TNotifyEvent;
begin
  EnterCriticalSection(FEventSection);
  try
    Result := FOnStateChange;
  finally
    LeaveCriticalSection(FEventSection);
  end;
end;

function TNodeManager.ActiveThreadCount: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    Result := FPingThreads.Count;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

{ TPingThread }

constructor TPingThread.Create(ANodeManager: TNodeManager; const NodeInfo: TNodeConfig);
begin
  inherited Create(True);
  FNodeManager := ANodeManager;
  FNodeId := NodeInfo.Id;
  FHost := NodeInfo.Host;
  FIntervalMs := NodeInfo.IntervalMs;
  FTimeoutMs := NodeInfo.TimeoutMs;
  FreeOnTerminate := False;
  FWakeEvent := TEvent.Create(nil, True, False, '');
end;

destructor TPingThread.Destroy;
begin
  RequestStop;
  // Also covers an exception before Start was called.
  if Suspended then Start;
  WaitFor;
  FreeAndNil(FWakeEvent);
  inherited Destroy;
end;

procedure TPingThread.RequestStop;
begin
  Terminate;
  if FWakeEvent <> nil then FWakeEvent.SetEvent;
end;

procedure TPingThread.Execute;
begin
  while not Terminated do
  begin
    try
      DoPing;
    except
      // Ignore exceptions during ping
    end;
    if not Terminated then
      FWakeEvent.WaitFor(FIntervalMs);
  end;
end;

procedure TPingThread.DoPing;
var
  Result: TPingResult;
  LocalNodeManager: TNodeManager;
begin
  // Manager ownership lasts until this worker has been joined.
  LocalNodeManager := FNodeManager;
  if LocalNodeManager = nil then Exit;

  try
    Result := FNodeManager.FPingFunction(FHost, FTimeoutMs);


    // Stop/reconfiguration waits for this operation before replacing data.
    if not Terminated then
      LocalNodeManager.RecordProbeResult(FNodeId, Result.Success, Now, Result.ResponseTime);
  except
    on E: Exception do
    begin
      if not Terminated then
        LocalNodeManager.RecordProbeResult(FNodeId, False, Now, -1);
    end;
  end;
end;

end.
