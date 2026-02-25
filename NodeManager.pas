unit NodeManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ConfigManager, PingHelper;

type
  TNodeArray = array of TNodeConfig;
  
  TPingThread = class;
  
  TNodeManager = class
  private
    FNodes: TNodeArray;
    FPingThreads: TList;
    FCriticalSection: TRTLCriticalSection;
    FOnStateChange: TNotifyEvent;
    procedure StartAllThreads;
    procedure DoStateChange;
  public
    procedure StopAllThreads;
    constructor Create;
    destructor Destroy; override;
    procedure ApplyConfig(const Nodes: TNodeArray);
    function GetNodes: TNodeArray;
    function GetNodeById(Id: Integer): TNodeConfig;
    procedure UpdateNodeState(Id: Integer; NewState: TNodeState);
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;
  
  TPingThread = class(TThread)
  private
    FNodeManager: TNodeManager;
    FNodeId: Integer;
    FHost: string;
    FIntervalMs: Integer;
    FTimeoutMs: Integer;
    FCurrentState: TNodeState;
    procedure DoPing;
  protected
    procedure Execute; override;
  public
    constructor Create(ANodeManager: TNodeManager; const NodeInfo: TNodeConfig);
  end;

implementation

constructor TNodeManager.Create;
begin
  inherited Create;
  FPingThreads := TList.Create;
  InitCriticalSection(FCriticalSection);
  SetLength(FNodes, 0);
end;

destructor TNodeManager.Destroy;
begin
  StopAllThreads;
  FreeAndNil(FPingThreads);
  DoneCriticalsection(FCriticalSection);
  inherited Destroy;
end;

procedure TNodeManager.StopAllThreads;
var
  I: Integer;
  Thread: TPingThread;
  WaitTime: Cardinal;
begin
  // Посылаем сигнал завершения всем потокам
  for I := FPingThreads.Count - 1 downto 0 do
  begin
    Thread := TPingThread(FPingThreads[I]);
    Thread.Terminate;
  end;
  
  // Ждем завершения с таймаутом 2 секунды
  for I := FPingThreads.Count - 1 downto 0 do
  begin
    Thread := TPingThread(FPingThreads[I]);
    WaitTime := 0;
    // Ждем максимум 2 секунды
    while (WaitTime < 2000) and not Thread.Finished do
    begin
      Sleep(50);
      WaitTime := WaitTime + 50;
    end;
    // Если поток завис - просто освобождаем память
    // ОС завершит поток при выходе процесса
    Thread.Free;
  end;
  FPingThreads.Clear;
end;

procedure TNodeManager.StartAllThreads;
var
  I: Integer;
  Thread: TPingThread;
begin
  for I := 0 to High(FNodes) do
  begin
    Thread := TPingThread.Create(Self, FNodes[I]);
    Thread.Start;
    FPingThreads.Add(Thread);
  end;
end;

procedure TNodeManager.ApplyConfig(const Nodes: TNodeArray);
begin
  EnterCriticalSection(FCriticalSection);
  try
    StopAllThreads;
    
    FNodes := Nodes;
    
    StartAllThreads;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TNodeManager.GetNodes: TNodeArray;
begin
  EnterCriticalSection(FCriticalSection);
  try
    Result := FNodes;
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
    FillChar(Result, SizeOf(Result), 0);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TNodeManager.UpdateNodeState(Id: Integer; NewState: TNodeState);
var
  I: Integer;
  StateChanged: Boolean;
begin
  StateChanged := False;

  EnterCriticalSection(FCriticalSection);
  try
    for I := 0 to High(FNodes) do
    begin
      if FNodes[I].Id = Id then
      begin
        FNodes[I].LastPingTime := Now;  // Обновляем время пинга
        if FNodes[I].State <> NewState then
        begin
          FNodes[I].State := NewState;
          StateChanged := True;
        end;
        Break;
      end;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
  
  if StateChanged then
    DoStateChange;
end;

procedure TNodeManager.DoStateChange;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
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
  FCurrentState := NodeInfo.State;
  FreeOnTerminate := False;
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
      Sleep(FIntervalMs);
  end;
end;

procedure TPingThread.DoPing;
var
  Result: TPingResult;
  NewState: TNodeState;
  NodeManager: TNodeManager;
begin
  NodeManager := FNodeManager;
  if NodeManager = nil then Exit;

  try
    Result := PingHost(FHost, FTimeoutMs);

    if Result.Success then
      NewState := nsUp
    else
      NewState := nsDown;

    if (FNodeManager <> nil) and not Terminated then
      NodeManager.UpdateNodeState(FNodeId, NewState);
  except
    on E: Exception do
    begin
      if (FNodeManager <> nil) and not Terminated then
        NodeManager.UpdateNodeState(FNodeId, nsDown);
    end;
  end;
end;

end.
