unit NodeManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ConfigManager, PingHelper;

type
  TIntArray = array of Integer;
  TNodeArray = array of TNodeConfig;

  TPingThread = class;

  { TNodeManager }

  TNodeManager = class
  private
    FNodes: TNodeArray;
    FPingThreads: TFPList;
    FCriticalSection: TRTLCriticalSection;
    FOnStateChange: TNotifyEvent;
    FModified: Boolean;  // Были ли изменения по сравнению с предыдущей конфигурацией
    procedure DoStateChange;
    function FindThreadIndexByNodeId(Id: Integer): Integer;
  public
    procedure StopAllThreads;
    constructor Create;
    destructor Destroy; override;
    procedure ApplyConfig(const Nodes: TNodeArray; ActiveIds:TIntArray);
    function GetNodes: TNodeArray;
    function GetNodeById(Id: Integer): TNodeConfig;
    function IdInArray(Id: Integer; const Arr: TIntArray): Boolean;
    procedure StartThreadInternal(Index: Integer);
    procedure UpdateNodeState(Id: Integer; NewState: TNodeState);
    procedure UpdateNodePingTime(Id: Integer; PingTime: TDateTime; PingMs: Integer);
    procedure ExecuteNodeCommand(const Node: TNodeConfig; const GroupName: string);
    procedure MarkModified;
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
    procedure OnTerminateHandler(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(ANodeManager: TNodeManager; const NodeInfo: TNodeConfig);
  end;

implementation

uses Windows, Process, LCLIntf;

procedure NodeDebugLog(const Msg: string);
begin
  OutputDebugString(PChar('multiPingLed[NodeMgr]: ' + Msg));
end;

constructor TNodeManager.Create;
begin
  inherited Create;
  FPingThreads := TFPList.Create;
  InitCriticalSection(FCriticalSection);
  SetLength(FNodes, 0);
  FModified := True;  // При первом запуске - всегда изменения
end;

destructor TNodeManager.Destroy;
begin
  // StopAllThreads сам возьмёт критическую секцию
  StopAllThreads;
  FPingThreads.Free;
  DoneCriticalsection(FCriticalSection);
  inherited Destroy;
end;

procedure TNodeManager.StopAllThreads;
var
  I: Integer;
  Thread: TPingThread;
  WaitTime: Cardinal;
  ThreadsCopy: array of TPingThread;
begin
  // Посылаем сигнал завершения всем потокам под блокировкой,
  // одновременно копируем указатели для ожидания без CS
  EnterCriticalSection(FCriticalSection);
  try
    SetLength(ThreadsCopy, FPingThreads.Count);
    for I := 0 to FPingThreads.Count - 1 do
    begin
      Thread := TPingThread(FPingThreads[I]);
      Thread.OnTerminate := nil;
      Thread.Terminate;
      ThreadsCopy[I] := Thread;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  // Ждем завершения каждого потока БЕЗ блокировки —
  // потоки должны иметь возможность войти в CS для UpdateNodeState.
  // Таймаут 2500 мс чтобы покрыть максимальный TimeoutMs пинга (типично 2000 мс) —
  // иначе поток в IcmpSendEcho не успеет завершиться и будет утечка.
  for I := 0 to High(ThreadsCopy) do
  begin
    Thread := ThreadsCopy[I];
    WaitTime := 0;
    while (WaitTime < 2500) and not Thread.Finished do
    begin
      Sleep(50);
      WaitTime := WaitTime + 50;
    end;
    if Thread.Finished then
      Thread.Free
    else
      Thread.FreeOnTerminate := True;
  end;

  // Очищаем список под блокировкой
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
  // Не пытаемся захватить повторно — будет deadlock
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
  I, J, ActiveCount: Integer;
  OldNodes: TNodeArray;
  ConfigChanged: Boolean;
  ThreadIdx: Integer;
  Thread: TPingThread;
  NewActiveIds: TIntArray;
  OldActiveIds: TIntArray;
  IdStillActive: Boolean;
  IdNewlyActive: Boolean;
  NodeStillExists: Boolean;
begin
  // Собираем старые активные ID
  OldActiveIds := nil;
  SetLength(OldActiveIds, 0);
  EnterCriticalSection(FCriticalSection);
  try
    OldNodes := FNodes;
    for I := 0 to High(FNodes) do
      if IdInArray(FNodes[I].Id, ActiveIds) then
      begin
        SetLength(OldActiveIds, Length(OldActiveIds) + 1);
        OldActiveIds[High(OldActiveIds)] := FNodes[I].Id;
      end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
  NewActiveIds := ActiveIds;

  // Проверяем, изменилась ли конфигурация
  ConfigChanged := FModified;
  if not ConfigChanged then
  begin
    // Сравниваем массивы узлов
    if Length(Nodes) <> Length(OldNodes) then
      ConfigChanged := True
    else
    begin
      for I := 0 to High(Nodes) do
      begin
        if (Nodes[I].Id <> OldNodes[I].Id) or
           (Nodes[I].Host <> OldNodes[I].Host) or
           (Nodes[I].IntervalMs <> OldNodes[I].IntervalMs) or
           (Nodes[I].TimeoutMs <> OldNodes[I].TimeoutMs) then
        begin
          ConfigChanged := True;
          Break;
        end;
      end;
    end;
    // Сравниваем активные ID
    if not ConfigChanged and (Length(NewActiveIds) <> Length(OldActiveIds)) then
      ConfigChanged := True;
  end;

  if not ConfigChanged then
  begin
    FModified := False;
    Exit;
  end;

  // Перезапускаем только изменённые/новые потоки
  EnterCriticalSection(FCriticalSection);
  try
    FNodes := Nodes;
    ActiveCount := 0;

    // Останавливаем потоки для узлов, полностью удалённых из конфига.
    // Основной цикл ниже проходит только по новым FNodes и такие «сиротские»
    // потоки не увидел бы — они работали бы вхолостую до выхода из приложения.
    for I := FPingThreads.Count - 1 downto 0 do
    begin
      Thread := TPingThread(FPingThreads[I]);
      NodeStillExists := False;
      for J := 0 to High(FNodes) do
        if FNodes[J].Id = Thread.FNodeId then
        begin
          NodeStillExists := True;
          Break;
        end;
      if not NodeStillExists then
      begin
        NodeDebugLog('Stopping orphan thread for removed node ' + IntToStr(Thread.FNodeId));
        Thread.OnTerminate := nil;
        Thread.FreeOnTerminate := True;
        Thread.Terminate;
        FPingThreads.Delete(I);
      end;
    end;

    for I := 0 to High(FNodes) do
    begin
      IdStillActive := IdInArray(FNodes[I].Id, NewActiveIds);
      IdNewlyActive := IdInArray(FNodes[I].Id, NewActiveIds) and not IdInArray(FNodes[I].Id, OldActiveIds);

      ThreadIdx := FindThreadIndexByNodeId(FNodes[I].Id);

      if IdNewlyActive then
      begin
        // Узел стал активным - запускаем поток
        NodeDebugLog('Starting new thread for node ' + IntToStr(FNodes[I].Id));
        StartThreadInternal(I);
        Inc(ActiveCount);
      end
      else if IdStillActive and (ThreadIdx < 0) then
      begin
        // Поток отсутствует, но должен быть - запускаем
        NodeDebugLog('Recreating thread for node ' + IntToStr(FNodes[I].Id));
        StartThreadInternal(I);
        Inc(ActiveCount);
      end
      else if not IdStillActive and (ThreadIdx >= 0) then
      begin
        // Узел стал неактивным - останавливаем поток
        NodeDebugLog('Stopping thread for node ' + IntToStr(FNodes[I].Id));
        Thread := TPingThread(FPingThreads[ThreadIdx]);
        Thread.OnTerminate := nil;  // handler уже не нужен — мы сами удаляем из списка
        Thread.FreeOnTerminate := True;  // освободит себя сам после завершения Execute
        Thread.Terminate;
        FPingThreads.Delete(ThreadIdx);
      end;
    end;

    NodeDebugLog('ApplyConfig completed, active threads: ' + IntToStr(FPingThreads.Count));
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  FModified := False;
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

procedure TNodeManager.MarkModified;
begin
  FModified := True;
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
var
  Thread: TPingThread;
begin
  if (Index < 0) or (Index > High(FNodes)) then Exit;

  Thread := TPingThread.Create(Self, FNodes[Index]);
  Thread.Start;
  // Вызывается только пока FCriticalSection захвачена вызывающим кодом
  FPingThreads.Add(Thread);
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
var
  FullCmd, Cmd, Params, Ext: string;
  Process: TProcess;
  StatusStr: string;
  SpacePos: Integer;
begin
  if Trim(Node.Command) = '' then Exit;

  FullCmd := Trim(Node.Command);

  // Подставляем макросы
  case Node.State of
    nsUp: StatusStr := 'up';
    nsDown: StatusStr := 'down';
  else
    StatusStr := 'unknown';
  end;

  FullCmd := StringReplace(FullCmd, '{host}', Node.Host, [rfReplaceAll, rfIgnoreCase]);
  FullCmd := StringReplace(FullCmd, '{name}', Node.Name, [rfReplaceAll, rfIgnoreCase]);
  FullCmd := StringReplace(FullCmd, '{status}', StatusStr, [rfReplaceAll, rfIgnoreCase]);
  FullCmd := StringReplace(FullCmd, '{group}', GroupName, [rfReplaceAll, rfIgnoreCase]);
  FullCmd := StringReplace(FullCmd, '{ping}', IntToStr(Node.LastPingMs), [rfReplaceAll, rfIgnoreCase]);

  // Разделяем на исполняемый файл и параметры (первый пробел = разделитель)
  SpacePos := Pos(' ', FullCmd);
  if SpacePos > 0 then
  begin
    Cmd := Trim(Copy(FullCmd, 1, SpacePos - 1));
    Params := Trim(Copy(FullCmd, SpacePos + 1, Length(FullCmd)));
  end
  else
  begin
    Cmd := FullCmd;
    Params := '';
  end;

  // Определяем тип файла
  Ext := LowerCase(ExtractFileExt(Cmd));
  
  Process := TProcess.Create(nil);
  try
    Process.Options := [poWaitOnExit];
    Process.ShowWindow := swoHide;

    if Ext = '.ps1' then
    begin
      // PowerShell
      Process.Executable := 'powershell.exe';
      Process.Parameters.Add('-NoProfile');
      Process.Parameters.Add('-ExecutionPolicy');
      Process.Parameters.Add('Bypass');
      Process.Parameters.Add('-File');
      Process.Parameters.Add('"' + Cmd + '"');
      if Params <> '' then
        Process.Parameters.Add(Params);
    end
    else if (Ext = '.cmd') or (Ext = '.bat') then
    begin
      // Batch
      Process.Executable := 'cmd.exe';
      Process.Parameters.Add('/C');
      Process.Parameters.Add('"' + Cmd + '"');
      if Params <> '' then
        Process.Parameters.Add(Params);
    end
    else
    begin
      // exe или другой
      Process.Executable := Cmd;
      if Params <> '' then
        Process.Parameters.Add(Params);
    end;

    NodeDebugLog('Executing: ' + Process.Executable + ' ' + Params);
    try
      Process.Execute;
      NodeDebugLog('Command exited with code: ' + IntToStr(Process.ExitStatus));
    except
      on E: Exception do
        NodeDebugLog('Command execution failed: ' + E.Message);
    end;
  finally
    Process.Free;
  end;
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
  OnTerminate := @OnTerminateHandler;
end;

procedure TPingThread.OnTerminateHandler(Sender: TObject);
var
  I: Integer;
begin
  if FNodeManager <> nil then
  begin
    EnterCriticalSection(FNodeManager.FCriticalSection);
    try
      for I := FNodeManager.FPingThreads.Count - 1 downto 0 do
      begin
        if TPingThread(FNodeManager.FPingThreads[I]) = Self then
        begin
          FNodeManager.FPingThreads.Delete(I);
          Break;
        end;
      end;
    finally
      LeaveCriticalSection(FNodeManager.FCriticalSection);
    end;
  end;
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
  LocalNodeManager: TNodeManager;
begin
  // Сохраняем ссылку локально ДО использования (защита от гонки)
  LocalNodeManager := FNodeManager;
  if LocalNodeManager = nil then Exit;

  try
    Result := PingHost(FHost, FTimeoutMs);

    if Result.Success then
      NewState := nsUp
    else
      NewState := nsDown;

    // Используем локальную копию — она не изменится даже если поток будет уничтожен
    if not Terminated then
      LocalNodeManager.UpdateNodeState(FNodeId, NewState);
  except
    on E: Exception do
    begin
      if not Terminated then
        LocalNodeManager.UpdateNodeState(FNodeId, nsDown);
    end;
  end;
end;

end.
