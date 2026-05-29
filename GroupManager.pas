unit GroupManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ConfigManager, NodeManager;

type
  TGroupInfo = record
    Id: integer;
    Name: string;
    GroupType: ConfigManager.TGroupType;
    NodeIds: array of integer;
    NodeStates: array of ConfigManager.TNodeState;
  end;

  TGroupArray = array of TGroupInfo;

  TGroupManager = class
  private
    FGroups: TGroupArray;
    FNodeManager: TNodeManager;
    FCriticalSection: TRTLCriticalSection;
    FLastExecutedStates: array of array of ConfigManager.TNodeState;
    // Для отслеживания выполненных команд
    FStatesInitialized: boolean;
    // Состояния посеяны после ApplyConfig (до этого изменения не считаем)
    procedure OnNodeStateChange(Sender: TObject);
    procedure UpdateGroupStates(RunCommands: boolean = True);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ApplyConfig(const Groups: array of TNodeGroup; NodeManager: TNodeManager);
    function GetGroups: TGroupArray;
    function GetGroupById(Id: integer): TGroupInfo;
  end;

implementation

uses Windows;

procedure GroupDebugLog(const Msg: string);
begin
  OutputDebugString(PChar('multiPingLed[GroupMgr]: ' + Msg));
end;

constructor TGroupManager.Create;
begin
  inherited Create;
  InitCriticalSection(FCriticalSection);
  SetLength(FGroups, 0);
  FStatesInitialized := False;
end;

destructor TGroupManager.Destroy;
begin
  if FNodeManager <> nil then
    FNodeManager.OnStateChange := nil;
  DoneCriticalsection(FCriticalSection);
  inherited Destroy;
end;

procedure TGroupManager.ApplyConfig(const Groups: array of TNodeGroup;
  NodeManager: TNodeManager);
var
  I, J: integer;
  GroupCount: integer;
  ArraySize: integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    if FNodeManager <> nil then
      FNodeManager.OnStateChange := nil;

    FNodeManager := NodeManager;

    GroupCount := Length(Groups);
    SetLength(FGroups, GroupCount);

    for I := 0 to GroupCount - 1 do
    begin
      FGroups[I].Id := Groups[I].Id;
      FGroups[I].Name := Groups[I].Name;
      FGroups[I].GroupType := Groups[I].GroupType;

      // Set array size based on actual NodeIds count from config
      ArraySize := Length(Groups[I].NodeIds);
      if ArraySize = 0 then
      begin
        // Fallback to defaults if empty
        case Groups[I].GroupType of
          gtSingle: ArraySize := 1;
          gt2x2: ArraySize := 4;
          gt3x3: ArraySize := 9;
          else
            ArraySize := 9;
        end;
      end;

      SetLength(FGroups[I].NodeIds, ArraySize);
      SetLength(FGroups[I].NodeStates, ArraySize);

      for J := 0 to ArraySize - 1 do
      begin
        if J < Length(Groups[I].NodeIds) then
          FGroups[I].NodeIds[J] := Groups[I].NodeIds[J]
        else
          FGroups[I].NodeIds[J] := 0;
        FGroups[I].NodeStates[J] := nsUnknown;
      end;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  // Update states OUTSIDE critical section to avoid deadlock (UpdateGroupStates re-enters CS)
  // RunCommands=False: при применении конфига только ПОСЕВ текущих состояний в
  // FLastExecutedStates, без запуска команд — иначе стартовое заполнение
  // (и любое «Применить») считалось бы изменением и дёргало все команды.
  UpdateGroupStates(False);

  EnterCriticalSection(FCriticalSection);
  try
    if FNodeManager <> nil then
      FNodeManager.OnStateChange := @OnNodeStateChange;
    GroupDebugLog('ApplyConfig completed');
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TGroupManager.OnNodeStateChange(Sender: TObject);
begin
  GroupDebugLog('Node state changed');
  UpdateGroupStates(True);
end;

procedure TGroupManager.UpdateGroupStates(RunCommands: boolean);
type
  TCommandEntry = record
    NodeId: integer;
    GroupName: string;
  end;
var
  I, J: integer;
  NodeInfo: TNodeConfig;
  ArraySize: integer;
  Commands: array of TCommandEntry;
  CmdCount: integer;
  OldState, NewState: ConfigManager.TNodeState;
  CanCompare: boolean;
begin
  if FNodeManager = nil then Exit;

  CmdCount := 0;
  SetLength(Commands, 0);

  // Всё под ОДНОЙ блокировкой: обновляем состояния групп, формируем список
  // изменившихся узлов и сразу же фиксируем новые состояния в FLastExecutedStates.
  // Это устраняет гонку «прочитал старое — обновил новое» между вызовами.
  // GetNodeById берёт NodeManager.CS (порядок GroupMgr.CS→NodeMgr.CS); обратный
  // путь UpdateNodeState освобождает NodeMgr.CS ДО OnNodeStateChange, поэтому
  // deadlock не возникает. Команды (ExecuteNodeCommand, запуск процессов)
  // запускаем уже ПОСЛЕ выхода из CS.
  CanCompare := RunCommands and FStatesInitialized;

  EnterCriticalSection(FCriticalSection);
  try
    for I := 0 to High(FGroups) do
    begin
      ArraySize := Length(FGroups[I].NodeIds);
      for J := 0 to ArraySize - 1 do
      begin
        if FGroups[I].NodeIds[J] > 0 then
        begin
          NodeInfo := FNodeManager.GetNodeById(FGroups[I].NodeIds[J]);
          NewState := NodeInfo.State;
        end
        else
          NewState := nsUnknown;

        // Команду запускаем только при реальном переходе между up/down.
        // Пропускаем первичную инициализацию и переходы в/из nsUnknown.
        if CanCompare and (FGroups[I].NodeIds[J] > 0) and
          (I < Length(FLastExecutedStates)) and
          (J < Length(FLastExecutedStates[I])) then
        begin
          OldState := FLastExecutedStates[I][J];
          if (NewState <> OldState) and (NewState <> nsUnknown) and
            (OldState <> nsUnknown) then
          begin
            SetLength(Commands, CmdCount + 1);
            Commands[CmdCount].NodeId := FGroups[I].NodeIds[J];
            Commands[CmdCount].GroupName := FGroups[I].Name;
            Inc(CmdCount);
          end;
        end;

        FGroups[I].NodeStates[J] := NewState;
      end;
    end;

    // Фиксируем новые состояния согласованно (под той же блокировкой)
    SetLength(FLastExecutedStates, Length(FGroups));
    for I := 0 to High(FGroups) do
    begin
      SetLength(FLastExecutedStates[I], Length(FGroups[I].NodeStates));
      for J := 0 to High(FGroups[I].NodeStates) do
        FLastExecutedStates[I][J] := FGroups[I].NodeStates[J];
    end;
    FStatesInitialized := True;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  // Запускаем команды для изменившихся узлов уже вне CS
  for I := 0 to CmdCount - 1 do
  begin
    NodeInfo := FNodeManager.GetNodeById(Commands[I].NodeId);
    if NodeInfo.Id = 0 then Continue;
    if Trim(NodeInfo.Command) = '' then Continue;
    GroupDebugLog('Executing command for node ' + IntToStr(NodeInfo.Id) +
      ' (' + NodeInfo.Name + ') state changed');
    FNodeManager.ExecuteNodeCommand(NodeInfo, Commands[I].GroupName);
  end;
end;

function TGroupManager.GetGroups: TGroupArray;
begin
  EnterCriticalSection(FCriticalSection);
  try
    Result := FGroups;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TGroupManager.GetGroupById(Id: integer): TGroupInfo;
var
  I: integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    for I := 0 to High(FGroups) do
    begin
      if FGroups[I].Id = Id then
      begin
        Result := FGroups[I];
        Exit;
      end;
    end;
    Result := Default(TGroupInfo);
    // запись содержит managed-поля — не FillChar
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

end.
