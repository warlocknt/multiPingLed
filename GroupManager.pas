unit GroupManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ConfigManager, NodeManager;

type
  TGroupInfo = record
    Id: Integer;
    Name: string;
    GroupType: ConfigManager.TGroupType;
    NodeIds: array of Integer;
    NodeStates: array of ConfigManager.TNodeState;
  end;
  
  TGroupArray = array of TGroupInfo;
  
  TGroupManager = class
  private
    FGroups: TGroupArray;
    FNodeManager: TNodeManager;
    FCriticalSection: TRTLCriticalSection;
    FLastExecutedStates: array of array of ConfigManager.TNodeState;  // Для отслеживания выполненных команд
    FLastGroupNames: array of string;  // Имена групп для команд
    procedure OnNodeStateChange(Sender: TObject);
    procedure UpdateGroupStates;
    procedure ExecuteCommandsForChangedStates;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ApplyConfig(const Groups: array of TNodeGroup; NodeManager: TNodeManager);
    function GetGroups: TGroupArray;
    function GetGroupById(Id: Integer): TGroupInfo;
  end;

implementation

uses Windows, Process;

procedure GroupDebugLog(const Msg: string);
begin
  OutputDebugString(PChar('multiPingLed[GroupMgr]: ' + Msg));
end;

constructor TGroupManager.Create;
begin
  inherited Create;
  InitCriticalSection(FCriticalSection);
  SetLength(FGroups, 0);
end;

destructor TGroupManager.Destroy;
begin
  if FNodeManager <> nil then
    FNodeManager.OnStateChange := nil;
  DoneCriticalsection(FCriticalSection);
  inherited Destroy;
end;

procedure TGroupManager.ApplyConfig(const Groups: array of TNodeGroup; NodeManager: TNodeManager);
var
  I, J: Integer;
  GroupCount: Integer;
  ArraySize: Integer;
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
  UpdateGroupStates;

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
  UpdateGroupStates;
end;

procedure TGroupManager.ExecuteCommandsForChangedStates;
type
  TCommandEntry = record
    NodeId: Integer;
    GroupName: string;
  end;
var
  I, J: Integer;
  Node: TNodeConfig;
  Commands: array of TCommandEntry;
  CmdCount: Integer;
begin
  if FNodeManager = nil then Exit;

  CmdCount := 0;
  SetLength(Commands, 0);

  // Собираем ID изменившихся узлов под CS.
  // GetNodeById вызывать здесь нельзя: он захватывает NodeManager.CS, тогда как
  // существующий путь NodeManager.CS→GroupManager.CS (UpdateNodeState→OnNodeStateChange)
  // создал бы обратный порядок захвата и deadlock.
  EnterCriticalSection(FCriticalSection);
  try
    for I := 0 to High(FGroups) do
      for J := 0 to High(FGroups[I].NodeIds) do
      begin
        if FGroups[I].NodeIds[J] <= 0 then Continue;

        if (I < Length(FLastExecutedStates)) and (J < Length(FLastExecutedStates[I])) then
          if FGroups[I].NodeStates[J] = FLastExecutedStates[I][J] then
            Continue;

        SetLength(Commands, CmdCount + 1);
        Commands[CmdCount].NodeId := FGroups[I].NodeIds[J];
        Commands[CmdCount].GroupName := FGroups[I].Name;
        Inc(CmdCount);
      end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  // Разрешаем узлы и запускаем команды уже вне CS
  for I := 0 to CmdCount - 1 do
  begin
    Node := FNodeManager.GetNodeById(Commands[I].NodeId);
    if Node.Id = 0 then Continue;
    if Trim(Node.Command) = '' then Continue;
    GroupDebugLog('Executing command for node ' + IntToStr(Node.Id) + ' (' + Node.Name + ') state changed');
    FNodeManager.ExecuteNodeCommand(Node, Commands[I].GroupName);
  end;
end;

procedure TGroupManager.UpdateGroupStates;
var
  I, J: Integer;
  NodeInfo: TNodeConfig;
  ArraySize: Integer;
begin
  if FNodeManager = nil then Exit;

  // Обновляем текущие состояния узлов в группах
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
          FGroups[I].NodeStates[J] := NodeInfo.State;
        end
        else
        begin
          FGroups[I].NodeStates[J] := nsUnknown;
        end;
      end;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  // Выполняем команды пока FLastExecutedStates содержит СТАРЫЕ состояния —
  // только так сравнение обнаружит изменение и сработает команда
  ExecuteCommandsForChangedStates;

  // Обновляем сохранённые состояния уже ПОСЛЕ выполнения команд
  EnterCriticalSection(FCriticalSection);
  try
    SetLength(FLastExecutedStates, Length(FGroups));
    SetLength(FLastGroupNames, Length(FGroups));
    for I := 0 to High(FGroups) do
    begin
      SetLength(FLastExecutedStates[I], Length(FGroups[I].NodeStates));
      for J := 0 to High(FGroups[I].NodeStates) do
        FLastExecutedStates[I][J] := FGroups[I].NodeStates[J];
      FLastGroupNames[I] := FGroups[I].Name;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
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

function TGroupManager.GetGroupById(Id: Integer): TGroupInfo;
var
  I: Integer;
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
    FillChar(Result, SizeOf(Result), 0);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

end.
