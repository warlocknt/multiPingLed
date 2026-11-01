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
    procedure OnNodeStateChange(Sender: TObject);
    procedure UpdateGroupStates;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ApplyConfig(const Groups: array of TNodeGroup; NodeManager: TNodeManager);
    function GetGroups: TGroupArray;
    function GetGroupById(Id: Integer): TGroupInfo;
  end;

implementation

uses LogWriter, Windows;

procedure GroupDebugLog(const Msg: string);
begin
  ApplicationLog('multiPingLed[GroupMgr]: ' + Msg);
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
  ArraySize, K, L: Integer;
  OldGroups: TGroupArray;
  OldStates: array of array of TNodeState;
begin
  if FNodeManager <> nil then FNodeManager.OnStateChange := nil;
  EnterCriticalSection(FCriticalSection);
  try
    FNodeManager := NodeManager;

    OldGroups := FGroups;
    OldStates := FLastExecutedStates;
    FGroups := nil;
    FLastExecutedStates := nil;
    GroupCount := Length(Groups);
    SetLength(FGroups, GroupCount);
    SetLength(FLastExecutedStates, GroupCount);

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
      SetLength(FLastExecutedStates[I], ArraySize);

      for J := 0 to ArraySize - 1 do
      begin
        if J < Length(Groups[I].NodeIds) then
          FGroups[I].NodeIds[J] := Groups[I].NodeIds[J]
        else
          FGroups[I].NodeIds[J] := 0;
        FGroups[I].NodeStates[J] := nsUnknown;
        for K := 0 to High(OldGroups) do
          if OldGroups[K].Id = FGroups[I].Id then
            for L := 0 to High(OldGroups[K].NodeIds) do
              if (OldGroups[K].NodeIds[L] = FGroups[I].NodeIds[J]) and
                (K < Length(OldStates)) and (L < Length(OldStates[K])) then
                FLastExecutedStates[I][J] := OldStates[K][L];
      end;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;

  // Subscription refreshes under NodeManager's event lock, closing the gap
  // between the initial snapshot and the first worker notification.
  if FNodeManager <> nil then FNodeManager.OnStateChange := @OnNodeStateChange;
end;

procedure TGroupManager.OnNodeStateChange(Sender: TObject);
begin
  GroupDebugLog('Node state changed');
  UpdateGroupStates;
end;

procedure TGroupManager.UpdateGroupStates;
var I, J: Integer; Node: TNodeConfig;
begin
  if FNodeManager = nil then Exit;
  EnterCriticalSection(FCriticalSection);
  try
    SetLength(FLastExecutedStates, Length(FGroups));
    for I := 0 to High(FGroups) do
    begin
      SetLength(FLastExecutedStates[I], Length(FGroups[I].NodeIds));
      for J := 0 to High(FGroups[I].NodeIds) do
      begin
        Node := FNodeManager.GetNodeById(FGroups[I].NodeIds[J]);
        FGroups[I].NodeStates[J] := Node.State;
        if Node.Id > 0 then
          if FLastExecutedStates[I][J] <> Node.State then
          begin
            FLastExecutedStates[I][J] := Node.State;
            if Node.State <> nsUnknown then
              FNodeManager.ExecuteNodeCommand(Node, FGroups[I].Name);
          end;
      end;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TGroupManager.GetGroups: TGroupArray;
var I: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  try
    Result := Copy(FGroups, 0, Length(FGroups));
    for I := 0 to High(Result) do
    begin
      Result[I].NodeIds := Copy(FGroups[I].NodeIds, 0, Length(FGroups[I].NodeIds));
      Result[I].NodeStates := Copy(FGroups[I].NodeStates, 0, Length(FGroups[I].NodeStates));
    end;
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
        Result.NodeIds := Copy(FGroups[I].NodeIds, 0, Length(FGroups[I].NodeIds));
        Result.NodeStates := Copy(FGroups[I].NodeStates, 0, Length(FGroups[I].NodeStates));
        Exit;
      end;
    end;
    Result := Default(TGroupInfo);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

end.
