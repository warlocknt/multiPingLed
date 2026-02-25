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
    
    UpdateGroupStates;
    
    if FNodeManager <> nil then
      FNodeManager.OnStateChange := @OnNodeStateChange;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TGroupManager.OnNodeStateChange(Sender: TObject);
begin
  UpdateGroupStates;
end;

procedure TGroupManager.UpdateGroupStates;
var
  I, J: Integer;
  NodeInfo: TNodeConfig;
  ArraySize: Integer;
begin
  if FNodeManager = nil then Exit;
  
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
