unit ConfigManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TNodeState = (nsUnknown, nsUp, nsDown);
  
  TNodeConfig = record
    Id: Integer;
    Name: string;
    Host: string;
    IntervalMs: Integer;
    TimeoutMs: Integer;
    State: TNodeState;
    LastPingTime: TDateTime;  // Время последнего изменения статуса
  end;
  
  TGroupType = (gtSingle, gt2x2, gt3x3);
  
  TNodeGroup = record
    Id: Integer;
    Name: string;
    GroupType: TGroupType;
    NodeIds: array of Integer;
  end;
  
  TAppConfig = record
    Version: string;
    Nodes: array of TNodeConfig;
    Groups: array of TNodeGroup;
  end;

  TConfigManager = class
  private
    FConfig: TAppConfig;
    FConfigPath: string;
    FConfigDir: string;
    function GetConfigFilePath: string;
    function NodeStateToStr(State: TNodeState): string;
    function StrToNodeState(const S: string): TNodeState;
    function GroupTypeToStr(GType: TGroupType): string;
    function StrToGroupType(const S: string): TGroupType;
  public
    constructor Create;
    function ConfigExists: Boolean;
    procedure LoadConfig;
    procedure SaveConfig;
    function GetConfig: TAppConfig;
    procedure SetConfig(const Config: TAppConfig);
    function ValidateConfig(out ErrorMsg: string): Boolean;
    function ImportConfig(const FileName: string; out ErrorMsg: string): Boolean;
    procedure ExportConfig(const FileName: string);
    
    // Language settings
    function GetLanguage: string;
    procedure SetLanguage(const LangCode: string);
  end;

implementation

constructor TConfigManager.Create;
begin
  inherited Create;
  FConfigPath := GetConfigFilePath;
  FConfigDir := ExtractFilePath(FConfigPath);
  FConfig.Version := '1.0';
  SetLength(FConfig.Nodes, 0);
  SetLength(FConfig.Groups, 0);
end;

function TConfigManager.GetConfigFilePath: string;
begin
  Result := GetEnvironmentVariable('APPDATA') + '\multiPingLed\config.ini';
end;

function TConfigManager.NodeStateToStr(State: TNodeState): string;
begin
  case State of
    nsUnknown: Result := 'unknown';
    nsUp: Result := 'up';
    nsDown: Result := 'down';
  end;
end;

function TConfigManager.StrToNodeState(const S: string): TNodeState;
begin
  if S = 'up' then Result := nsUp
  else if S = 'down' then Result := nsDown
  else Result := nsUnknown;
end;

function TConfigManager.GroupTypeToStr(GType: TGroupType): string;
begin
  case GType of
    gtSingle: Result := 'single';
    gt2x2: Result := '2x2';
    gt3x3: Result := '3x3';
  end;
end;

function TConfigManager.StrToGroupType(const S: string): TGroupType;
begin
  if S = '2x2' then Result := gt2x2
  else if S = '3x3' then Result := gt3x3
  else Result := gtSingle;
end;

function TConfigManager.ConfigExists: Boolean;
begin
  Result := FileExists(FConfigPath);
end;

procedure TConfigManager.LoadConfig;
var
  Ini: TIniFile;
  NodeCount, GroupCount: Integer;
  I, J: Integer;
  NodeIdsStr: string;
  TempIds: TStringList;
  PosComma: Integer;
begin
  if not FileExists(FConfigPath) then Exit;
  
  Ini := TIniFile.Create(FConfigPath);
  try
    FConfig.Version := Ini.ReadString('General', 'Version', '1.0');
    
    NodeCount := Ini.ReadInteger('General', 'NodeCount', 0);
    SetLength(FConfig.Nodes, NodeCount);
    
    for I := 0 to NodeCount - 1 do
    begin
      FConfig.Nodes[I].Id := Ini.ReadInteger('Node' + IntToStr(I), 'Id', I + 1);
      FConfig.Nodes[I].Name := Ini.ReadString('Node' + IntToStr(I), 'Name', '');
      FConfig.Nodes[I].Host := Ini.ReadString('Node' + IntToStr(I), 'Host', '');
      FConfig.Nodes[I].IntervalMs := Ini.ReadInteger('Node' + IntToStr(I), 'IntervalMs', 5000);
      FConfig.Nodes[I].TimeoutMs := Ini.ReadInteger('Node' + IntToStr(I), 'TimeoutMs', 2000);
      FConfig.Nodes[I].State := StrToNodeState(Ini.ReadString('Node' + IntToStr(I), 'State', 'unknown'));
    end;
    
    GroupCount := Ini.ReadInteger('General', 'GroupCount', 0);
    SetLength(FConfig.Groups, GroupCount);
    
    for I := 0 to GroupCount - 1 do
    begin
      FConfig.Groups[I].Id := Ini.ReadInteger('Group' + IntToStr(I), 'Id', I + 1);
      FConfig.Groups[I].Name := Ini.ReadString('Group' + IntToStr(I), 'Name', '');
      FConfig.Groups[I].GroupType := StrToGroupType(Ini.ReadString('Group' + IntToStr(I), 'Type', 'single'));
      
      NodeIdsStr := Ini.ReadString('Group' + IntToStr(I), 'NodeIds', '');
      
      // Parse NodeIds dynamically
      TempIds := TStringList.Create;
      try
        // Split by comma
        while NodeIdsStr <> '' do
        begin
          PosComma := Pos(',', NodeIdsStr);
          if PosComma > 0 then
          begin
            TempIds.Add(Trim(Copy(NodeIdsStr, 1, PosComma - 1)));
            Delete(NodeIdsStr, 1, PosComma);
          end
          else
          begin
            TempIds.Add(Trim(NodeIdsStr));
            NodeIdsStr := '';
          end;
        end;
        
        // Set dynamic array size and fill
        SetLength(FConfig.Groups[I].NodeIds, TempIds.Count);
        for J := 0 to TempIds.Count - 1 do
          FConfig.Groups[I].NodeIds[J] := StrToIntDef(TempIds[J], 0);
      finally
        TempIds.Free;
      end;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TConfigManager.SaveConfig;
var
  Ini: TIniFile;
  I, J: Integer;
  NodeIdsStr: string;
begin
  if not DirectoryExists(FConfigDir) then
    ForceDirectories(FConfigDir);
  
  Ini := TIniFile.Create(FConfigPath);
  try
    Ini.WriteString('General', 'Version', FConfig.Version);
    Ini.WriteInteger('General', 'NodeCount', Length(FConfig.Nodes));
    Ini.WriteInteger('General', 'GroupCount', Length(FConfig.Groups));
    
    for I := 0 to High(FConfig.Nodes) do
    begin
      Ini.WriteInteger('Node' + IntToStr(I), 'Id', FConfig.Nodes[I].Id);
      Ini.WriteString('Node' + IntToStr(I), 'Name', FConfig.Nodes[I].Name);
      Ini.WriteString('Node' + IntToStr(I), 'Host', FConfig.Nodes[I].Host);
      Ini.WriteInteger('Node' + IntToStr(I), 'IntervalMs', FConfig.Nodes[I].IntervalMs);
      Ini.WriteInteger('Node' + IntToStr(I), 'TimeoutMs', FConfig.Nodes[I].TimeoutMs);
      Ini.WriteString('Node' + IntToStr(I), 'State', NodeStateToStr(FConfig.Nodes[I].State));
    end;
    
    for I := 0 to High(FConfig.Groups) do
    begin
      Ini.WriteInteger('Group' + IntToStr(I), 'Id', FConfig.Groups[I].Id);
      Ini.WriteString('Group' + IntToStr(I), 'Name', FConfig.Groups[I].Name);
      Ini.WriteString('Group' + IntToStr(I), 'Type', GroupTypeToStr(FConfig.Groups[I].GroupType));
      
      NodeIdsStr := '';
      for J := 0 to High(FConfig.Groups[I].NodeIds) do
      begin
        if FConfig.Groups[I].NodeIds[J] > 0 then
        begin
          if NodeIdsStr <> '' then NodeIdsStr := NodeIdsStr + ',';
          NodeIdsStr := NodeIdsStr + IntToStr(FConfig.Groups[I].NodeIds[J]);
        end;
      end;
      Ini.WriteString('Group' + IntToStr(I), 'NodeIds', NodeIdsStr);
    end;
  finally
    Ini.Free;
  end;
end;

function TConfigManager.GetConfig: TAppConfig;
begin
  Result := FConfig;
end;

procedure TConfigManager.SetConfig(const Config: TAppConfig);
begin
  FConfig := Config;
end;

function TConfigManager.ValidateConfig(out ErrorMsg: string): Boolean;
var
  I, J, K: Integer;
  NodeCount: Integer;
  Hosts: TStringList;
  NodeIdFound: Boolean;
  GroupNodeCount: Integer;
  NodeIdList: TStringList;
begin
  Result := False;
  ErrorMsg := '';
  
  Hosts := TStringList.Create;
  NodeIdList := TStringList.Create;
  try
    NodeCount := Length(FConfig.Nodes);
    
    for I := 0 to NodeCount - 1 do
    begin
      if Trim(FConfig.Nodes[I].Host) = '' then
      begin
        ErrorMsg := Format('Node %d has empty host', [FConfig.Nodes[I].Id]);
        Exit;
      end;
      
      if Hosts.IndexOf(FConfig.Nodes[I].Host) >= 0 then
      begin
        ErrorMsg := Format('Duplicate host: %s', [FConfig.Nodes[I].Host]);
        Exit;
      end;
      Hosts.Add(FConfig.Nodes[I].Host);
      
      if FConfig.Nodes[I].IntervalMs <= 0 then
      begin
        ErrorMsg := Format('Node %d has invalid interval', [FConfig.Nodes[I].Id]);
        Exit;
      end;
      
      if FConfig.Nodes[I].TimeoutMs <= 0 then
      begin
        ErrorMsg := Format('Node %d has invalid timeout', [FConfig.Nodes[I].Id]);
        Exit;
      end;
    end;
    
    for I := 0 to High(FConfig.Groups) do
    begin
      GroupNodeCount := 0;
      NodeIdList.Clear;
      
      for J := 0 to High(FConfig.Groups[I].NodeIds) do
      begin
        if FConfig.Groups[I].NodeIds[J] > 0 then
        begin
          Inc(GroupNodeCount);
          
          NodeIdFound := False;
          for K := 0 to NodeCount - 1 do
          begin
            if FConfig.Nodes[K].Id = FConfig.Groups[I].NodeIds[J] then
            begin
              NodeIdFound := True;
              Break;
            end;
          end;
          
          if not NodeIdFound then
          begin
            ErrorMsg := Format('Group %d references non-existent node %d', 
              [FConfig.Groups[I].Id, FConfig.Groups[I].NodeIds[J]]);
            Exit;
          end;
          
          if NodeIdList.IndexOf(IntToStr(FConfig.Groups[I].NodeIds[J])) >= 0 then
          begin
            ErrorMsg := Format('Group %d has duplicate node %d', 
              [FConfig.Groups[I].Id, FConfig.Groups[I].NodeIds[J]]);
            Exit;
          end;
          NodeIdList.Add(IntToStr(FConfig.Groups[I].NodeIds[J]));
        end;
      end;
      
      if GroupNodeCount > 9 then
      begin
        ErrorMsg := Format('Group %d has more than 9 nodes', [FConfig.Groups[I].Id]);
        Exit;
      end;
    end;
    
    Result := True;
  finally
    Hosts.Free;
    NodeIdList.Free;
  end;
end;

function TConfigManager.ImportConfig(const FileName: string; out ErrorMsg: string): Boolean;
var
  TempConfig: TAppConfig;
  Ini: TIniFile;
  NodeCount, GroupCount: Integer;
  I, J: Integer;
  NodeIdsStr: string;
  OldConfig: TAppConfig;
begin
  Result := False;
  ErrorMsg := '';
  
  if not FileExists(FileName) then
  begin
    ErrorMsg := 'File not found';
    Exit;
  end;
  
  OldConfig := FConfig;
  
  try
    Ini := TIniFile.Create(FileName);
    try
      TempConfig.Version := Ini.ReadString('General', 'Version', '1.0');
      
      NodeCount := Ini.ReadInteger('General', 'NodeCount', 0);
      SetLength(TempConfig.Nodes, NodeCount);
      
      for I := 0 to NodeCount - 1 do
      begin
        TempConfig.Nodes[I].Id := Ini.ReadInteger('Node' + IntToStr(I), 'Id', I + 1);
        TempConfig.Nodes[I].Name := Ini.ReadString('Node' + IntToStr(I), 'Name', '');
        TempConfig.Nodes[I].Host := Ini.ReadString('Node' + IntToStr(I), 'Host', '');
        TempConfig.Nodes[I].IntervalMs := Ini.ReadInteger('Node' + IntToStr(I), 'IntervalMs', 5000);
        TempConfig.Nodes[I].TimeoutMs := Ini.ReadInteger('Node' + IntToStr(I), 'TimeoutMs', 2000);
        TempConfig.Nodes[I].State := StrToNodeState(Ini.ReadString('Node' + IntToStr(I), 'State', 'unknown'));
      end;
      
      GroupCount := Ini.ReadInteger('General', 'GroupCount', 0);
      SetLength(TempConfig.Groups, GroupCount);
      
      for I := 0 to GroupCount - 1 do
      begin
        TempConfig.Groups[I].Id := Ini.ReadInteger('Group' + IntToStr(I), 'Id', I + 1);
        TempConfig.Groups[I].Name := Ini.ReadString('Group' + IntToStr(I), 'Name', '');
        TempConfig.Groups[I].GroupType := StrToGroupType(Ini.ReadString('Group' + IntToStr(I), 'Type', 'single'));
        
        NodeIdsStr := Ini.ReadString('Group' + IntToStr(I), 'NodeIds', '');
        for J := 0 to High(FConfig.Groups[I].NodeIds) do
          TempConfig.Groups[I].NodeIds[J] := 0;
        
        J := 0;
        while (NodeIdsStr <> '') and (J < 9) do
        begin
          TempConfig.Groups[I].NodeIds[J] := StrToIntDef(Trim(Copy(NodeIdsStr, 1, Pos(',', NodeIdsStr + ',') - 1)), 0);
          if Pos(',', NodeIdsStr) > 0 then
            Delete(NodeIdsStr, 1, Pos(',', NodeIdsStr))
          else
            NodeIdsStr := '';
          Inc(J);
        end;
      end;
    finally
      Ini.Free;
    end;
    
    FConfig := TempConfig;
    
    if not ValidateConfig(ErrorMsg) then
    begin
      FConfig := OldConfig;
      Exit;
    end;
    
    SaveConfig;
    Result := True;
  except
    on E: Exception do
    begin
      FConfig := OldConfig;
      ErrorMsg := E.Message;
    end;
  end;
end;

procedure TConfigManager.ExportConfig(const FileName: string);
var
  Ini: TIniFile;
  I, J: Integer;
  NodeIdsStr: string;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteString('General', 'Version', FConfig.Version);
    Ini.WriteInteger('General', 'NodeCount', Length(FConfig.Nodes));
    Ini.WriteInteger('General', 'GroupCount', Length(FConfig.Groups));
    
    for I := 0 to High(FConfig.Nodes) do
    begin
      Ini.WriteInteger('Node' + IntToStr(I), 'Id', FConfig.Nodes[I].Id);
      Ini.WriteString('Node' + IntToStr(I), 'Name', FConfig.Nodes[I].Name);
      Ini.WriteString('Node' + IntToStr(I), 'Host', FConfig.Nodes[I].Host);
      Ini.WriteInteger('Node' + IntToStr(I), 'IntervalMs', FConfig.Nodes[I].IntervalMs);
      Ini.WriteInteger('Node' + IntToStr(I), 'TimeoutMs', FConfig.Nodes[I].TimeoutMs);
      Ini.WriteString('Node' + IntToStr(I), 'State', NodeStateToStr(FConfig.Nodes[I].State));
    end;
    
    for I := 0 to High(FConfig.Groups) do
    begin
      Ini.WriteInteger('Group' + IntToStr(I), 'Id', FConfig.Groups[I].Id);
      Ini.WriteString('Group' + IntToStr(I), 'Name', FConfig.Groups[I].Name);
      Ini.WriteString('Group' + IntToStr(I), 'Type', GroupTypeToStr(FConfig.Groups[I].GroupType));
      
      NodeIdsStr := '';
      for J := 0 to High(FConfig.Groups[I].NodeIds) do
      begin
        if FConfig.Groups[I].NodeIds[J] > 0 then
        begin
          if NodeIdsStr <> '' then NodeIdsStr := NodeIdsStr + ',';
          NodeIdsStr := NodeIdsStr + IntToStr(FConfig.Groups[I].NodeIds[J]);
        end;
      end;
      Ini.WriteString('Group' + IntToStr(I), 'NodeIds', NodeIdsStr);
    end;
  finally
    Ini.Free;
  end;
end;

function TConfigManager.GetLanguage: string;
var
  Ini: TIniFile;
begin
  Result := 'english'; // Default: English
  if not FileExists(FConfigPath) then Exit;

  Ini := TIniFile.Create(FConfigPath);
  try
    Result := Ini.ReadString('General', 'Language', 'english');
  finally
    Ini.Free;
  end;
end;

procedure TConfigManager.SetLanguage(const LangCode: string);
var
  Ini: TIniFile;
begin
  if not DirectoryExists(FConfigDir) then
    ForceDirectories(FConfigDir);

  Ini := TIniFile.Create(FConfigPath);
  try
    Ini.WriteString('General', 'Language', LangCode);
  finally
    Ini.Free;
  end;
end;

end.
