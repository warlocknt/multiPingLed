unit ConfigManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Windows;

var
  GlobalConfigPath: string = '';  // Глобальный путь к конфигу, устанавливается при старте приложения
  GlobalLangPath: string = '';    // Глобальный путь к языковым файлам

type
  TNodeState = (nsUnknown, nsUp, nsDown);
  
  TNodeConfig = record
    Id: Integer;
    Name: string;
    Host: string;
    IntervalMs: Integer;
    TimeoutMs: Integer;
    FailThreshold: Integer; // Zero means the default of three consecutive failures.
    NotificationsMuted: Boolean;
    LastStateChange: TDateTime;
    ConsecutiveFailures: Integer;
    ProbeCount, ProbeIndex, ProbeLosses: Integer;
    ProbeHistory: array[0..19] of Boolean; // True means a lost probe.
    State: TNodeState;
    LastPingTime: TDateTime;
    LastPingMs: Integer;
    LastPingError: string; // Runtime only; never persisted to INI.
    // Внешняя команда при смене статуса (единая строка: путь + параметры)
    Command: string;
    DownCommand: string;
    UpCommand: string;
  end;
  
  TGroupType = (gtSingle, gt2x2, gt3x3);
  
  TNodeGroup = record
    Id: Integer;
    Name: string;
    GroupType: TGroupType;
    NodeIds: array of Integer;
    Enabled: Boolean;
  end;
  
  TAppConfig = record
    Version: string;
    LoggingEnabled: Boolean;
    ComparePingEnabled: Boolean;
    CreateSessionReport: Boolean;
    Nodes: array of TNodeConfig;
    Groups: array of TNodeGroup;
  end;

  { TConfigManager }

  TConfigManager = class
  private
    FConfig: TAppConfig;
    FConfigPath: string;
    FConfigDir: string;
    FImportedLanguage: string;
    FImportedBalloon: Integer;
    function NodeStateToStr(State: TNodeState): string;
    function StrToNodeState(const S: string): TNodeState;
    function GroupTypeToStr(GType: TGroupType): string;
    function StrToGroupType(const S: string): TGroupType;
    function BoolToStrEx(Value: Boolean): string;
    function StrToBoolEx(const S: string): Boolean;

  public
    constructor Create(ConfigDir: string = '');
    function ConfigExists: Boolean;
    procedure LoadConfig;
    procedure SaveConfig(const Language: string = ''; BalloonHint: Integer = -1);
    procedure SetVersion(Ver: string);
    function GetVersion: string;
    function GetConfig: TAppConfig;
    procedure SetConfig(const Config: TAppConfig);
    function ValidateConfig(out ErrorMsg: string): Boolean;
    function ImportConfig(const FileName: string; out ErrorMsg: string): Boolean;
    procedure ExportConfig(const FileName: string; const Language: string = ''; BalloonHint: Integer = -1);
    
    // Language settings
    function GetLanguage: string;
    procedure SetLanguage(const LangCode: string);

    // Balloon hint settings
    function GetBalloonHintEnabled: Boolean;
    procedure SetBalloonHintEnabled(Value: Boolean);
  end;

function ResolveAppConfigPath: string; overload;
function ResolveAppConfigPath(const ExeDirectory, RoamingDirectory: string): string; overload;

function CloneConfig(const Config: TAppConfig): TAppConfig;

function SameEditableConfig(const A, B: TAppConfig): Boolean;

implementation

uses LogWriter, LangManager;

function ResolveAppConfigPath(const ExeDirectory, RoamingDirectory: string): string;
var LocalConfig: string;
begin
  LocalConfig := IncludeTrailingPathDelimiter(ExeDirectory) + 'config.ini';
  if FileExists(LocalConfig) then Exit(LocalConfig);
  Result := IncludeTrailingPathDelimiter(RoamingDirectory) + 'multiPingLed' + PathDelim + 'config.ini';
end;

function ResolveAppConfigPath: string;
begin
  Result := ResolveAppConfigPath(ExtractFilePath(ParamStr(0)), SysUtils.GetEnvironmentVariable('APPDATA'));
end;


function SameEditableConfig(const A, B: TAppConfig): Boolean;
var I, J: Integer;
begin
  Result := False;
  if (A.LoggingEnabled <> B.LoggingEnabled) or (A.ComparePingEnabled <> B.ComparePingEnabled) or
    (A.CreateSessionReport <> B.CreateSessionReport) or (Length(A.Nodes) <> Length(B.Nodes)) or
    (Length(A.Groups) <> Length(B.Groups)) then Exit;
  for I := 0 to High(A.Nodes) do
    if (A.Nodes[I].Id <> B.Nodes[I].Id) or (A.Nodes[I].Name <> B.Nodes[I].Name) or
      (A.Nodes[I].Host <> B.Nodes[I].Host) or (A.Nodes[I].IntervalMs <> B.Nodes[I].IntervalMs) or
      (A.Nodes[I].TimeoutMs <> B.Nodes[I].TimeoutMs) or (A.Nodes[I].FailThreshold <> B.Nodes[I].FailThreshold) or
      (A.Nodes[I].NotificationsMuted <> B.Nodes[I].NotificationsMuted) or
      (A.Nodes[I].Command <> B.Nodes[I].Command) or (A.Nodes[I].UpCommand <> B.Nodes[I].UpCommand) or
      (A.Nodes[I].DownCommand <> B.Nodes[I].DownCommand) then Exit;
  for I := 0 to High(A.Groups) do
  begin
    if (A.Groups[I].Id <> B.Groups[I].Id) or (A.Groups[I].Name <> B.Groups[I].Name) or
      (A.Groups[I].GroupType <> B.Groups[I].GroupType) or (A.Groups[I].Enabled <> B.Groups[I].Enabled) or
      (Length(A.Groups[I].NodeIds) <> Length(B.Groups[I].NodeIds)) then Exit;
    for J := 0 to High(A.Groups[I].NodeIds) do
      if A.Groups[I].NodeIds[J] <> B.Groups[I].NodeIds[J] then Exit;
  end;
  Result := True;
end;

function CloneConfig(const Config: TAppConfig): TAppConfig;
var I: Integer;
begin
  Result := Config;
  Result.Nodes := Copy(Config.Nodes, 0, Length(Config.Nodes));
  Result.Groups := Copy(Config.Groups, 0, Length(Config.Groups));
  for I := 0 to High(Result.Groups) do
    Result.Groups[I].NodeIds := Copy(Config.Groups[I].NodeIds, 0, Length(Config.Groups[I].NodeIds));
end;

procedure CheckCount(Value: Integer; const Kind: string);
begin
  if (Value < 0) or (Value > 4096) then
    raise ERangeError.Create(Format(_('error_count'), [_('count_' + Kind + 's')]));
end;

procedure ConfigDebugLog(const Msg: string);
begin
  ApplicationLog('multiPingLed[Config]: ' + Msg);
end;

constructor TConfigManager.Create(ConfigDir:string);
begin
  inherited Create;
  if ConfigDir <> '' then
    FConfigPath := ConfigDir
  else
    FConfigPath := GlobalConfigPath;
  FConfigDir := ExtractFilePath(FConfigPath);
  FImportedBalloon := -1;
  FConfig.Version := '0.0';
  SetLength(FConfig.Nodes, 0);
  SetLength(FConfig.Groups, 0);
  ConfigDebugLog('Created, path=' + FConfigPath);
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

function TConfigManager.BoolToStrEx(Value: Boolean): string;
begin
  Result := BoolToStr(Value, True);
end;

function TConfigManager.StrToBoolEx(const S: string): Boolean;
begin
 if not TryStrToBool(S, Result) then
    Result := False; // если строка некорректная, возвращаем False
end;

function TConfigManager.ConfigExists: Boolean;
begin
  Result := FileExists(FConfigPath);
end;

procedure TConfigManager.LoadConfig;
var ErrorMsg: string;
begin
  if not FileExists(FConfigPath) then Exit;
  if not ImportConfig(FConfigPath, ErrorMsg) then
    raise EInOutError.Create(ErrorMsg);
end;

procedure TConfigManager.SaveConfig(const Language: string; BalloonHint: Integer);
var
  Ini: TMemIniFile;
  I, J: Integer;
  NodeIdsStr, TempFile, ErrorMsg, DesiredLanguage: string;
  DesiredBalloon: Integer;
begin
  if FConfigPath = '' then raise EInOutError.Create(_('error_config_path'));
  if not ValidateConfig(ErrorMsg) then raise EInOutError.Create(ErrorMsg);
  ConfigDebugLog('Saving config to ' + FConfigPath);
  if (FConfigDir <> '') and not DirectoryExists(FConfigDir) then
    ForceDirectories(FConfigDir);
  
  TempFile := FConfigPath + '.tmp-' + IntToStr(GetCurrentProcessId) + '-' + IntToStr(GetTickCount64);
  Ini := nil;
  try
    Ini := TMemIniFile.Create(FConfigPath);
    Ini.Rename(TempFile, False);
    DesiredLanguage := Language;
    if DesiredLanguage = '' then DesiredLanguage := FImportedLanguage;
    DesiredBalloon := BalloonHint;
    if DesiredBalloon < 0 then DesiredBalloon := FImportedBalloon;
    if DesiredLanguage <> '' then Ini.WriteString('General', 'Language', DesiredLanguage);
    if DesiredBalloon >= 0 then
      Ini.WriteString('General', 'BalloonHintEnabled', BoolToStrEx(DesiredBalloon <> 0));
    Ini.WriteString('General', 'Version', FConfig.Version);
    Ini.WriteBool('General', 'LoggingEnabled', FConfig.LoggingEnabled);
    Ini.WriteBool('General', 'ComparePingEnabled', FConfig.ComparePingEnabled);
    Ini.WriteBool('General', 'CreateSessionReport', FConfig.CreateSessionReport);
    Ini.WriteInteger('General', 'NodeCount', Length(FConfig.Nodes));
    Ini.WriteInteger('General', 'GroupCount', Length(FConfig.Groups));
    
    for I := 0 to High(FConfig.Nodes) do
    begin
      Ini.WriteInteger('Node' + IntToStr(I), 'Id', FConfig.Nodes[I].Id);
      Ini.WriteString('Node' + IntToStr(I), 'Name', FConfig.Nodes[I].Name);
      Ini.WriteString('Node' + IntToStr(I), 'Host', FConfig.Nodes[I].Host);
      Ini.WriteInteger('Node' + IntToStr(I), 'IntervalMs', FConfig.Nodes[I].IntervalMs);
      Ini.WriteInteger('Node' + IntToStr(I), 'TimeoutMs', FConfig.Nodes[I].TimeoutMs);
      Ini.WriteInteger('Node' + IntToStr(I), 'FailThreshold', FConfig.Nodes[I].FailThreshold);
      Ini.WriteBool('Node' + IntToStr(I), 'NotificationsMuted', FConfig.Nodes[I].NotificationsMuted);
      Ini.WriteString('Node' + IntToStr(I), 'State', NodeStateToStr(FConfig.Nodes[I].State));
      Ini.WriteString('Node' + IntToStr(I), 'Command', FConfig.Nodes[I].Command);
      Ini.WriteString('Node' + IntToStr(I), 'DownCommand', FConfig.Nodes[I].DownCommand);
      Ini.WriteString('Node' + IntToStr(I), 'UpCommand', FConfig.Nodes[I].UpCommand);
    end;
    
    for I := 0 to High(FConfig.Groups) do
    begin
      Ini.WriteInteger('Group' + IntToStr(I), 'Id', FConfig.Groups[I].Id);
      Ini.WriteString('Group' + IntToStr(I), 'Name', FConfig.Groups[I].Name);
      Ini.WriteString('Group' + IntToStr(I), 'Type', GroupTypeToStr(FConfig.Groups[I].GroupType));
      Ini.WriteString('Group' + IntToStr(I), 'Enabled', BoolToStrEx(FConfig.Groups[I].Enabled));

      
      NodeIdsStr := '';
      for J := 0 to High(FConfig.Groups[I].NodeIds) do
      begin
        if J > 0 then NodeIdsStr := NodeIdsStr + ',';
        NodeIdsStr := NodeIdsStr + IntToStr(FConfig.Groups[I].NodeIds[J]);
      end;
      Ini.WriteString('Group' + IntToStr(I), 'NodeIds', NodeIdsStr);
    end;
    Ini.UpdateFile;
    FreeAndNil(Ini);
    if not MoveFileExW(PWideChar(UTF8Decode(TempFile)), PWideChar(UTF8Decode(FConfigPath)),
      MOVEFILE_REPLACE_EXISTING or $00000008 { MOVEFILE_WRITE_THROUGH }) then RaiseLastOSError;
    FImportedLanguage := '';
    FImportedBalloon := -1;
  finally
    Ini.Free;
    if FileExists(TempFile) then SysUtils.DeleteFile(TempFile);
  end;
end;

procedure TConfigManager.SetVersion(Ver: string);
begin
  FConfig.Version := Ver;
end;

function TConfigManager.GetVersion: string;
begin
  Result := FConfig.Version;
end;

function TConfigManager.GetConfig: TAppConfig;
begin
  Result := CloneConfig(FConfig);
end;

procedure TConfigManager.SetConfig(const Config: TAppConfig);
begin
  FConfig := CloneConfig(Config);
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
    if (NodeCount > 4096) or (Length(FConfig.Groups) > 4096) then
    begin
      ErrorMsg := _('error_too_many');
      Exit;
    end;
    
    for I := 0 to NodeCount - 1 do
    begin
      if FConfig.Nodes[I].Id <= 0 then
      begin
        ErrorMsg := _('error_node_id_positive'); Exit;
      end;
      for K := 0 to I - 1 do
        if FConfig.Nodes[K].Id = FConfig.Nodes[I].Id then
        begin
          ErrorMsg := _('error_duplicate_node_id'); Exit;
        end;
      if Trim(FConfig.Nodes[I].Host) = '' then
      begin
        ErrorMsg := Format(_('error_node_empty_host'), [FConfig.Nodes[I].Id]);
        Exit;
      end;
      
      if Hosts.IndexOf(FConfig.Nodes[I].Host) >= 0 then
      begin
        ErrorMsg := Format(_('error_duplicate_host'), [FConfig.Nodes[I].Host]);
        Exit;
      end;
      Hosts.Add(FConfig.Nodes[I].Host);
      
      if FConfig.Nodes[I].IntervalMs <= 0 then
      begin
        ErrorMsg := Format(_('error_node_interval'), [FConfig.Nodes[I].Id]);
        Exit;
      end;
      
      if (FConfig.Nodes[I].FailThreshold < 0) or (FConfig.Nodes[I].FailThreshold > 20) then
      begin
        ErrorMsg := _('error_failure_range');
        Exit(False);
      end;
      if (FConfig.Nodes[I].TimeoutMs <= 0) or (FConfig.Nodes[I].TimeoutMs > 60000) then
      begin
        ErrorMsg := Format(_('error_node_timeout'), [FConfig.Nodes[I].Id]);
        Exit;
      end;
    end;
    
    for I := 0 to High(FConfig.Groups) do
    begin
      if FConfig.Groups[I].Id <= 0 then
      begin
        ErrorMsg := _('error_group_id_positive'); Exit;
      end;
      for K := 0 to I - 1 do
        if FConfig.Groups[K].Id = FConfig.Groups[I].Id then
        begin
          ErrorMsg := _('error_duplicate_group_id'); Exit;
        end;
      if Length(FConfig.Groups[I].NodeIds) > 9 then
      begin
        ErrorMsg := _('error_group_slots'); Exit;
      end;
      GroupNodeCount := 0;
      NodeIdList.Clear;
      
      for J := 0 to High(FConfig.Groups[I].NodeIds) do
      begin
        if FConfig.Groups[I].NodeIds[J] < 0 then
        begin
          ErrorMsg := _('error_negative_node_id'); Exit;
        end;
        if FConfig.Groups[I].NodeIds[J] > 0 then
        begin
          if ((FConfig.Groups[I].GroupType = gtSingle) and (J >= 1)) or
            ((FConfig.Groups[I].GroupType = gt2x2) and (J >= 4)) then
          begin
            ErrorMsg := Format(_('error_group_node_limit'), [FConfig.Groups[I].Id]);
            Exit;
          end;
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
            ErrorMsg := Format(_('error_group_missing_node'), 
              [FConfig.Groups[I].Id, FConfig.Groups[I].NodeIds[J]]);
            Exit;
          end;
          
          if NodeIdList.IndexOf(IntToStr(FConfig.Groups[I].NodeIds[J])) >= 0 then
          begin
            ErrorMsg := Format(_('error_group_duplicate_node'), 
              [FConfig.Groups[I].Id, FConfig.Groups[I].NodeIds[J]]);
            Exit;
          end;
          NodeIdList.Add(IntToStr(FConfig.Groups[I].NodeIds[J]));
        end;
      end;
      
      if ((FConfig.Groups[I].GroupType = gtSingle) and (GroupNodeCount > 1)) or
        ((FConfig.Groups[I].GroupType = gt2x2) and (GroupNodeCount > 4)) or
        (GroupNodeCount > 9) then
      begin
        ErrorMsg := Format(_('error_group_node_limit'), [FConfig.Groups[I].Id]);
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
  ImportedLanguage: string;
  ImportedBalloon: Integer;
begin
  Result := False;
  ErrorMsg := '';
  
  if not FileExists(FileName) then
  begin
    ErrorMsg := _('error_file_not_found');
    Exit;
  end;
  
  OldConfig := FConfig;
  
  try
    Ini := TIniFile.Create(FileName);
    try
      ImportedLanguage := Ini.ReadString('General', 'Language', '');
      ImportedBalloon := -1;
      if Ini.ValueExists('General', 'BalloonHintEnabled') then
        ImportedBalloon := Ord(StrToBoolEx(Ini.ReadString('General', 'BalloonHintEnabled', 'True')));
      NodeCount := Ini.ReadInteger('General', 'NodeCount', 0);
      CheckCount(NodeCount, 'node');
      TempConfig.Version := Ini.ReadString('General', 'Version', FConfig.Version);
      TempConfig.LoggingEnabled := Ini.ReadBool('General', 'LoggingEnabled', False);
      TempConfig.ComparePingEnabled := Ini.ReadBool('General', 'ComparePingEnabled', False);
      TempConfig.CreateSessionReport := Ini.ReadBool('General', 'CreateSessionReport', False);
      SetLength(TempConfig.Nodes, NodeCount);
      
      for I := 0 to NodeCount - 1 do
      begin
        TempConfig.Nodes[I].Id := Ini.ReadInteger('Node' + IntToStr(I), 'Id', I + 1);
        TempConfig.Nodes[I].Name := Ini.ReadString('Node' + IntToStr(I), 'Name', '');
        TempConfig.Nodes[I].Host := Ini.ReadString('Node' + IntToStr(I), 'Host', '');
        TempConfig.Nodes[I].IntervalMs := Ini.ReadInteger('Node' + IntToStr(I), 'IntervalMs', 5000);
        TempConfig.Nodes[I].TimeoutMs := Ini.ReadInteger('Node' + IntToStr(I), 'TimeoutMs', 2000);
        TempConfig.Nodes[I].FailThreshold := Ini.ReadInteger('Node' + IntToStr(I), 'FailThreshold', 3);
        TempConfig.Nodes[I].NotificationsMuted := Ini.ReadBool('Node' + IntToStr(I), 'NotificationsMuted', False);
        TempConfig.Nodes[I].State := StrToNodeState(Ini.ReadString('Node' + IntToStr(I), 'State', 'unknown'));
        TempConfig.Nodes[I].Command := Ini.ReadString('Node' + IntToStr(I), 'Command', '');
        TempConfig.Nodes[I].DownCommand := Ini.ReadString('Node' + IntToStr(I), 'DownCommand', '');
        TempConfig.Nodes[I].UpCommand := Ini.ReadString('Node' + IntToStr(I), 'UpCommand', '');
      end;
      
      GroupCount := Ini.ReadInteger('General', 'GroupCount', 0);
      CheckCount(GroupCount, 'group');
      SetLength(TempConfig.Groups, GroupCount);
      
      for I := 0 to GroupCount - 1 do
      begin
        TempConfig.Groups[I].Id := Ini.ReadInteger('Group' + IntToStr(I), 'Id', I + 1);
        TempConfig.Groups[I].Name := Ini.ReadString('Group' + IntToStr(I), 'Name', '');
        TempConfig.Groups[I].GroupType := StrToGroupType(Ini.ReadString('Group' + IntToStr(I), 'Type', 'single'));
        TempConfig.Groups[I].Enabled := StrToBoolEx(Ini.ReadString('Group' + IntToStr(I), 'Enabled', 'True'));

        NodeIdsStr := Ini.ReadString('Group' + IntToStr(I), 'NodeIds', '');

        // Парсим NodeIds через TStringList (как в LoadConfig)
        with TStringList.Create do
        try
          Delimiter := ',';
          DelimitedText := NodeIdsStr;
          SetLength(TempConfig.Groups[I].NodeIds, Count);
          for J := 0 to Count - 1 do
            if not TryStrToInt(Strings[J], TempConfig.Groups[I].NodeIds[J]) then
              raise EConvertError.Create(Format(_('error_invalid_node_id'), [Strings[J]]));
        finally
          Free;
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
    
    FImportedLanguage := ImportedLanguage;
    FImportedBalloon := ImportedBalloon;
    Result := True;
  except
    on E: Exception do
    begin
      FConfig := OldConfig;
      ErrorMsg := E.Message;
    end;
  end;
end;

procedure TConfigManager.ExportConfig(const FileName: string; const Language: string; BalloonHint: Integer);
var
  Ini: TIniFile;
  I, J: Integer;
  NodeIdsStr: string;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteString('General', 'Version', FConfig.Version);
    Ini.WriteBool('General', 'LoggingEnabled', FConfig.LoggingEnabled);
    Ini.WriteBool('General', 'ComparePingEnabled', FConfig.ComparePingEnabled);
    Ini.WriteBool('General', 'CreateSessionReport', FConfig.CreateSessionReport);
    if Language <> '' then Ini.WriteString('General', 'Language', Language)
    else Ini.WriteString('General', 'Language', GetLanguage);
    if BalloonHint >= 0 then Ini.WriteString('General', 'BalloonHintEnabled', BoolToStrEx(BalloonHint <> 0))
    else Ini.WriteString('General', 'BalloonHintEnabled', BoolToStrEx(GetBalloonHintEnabled));
    Ini.WriteInteger('General', 'NodeCount', Length(FConfig.Nodes));
    Ini.WriteInteger('General', 'GroupCount', Length(FConfig.Groups));
    
    for I := 0 to High(FConfig.Nodes) do
    begin
      Ini.WriteInteger('Node' + IntToStr(I), 'Id', FConfig.Nodes[I].Id);
      Ini.WriteString('Node' + IntToStr(I), 'Name', FConfig.Nodes[I].Name);
      Ini.WriteString('Node' + IntToStr(I), 'Host', FConfig.Nodes[I].Host);
      Ini.WriteInteger('Node' + IntToStr(I), 'IntervalMs', FConfig.Nodes[I].IntervalMs);
      Ini.WriteInteger('Node' + IntToStr(I), 'TimeoutMs', FConfig.Nodes[I].TimeoutMs);
      Ini.WriteInteger('Node' + IntToStr(I), 'FailThreshold', FConfig.Nodes[I].FailThreshold);
      Ini.WriteBool('Node' + IntToStr(I), 'NotificationsMuted', FConfig.Nodes[I].NotificationsMuted);
      Ini.WriteString('Node' + IntToStr(I), 'State', NodeStateToStr(FConfig.Nodes[I].State));
      Ini.WriteString('Node' + IntToStr(I), 'Command', FConfig.Nodes[I].Command);
      Ini.WriteString('Node' + IntToStr(I), 'DownCommand', FConfig.Nodes[I].DownCommand);
      Ini.WriteString('Node' + IntToStr(I), 'UpCommand', FConfig.Nodes[I].UpCommand);
    end;
    
    for I := 0 to High(FConfig.Groups) do
    begin
      Ini.WriteInteger('Group' + IntToStr(I), 'Id', FConfig.Groups[I].Id);
      Ini.WriteString('Group' + IntToStr(I), 'Name', FConfig.Groups[I].Name);
      Ini.WriteString('Group' + IntToStr(I), 'Type', GroupTypeToStr(FConfig.Groups[I].GroupType));
      Ini.WriteString('Group' + IntToStr(I), 'Enabled', BoolToStrEx(FConfig.Groups[I].Enabled));
      
      NodeIdsStr := '';
      for J := 0 to High(FConfig.Groups[I].NodeIds) do
      begin
        if J > 0 then NodeIdsStr := NodeIdsStr + ',';
        NodeIdsStr := NodeIdsStr + IntToStr(FConfig.Groups[I].NodeIds[J]);
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
  if FImportedLanguage <> '' then Exit(FImportedLanguage);
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
  if (FConfigDir <> '') and not DirectoryExists(FConfigDir) then
    ForceDirectories(FConfigDir);

  Ini := TIniFile.Create(FConfigPath);
  try
    Ini.WriteString('General', 'Language', LangCode);
    FImportedLanguage := '';
  finally
    Ini.Free;
  end;
end;

function TConfigManager.GetBalloonHintEnabled: Boolean;
var
  Ini: TIniFile;
begin
  if FImportedBalloon >= 0 then Exit(FImportedBalloon <> 0);
  Result := True; // Default: enabled
  if not FileExists(FConfigPath) then Exit;

  Ini := TIniFile.Create(FConfigPath);
  try
    Result := StrToBoolEx(Ini.ReadString('General', 'BalloonHintEnabled', 'True'));
  finally
    Ini.Free;
  end;
end;

procedure TConfigManager.SetBalloonHintEnabled(Value: Boolean);
var
  Ini: TIniFile;
begin
  if (FConfigDir <> '') and not DirectoryExists(FConfigDir) then
    ForceDirectories(FConfigDir);

  Ini := TIniFile.Create(FConfigPath);
  try
    Ini.WriteString('General', 'BalloonHintEnabled', BoolToStrEx(Value));
    FImportedBalloon := -1;
  finally
    Ini.Free;
  end;
end;

end.

