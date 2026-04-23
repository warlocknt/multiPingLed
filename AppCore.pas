unit AppCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Windows,
  ConfigManager, NodeManager, GroupManager, TrayController, SettingsForm, LangManager,
  fpjson, jsonparser, opensslsockets, FileInfo, WinHttp, AboutUnit;

type
  // Главный класс приложения (паттерн Singleton)
  // Управляет всеми компонентами и их взаимодействием

  TIntArray = array of Integer;

  { TAppCore }
  TAppCore = class;  // Forward declaration

  { TVersionCheckThread }

  TVersionCheckThread = class(TThread)
  private
    FLocalVersion: string;
    FLatestTag: string;
    FSuccess: Boolean;
    FUpdateAvailable: Boolean;
    FAppCore: TAppCore;
    procedure OnCheckDone;
  protected
    procedure Execute; override;
  public
    constructor Create(const LocalVer: string; AAppCore: TAppCore);
  end;

  TAppCore = class
  private
    class var FInstance: TAppCore;  // Статическая переменная для хранения единственного экземпляра
    FConfigManager: TConfigManager;  // Менеджер конфигурации (загрузка/сохранение настроек)
    FNodeManager: TNodeManager;      // Менеджер узлов (пингование хостов)
    FGroupManager: TGroupManager;    // Менеджер групп (объединение узлов в визуальные группы)
    FTrayController: TTrayController; // Контроллер иконок в системном трее
    FAboutForm: TAboutForm;
    FVersionCheckThread: TVersionCheckThread; // Поток проверки версии
    FInitialized: Boolean;           // Флаг инициализации
    FApplyingConfig: Boolean;        // Флаг: применяется конфигурация
    
    // Инициализация конфигурации по умолчанию (если конфиг не существует)
    procedure InitializeConfig;
    // Применение конфигурации к менеджерам узлов и групп
    procedure ApplyConfig(Sender: TObject);
    // Показать окно настроек
    procedure ShowSettings(Sender: TObject);
    // Показать окно "О программе"
    procedure ShowAbout(Sender: TObject);
    // Завершение работы приложения
    procedure ExitApp(Sender: TObject);
    // Запуск асинхронной проверки версии
    procedure StartVersionCheck;
    // Получение текущей версии приложения
    function GetAppVersion: string;
    function ResolveConfigPath: string;
    function NormalizeVersion(const S: string): string;
    function CompareVersions(const V1, V2: string): Integer;
    function GetActiveNodeIds(const Groups: array of TNodeGroup): TIntArray;

  public
    LatestReleaseTag: string;
    UpdateAvailable: Boolean;
    UpdateCheckFailed: Boolean;   // Флаг: не удалось проверить версию (нет интернета)
    UpdateCheckFinished: Boolean; // Флаг: проверка завершена (успешно или с ошибкой)
    constructor Create;
    destructor Destroy; override;
    // Основная инициализация приложения
    procedure Initialize;
    // Получить единственный экземпляр класса (Singleton)
    class function Instance: TAppCore;
    // Освободить экземпляр класса
    class procedure ReleaseInstance;
    // Обновить меню в трее (после смены языка)
    procedure UpdateTrayMenu(Sender: TObject);
  end;

implementation

// Устанавливает таймауты WinHttp чтобы HTTP-запрос не висел 30+ секунд по
// дефолту и поток корректно завершался при выходе приложения.
function WinHttpSetTimeouts(hInternet: Pointer;
  ResolveTimeout, ConnectTimeout, SendTimeout, ReceiveTimeout: Integer): LongBool;
  stdcall; external 'winhttp.dll';

procedure AppDebugLog(const Msg: string);
begin
  OutputDebugString(PChar('multiPingLed[App]: ' + Msg));
end;

constructor TAppCore.Create;
begin
  inherited Create;
  FInitialized := False;
  FApplyingConfig := False;
  UpdateAvailable := False;
  UpdateCheckFailed := False;
  UpdateCheckFinished := False;
  LatestReleaseTag := '';
  // Инициализируем все поля в nil для безопасности
  FConfigManager := nil;
  FNodeManager := nil;
  FGroupManager := nil;
  FTrayController := nil;
  FAboutForm := nil;
end;

destructor TAppCore.Destroy;
begin
  // Останавливаем и освобождаем поток проверки версии.
  // Timeouts из WinHttpSetTimeouts гарантируют, что WaitFor не заблокирует
  // выход надолго (максимум ~5 сек, типично — мгновенно).
  if FVersionCheckThread <> nil then
  begin
    FVersionCheckThread.Terminate;
    FVersionCheckThread.WaitFor;
    FreeAndNil(FVersionCheckThread);
  end;

  // Освобождаем ресурсы в обратном порядке создания
  FreeAndNil(FAboutForm);
  FreeAndNil(FTrayController);
  FreeAndNil(FGroupManager);
  FreeAndNil(FNodeManager);
  FreeAndNil(FConfigManager);
  inherited Destroy;
end;

class function TAppCore.Instance: TAppCore;
begin
  // Создаем экземпляр только если его еще нет (паттерн Singleton)
  if FInstance = nil then
    FInstance := TAppCore.Create;
  Result := FInstance;
end;

class procedure TAppCore.ReleaseInstance;
begin
  // Уничтожаем единственный экземпляр
  FreeAndNil(FInstance);
end;

procedure TAppCore.Initialize;
var
  ErrorMsg: string;
begin
  AppDebugLog('Application started, PID=' + IntToStr(GetCurrentProcessId));
  
  // Предотвращаем повторную инициализацию
  if FInitialized then Exit;

  try
    // Создаем все менеджеры
    GlobalConfigPath := ResolveConfigPath;
    FConfigManager := TConfigManager.Create('');
    FNodeManager := TNodeManager.Create;
    FGroupManager := TGroupManager.Create;
    FTrayController := TTrayController.Create;
    
    // Загружаем или создаем конфигурацию по умолчанию
    InitializeConfig;

    // Применяем настройку balloon hint из конфига
    FTrayController.BalloonHintEnabled := FConfigManager.GetBalloonHintEnabled;

    // Устанавливаем версию ПОСЛЕ загрузки конфига (чтобы не затерлась)
    FConfigManager.SetVersion(GetAppVersion);

    // Асинхронная проверка версии (не блокирует запуск)
    StartVersionCheck;
    
    // Проверяем валидность конфигурации
    if not FConfigManager.ValidateConfig(ErrorMsg) then
    begin
      MessageDlg(_('title_config_error'), ErrorMsg, mtError, [mbOK], 0);
      Application.Terminate;
      Exit;
    end;
    
    // Применяем конфигурацию (запускаем пингование и отображение)
    ApplyConfig(Self);
    
    // Настраиваем обработчики событий для трея
    FTrayController.OnShowSettings := @ShowSettings;
    FTrayController.OnExit := @ExitApp;
    FTrayController.OnAboutShow := @ShowAbout;
    FTrayController.Initialize;
    
    FInitialized := True;
  except
    on E: Exception do
    begin
      MessageDlg(_('title_init_error'), E.Message, mtError, [mbOK], 0);
      Application.Terminate;
    end;
  end;
end;

procedure TAppCore.InitializeConfig;
const
  // Количество узлов по умолчанию
  NodeCount = 14;
  // IP-адреса DNS-серверов для мониторинга
  NodeHosts: array[0..NodeCount-1] of string = (
    '1.1.1.1', '1.0.0.1', '8.8.8.8', '8.8.4.4',
    '9.9.9.9', '149.112.112.112', '208.67.222.222', '208.67.220.220',
    '64.6.64.6', '64.6.65.6', '4.2.2.2', '94.140.14.14',
    '185.228.168.9', '76.76.19.19'
  );
  // Названия провайдеров DNS
  NodeNames: array[0..NodeCount-1] of string = (
    'Cloudflare', 'Cloudflare', 'Google', 'Google',
    'Quad9', 'Quad9', 'OpenDNS', 'OpenDNS',
    'Verisign', 'Verisign', 'Lumen Technologies', 'AdGuard DNS',
    'CleanBrowsing', 'Control D'
  );
var
  Config: TAppConfig;
  I: Integer;
begin
  // Если конфиг уже существует - просто загружаем его
  if FConfigManager.ConfigExists then
  begin
    FConfigManager.LoadConfig;
    Exit;
  end;

  // Создаем конфигурацию по умолчанию
  Config.Version := '1.0';
  SetLength(Config.Nodes, NodeCount);

  // Заполняем массив узлов
  for I := 0 to NodeCount - 1 do
  begin
    Config.Nodes[I].Id := I + 1;
    Config.Nodes[I].Name := NodeNames[I];
    Config.Nodes[I].Host := NodeHosts[I];
    Config.Nodes[I].IntervalMs := 5000;  // Интервал пинга 5 секунд
    Config.Nodes[I].TimeoutMs := 2000;   // Таймаут 2 секунды
    Config.Nodes[I].State := nsUnknown;  // Начальное состояние - неизвестно
    Config.Nodes[I].LastPingTime := 0;   // Время еще не установлено
  end;

  // Создаем 3 группы иконок:
  // - Группа 1: одиночная иконка (1 узел)
  // - Группа 2: сетка 2x2 (4 узла)
  // - Группа 3: сетка 3x3 (9 узлов)
  // Всего: 1 + 4 + 9 = 14 узлов
  SetLength(Config.Groups, 3);

  Config.Groups[0].Enabled := True;
  Config.Groups[1].Enabled := True;
  Config.Groups[2].Enabled := True;

  // Группа 1: одиночный узел (Cloudflare 1.1.1.1)
  Config.Groups[0].Id := 1;
  Config.Groups[0].Name := NodeNames[0];
  Config.Groups[0].GroupType := gtSingle;
  SetLength(Config.Groups[0].NodeIds, 1);
  Config.Groups[0].NodeIds[0] := 1;

  // Группа 2: сетка 2x2 (узлы 2-5)
  Config.Groups[1].Id := 2;
  Config.Groups[1].Name := 'DNS Group 2x2';
  Config.Groups[1].GroupType := gt2x2;
  SetLength(Config.Groups[1].NodeIds, 4);
  Config.Groups[1].NodeIds[0] := 2;
  Config.Groups[1].NodeIds[1] := 3;
  Config.Groups[1].NodeIds[2] := 4;
  Config.Groups[1].NodeIds[3] := 5;

  // Группа 3: сетка 3x3 (узлы 6-14)
  Config.Groups[2].Id := 3;
  Config.Groups[2].Name := 'DNS Group 3x3';
  Config.Groups[2].GroupType := gt3x3;
  SetLength(Config.Groups[2].NodeIds, 9);
  for I := 0 to 8 do
    Config.Groups[2].NodeIds[I] := I + 6;  // Узлы 6-14

  // Сохраняем конфигурацию
  FConfigManager.SetConfig(Config);
  FConfigManager.SaveConfig;
end;

procedure TAppCore.ApplyConfig(Sender: TObject);
var
  Config: TAppConfig;
  Count, I: Integer;
  EnabledGroups: array of TNodeGroup;
  ActiveNodeIds : TIntArray;
begin
  FApplyingConfig := True;
  try
    // Блокируем пункт меню настроек и обновляем UI
    if FTrayController <> nil then
    begin
      FTrayController.SetSettingsEnabled(False);
      Application.ProcessMessages;  // Даём UI обновиться
    end;
    
    AppDebugLog('ApplyConfig called');
    Config := FConfigManager.GetConfig;

    // Переприменяем настройку balloon hint (могла измениться в настройках)
    if FTrayController <> nil then
      FTrayController.BalloonHintEnabled := FConfigManager.GetBalloonHintEnabled;
    AppDebugLog('Config has ' + IntToStr(Length(Config.Groups)) + ' groups, ' + IntToStr(Length(Config.Nodes)) + ' nodes');
    
    ActiveNodeIds := GetActiveNodeIds(Config.Groups);
    AppDebugLog('ActiveNodeIds count: ' + IntToStr(Length(ActiveNodeIds)));

    // Пингуем только узлы из активных групп
    FNodeManager.ApplyConfig(Config.Nodes, ActiveNodeIds);

    // Оставляем только включённые группы
    Count := 0;
    EnabledGroups := nil;
    SetLength(EnabledGroups, Length(Config.Groups));
    for I := 0 to High(Config.Groups) do
    begin
      if Config.Groups[I].Enabled then
      begin
        EnabledGroups[Count] := Config.Groups[I];
        Inc(Count);
      end;
    end;
    SetLength(EnabledGroups, Count);

    AppDebugLog('Enabled groups after filter: ' + IntToStr(Count));

    // Передаём GroupManager только включённые группы
    FGroupManager.ApplyConfig(EnabledGroups, FNodeManager);
    AppDebugLog('GroupManager.ApplyConfig called');
    
    // Инкрементальное обновление иконок (только новые/удалённые)
    FTrayController.SyncGroups(FGroupManager, FNodeManager);
    AppDebugLog('TrayController.SyncGroups called from ApplyConfig');
  finally
    // Разблокируем пункт меню настроек
    if FTrayController <> nil then
      FTrayController.SetSettingsEnabled(True);
    FApplyingConfig := False;
  end;
end;

procedure TAppCore.ShowSettings(Sender: TObject);
var
  Form: TSettingsForm;
begin
  if FApplyingConfig then
  begin
    AppDebugLog('ShowSettings: config apply in progress, ignoring');
    Exit;
  end;

  AppDebugLog('ShowSettings called');
  // Создаём форму при каждом вызове и уничтожаем сразу после закрытия —
  // и по Apply (mrOK), и по Cancel / крестику (mrCancel). Одноразовая форма
  // исключает залипание состояния между вызовами.
  Form := TSettingsForm.Create(nil);
  try
    Form.SetConfig(FConfigManager.GetConfig);
    Form.ConfigManager := FConfigManager;
    Form.OnApply := @ApplyConfig;
    Form.OnLanguageChanged := @UpdateTrayMenu;
    AppDebugLog('Showing settings modal');
    Form.ShowModal;
    AppDebugLog('Settings form closed');
  finally
    Form.Free;
  end;
end;

procedure TAppCore.ShowAbout(Sender: TObject);
begin
  // Создаем форму настроек при первом обращении
  if FAboutForm = nil then
    FAboutForm := TAboutForm.Create(nil);

  // Передаем текущую конфигурацию и ConfigManager
  FAboutForm.AppVersion := FConfigManager.GetVersion;
  FAboutForm.ApplyLocalization;
  if TAppCore.Instance.UpdateCheckFinished then
    begin
    if TAppCore.Instance.UpdateAvailable then
      FAboutForm.LUpdateStatus.Caption := LangMgr.GetStringFmt('update_available', [TAppCore.Instance.LatestReleaseTag])
    else
      if TAppCore.Instance.UpdateCheckFailed then
         FAboutForm.LUpdateStatus.Caption := _('update_check_failed')
      else
         FAboutForm.LUpdateStatus.Caption := _('update_none');
    end;

  FAboutForm.ShowModal;
end;

procedure TAppCore.ExitApp(Sender: TObject);
begin
  AppDebugLog('ExitApp called');
  // Нормальное завершение через LCL — финальный try..finally в .lpr
  // вызовет ReleaseInstance → Destroy, который корректно освободит все
  // менеджеры и потоки. Halt(0) здесь пропускал бы блок finally и вызывал
  // утечки (TVersionCheckThread, возможные «зависшие» ping-потоки).
  Application.Terminate;
end;

procedure TAppCore.StartVersionCheck;
var
  LocalVer: string;
begin
  LocalVer := GetAppVersion;
  if LocalVer = '' then Exit;

  FVersionCheckThread := TVersionCheckThread.Create(LocalVer, Self);
end;

// ========== TVersionCheckThread ==========

constructor TVersionCheckThread.Create(const LocalVer: string; AAppCore: TAppCore);
begin
  FLocalVersion := LocalVer;
  FAppCore := AAppCore;
  FSuccess := False;
  FUpdateAvailable := False;
  FLatestTag := '';
  // Владеет объектом TAppCore — освободит его в Destroy после WaitFor.
  // FreeOnTerminate=True + Halt(0) ранее приводил к утечке thread-объекта.
  FreeOnTerminate := False;
  inherited Create(False);
end;

procedure TVersionCheckThread.Execute;
var
  hSession, hConnect, hRequest: HINTERNET;
  DataAvailable, BytesRead: DWORD;
  Buffer: array[0..1023] of Byte;
  Response: TStringStream;
  JSON, TagNode: TJSONData;
  URLPath, RawData: string;
  URLPathW: WideString;
  RawTag: string;
begin
  hSession := nil;
  hConnect := nil;
  hRequest := nil;
  URLPathW := '/repos/warlocknt/multiPingLed/releases/latest';
  Response := TStringStream.Create('');
  try
    if Terminated then Exit;

    hSession := WinHttpOpen(PWideChar('multiPingLed/' + FLocalVersion),
      WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
      WINHTTP_NO_PROXY_NAME,
      WINHTTP_NO_PROXY_BYPASS, 0);
    if (hSession = nil) or Terminated then Exit;
    // 5 секунд на каждый этап — иначе WaitFor при выходе заблокирует надолго
    WinHttpSetTimeouts(hSession, 5000, 5000, 5000, 5000);

    hConnect := WinHttpConnect(hSession, 'api.github.com',
      INTERNET_DEFAULT_HTTPS_PORT, 0);
    if (hConnect = nil) or Terminated then Exit;

    hRequest := WinHttpOpenRequest(hConnect, 'GET', PWideChar(URLPathW),
      nil, WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, WINHTTP_FLAG_SECURE);
    if (hRequest = nil) or Terminated then Exit;

    WinHttpAddRequestHeaders(hRequest, 'User-Agent: multiPingLed'#13#10,
      DWORD(-1), WINHTTP_ADDREQ_FLAG_ADD);

    if not WinHttpSendRequest(hRequest, WINHTTP_NO_ADDITIONAL_HEADERS, 0,
      WINHTTP_NO_REQUEST_DATA, 0, 0, 0) then Exit;
    if Terminated then Exit;
    if not WinHttpReceiveResponse(hRequest, nil) then Exit;

    repeat
      if Terminated then Break;
      DataAvailable := 0;
      WinHttpQueryDataAvailable(hRequest, @DataAvailable);
      if DataAvailable = 0 then Break;
      WinHttpReadData(hRequest, @Buffer, SizeOf(Buffer), @BytesRead);
      if BytesRead > 0 then
        Response.WriteBuffer(Buffer, BytesRead);
    until DataAvailable = 0;

    // Сохраняем данные ДО освобождения Response
    RawData := Response.DataString;
  finally
    if hRequest <> nil then WinHttpCloseHandle(hRequest);
    if hConnect <> nil then WinHttpCloseHandle(hConnect);
    if hSession <> nil then WinHttpCloseHandle(hSession);
    Response.Free;
  end;

  if Terminated then Exit;

  // Парсим JSON
  JSON := GetJSON(RawData);
  try
    if JSON <> nil then
    begin
      TagNode := JSON.FindPath('tag_name');
      if TagNode <> nil then
        RawTag := TagNode.AsString
      else
        Exit;
    end
    else
      Exit;
  finally
    JSON.Free;
  end;

  // Извлекаем версию из тега (убираем 'v', суффиксы типа -beta, -rc)
  FLatestTag := RawTag;
  if (FLatestTag <> '') and (FLatestTag[1] = 'v') then
    Delete(FLatestTag, 1, 1);

  // Убираем суффиксы (-beta, -rc, -pre и т.д.)
  while (Length(FLatestTag) > 0) and (FLatestTag[Length(FLatestTag)] in ['a'..'z', 'A'..'Z', '-', '_', '.']) do
  begin
    // Ищем последний дефис или точку перед цифрами
    if Pos('-', FLatestTag) > 0 then
      FLatestTag := Copy(FLatestTag, 1, Pos('-', FLatestTag) - 1)
    else
      Break;
  end;

  if FLatestTag = '' then Exit;

  FSuccess := True;
  FUpdateAvailable := (FAppCore.CompareVersions(FLocalVersion, FLatestTag) < 0);

  // Обновляем UI в основном потоке
  Synchronize(@OnCheckDone);
end;

procedure TVersionCheckThread.OnCheckDone;
begin
  if not FSuccess then
  begin
    FAppCore.UpdateCheckFailed := True;
    AppDebugLog('Version check failed (no internet or API error)');
  end
  else
  begin
    FAppCore.LatestReleaseTag := FLatestTag;
    FAppCore.UpdateAvailable := FUpdateAvailable;
    AppDebugLog('Version check: local=' + FLocalVersion + ', latest=' + FLatestTag + ', update=' + BoolToStr(FUpdateAvailable, True));
  end;
  FAppCore.UpdateCheckFinished := True;
end;

// ========== Version helpers ==========

function TAppCore.GetAppVersion: string;
var
  inf : TVersionInfo;
begin
  inf := TVersionInfo.Create;
  inf.Load(HINSTANCE);
  with inf.FixedInfo do
    Result := Format('%d.%d.%d.%d', [FileVersion[0], FileVersion[1], FileVersion[2], FileVersion[3]]);
  inf.Free;
end;

function TAppCore.ResolveConfigPath: string;
var
  ExeDir: string;
  AppDataDir: string;
  LocalConfig: string;
begin
  ExeDir := ExtractFilePath(ParamStr(0));
   LocalConfig := ExeDir + 'config.ini';

   // Приоритет 1: конфиг рядом с exe
   if FileExists(LocalConfig) then
   begin
     Result := LocalConfig;
     Exit;
   end;

   // Приоритет 2: AppData\Roaming\multiPingLed\
   AppDataDir := SysUtils.GetEnvironmentVariable('APPDATA') + '\multiPingLed\';
   Result := AppDataDir + 'config.ini';
end;

function TAppCore.NormalizeVersion(const S: string): string;
var
  DashPos: Integer;
begin
  Result := Trim(S);
  // Убираем префикс 'v' или 'V'
  if (Result <> '') and (UpCase(Result[1]) = 'V') then
    Delete(Result, 1, 1);
  // Убираем суффиксы (-beta, -rc, -pre и т.д.)
  DashPos := Pos('-', Result);
  if DashPos > 0 then
    Result := Trim(Copy(Result, 1, DashPos - 1));
end;

function TAppCore.CompareVersions(const V1, V2: string): Integer;
var
  Parts1, Parts2: TStringArray;
  I, N1, N2, MaxLen: Integer;
  S1, S2: string;
begin
  S1 := NormalizeVersion(V1);
    S2 := NormalizeVersion(V2);

    Parts1 := S1.Split('.');
    Parts2 := S2.Split('.');

    MaxLen := Length(Parts1);
    if Length(Parts2) > MaxLen then
      MaxLen := Length(Parts2);

    for I := 0 to MaxLen - 1 do
    begin
      if I < Length(Parts1) then
        N1 := StrToIntDef(Parts1[I], 0)
      else
        N1 := 0;

      if I < Length(Parts2) then
        N2 := StrToIntDef(Parts2[I], 0)
      else
        N2 := 0;

      if N1 > N2 then Exit(1);
      if N1 < N2 then Exit(-1);
    end;

    Result := 0;
end;

function TAppCore.GetActiveNodeIds(const Groups: array of TNodeGroup
  ): TIntArray;
var
  I, J, Count: Integer;
  Tmp: array of Integer;
  Exists: Boolean;
  K: Integer;
begin
  Tmp := nil;
  SetLength(Tmp, 0);

  for I := 0 to High(Groups) do
  begin
    if not Groups[I].Enabled then Continue;

    for J := 0 to High(Groups[I].NodeIds) do
    begin
      if Groups[I].NodeIds[J] <= 0 then Continue;

      // проверка на уникальность
      Exists := False;
      for K := 0 to High(Tmp) do
        if Tmp[K] = Groups[I].NodeIds[J] then
        begin
          Exists := True;
          Break;
        end;

      if not Exists then
      begin
        Count := Length(Tmp);
        SetLength(Tmp, Count + 1);
        Tmp[Count] := Groups[I].NodeIds[J];
      end;
    end;
  end;

  Result := Tmp;
end;

procedure TAppCore.UpdateTrayMenu(Sender: TObject);
begin
  if FTrayController <> nil then
    FTrayController.UpdateMenuLocalization;
end;

initialization
  // Секция инициализации модуля

finalization
  // Гарантированно освобождаем экземпляр при завершении программы
  TAppCore.ReleaseInstance;

end.
