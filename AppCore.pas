unit AppCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs,
  ConfigManager, NodeManager, GroupManager, TrayController, SettingsForm, LangManager;

type
  // Главный класс приложения (паттерн Singleton)
  // Управляет всеми компонентами и их взаимодействием
  TAppCore = class
  private
    class var FInstance: TAppCore;  // Статическая переменная для хранения единственного экземпляра
    FConfigManager: TConfigManager;  // Менеджер конфигурации (загрузка/сохранение настроек)
    FNodeManager: TNodeManager;      // Менеджер узлов (пингование хостов)
    FGroupManager: TGroupManager;    // Менеджер групп (объединение узлов в визуальные группы)
    FTrayController: TTrayController; // Контроллер иконок в системном трее
    FSettingsForm: TSettingsForm;    // Форма настроек
    FInitialized: Boolean;           // Флаг инициализации
    
    // Инициализация конфигурации по умолчанию (если конфиг не существует)
    procedure InitializeConfig;
    // Применение конфигурации к менеджерам узлов и групп
    procedure ApplyConfig(Sender: TObject);
    // Показать окно настроек
    procedure ShowSettings(Sender: TObject);
    // Завершение работы приложения
    procedure ExitApp(Sender: TObject);
  public
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

constructor TAppCore.Create;
begin
  inherited Create;
  FInitialized := False;
  // Инициализируем все поля в nil для безопасности
  FConfigManager := nil;
  FNodeManager := nil;
  FGroupManager := nil;
  FTrayController := nil;
  FSettingsForm := nil;
end;

destructor TAppCore.Destroy;
begin
  // Освобождаем ресурсы в обратном порядке создания
  FreeAndNil(FSettingsForm);
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
  // Предотвращаем повторную инициализацию
  if FInitialized then Exit;

  try
    // Создаем все менеджеры
    FConfigManager := TConfigManager.Create;
    FNodeManager := TNodeManager.Create;
    FGroupManager := TGroupManager.Create;
    FTrayController := TTrayController.Create;
    
    // Загружаем или создаем конфигурацию по умолчанию
    InitializeConfig;
    
    // Проверяем валидность конфигурации
    if not FConfigManager.ValidateConfig(ErrorMsg) then
    begin
      MessageDlg('Ошибка конфигурации', ErrorMsg, mtError, [mbOK], 0);
      Application.Terminate;
      Exit;
    end;
    
    // Применяем конфигурацию (запускаем пингование и отображение)
    ApplyConfig(Self);
    
    // Настраиваем обработчики событий для трея
    FTrayController.OnShowSettings := @ShowSettings;
    FTrayController.OnExit := @ExitApp;
    FTrayController.Initialize;
    
    FInitialized := True;
  except
    on E: Exception do
    begin
      MessageDlg('Ошибка инициализации', E.Message, mtError, [mbOK], 0);
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
  PosInGroup: Integer;
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
begin
  // Получаем текущую конфигурацию
  Config := FConfigManager.GetConfig;

  // Применяем к менеджерам
  FNodeManager.ApplyConfig(Config.Nodes);
  FGroupManager.ApplyConfig(Config.Groups, FNodeManager);
  FTrayController.Rebuild(FGroupManager, FNodeManager);
end;

procedure TAppCore.ShowSettings(Sender: TObject);
begin
  // Создаем форму настроек при первом обращении
  if FSettingsForm = nil then
    FSettingsForm := TSettingsForm.Create(nil);
  
  // Передаем текущую конфигурацию и ConfigManager
  FSettingsForm.SetConfig(FConfigManager.GetConfig);
  FSettingsForm.ConfigManager := FConfigManager;
  FSettingsForm.OnApply := @ApplyConfig;
  FSettingsForm.OnLanguageChanged := @UpdateTrayMenu;
  FSettingsForm.ShowModal;
end;

procedure TAppCore.ExitApp(Sender: TObject);
begin
  // Останавливаем потоки пинга с таймаутом
  if FNodeManager <> nil then
    FNodeManager.StopAllThreads;
  
  // Удаляем иконки трея
  if FTrayController <> nil then
    FreeAndNil(FTrayController);
  
  // Завершаем приложение
  Halt(0);
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
