program MultiPingLed;

{$mode objfpc}{$H+}
{$APPTYPE GUI}  // Компилируем как GUI приложение (без консольного окна)

uses
  {$IFDEF UNIX}
  cthreads,  // Поддержка потоков в Linux/Unix
  {$ENDIF}
  Interfaces,  // LCL интерфейс
  Forms,       // Формы LCL
  Dialogs,     // Для ShowMessage
  SysUtils,    // для IncludeTrailingPathDelimiter
  AppCore, ConfigManager, GroupManager, IconRenderer, NodeManager, PingHelper,
  SettingsForm, NodeEditForm, GroupEditForm, TrayController, LangManager, AboutUnit;     // Ядро приложения

var
  ConfigMgr: TConfigManager;
  ConfigDir, LangDir, LangCode: string;

{$R *.res}

begin
  // Включаем масштабирование для высокого DPI
  Application.Scaled:=True;
  // Инициализация приложения
  Application.Initialize;

  //Получение пути к конфиг файлу
  ConfigDir := GetEnvironmentVariable('APPDATA') + '\multiPingLed\config.ini';
  GlobalConfigPath := ConfigDir;  // Устанавливаем глобальную переменную

  // Инициализация менеджера локализации
  LangDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'lang';
  GlobalLangPath := LangDir;  // Устанавливаем глобальную переменную
  LangMgr.Initialize( LangDir );

  // Загрузка сохраненного языка
  ConfigMgr := TConfigManager.Create('');
  try
    LangCode := ConfigMgr.GetLanguage;
    LangMgr.SetLanguage(LangCode);
  finally
    ConfigMgr.Free;
  end;

  // Инициализация ядра приложения (Singleton)
  TAppCore.Instance.Initialize;
  try
    // Запуск главного цикла приложения
    Application.Run;
  finally
    // Освобождаем ядро приложения
    TAppCore.ReleaseInstance;
  end;
end.
