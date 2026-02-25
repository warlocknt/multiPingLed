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
  AppCore, ConfigManager, GroupManager, IconRenderer, NodeManager, PingHelper,
  SettingsForm, NodeEditForm, GroupEditForm, TrayController, LangManager;     // Ядро приложения

var
  ConfigMgr: TConfigManager;
  LangCode: string;

{$R *.res}

begin
  // Включаем масштабирование для высокого DPI
  Application.Scaled:=True;
  // Инициализация приложения
  Application.Initialize;

  // Инициализация менеджера локализации
  LangMgr.Initialize(Application.ExeName);

  // Загрузка сохраненного языка
  ConfigMgr := TConfigManager.Create;
  try
    LangCode := ConfigMgr.GetLanguage;
    LangMgr.SetLanguage(LangCode);
  finally
    ConfigMgr.Free;
  end;

  // Инициализация ядра приложения (Singleton)
  TAppCore.Instance.Initialize;

  // Запуск главного цикла приложения
  Application.Run;
end.
