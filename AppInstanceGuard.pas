unit AppInstanceGuard;

{$mode objfpc}{$H+}

interface

uses Windows;

const
  ApplicationMutexName = 'Local\multiPingLed.SingleInstance.{3FD04483-A71E-4D74-BC4A-721501073DE4}';

type
  TSingleInstance = class
  private
    FHandle: THandle;
    function GetAcquired: Boolean;
  public
    constructor Create(const Name: UnicodeString = ApplicationMutexName);
    destructor Destroy; override;
    property Acquired: Boolean read GetAcquired;
  end;

procedure WarnAlreadyRunning;

implementation

uses SysUtils, Classes, IniFiles, BuiltInTranslations, ConfigManager;

procedure WarnAlreadyRunning;
var Language, ConfigPath: string; Ini: TMemIniFile; Labels: TStringList;
begin
  case GetUserDefaultLangID and $03FF of
    $19: Language := 'russian';
    $07: Language := 'german';
    $0C: Language := 'french';
    $3F: Language := 'kazakh';
  else Language := 'english'; end;
  // Read the language only. Do not initialize the application or write settings.
  ConfigPath := ResolveAppConfigPath;
  try
    if FileExists(ConfigPath) then
    begin
      Ini := TMemIniFile.Create(ConfigPath);
      try Language := Ini.ReadString('General', 'Language', Language);
      finally Ini.Free; end;
    end;
  except
    // A settings read failure must not start a second monitoring session.
  end;
  Labels := TStringList.Create;
  try
    LoadBuiltInTranslations(Language, Labels);
    MessageBoxW(0, PWideChar(UTF8Decode(Labels.Values['app_already_running'])),
      'multiPingLED', MB_OK or MB_ICONWARNING or MB_SETFOREGROUND);
  finally Labels.Free; end;
end;


constructor TSingleInstance.Create(const Name: UnicodeString);
var ErrorCode: DWORD;
begin
  inherited Create;
  // Holding the handle keeps the named object alive; no thread owns the mutex.
  FHandle := CreateMutexW(nil, False, PWideChar(Name));
  ErrorCode := GetLastError;
  if FHandle = 0 then RaiseLastOSError(ErrorCode);
  if ErrorCode = ERROR_ALREADY_EXISTS then
  begin
    CloseHandle(FHandle);
    FHandle := 0;
  end;
end;

destructor TSingleInstance.Destroy;
begin
  if FHandle <> 0 then CloseHandle(FHandle);
  inherited Destroy;
end;

function TSingleInstance.GetAcquired: Boolean;
begin
  Result := FHandle <> 0;
end;

end.

