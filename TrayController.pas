unit TrayController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Menus, ExtCtrls, Windows, Controls,
  ConfigManager, GroupManager, NodeManager, LangManager;

type
  // Класс для управления иконками в системном трее
  // Отображает состояние групп узлов в виде иконок с LED-индикаторами

  { TTrayController }

  TTrayController = class
  private
    FPopupMenu: TPopupMenu;           // Контекстное меню для иконок
    FGroupIcons: array of TTrayIcon;  // Массив иконок групп
    FGroupManager: TGroupManager;     // Ссылка на менеджер групп
    FNodeManager: TNodeManager;       // Ссылка на менеджер узлов
    FOnShowSettings: TNotifyEvent;    // Обработчик открытия настроек
    FOnExit: TNotifyEvent;            // Обработчик выхода
    FTimer: TTimer;                   // Таймер для обновления иконок
    // Сохраненные состояния узлов для определения изменений
    FLastGroupStates: array of array of ConfigManager.TNodeState;
    
    // Создание контекстного меню (Settings, Exit)
    procedure CreatePopupMenu;
    // Обработчик клика по "Settings"
    procedure OnSettingsClick(Sender: TObject);
    // Обработчик клика по "Exit"
    procedure OnExitClick(Sender: TObject);
    // Обработчик таймера
    procedure OnTimer(Sender: TObject);
    // Обновление всех иконок групп
    procedure UpdateTrayIcons;
    // Проверка изменения состояний узлов
    function GroupStatesChanged: Boolean;
    // Формирование подсказки для группы
    function BuildGroupHint(const Group: TGroupInfo): string;
  public
    constructor Create;
    destructor Destroy; override;
    // Инициализация (показ иконок)
    procedure Initialize;
    // Пересоздание иконок при изменении конфигурации
    procedure Rebuild(GroupManager: TGroupManager; NodeManager: TNodeManager);
    procedure UpdateMenuLocalization;
    property OnShowSettings: TNotifyEvent read FOnShowSettings write FOnShowSettings;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
  end;

implementation

uses
  IconRenderer;

constructor TTrayController.Create;
begin
  inherited Create;


  // Создаем контекстное меню
  CreatePopupMenu;

  // Создаем таймер для обновления иконок (каждую секунду)
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 1000;  // 1 секунда
  FTimer.OnTimer := @OnTimer;
  FTimer.Enabled := False;  // Включим после Rebuild

  // Инициализируем массивы
  SetLength(FGroupIcons, 0);
  SetLength(FLastGroupStates, 0);
  FGroupManager := nil;
  FNodeManager := nil;
  
end;

destructor TTrayController.Destroy;
var
  I: Integer;
begin
  // Останавливаем таймер
  if FTimer <> nil then
  begin
    FTimer.Enabled := False;
    FreeAndNil(FTimer);
  end;

  // Скрываем и уничтожаем все иконки групп
  for I := High(FGroupIcons) downto 0 do
  begin
    if FGroupIcons[I] <> nil then
    begin
      FGroupIcons[I].Visible := False;  // Скрываем иконку из трея
      FreeAndNil(FGroupIcons[I]);       // Уничтожаем объект
    end;
  end;

  // Освобождаем меню
  FreeAndNil(FPopupMenu);
  
  inherited Destroy;
end;

procedure TTrayController.CreatePopupMenu;
var
  MenuItem: TMenuItem;
begin
  // Создаем всплывающее меню
  FPopupMenu := TPopupMenu.Create(nil);

  // Пункт меню "Settings" (Настройки)
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('menu_settings');
  MenuItem.OnClick := @OnSettingsClick;
  FPopupMenu.Items.Add(MenuItem);

  // Разделитель
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := '-';
  FPopupMenu.Items.Add(MenuItem);

  // Пункт меню "Exit" (Выход)
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('menu_exit');
  MenuItem.OnClick := @OnExitClick;
  FPopupMenu.Items.Add(MenuItem);
end;

procedure TTrayController.Initialize;
begin
  // Иконки групп создаются и показываются в методе Rebuild
end;

function TTrayController.BuildGroupHint(const Group: TGroupInfo): string;

  function GetNodeName(NodeId: Integer): string;
  var
    Node: TNodeConfig;
  begin
    Result := '';
    if NodeId = 0 then Exit;

    Node := FNodeManager.GetNodeById(NodeId);
    if Node.Id = 0 then Exit;

    Result := '"' + Node.Name + '"';
  end;

  function FormatNodeFull(NodeId: Integer): string;
  var
    Node: TNodeConfig;
    StatusSymbol: string;
    StatusText: string;
    TimeStr: string;
  begin
    Result := '';
    if NodeId = 0 then Exit;
    
    Node := FNodeManager.GetNodeById(NodeId);
    if Node.Id = 0 then Exit;
    
    // Выбираем символ статуса
    case Node.State of
      nsUp: StatusSymbol := '+';
      nsDown: StatusSymbol := 'X';
      else StatusSymbol := '?';
    end;
    
    // Форматируем время
    if Node.LastPingTime > 0 then
      TimeStr := FormatDateTime('hh:nn', Node.LastPingTime)
    else
      TimeStr := '--:--';
    
    // Определяем текст статуса
    case Node.State of
      nsUp: StatusText := 'ok';
      nsDown: StatusText := 'fail';
    else
      StatusText := '?';
    end;
    
    Result := Format('%s %s - %s %s', [StatusSymbol, Node.Host, StatusText, TimeStr]);
  end;

var
  I: Integer;
  GroupName: string;
  FirstNodeName: string;
  LeftName, RightName: string;
begin
  if FNodeManager = nil then Exit;
  
  GroupName := Group.Name;
  
  // Для группы 3x3 используем сверхкомпактный формат - только имена
  if Group.GroupType = gt3x3 then
  begin
    Result := GroupName;
    
    // Выводим все узлы группы
    for I := 0 to High(Group.NodeIds) do
    begin
      if I = 0 then
      begin
        // Первая строка: название группы + имя первого узла
        FirstNodeName := GetNodeName(Group.NodeIds[0]);
        if FirstNodeName <> '' then
          Result := Result + '  ' + FirstNodeName;
      end
      else if I mod 2 = 1 then
      begin
        // Нечетные индексы (1, 3, 5, 7) - начало новой строки
        LeftName := GetNodeName(Group.NodeIds[I]);
        if I + 1 <= High(Group.NodeIds) then
          RightName := GetNodeName(Group.NodeIds[I + 1])
        else
          RightName := '';
        
        if (LeftName <> '') or (RightName <> '') then
        begin
          Result := Result + #13#10;
          
          if LeftName <> '' then
            Result := Result + LeftName;
          
          if RightName <> '' then
          begin
            if LeftName <> '' then
              Result := Result + '  ' + RightName
            else
              Result := Result + RightName;
          end;
        end;
      end;
    end;
  end
  else
  begin
    // Для Single и 2x2 используем полный формат
    Result := GroupName + #13#10;
    
    for I := 0 to Length(Group.NodeIds)-1 do
    begin
      if Group.NodeIds[I] = 0 then Continue;
      
      Result := Result + FormatNodeFull(Group.NodeIds[I]) + #13#10;
    end;
    
    // Удаляем последний перевод строки
    if Length(Result) > 2 then
      SetLength(Result, Length(Result) - 2);
  end;
end;

procedure TTrayController.Rebuild(GroupManager: TGroupManager; NodeManager: TNodeManager);
var
  I: Integer;
  Groups: TGroupArray;
  NewIcon: TTrayIcon;
  TempIcon: TIcon;
begin
  FGroupManager := GroupManager;
  FNodeManager := NodeManager;

  // Уничтожаем старые иконки групп
  for I := High(FGroupIcons) downto 0 do
  begin
    FGroupIcons[I].Visible := False;
    FreeAndNil(FGroupIcons[I]);
  end;

  SetLength(FGroupIcons, 0);

  // Если менеджер групп не задан - выходим
  if FGroupManager = nil then Exit;

  // Получаем массив групп
  Groups := FGroupManager.GetGroups;
  SetLength(FGroupIcons, Length(Groups));
  SetLength(FLastGroupStates, Length(Groups));

  // Инициализируем внутренние массивы состояний в зависимости от фактического количества узлов
  for I := 0 to High(Groups) do
  begin
    SetLength(FLastGroupStates[I], Length(Groups[I].NodeIds));
  end;

  // Создаем иконку для каждой группы
  for I := 0 to High(Groups) do
  begin
    NewIcon := TTrayIcon.Create(nil);
    NewIcon.Hint := BuildGroupHint(Groups[I]);
    NewIcon.PopUpMenu := FPopupMenu;
    
    // Создаем и устанавливаем начальную иконку
    try
      TempIcon := TIconRenderer.RenderGroupIcon(Groups[I]);
      try
        if (TempIcon <> nil) and (TempIcon.Handle <> 0) then
        begin
          NewIcon.Icon.Assign(TempIcon);
        end;
      finally
        TempIcon.Free;
      end;
    except
      // Игнорируем ошибки отрисовки иконки
    end;
    
    NewIcon.Visible := True;
    FGroupIcons[I] := NewIcon;
  end;

  // Обновляем иконки (цвета LED-индикаторов)
  UpdateTrayIcons;

  // Включаем таймер для периодического обновления
  if FTimer <> nil then
    FTimer.Enabled := True;
end;

procedure TTrayController.OnTimer(Sender: TObject);
begin
  // Обновляем иконки каждую секунду
  if GroupStatesChanged then
    UpdateTrayIcons;
end;

procedure TTrayController.OnSettingsClick(Sender: TObject);
begin
  // Вызываем внешний обработчик открытия настроек
  if Assigned(FOnShowSettings) then
    FOnShowSettings(Self);
end;

procedure TTrayController.OnExitClick(Sender: TObject);
begin
  // Вызываем внешний обработчик выхода
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

procedure TTrayController.UpdateTrayIcons;
var
  I: Integer;
  Groups: TGroupArray;
  NewIcon: TIcon;
  J: Integer;
begin
  if FGroupManager = nil then Exit;

  Groups := FGroupManager.GetGroups;

  // Если количество групп изменилось - пересоздаем иконки
  if Length(Groups) <> Length(FLastGroupStates) then
  begin
    Rebuild(FGroupManager, FNodeManager);
    Exit;
  end;

  // Обновляем каждую иконку группы
  for I := 0 to High(Groups) do
  begin
    if I > High(FGroupIcons) then Break;
    if FGroupIcons[I] = nil then Continue;

    // Рисуем новую иконку с текущими состояниями
    NewIcon := TIconRenderer.RenderGroupIcon(Groups[I]);
    try
      if (NewIcon <> nil) and (NewIcon.Handle <> 0) then
      begin
        FGroupIcons[I].Icon.Assign(NewIcon);
        FGroupIcons[I].Hint := BuildGroupHint(Groups[I]);  // Обновляем подсказку
        FGroupIcons[I].Visible := True;
      end;
    finally
      NewIcon.Free;
    end;

    // Сохраняем текущие состояния для сравнения в следующий раз
    for J := 0 to High(FLastGroupStates[I]) do
      if J < Length(Groups[I].NodeStates) then
        FLastGroupStates[I][J] := Groups[I].NodeStates[J];
  end;
end;

function TTrayController.GroupStatesChanged: Boolean;
var
  I, J: Integer;
  Groups: TGroupArray;
begin
  Result := False;

  if FGroupManager = nil then Exit;

  Groups := FGroupManager.GetGroups;

  // Если количество групп изменилось - нужно обновление
  if Length(Groups) <> Length(FLastGroupStates) then
  begin
    Result := True;
    Exit;
  end;

  // Проверяем каждое состояние узла
  for I := 0 to High(Groups) do
  begin
    for J := 0 to High(Groups[I].NodeStates) do
    begin
      if J < Length(FLastGroupStates[I]) then
        if Groups[I].NodeStates[J] <> FLastGroupStates[I][J] then
        begin
          Result := True;
          Exit;
        end;
    end;
  end;
end;

procedure TTrayController.UpdateMenuLocalization;
var
  MenuItem: TMenuItem;
  I: Integer;
begin
  // Recreate popup menu with new language
  if FPopupMenu <> nil then
  begin
    // Save old menu reference for group icons
    for I := 0 to High(FGroupIcons) do
      if FGroupIcons[I] <> nil then
        FGroupIcons[I].PopUpMenu := nil;
    
    FreeAndNil(FPopupMenu);
  end;
  
  // Create new menu
  FPopupMenu := TPopupMenu.Create(nil);
  
  // Settings item
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('menu_settings');
  MenuItem.OnClick := @OnSettingsClick;
  FPopupMenu.Items.Add(MenuItem);
  
  // Separator
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := '-';
  FPopupMenu.Items.Add(MenuItem);
  
  // Exit item
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('menu_exit');
  MenuItem.OnClick := @OnExitClick;
  FPopupMenu.Items.Add(MenuItem);
  
  // Assign new menu to all group icons
  for I := 0 to High(FGroupIcons) do
    if FGroupIcons[I] <> nil then
      FGroupIcons[I].PopUpMenu := FPopupMenu;
end;

end.



