unit TrayController;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Graphics, Forms, Menus, ExtCtrls, Controls,
  LCLIntf, LCLType,
  ConfigManager, GroupManager, NodeManager, LangManager, IconRenderer;

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
    FAboutShow: TNotifyEvent;        // Обработчик "О программе"
    FTimer: TTimer;                   // Таймер для обновления иконок
    FSettingsShowing: Boolean;         // Флаг: форма настроек показывается
    FBalloonHintEnabled: Boolean;      // Показывать balloon hint при смене статуса
    // Сохраненные состояния узлов для определения изменений
    FLastGroupStates: array of array of ConfigManager.TNodeState;

    // Создание контекстного меню (Settings, Exit)
    procedure CreatePopupMenu;
    // Создание иконки-заглушки (зеленый круг)
    function CreateGreenDotIcon(Size: Integer): TIcon;
    // Обработчик клика по "Settings"
    procedure RefreshHints;
    procedure OnSettingsClick(Sender: TObject);
    // Отложенный вызов ShowSettings
    procedure DoShowSettings(Data: PtrInt);
    // Обработчик клика по "About"
    procedure OnAboutClick(Sender: TObject);
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
    // Поиск иконки по ID группы
    function FindIconIndexByGroupId(Id: Integer): Integer;
    // Создание одной иконки для группы
    procedure CreateIconForGroup(const Group: TGroupInfo; var OutIdx: Integer);
    // Показать balloon hint об изменении статуса узла
    procedure ShowStatusBalloon(IconIdx, NodeId: Integer; NewState: ConfigManager.TNodeState);
  public
    constructor Create;
    destructor Destroy; override;
    // Инициализация (показ иконок)
    procedure Initialize;
    // Полное пересоздание иконок при изменении конфигурации
    procedure Rebuild(GroupManager: TGroupManager; NodeManager: TNodeManager);
    // Инкрементальное обновление - только новые/удалённые группы
    procedure SyncGroups(GroupManager: TGroupManager; NodeManager: TNodeManager);
    procedure UpdateMenuLocalization;
    procedure SetSettingsEnabled(Enabled: Boolean);
    property OnShowSettings: TNotifyEvent read FOnShowSettings write FOnShowSettings;
    property OnAboutShow: TNotifyEvent read FAboutShow write FAboutShow;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property BalloonHintEnabled: Boolean read FBalloonHintEnabled write FBalloonHintEnabled;
  end;

implementation

uses LogWriter;

procedure DebugLog(const Msg: string);
begin
  ApplicationLog('multiPingLed[Tray]: ' + Msg);
end;

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
  FBalloonHintEnabled := True;  // По умолчанию включено
end;

destructor TTrayController.Destroy;
var
  I: Integer;
begin
  Application.RemoveAsyncCalls(Self);
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
      FGroupIcons[I].Hide;  // Скрываем иконку из трея
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

  // Пункт меню "About"
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('menu_about');
  MenuItem.OnClick := @OnAboutClick;
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

function TTrayController.CreateGreenDotIcon(Size: Integer): TIcon;
var
  FakeGroup: TGroupInfo;
begin
  Result := nil;
  
  // Создаем фиктивную группу с одним узлом в состоянии nsUp
  FakeGroup.Id := 0;
  FakeGroup.Name := '';
  FakeGroup.GroupType := ConfigManager.gtSingle;
  SetLength(FakeGroup.NodeIds, 1);
  FakeGroup.NodeIds[0] := 0;
  SetLength(FakeGroup.NodeStates, 1);
  FakeGroup.NodeStates[0] := ConfigManager.nsUp;
  
  // Используем IconRenderer для создания иконки
  Result := TIconRenderer.RenderGroupIcon(FakeGroup);
end;

procedure TTrayController.Initialize;
begin
  DebugLog('Initialize called');
  // Иконки групп создаются и показываются в методе Rebuild
end;

function TTrayController.BuildGroupHint(const Group: TGroupInfo): string;

  function FitText(const Text: string; MaxChars: Integer): string;
  var U: UnicodeString;
  begin
    U := UTF8Decode(Text);
    if Length(U) > MaxChars then
    begin
      U := Copy(U, 1, MaxChars);
      if (Length(U) > 0) and (Ord(U[Length(U)]) >= $D800) and
        (Ord(U[Length(U)]) <= $DBFF) then SetLength(U, Length(U) - 1);
    end;
    Result := UTF8Encode(U);
  end;

  function GetNodeName(NodeId: Integer): string;
  var
    Node: TNodeConfig;
  begin
    Result := '';
    if NodeId = 0 then Exit;

    Node := FNodeManager.GetNodeById(NodeId);
    if Node.Id = 0 then Exit;

    case Node.State of
      nsUp: Result := '+';
      nsDown: Result := '-';
      else Result := '?';
    end;
    Result := '"' + Result + Node.Name + '"';
  end;

  function FormatNodeFull(NodeId, MaxChars: Integer; Compact: Boolean): string;
  var
    Node: TNodeConfig;
    StatusText: string;
    TimeStr, PingStr, Tail: string;
    NameBudget: Integer;
  begin
    Result := '';
    if NodeId = 0 then Exit;
    
    Node := FNodeManager.GetNodeById(NodeId);
    if Node.Id = 0 then Exit;
    
    // Форматируем время
    if Node.LastStateChange > 0 then
    begin
      if Compact then TimeStr := FormatDateTime('hh:nn', Node.LastStateChange)
      else TimeStr := FormatDateTime('dd.mm hh:nn', Node.LastStateChange);
    end
    else
      TimeStr := '--:--';
    
    // Определяем текст статуса
    case Node.State of
      nsUp: StatusText := _('status_ok');
      nsDown: StatusText := _('status_fail');
    else
      StatusText := '?';
    end;
    
    if Node.LastPingMs >= 0 then PingStr := IntToStr(Node.LastPingMs) + _('unit_ms')
    else PingStr := '--';
    Tail := Format('%s@%s %s -%d/%d',
      [StatusText, TimeStr, PingStr, Node.ProbeLosses, Node.ProbeCount]);
    NameBudget := MaxChars - Length(UTF8Decode(Tail)) - 1;
    if NameBudget < 1 then NameBudget := 1;
    Result := FitText(FitText(Node.Name, NameBudget) + ':' + Tail, MaxChars);
  end;

var
  I, NodeCount, LineBudget: Integer;
  GroupName: string;
  FirstNodeName: string;
  LeftName, RightName: string;
begin
  Result := FitText(Group.Name, 127);
  if Result = '' then Result := 'multiPingLed';
  if FNodeManager = nil then Exit;

  GroupName := Result;
  
  // Для группы 3x3 используем компактные имена с символом статуса.
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
    Result := FitText(GroupName, 24) + #13#10;
    NodeCount := 0;
    for I := 0 to High(Group.NodeIds) do
      if Group.NodeIds[I] <> 0 then Inc(NodeCount);
    LineBudget := 100;
    if NodeCount > 0 then
      LineBudget := (127 - Length(UTF8Decode(Result)) - 2 * (NodeCount - 1)) div NodeCount;
    
    for I := 0 to Length(Group.NodeIds)-1 do
    begin
      if Group.NodeIds[I] = 0 then Continue;
      
      Result := Result + FormatNodeFull(Group.NodeIds[I], LineBudget, Group.GroupType = gt2x2) + #13#10;
    end;
    
    // Удаляем последний перевод строки
    if Length(Result) > 2 then
      SetLength(Result, Length(Result) - 2);
  end;
  Result := FitText(Result, 127);
end;

procedure TTrayController.Rebuild(GroupManager: TGroupManager; NodeManager: TNodeManager);
var
  I: Integer;
  Groups: TGroupArray;
  NewIcon: TTrayIcon;
  TempIcon: TIcon;
begin
DebugLog('Rebuild called');
FGroupManager := GroupManager;
  FNodeManager := NodeManager;

  // Уничтожаем старые иконки групп
  for I := High(FGroupIcons) downto 0 do
  begin
    if FGroupIcons[I] <> nil then
    begin
      FGroupIcons[I].Hide;
      FreeAndNil(FGroupIcons[I]);
    end;
  end;

  SetLength(FGroupIcons, 0);

  // Если менеджер групп не задан - выходим
  if FGroupManager = nil then 
  begin
DebugLog('FGroupManager is nil, exiting');
Exit;
  end;

  // Получаем массив групп
  Groups := FGroupManager.GetGroups;
DebugLog('Got ' + IntToStr(Length(Groups)) + ' groups');
// Если нет групп - создаем заглушку-иконку
  if Length(Groups) = 0 then
  begin
DebugLog('No groups found, creating placeholder icon');
SetLength(FGroupIcons, 1);
    SetLength(FLastGroupStates, 1);
    SetLength(FLastGroupStates[0], 0);
    
    NewIcon := TTrayIcon.Create(nil);
    try
      NewIcon.Hint := 'multiPingLed';
      NewIcon.PopUpMenu := FPopupMenu;
      NewIcon.OnDblClick := @OnSettingsClick;
      
      // Создаем зеленую иконку (как один узел в состоянии Up)
      TempIcon := CreateGreenDotIcon(16);
      try
        if (TempIcon <> nil) and (TempIcon.Handle <> 0) then
        begin
          NewIcon.Icon.Assign(TempIcon);
        end;
      finally
        TempIcon.Free;
      end;
      
      FGroupIcons[0] := NewIcon;
      NewIcon.Show;
DebugLog('Placeholder icon shown');
except
      on E: Exception do
      begin
DebugLog('Exception creating placeholder icon: ' + E.Message);
FreeAndNil(NewIcon);
        FGroupIcons[0] := nil;
      end;
    end;
    
    // Включаем таймер
    if FTimer <> nil then
      FTimer.Enabled := True;
    Exit;
  end;

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
    try
DebugLog('Creating tray icon for group ' + IntToStr(I) + ': ' + Groups[I].Name);

      // В Lazarus TIcon создаётся автоматически при создании TTrayIcon
      // Но проверим, что он существует
      if NewIcon.Icon = nil then
      begin
        DebugLog('Icon is nil after TTrayIcon.Create!');
      end;
NewIcon.Hint := BuildGroupHint(Groups[I]);
      NewIcon.PopUpMenu := FPopupMenu;
      NewIcon.OnDblClick := @OnSettingsClick;
      NewIcon.Tag := Groups[I].Id;

      // Создаем и устанавливаем начальную иконку
      TempIcon := TIconRenderer.RenderGroupIcon(Groups[I]);
      try
        if (TempIcon <> nil) and (TempIcon.Handle <> 0) then
        begin
          NewIcon.Icon.Assign(TempIcon);
DebugLog('Icon assigned successfully');
end
        else
        begin
DebugLog('RenderGroupIcon returned nil or invalid handle');
end;
      finally
        TempIcon.Free;
      end;

      FGroupIcons[I] := NewIcon;
      
      // В Lazarus используется Show вместо Visible:=True
      NewIcon.Show;
DebugLog('Icon shown successfully for group ' + IntToStr(I));
except
      on E: Exception do
      begin
DebugLog('Exception creating icon: ' + E.Message);
FreeAndNil(NewIcon);
        FGroupIcons[I] := nil;
      end;
    end;
  end;

  // Обновляем иконки (цвета LED-индикаторов)
  UpdateTrayIcons;

  // Включаем таймер для периодического обновления
  if FTimer <> nil then
    FTimer.Enabled := True;
end;

procedure TTrayController.OnTimer(Sender: TObject);
begin
  DebugLog('OnTimer triggered');
  // Обновляем иконки каждую секунду
  if GroupStatesChanged then
  begin
    DebugLog('OnTimer: states changed, updating');
    UpdateTrayIcons;
  end;
  RefreshHints;
end;

procedure TTrayController.RefreshHints;
var Groups: TGroupArray; I, IconIdx: Integer; HintText: string;
begin
  if FGroupManager = nil then Exit;
  Groups := FGroupManager.GetGroups;
  for I := 0 to High(Groups) do
    begin
      IconIdx := FindIconIndexByGroupId(Groups[I].Id);
      if IconIdx < 0 then Continue;
      HintText := BuildGroupHint(Groups[I]);
      if FGroupIcons[IconIdx].Hint <> HintText then FGroupIcons[IconIdx].Hint := HintText;
    end;
end;

procedure TTrayController.OnSettingsClick(Sender: TObject);
begin
  if (FPopupMenu <> nil) and (FPopupMenu.Items.Count > 0) and
    not FPopupMenu.Items[0].Enabled then Exit;
  if FSettingsShowing then Exit;
  FSettingsShowing := True;
  // Отложенный вызов чтобы popup меню успело закрыться
  Application.QueueAsyncCall(@DoShowSettings, 0);
end;

procedure TTrayController.DoShowSettings(Data: PtrInt);
begin
  try
    if Assigned(FOnShowSettings) then
      FOnShowSettings(Self);
  finally
    FSettingsShowing := False;
  end;
end;

procedure TTrayController.OnAboutClick(Sender: TObject);
begin
  // Вызываем внешний обработчик открытия "О программе"
  if Assigned(FAboutShow) then
    FAboutShow(Self);
end;

procedure TTrayController.OnExitClick(Sender: TObject);
begin
  // Вызываем внешний обработчик выхода
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

procedure TTrayController.UpdateTrayIcons;
var
  I, IconIdx: Integer;
  Groups: TGroupArray;
  NewIcon: TIcon;
  J: Integer;
  OldState: ConfigManager.TNodeState;
begin
  if FGroupManager = nil then Exit;

  Groups := FGroupManager.GetGroups;

  // Если количество групп изменилось - пересоздаем иконки
  if Length(Groups) <> Length(FLastGroupStates) then
  begin
    // Не вызываем Rebuild из UpdateTrayIcons (может быть рекурсия)
    // Вместо этого используем SyncGroups
    SyncGroups(FGroupManager, FNodeManager);
    Exit;
  end;

  // Если нет групп (заглушка) - ничего не обновляем
  if Length(Groups) = 0 then Exit;

  // Обновляем каждую иконку группы
  for I := 0 to High(Groups) do
  begin
    IconIdx := FindIconIndexByGroupId(Groups[I].Id);
    if IconIdx < 0 then Continue;

    // Рисуем новую иконку с текущими состояниями
    NewIcon := TIconRenderer.RenderGroupIcon(Groups[I]);
    try
      if (NewIcon <> nil) and (NewIcon.Handle <> 0) then
      begin
        FGroupIcons[IconIdx].Icon.Assign(NewIcon);
        FGroupIcons[IconIdx].Hint := BuildGroupHint(Groups[I]);  // Обновляем подсказку
        // Иконка уже показана, просто обновляем
      end;
    finally
      NewIcon.Free;
    end;

    // Показываем balloon для каждого узла, чьё состояние изменилось.
    // Пропускаем переходы из nsUnknown — это стартовая инициализация.
    for J := 0 to High(Groups[I].NodeStates) do
    begin
      if J >= Length(FLastGroupStates[IconIdx]) then Continue;
      OldState := FLastGroupStates[IconIdx][J];
      if (Groups[I].NodeStates[J] <> OldState) and (OldState <> ConfigManager.nsUnknown) then
        ShowStatusBalloon(IconIdx, Groups[I].NodeIds[J], Groups[I].NodeStates[J]);
    end;

    // Сохраняем текущие состояния для сравнения в следующий раз.
    // Подгоняем длину под актуальный NodeStates — иначе новые узлы не отслеживаются.
    if Length(FLastGroupStates[IconIdx]) <> Length(Groups[I].NodeStates) then
      SetLength(FLastGroupStates[IconIdx], Length(Groups[I].NodeStates));
    for J := 0 to High(Groups[I].NodeStates) do
      FLastGroupStates[IconIdx][J] := Groups[I].NodeStates[J];
  end;
  DebugLog('UpdateTrayIcons completed');
end;

procedure TTrayController.ShowStatusBalloon(IconIdx, NodeId: Integer; NewState: ConfigManager.TNodeState);
var
  Node: TNodeConfig;
  StateText: string;
  Flags: TBalloonFlags;
begin
  if not FBalloonHintEnabled then Exit;
  if (IconIdx < 0) or (IconIdx > High(FGroupIcons)) then Exit;
  if FGroupIcons[IconIdx] = nil then Exit;
  if FNodeManager = nil then Exit;

  Node := FNodeManager.GetNodeById(NodeId);
  if (Node.Id = 0) or Node.NotificationsMuted then Exit;

  case NewState of
    nsUp:   begin StateText := _('state_up');   Flags := bfInfo; end;
    nsDown: begin StateText := _('state_down'); Flags := bfWarning; end;
  else
    Exit;  // nsUnknown — не показываем
  end;

  FGroupIcons[IconIdx].BalloonTitle := Node.Name;
  FGroupIcons[IconIdx].BalloonHint := UTF8Encode(UTF8Decode(Node.Host) +
    ' — ' + UTF8Decode(StateText));
  FGroupIcons[IconIdx].BalloonFlags := Flags;
  FGroupIcons[IconIdx].BalloonTimeout := 2000;
  FGroupIcons[IconIdx].ShowBalloonHint;
end;

function TTrayController.FindIconIndexByGroupId(Id: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FGroupIcons) do
  begin
    if (FGroupIcons[I] <> nil) and (FGroupIcons[I].Tag = Id) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

procedure TTrayController.CreateIconForGroup(const Group: TGroupInfo; var OutIdx: Integer);
var
  NewIcon: TTrayIcon;
  TempIcon: TIcon;
  I: Integer;
begin
  OutIdx := -1;
  NewIcon := TTrayIcon.Create(nil);
  try
    NewIcon.Hint := BuildGroupHint(Group);
    NewIcon.PopUpMenu := FPopupMenu;
    NewIcon.OnDblClick := @OnSettingsClick;
    NewIcon.Tag := Group.Id;

    TempIcon := TIconRenderer.RenderGroupIcon(Group);
    try
      if (TempIcon <> nil) and (TempIcon.Handle <> 0) then
        NewIcon.Icon.Assign(TempIcon);
    finally
      TempIcon.Free;
    end;

    OutIdx := Length(FGroupIcons);
    SetLength(FGroupIcons, OutIdx + 1);
    FGroupIcons[OutIdx] := NewIcon;

    // Инициализируем массив состояний начальными значениями
    SetLength(FLastGroupStates, OutIdx + 1);
    SetLength(FLastGroupStates[OutIdx], Length(Group.NodeIds));
    for I := 0 to High(Group.NodeIds) do
      FLastGroupStates[OutIdx][I] := Group.NodeStates[I];

    NewIcon.Show;
    DebugLog('Created icon for group ' + IntToStr(Group.Id) + ': ' + Group.Name);
  except
    on E: Exception do
    begin
      DebugLog('Exception creating icon: ' + E.Message);
      if (OutIdx >= 0) and (OutIdx < Length(FGroupIcons)) then
      begin
        FGroupIcons[OutIdx] := nil;
        SetLength(FGroupIcons, OutIdx);
        SetLength(FLastGroupStates, OutIdx);
      end;
      FreeAndNil(NewIcon);
      OutIdx := -1;
    end;
  end;
end;

procedure TTrayController.SyncGroups(GroupManager: TGroupManager; NodeManager: TNodeManager);
var
  Groups: TGroupArray;
  I, Idx: Integer;
  ExistingIds: array of Boolean;
  RemovedCount: Integer;
begin
  if GroupManager = nil then Exit;

  Groups := GroupManager.GetGroups;

  // Если это первый запуск или нет иконок - полный rebuild
  if Length(FGroupIcons) = 0 then
  begin
    Rebuild(GroupManager, NodeManager);
    Exit;
  end;

  // Если все группы отключены - создаём заглушку
  if Length(Groups) = 0 then
  begin
    DebugLog('All groups disabled, creating placeholder');
    // Удаляем все существующие иконки
    for I := High(FGroupIcons) downto 0 do
    begin
      if FGroupIcons[I] <> nil then
      begin
        FGroupIcons[I].Hide;
        FreeAndNil(FGroupIcons[I]);
      end;
    end;
    SetLength(FGroupIcons, 0);
    SetLength(FLastGroupStates, 0);
    // Пересоздаём заглушку
    Rebuild(GroupManager, NodeManager);
    Exit;
  end;

  FGroupManager := GroupManager;
  FNodeManager := NodeManager;

  // Помечаем существующие иконки
  ExistingIds := nil;
  SetLength(ExistingIds, Length(FGroupIcons));
  for I := 0 to High(ExistingIds) do
    ExistingIds[I] := False;

  // Находим совпадающие группы по ID
  for I := 0 to High(Groups) do
  begin
    Idx := FindIconIndexByGroupId(Groups[I].Id);
    if Idx >= 0 then
      ExistingIds[Idx] := True;
  end;

  // Удаляем иконки удалённых/отключённых групп
  RemovedCount := 0;
  for I := High(FGroupIcons) downto 0 do
  begin
    if not ExistingIds[I] then
    begin
      DebugLog('Removing icon at index ' + IntToStr(I) + ' (group removed)');
      if FGroupIcons[I] <> nil then
      begin
        FGroupIcons[I].Hide;
        FreeAndNil(FGroupIcons[I]);
      end;
      Inc(RemovedCount);
    end;
  end;

  // Сжимаем массивы, удаляя nil-элементы (параллельно для FGroupIcons и FLastGroupStates)
  if RemovedCount > 0 then
  begin
    Idx := 0;
    for I := 0 to High(FGroupIcons) do
    begin
      if FGroupIcons[I] <> nil then
      begin
        if Idx <> I then
        begin
          FGroupIcons[Idx] := FGroupIcons[I];
          // Копируем соответствующие состояния параллельно
          if I < Length(FLastGroupStates) then
          begin
            if Idx >= Length(FLastGroupStates) then
              SetLength(FLastGroupStates, Idx + 1);
            FLastGroupStates[Idx] := FLastGroupStates[I];
          end;
        end;
        Inc(Idx);
      end;
    end;
    SetLength(FGroupIcons, Idx);
    SetLength(FLastGroupStates, Idx);
  end;

  // Создаём иконки для новых групп
  for I := 0 to High(Groups) do
  begin
    if FindIconIndexByGroupId(Groups[I].Id) < 0 then
      CreateIconForGroup(Groups[I], Idx);
  end;

  // Обновляем все иконки
  UpdateTrayIcons;

  // Включаем таймер
  if FTimer <> nil then
    FTimer.Enabled := True;
end;

function TTrayController.GroupStatesChanged: Boolean;
var
  I, J, IconIdx: Integer;
  Groups: TGroupArray;
begin
  Result := False;

  if FGroupManager = nil then Exit;

  Groups := FGroupManager.GetGroups;

  // Если нет групп (режим заглушки) - не обновляем
  if Length(Groups) = 0 then Exit;

  // Если количество групп изменилось - нужно обновление
  if Length(Groups) <> Length(FLastGroupStates) then
  begin
    Result := True;
    Exit;
  end;

  // Проверяем каждое состояние узла
  for I := 0 to High(Groups) do
  begin
    IconIdx := FindIconIndexByGroupId(Groups[I].Id);
    if (IconIdx < 0) or (IconIdx >= Length(FLastGroupStates)) then Exit(True);
    if Length(Groups[I].NodeStates) <> Length(FLastGroupStates[IconIdx]) then Exit(True);
    for J := 0 to High(Groups[I].NodeStates) do
    begin
      if J < Length(FLastGroupStates[IconIdx]) then
        if Groups[I].NodeStates[J] <> FLastGroupStates[IconIdx][J] then
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

  // About item
  MenuItem := TMenuItem.Create(FPopupMenu);
  MenuItem.Caption := _('menu_about');
  MenuItem.OnClick := @OnAboutClick;
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

procedure TTrayController.SetSettingsEnabled(Enabled: Boolean);
var
  MenuItem: TMenuItem;
begin
  // Находим пункт Settings в меню (первый элемент)
  if (FPopupMenu <> nil) and (FPopupMenu.Items.Count > 0) then
  begin
    MenuItem := FPopupMenu.Items[0];
    if (MenuItem <> nil) and (MenuItem.Caption <> '-') then
    begin
      MenuItem.Enabled := Enabled;
      DebugLog('Settings menu item enabled=' + BoolToStr(Enabled, True));
    end;
  end;
end;

end.



