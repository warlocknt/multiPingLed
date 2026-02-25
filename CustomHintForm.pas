unit CustomHintForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls,
  ConfigManager, NodeManager, GroupManager;

type
  TNodeHintInfo = record
    NodeId: Integer;
    NodeName: string;
    NodeHost: string;
    NodeState: TNodeState;
    LastPingTime: TDateTime;
  end;

  { TCustomHintForm }
  TCustomHintForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
  private
    FNodeManager: TNodeManager;
    FHideTimer: TTimer;
    procedure OnHideTimer(Sender: TObject);
    procedure DrawNodeInfo(const NodeInfo: TNodeHintInfo; Y: Integer);
    function GetStateColor(State: TNodeState): TColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowForGroup(const Group: TGroupInfo; const MousePos: TPoint; NodeManager: TNodeManager);
    procedure HideHint;
  end;

implementation

uses
  LCLType, LCLIntf;

{ TCustomHintForm }

constructor TCustomHintForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, 0);
  
  // Настройка формы
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;
  Color := RGB(30, 30, 30); // Темно-серый фон
  
  // Размеры будут установлены динамически
  Width := 250;
  Height := 100;
  
  // Таймер для автоматического скрытия
  FHideTimer := TTimer.Create(Self);
  FHideTimer.Interval := 5000; // 5 секунд
  FHideTimer.OnTimer := @OnHideTimer;
  FHideTimer.Enabled := False;
end;

destructor TCustomHintForm.Destroy;
begin
  FHideTimer.Free;
  inherited Destroy;
end;

procedure TCustomHintForm.FormCreate(Sender: TObject);
begin
  // Обработка событий мыши для скрытия
  OnMouseLeave := @FormMouseLeave;
end;

procedure TCustomHintForm.FormMouseLeave(Sender: TObject);
begin
  HideHint;
end;

procedure TCustomHintForm.OnHideTimer(Sender: TObject);
begin
  HideHint;
end;

function TCustomHintForm.GetStateColor(State: TNodeState): TColor;
begin
  case State of
    nsUp: Result := clGreen;
    nsDown: Result := clRed;
    else Result := clGray;
  end;
end;

procedure TCustomHintForm.DrawNodeInfo(const NodeInfo: TNodeHintInfo; Y: Integer);
var
  TimeStr: string;
  StatusText: string;
  TextY: Integer;
  OldFont: TFont;
begin
  OldFont := TFont.Create;
  try
    OldFont.Assign(Self.Canvas.Font);

    // Имя узла - белым цветом
    Self.Canvas.Font.Color := clWhite;
    Self.Canvas.Font.Style := [fsBold];
    Self.Canvas.Font.Size := 9;
    Self.Canvas.TextOut(10, Y, NodeInfo.NodeName);

    // IP - серым
    Self.Canvas.Font.Color := clSilver;
    Self.Canvas.Font.Style := [];
    Self.Canvas.TextOut(10, Y + 16, NodeInfo.NodeHost);

    // Статус и время - цветом состояния
    Self.Canvas.Font.Color := GetStateColor(NodeInfo.NodeState);
    Self.Canvas.Font.Style := [fsBold];

    case NodeInfo.NodeState of
      nsUp: StatusText := 'ONLINE';
      nsDown: StatusText := 'OFFLINE';
      else StatusText := 'UNKNOWN';
    end;

    if NodeInfo.LastPingTime > 0 then
      TimeStr := FormatDateTime('hh:nn:ss', NodeInfo.LastPingTime)
    else
      TimeStr := '--:--:--';

    TextY := Y + 32;
    Self.Canvas.TextOut(10, TextY, StatusText + '  ' + TimeStr);

    Self.Canvas.Font.Assign(OldFont);
  finally
    OldFont.Free;
  end;
end;

procedure TCustomHintForm.ShowForGroup(const Group: TGroupInfo; const MousePos: TPoint; NodeManager: TNodeManager);
var
  I: Integer;
  Node: TNodeConfig;
  NodeInfo: TNodeHintInfo;
  TotalHeight: Integer;
  X, Y: Integer;
  ScreenWidth, ScreenHeight: Integer;
begin
  FNodeManager := NodeManager;
  
  // Очищаем и рисуем заново
  Canvas.Brush.Color := RGB(30, 30, 30);
  Canvas.FillRect(ClientRect);
  
  // Рамка
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(0, 0, Width, Height);
  
  // Заголовок - имя группы
  Canvas.Font.Color := clYellow;
  Canvas.Font.Style := [fsBold];
  Canvas.Font.Size := 10;
  Canvas.TextOut(10, 8, Group.Name);
  
  Canvas.Pen.Color := clGray;
  Canvas.MoveTo(10, 28);
  Canvas.LineTo(Width - 10, 28);
  
  // Рисуем информацию о каждом узле
  TotalHeight := 35;
  for I := 0 to High(Group.NodeIds) do
  begin
    if Group.NodeIds[I] = 0 then Continue;
    
    Node := FNodeManager.GetNodeById(Group.NodeIds[I]);
    if Node.Id = 0 then Continue;
    
    NodeInfo.NodeId := Node.Id;
    NodeInfo.NodeName := Node.Name;
    NodeInfo.NodeHost := Node.Host;
    NodeInfo.NodeState := Node.State;
    NodeInfo.LastPingTime := Node.LastPingTime;
    
    DrawNodeInfo(NodeInfo, TotalHeight);
    
    TotalHeight := TotalHeight + 55; // Высота одного блока узла
    
    if TotalHeight > 300 then Break; // Максимальная высота
  end;
  
  // Устанавливаем высоту формы
  Height := TotalHeight + 10;
  
  // Позиционируем окно рядом с курсором
  ScreenWidth := Screen.Width;
  ScreenHeight := Screen.Height;
  
  X := MousePos.X + 16;
  Y := MousePos.Y + 16;
  
  // Проверяем границы экрана
  if X + Width > ScreenWidth then
    X := MousePos.X - Width - 8;
  if Y + Height > ScreenHeight then
    Y := MousePos.Y - Height - 8;
  
  Left := X;
  Top := Y;
  
  // Показываем
  Show;
  BringToFront;
  
  // Запускаем таймер скрытия
  FHideTimer.Enabled := False;
  FHideTimer.Enabled := True;
end;

procedure TCustomHintForm.HideHint;
begin
  FHideTimer.Enabled := False;
  Hide;
end;

end.
