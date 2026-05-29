unit IconRenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ConfigManager, GroupManager;

type
  TIconRenderer = class
  private
    class function GetStateColor(State: ConfigManager.TNodeState): TColor;
  public
    class function RenderGroupIcon(const Group: TGroupInfo): TIcon;
  end;

implementation

uses
  LCLType, LCLIntf;

class function TIconRenderer.GetStateColor(State: ConfigManager.TNodeState): TColor;
begin
  case State of
    nsUnknown: Result := clGray;
    nsUp: Result := clLime;
    nsDown: Result := clRed;
    else
      Result := clGray;
      // дефолт на случай неизвестного значения
  end;
end;


// ScanLine помечен как «not portable», но это намеренная техника попиксельного
// рендера; приложение Windows-only, поэтому предупреждение 5044 здесь подавляем.
{$push}{$warn 5044 off}
class function TIconRenderer.RenderGroupIcon(const Group: TGroupInfo): TIcon;
var
  Bmp: TBitmap;
  P: PRGBQuad;
  X, Y: integer;
  LEDSize, Spacing, Margin, I: integer;
begin
  Result := TIcon.Create;

  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(16, 16);
    Bmp.PixelFormat := pf32bit;

    // Обнуляем bitmap (полностью прозрачный)
    for Y := 0 to 15 do
    begin
      P := Bmp.ScanLine[Y];
      for X := 0 to 15 do
      begin
        P^.rgbBlue := 1;
        P^.rgbGreen := 1;
        P^.rgbRed := 1;
        P^.rgbReserved := 0; // <-- альфа = 0 (прозрачный)
        Inc(P);
      end;
    end;

    case Group.GroupType of
      gtSingle:
      begin
        LEDSize := 12;
        Margin := 2;

        // Защита от пустого NodeStates: иначе обращение к [0] вне границ
        if Length(Group.NodeStates) > 0 then
          Bmp.Canvas.Brush.Color := GetStateColor(Group.NodeStates[0])
        else
          Bmp.Canvas.Brush.Color := GetStateColor(nsUnknown);
        Bmp.Canvas.Pen.Style := psClear;
        Bmp.Canvas.Ellipse(Margin, Margin, Margin + LEDSize, Margin + LEDSize);
      end;

      gt2x2:
      begin
        LEDSize := 6;
        Spacing := 1;
        Margin := 1;

        for I := 0 to High(Group.NodeStates) do
        begin
          X := Margin + (I mod 2) * (LEDSize + Spacing);
          Y := Margin + (I div 2) * (LEDSize + Spacing);

          Bmp.Canvas.Brush.Color := GetStateColor(Group.NodeStates[I]);
          Bmp.Canvas.Pen.Style := psClear;
          Bmp.Canvas.Ellipse(X, Y, X + LEDSize, Y + LEDSize);
        end;
      end;

      gt3x3:
      begin
        LEDSize := 4;
        Spacing := 1;
        Margin := 0;

        for I := 0 to High(Group.NodeStates) do
        begin
          X := Margin + (I mod 3) * (LEDSize + Spacing);
          Y := Margin + (I div 3) * (LEDSize + Spacing);

          Bmp.Canvas.Brush.Color := GetStateColor(Group.NodeStates[I]);
          Bmp.Canvas.Pen.Style := psClear;
          Bmp.Canvas.Ellipse(X, Y, X + LEDSize, Y + LEDSize);
        end;
      end;
    end;


    for Y := 0 to 15 do
    begin
      P := Bmp.ScanLine[Y];
      for X := 0 to 15 do
      begin
        if P^.rgbReserved = 0 then
        begin
          // Проверяем: это фон (1,1,1) или GDI-пиксель?
          if (P^.rgbRed = 1) and (P^.rgbGreen = 1) and (P^.rgbBlue = 1) then
          begin
            // фон — оставляем прозрачным, зануляем
            P^.rgbRed := 0;
            P^.rgbGreen := 0;
            P^.rgbBlue := 0;
          end
          else
            P^.rgbReserved := 255;
          // нарисованный пиксель — делаем непрозрачным
        end;
        Inc(P);
      end;
    end;

    Result.Assign(Bmp);

  finally
    Bmp.Free;
  end;
end;
{$pop}

end.
