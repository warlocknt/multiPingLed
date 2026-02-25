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
    nsUp: Result := clGreen;
    nsDown: Result := clRed;
  end;
end;


class function TIconRenderer.RenderGroupIcon(const Group: TGroupInfo): TIcon;
var
  Bmp: TBitmap;
  P: PRGBQuad;
  X, Y: Integer;
  LEDSize, Spacing, Margin, I: Integer;
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

          Bmp.Canvas.Brush.Color := GetStateColor(Group.NodeStates[0]);
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
        P^.rgbRed := 0; P^.rgbGreen := 0; P^.rgbBlue := 0;
      end
      else
        P^.rgbReserved := 255; // нарисованный пиксель — делаем непрозрачным
    end;
    Inc(P);
  end;
end;

    Result.Assign(Bmp);

  finally
    Bmp.Free;
  end;
end;




{
class function TIconRenderer.RenderGroupIcon(const Group: TGroupInfo): TIcon;
var
  BmpColor: TBitmap;
  BmpMask: TBitmap;
  LEDSize, Spacing, Margin: Integer;
  I, X, Y: Integer;
  IconWidth, IconHeight: Integer;
begin
  IconWidth := 16;
  IconHeight := 16;

  Result := TIcon.Create;
  
  // Создаем цветной битмап (24-бит)
  BmpColor := TBitmap.Create;
  BmpColor.Width := IconWidth;
  BmpColor.Height := IconHeight;
  BmpColor.PixelFormat := pf24bit;
  
  // Создаем маску (монохромная)
  BmpMask := TBitmap.Create;
  BmpMask.Width := IconWidth;
  BmpMask.Height := IconHeight;
  BmpMask.Monochrome := True;
  BmpMask.PixelFormat := pf1bit;

  try
    // Очищаем цветной битмап - черный фон
    BmpColor.Canvas.Brush.Color := clBlack;
    BmpColor.Canvas.FillRect(0, 0, IconWidth, IconHeight);
    
    // Очищаем маску - ЧЕРНЫЙ (все прозрачное)
    // В маске: черный = прозрачный, белый = непрозрачный
    BmpMask.Canvas.Brush.Color := clBlack;
    BmpMask.Canvas.FillRect(0, 0, IconWidth, IconHeight);

    // Рисуем LED-индикаторы
    case Group.GroupType of
      gtSingle:
        begin
          LEDSize := 12;
          Margin := 2;
          
          // Цветной круг
          BmpColor.Canvas.Brush.Color := GetStateColor(Group.NodeStates[0]);
          BmpColor.Canvas.Pen.Color := GetStateColor(Group.NodeStates[0]);
          BmpColor.Canvas.Ellipse(Margin, Margin, Margin + LEDSize, Margin + LEDSize);
          
          // БЕЛЫЙ круг в маске (непрозрачный)
          BmpMask.Canvas.Brush.Color := clWhite;
          BmpMask.Canvas.Pen.Color := clWhite;
          BmpMask.Canvas.Ellipse(Margin, Margin, Margin + LEDSize, Margin + LEDSize);
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
            
            BmpColor.Canvas.Brush.Color := GetStateColor(Group.NodeStates[I]);
            BmpColor.Canvas.Pen.Color := GetStateColor(Group.NodeStates[I]);
            BmpColor.Canvas.Ellipse(X, Y, X + LEDSize, Y + LEDSize);
            
            BmpMask.Canvas.Brush.Color := clWhite;
            BmpMask.Canvas.Pen.Color := clWhite;
            BmpMask.Canvas.Ellipse(X, Y, X + LEDSize, Y + LEDSize);
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
            
            BmpColor.Canvas.Brush.Color := GetStateColor(Group.NodeStates[I]);
            BmpColor.Canvas.Pen.Color := GetStateColor(Group.NodeStates[I]);
            BmpColor.Canvas.Ellipse(X, Y, X + LEDSize, Y + LEDSize);
            
            BmpMask.Canvas.Brush.Color := clWhite;
            BmpMask.Canvas.Pen.Color := clWhite;
            BmpMask.Canvas.Ellipse(X, Y, X + LEDSize, Y + LEDSize);
          end;
        end;
    end;

    // Создаем иконку с маской
    Result.Width := IconWidth;
    Result.Height := IconHeight;
    Result.Assign(BmpColor);
    Result.MaskHandle := BmpMask.Handle;
  finally
    BmpColor.Free;
    BmpMask.Free;
  end;
end;
}

end.
