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

class function TIconRenderer.GetStateColor(State: ConfigManager.TNodeState): TColor;
begin
  case State of
    nsUp: Result := RGBToColor(47, 225, 119);
    nsDown: Result := RGBToColor(245, 69, 83);
  else
    Result := RGBToColor(129, 140, 155);
  end;
end;

class function TIconRenderer.RenderGroupIcon(const Group: TGroupInfo): TIcon;
const
  IconSize = 32;
  PixelOffset = 6 + 16 + 40;
  PixelBytes = IconSize * IconSize * 4;
  MaskBytes = IconSize * 4; // Each monochrome mask row is DWORD aligned.
var
  Data: array[0..PixelOffset + PixelBytes + MaskBytes - 1] of Byte;
  Stream: TMemoryStream;
  Side, Count, I, X, Y, Offset, MaskOffset: Integer;
  Cell, CX, CY, Radius, Distance, Coverage: Double;
  Color: TColor;
  State: ConfigManager.TNodeState;

  procedure PutWord(Index: Integer; Value: Word);
  begin
    Data[Index] := Value and $FF;
    Data[Index + 1] := (Value shr 8) and $FF;
  end;

  procedure PutDWord(Index: Integer; Value: LongWord);
  begin
    PutWord(Index, Value and $FFFF);
    PutWord(Index + 2, Value shr 16);
  end;

begin
  FillChar(Data, SizeOf(Data), 0);
  // ICO directory, one 32-bit DIB with straight alpha and an AND mask.
  PutWord(2, 1);
  PutWord(4, 1);
  Data[6] := IconSize;
  Data[7] := IconSize;
  PutWord(10, 1);
  PutWord(12, 32);
  PutDWord(14, 40 + PixelBytes + MaskBytes);
  PutDWord(18, 22);
  PutDWord(22, 40);
  PutDWord(26, IconSize);
  PutDWord(30, IconSize * 2); // DIB height includes the AND mask.
  PutWord(34, 1);
  PutWord(36, 32);
  PutDWord(42, PixelBytes + MaskBytes);

  case Group.GroupType of
    gt2x2: Side := 2;
    gt3x3: Side := 3;
  else
    Side := 1;
  end;
  Cell := IconSize / Side;
  Radius := Cell * 0.40;
  Count := Length(Group.NodeStates);
  if Count > Side * Side then Count := Side * Side;
  // Keep the single unknown indicator before the first state arrives.
  if (Side = 1) and (Count = 0) then Count := 1;
  for I := 0 to Count - 1 do
  begin
    State := nsUnknown;
    if I < Length(Group.NodeStates) then State := Group.NodeStates[I];
    Color := GetStateColor(State);
    CX := (I mod Side) * Cell + Cell / 2;
    CY := (I div Side) * Cell + Cell / 2;
    for Y := 0 to IconSize - 1 do
      for X := 0 to IconSize - 1 do
      begin
        Distance := Sqrt(Sqr(X + 0.5 - CX) + Sqr(Y + 0.5 - CY));
        if Distance >= Radius then Continue;
        Coverage := Radius - Distance;
        if Coverage > 1 then Coverage := 1;
        // DIB rows run bottom to top; color bytes are BGRA.
        Offset := PixelOffset + ((IconSize - 1 - Y) * IconSize + X) * 4;
        Data[Offset] := Blue(Color);
        Data[Offset + 1] := Green(Color);
        Data[Offset + 2] := Red(Color);
        Data[Offset + 3] := Trunc(Coverage * 255);
      end;
  end;
  for Y := 0 to IconSize - 1 do
    for X := 0 to IconSize - 1 do
    begin
      Offset := PixelOffset + (Y * IconSize + X) * 4;
      if Data[Offset + 3] = 0 then
      begin
        MaskOffset := PixelOffset + PixelBytes + Y * 4 + X div 8;
        Data[MaskOffset] := Data[MaskOffset] or (Byte($80) shr (X mod 8));
      end;
    end;

  Result := TIcon.Create;
  try
    Stream := TMemoryStream.Create;
    try
      Stream.WriteBuffer(Data, SizeOf(Data));
      Stream.Position := 0;
      Result.LoadFromStream(Stream);
    finally
      FreeAndNil(Stream);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

end.
