unit PingHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Winsock2;

type
  TPingResult = record
    Success: Boolean;
    ResponseTime: Integer;
    ErrorMessage: string;
  end;

function PingHost(const Host: string; TimeoutMs: Integer): TPingResult;

implementation

uses PingCompare;

// ICMP API из iphlpapi.dll
const
  IP_STATUS_BASE = 11000;
  IP_SUCCESS = 0;
  IP_DEST_NET_UNREACHABLE = IP_STATUS_BASE + 2;
  IP_DEST_HOST_UNREACHABLE = IP_STATUS_BASE + 3;
  IP_REQ_TIMED_OUT = IP_STATUS_BASE + 10;

type
  TPingPayload = array[0..31] of Byte;
  PICMP_ECHO_REPLY = ^TICMP_ECHO_REPLY;
  TICMP_ECHO_REPLY = record
    Address: DWORD;
    Status: DWORD;
    RoundTripTime: DWORD;
    DataSize: WORD;
    Reserved: WORD;
    Data: Pointer;
    Options: record
      Ttl: BYTE;
      Tos: BYTE;
      Flags: BYTE;
      OptionsSize: BYTE;
      OptionsData: Pointer;
    end;
  end;


var
  WSAInitialized: Boolean = False;
  WSAData: TWSAData;

// Импортируем функции из iphlpapi.dll
function IcmpCreateFile: THandle; stdcall; external 'iphlpapi.dll';
function IcmpCloseHandle(IcmpHandle: THandle): BOOL; stdcall; external 'iphlpapi.dll';
function IcmpSendEcho(
  IcmpHandle: THandle;
  DestinationAddress: DWORD;
  RequestData: Pointer;
  RequestSize: WORD;
  RequestOptions: Pointer;
  ReplyBuffer: Pointer;
  ReplySize: DWORD;
  Timeout: DWORD
): DWORD; stdcall; external 'iphlpapi.dll';

function GetHostIP(const Host: string): DWORD;
var
  HostEnt: PHostEnt;
  Addr: PDWORD;
begin
  Result := 0;
  if not WSAInitialized then Exit;
  HostEnt := GetHostByName(PChar(Host));
  if HostEnt = nil then Exit;
  Addr := PDWORD(HostEnt^.h_addr^);
  if Addr <> nil then
    Result := Addr^;
end;

procedure CleanupPingHelper;
begin
  if WSAInitialized then
  begin
    WSACleanup;
    WSAInitialized := False;
  end;
end;

function PingHost(const Host: string; TimeoutMs: Integer): TPingResult;
var
  ReplySize: DWORD;
  ReplyBuffer: Pointer;
  IPAddr: DWORD;
  PingResult: DWORD;
  pEchoReply: PICMP_ECHO_REPLY;
  hIcmp: THandle;
  Comparison: TPingComparison;
  AddressText: string;
  RequestData: TPingPayload;
begin
  Result.Success := False;
  Result.ResponseTime := -1;
  Result.ErrorMessage := '';

  if TimeoutMs <= 0 then
  begin
    Result.ErrorMessage := 'Invalid timeout';
    Exit;
  end;
  hIcmp := IcmpCreateFile;
  if hIcmp = INVALID_HANDLE_VALUE then
  begin
    Result.ErrorMessage := 'Failed to create ICMP handle';
    Exit;
  end;

  ReplyBuffer := nil;
  Comparison := nil;
  try
    // Получаем IP адрес хоста
    IPAddr := GetHostIP(Host);
    if IPAddr = 0 then
    begin
      Result.ErrorMessage := 'Cannot resolve host: ' + Host;
      Exit;
    end;

    if ComparisonEnabled then
    begin
      AddressText := Format('%d.%d.%d.%d', [IPAddr and $FF,
        (IPAddr shr 8) and $FF, (IPAddr shr 16) and $FF, IPAddr shr 24]);
      try
        Comparison := TPingComparison.Create(Host, AddressText, TimeoutMs);
      except
        // A diagnostic launch failure must not prevent the ICMP probe.
      end;
    end;

    // Размер буфера для ответа
    RequestData := Default(TPingPayload);
    FillChar(RequestData, SizeOf(RequestData), Ord('a'));
    ReplySize := SizeOf(TICMP_ECHO_REPLY) + SizeOf(RequestData) + 8;
    GetMem(ReplyBuffer, ReplySize);
    // Отправляем ICMP echo request
    PingResult := IcmpSendEcho(
      hIcmp,
      IPAddr,
      @RequestData[0], SizeOf(RequestData), // 32 bytes of echo data
      nil,              // Опции по умолчанию
      ReplyBuffer,
      ReplySize,
      TimeoutMs
    );

    if PingResult = 0 then
    begin
      // Ошибка
      PingResult := GetLastError;
      case PingResult of
        IP_REQ_TIMED_OUT: Result.ErrorMessage := 'Request timed out';
        IP_DEST_HOST_UNREACHABLE: Result.ErrorMessage := 'Host unreachable';
        IP_DEST_NET_UNREACHABLE: Result.ErrorMessage := 'Network unreachable';
        else Result.ErrorMessage := 'Ping failed: ' + IntToStr(PingResult);
      end;
    end
    else
    begin
      // Успех
      pEchoReply := PICMP_ECHO_REPLY(ReplyBuffer);
      if pEchoReply^.Status = IP_SUCCESS then
      begin
        Result.Success := True;
        Result.ResponseTime := pEchoReply^.RoundTripTime;
      end
      else
      begin
        Result.ErrorMessage := 'Ping status: ' + IntToStr(pEchoReply^.Status);
      end;
    end;

  finally
    if ReplyBuffer <> nil then FreeMem(ReplyBuffer);
    IcmpCloseHandle(hIcmp);
    if Comparison <> nil then
    begin
      try
        Comparison.Finish(Result.Success, Result.ResponseTime, Result.ErrorMessage);
      finally Comparison.Free; end;
    end;
  end;
end;

initialization
  // Инициализируем один раз до старта потоков — исключает гонку между потоками
  WSAData := Default(TWSAData);
  if WSAStartup($0202, WSAData) = 0 then
    WSAInitialized := True;

finalization
  CleanupPingHelper;

end.
