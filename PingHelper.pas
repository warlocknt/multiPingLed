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

// ICMP API из iphlpapi.dll
const
  IP_STATUS_BASE = 11000;
  IP_SUCCESS = 0;
  IP_BUF_TOO_SMALL = IP_STATUS_BASE + 1;
  IP_DEST_NET_UNREACHABLE = IP_STATUS_BASE + 2;
  IP_DEST_HOST_UNREACHABLE = IP_STATUS_BASE + 3;
  IP_DEST_PROT_UNREACHABLE = IP_STATUS_BASE + 4;
  IP_DEST_PORT_UNREACHABLE = IP_STATUS_BASE + 5;
  IP_NO_RESOURCES = IP_STATUS_BASE + 6;
  IP_BAD_OPTION = IP_STATUS_BASE + 7;
  IP_HW_ERROR = IP_STATUS_BASE + 8;
  IP_PACKET_TOO_BIG = IP_STATUS_BASE + 9;
  IP_REQ_TIMED_OUT = IP_STATUS_BASE + 10;
  IP_BAD_REQ = IP_STATUS_BASE + 11;
  IP_BAD_ROUTE = IP_STATUS_BASE + 12;
  IP_TTL_EXPIRED_TRANSIT = IP_STATUS_BASE + 13;
  IP_TTL_EXPIRED_REASSEM = IP_STATUS_BASE + 14;
  IP_PARAM_PROBLEM = IP_STATUS_BASE + 15;
  IP_SOURCE_QUENCH = IP_STATUS_BASE + 16;
  IP_OPTION_TOO_BIG = IP_STATUS_BASE + 17;
  IP_BAD_DESTINATION = IP_STATUS_BASE + 18;

type
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

  TIpOptionInformation = record
    Ttl: BYTE;
    Tos: BYTE;
    Flags: BYTE;
    OptionsSize: BYTE;
    OptionsData: PChar;
  end;

var
  hIcmp: THandle = INVALID_HANDLE_VALUE;
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
  
  // Initialize Winsock if not already done
  if not WSAInitialized then
  begin
    if WSAStartup($0202, WSAData) <> 0 then Exit;
    WSAInitialized := True;
  end;
  
  HostEnt := GetHostByName(PChar(Host));
  if HostEnt = nil then Exit;
  
  Addr := PDWORD(HostEnt^.h_addr^);
  if Addr <> nil then
    Result := Addr^;
end;

function PingHost(const Host: string; TimeoutMs: Integer): TPingResult;
var
  ReplySize: DWORD;
  ReplyBuffer: Pointer;
  IPAddr: DWORD;
  PingResult: DWORD;
  pEchoReply: PICMP_ECHO_REPLY;
begin
  Result.Success := False;
  Result.ResponseTime := -1;
  Result.ErrorMessage := '';

  // Создаем ICMP handle при первом вызове
  if hIcmp = INVALID_HANDLE_VALUE then
  begin
    hIcmp := IcmpCreateFile;
    if hIcmp = INVALID_HANDLE_VALUE then
    begin
      Result.ErrorMessage := 'Failed to create ICMP handle';
      Exit;
    end;
  end;

  // Получаем IP адрес хоста
  IPAddr := GetHostIP(Host);
  if IPAddr = 0 then
  begin
    Result.ErrorMessage := 'Cannot resolve host: ' + Host;
    Exit;
  end;

  // Размер буфера для ответа
  ReplySize := SizeOf(TICMP_ECHO_REPLY) + 32;
  GetMem(ReplyBuffer, ReplySize);

  try
    // Отправляем ICMP echo request
    PingResult := IcmpSendEcho(
      hIcmp,
      IPAddr,
      nil, 0,           // Нет данных
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
    FreeMem(ReplyBuffer);
  end;
end;

end.