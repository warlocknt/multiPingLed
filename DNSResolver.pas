unit DNSResolver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

const
  DNSLookupTimeoutMs = 3000;

type
  {$push}
  {$packrecords c}
  PDNSAddressInfo = ^TDNSAddressInfo;
  TDNSAddressInfo = record
    Flags, Family, SocketType, Protocol: LongInt;
    AddressLength: PtrUInt;
    CanonicalName: PWideChar;
    Address, Blob: Pointer;
    BlobLength: PtrUInt;
    Provider: Pointer;
    Next: PDNSAddressInfo;
  end;
  PDNSTimeout = ^TDNSTimeout;
  TDNSTimeout = record
    Seconds, Microseconds: LongInt;
  end;
  PDNSIPv4Address = ^TDNSIPv4Address;
  TDNSIPv4Address = record
    Family, Port: Word;
    Address: DWORD;
    Padding: array[0..7] of Byte;
  end;
  {$pop}

  TDNSLookup = function(Name, Service: PWideChar; NameSpace: DWORD;
    Provider: Pointer; Hints: PDNSAddressInfo; var Addresses: PDNSAddressInfo;
    Timeout: PDNSTimeout; Overlapped: POverlapped; Completion: Pointer;
    var CancelHandle: THandle): LongInt; stdcall;
  TDNSCancel = function(var CancelHandle: THandle): LongInt; stdcall;
  TDNSResult = function(Overlapped: POverlapped): LongInt; stdcall;
  TDNSFree = procedure(Addresses: PDNSAddressInfo); stdcall;
  TDNSResolverAPI = record
    Lookup: TDNSLookup;
    Cancel: TDNSCancel;
    GetResult: TDNSResult;
    FreeAddresses: TDNSFree;
  end;

  TIPv4Resolver = class
  private
    FAPI: TDNSResolverAPI;
  public
    constructor Create; overload;
    constructor Create(const API: TDNSResolverAPI); overload;
    function Resolve(const Host: string; TimeoutMs: Integer; CancelEvent: THandle;
      out Address: DWORD; out ErrorText: string): Boolean;
  end;

function ResolveIPv4(const Host: string; TimeoutMs: Integer; CancelEvent: THandle;
  out Address: DWORD; out ErrorText: string): Boolean;

implementation

uses Winsock2, AsyncCompletion;

var
  DefaultResolver: TIPv4Resolver;
  WinsockReady: Boolean;
  WinsockData: TWSAData;

function ParseIPv4(const Text: string; out Address: DWORD): Boolean;
var I, Part, Value, Digits: Integer;
begin
  Address := 0;
  I := 1;
  for Part := 0 to 3 do
  begin
    Value := 0; Digits := 0;
    while (I <= Length(Text)) and (Text[I] in ['0'..'9']) do
    begin
      Inc(Digits);
      if Digits > 3 then Exit(False);
      Value := Value * 10 + Ord(Text[I]) - Ord('0'); Inc(I);
    end;
    if (Digits = 0) or (Value > 255) then Exit(False);
    Address := Address or (DWORD(Value) shl (Part * 8));
    if Part < 3 then
    begin
      if (I > Length(Text)) or (Text[I] <> '.') then Exit(False);
      Inc(I);
    end;
  end;
  Result := I > Length(Text);
end;

constructor TIPv4Resolver.Create;
var LibraryHandle: HMODULE;
begin
  inherited Create;
  FAPI := Default(TDNSResolverAPI);
  LibraryHandle := GetModuleHandleW('Ws2_32.dll');
  if LibraryHandle <> 0 then
  begin
    Pointer(FAPI.Lookup) := GetProcAddress(LibraryHandle, 'GetAddrInfoExW');
    Pointer(FAPI.Cancel) := GetProcAddress(LibraryHandle, 'GetAddrInfoExCancel');
    Pointer(FAPI.GetResult) := GetProcAddress(LibraryHandle, 'GetAddrInfoExOverlappedResult');
    Pointer(FAPI.FreeAddresses) := GetProcAddress(LibraryHandle, 'FreeAddrInfoExW');
  end;
end;

constructor TIPv4Resolver.Create(const API: TDNSResolverAPI);
begin
  inherited Create;
  FAPI := API;
end;

type
  PDNSRequest = ^TDNSRequest;
  TDNSRequest = record
    Async: TAsyncContext;
    API: TDNSResolverAPI;
    Name: PWideChar;
    Hints: TDNSAddressInfo;
    Addresses: PDNSAddressInfo;
    Overlapped: TOverlapped;
    Timeout: TDNSTimeout;
    CancelHandle: THandle;
  end;

procedure DestroyDNSRequest(Context: Pointer); stdcall;
var Request: PDNSRequest;
begin
  Request := PDNSRequest(Context);
  if Request^.Addresses <> nil then Request^.API.FreeAddresses(Request^.Addresses);
  if Request^.Name <> nil then HeapFree(GetProcessHeap, 0, Request^.Name);
end;

function TIPv4Resolver.Resolve(const Host: string; TimeoutMs: Integer; CancelEvent: THandle;
  out Address: DWORD; out ErrorText: string): Boolean;
var
  Name: UnicodeString;
  Text: string;
  Request: PDNSRequest;
  Current: PDNSAddressInfo;
  Handles: array[0..1] of THandle;
  Code, CancelCode: LongInt;
  WaitResult, HandleCount, CompletionIndex: DWORD;
  Pending: Boolean;
begin
  Result := False; Address := 0; ErrorText := '';
  if TimeoutMs <= 0 then
  begin ErrorText := 'Invalid DNS timeout'; Exit; end;
  if CancelEvent <> 0 then
  begin
    WaitResult := WaitForSingleObject(CancelEvent, 0);
    if WaitResult = WAIT_OBJECT_0 then
    begin ErrorText := 'DNS lookup cancelled'; Exit; end;
    if WaitResult = WAIT_FAILED then
    begin ErrorText := 'Invalid DNS cancellation event'; Exit; end;
  end;
  Text := Trim(Host);
  if (Text = '') or (Pos(#0, Text) > 0) then
  begin ErrorText := 'Invalid host name'; Exit; end;
  if ParseIPv4(Text, Address) then Exit(True);
  Address := 0;
  if not Assigned(FAPI.Lookup) or not Assigned(FAPI.Cancel) or
    not Assigned(FAPI.GetResult) or not Assigned(FAPI.FreeAddresses) then
  begin ErrorText := 'Asynchronous DNS resolution is unavailable'; Exit; end;

  // Reserve completion cleanup before starting native work. No allocation or
  // worker creation is required on the cancellation/error path.
  Request := CreateAsyncContext(SizeOf(TDNSRequest), @DestroyDNSRequest);
  if Request = nil then
  begin ErrorText := 'Cannot create DNS request context'; Exit; end;
  Pending := False;
  try
    Request^.API := FAPI;
    Name := UTF8Decode(Text);
    Request^.Name := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, (Length(Name) + 1) * SizeOf(WideChar));
    if Request^.Name = nil then
    begin ErrorText := 'Cannot allocate DNS host name'; Exit; end;
    Move(Name[1], Request^.Name^, Length(Name) * SizeOf(WideChar));
    Request^.Hints.Family := AF_INET;
    Request^.Timeout.Seconds := TimeoutMs div 1000;
    Request^.Timeout.Microseconds := (TimeoutMs mod 1000) * 1000;
    Request^.Overlapped.hEvent := Request^.Async.CompletionEvent;
    Code := Request^.API.Lookup(Request^.Name, nil, 0, nil, @Request^.Hints, Request^.Addresses,
      @Request^.Timeout, @Request^.Overlapped, nil, Request^.CancelHandle);
    Pending := Code = WSA_IO_PENDING;
    if Pending then
    begin
      HandleCount := 1; CompletionIndex := 0; Handles[0] := Request^.Async.CompletionEvent;
      if CancelEvent <> 0 then
      begin Handles[0] := CancelEvent; Handles[1] := Request^.Async.CompletionEvent; HandleCount := 2; CompletionIndex := 1; end;
      WaitResult := WaitForMultipleObjects(HandleCount, @Handles[0], False, TimeoutMs);
      if WaitResult <> WAIT_OBJECT_0 + CompletionIndex then
      begin
        if WaitResult = WAIT_TIMEOUT then ErrorText := 'DNS lookup timed out: ' + Text
        else if (CancelEvent <> 0) and (WaitResult = WAIT_OBJECT_0) then ErrorText := 'DNS lookup cancelled'
        else ErrorText := 'DNS wait failed: ' + IntToStr(GetLastError);
        CancelCode := Request^.API.Cancel(Request^.CancelHandle);
        if (CancelCode <> 0) and (CancelCode <> WSA_INVALID_HANDLE) then
          ErrorText := ErrorText + ' (cancel error ' + IntToStr(CancelCode) + ')';
        WaitResult := WaitForSingleObject(Request^.Async.CompletionEvent, 100);
        if WaitResult <> WAIT_OBJECT_0 then Exit;
      end;
      Code := Request^.API.GetResult(@Request^.Overlapped);
      Pending := False;
      if ErrorText <> '' then Exit;
    end;
    if (CancelEvent <> 0) and (WaitForSingleObject(CancelEvent, 0) = WAIT_OBJECT_0) then
    begin ErrorText := 'DNS lookup cancelled'; Exit; end;
    if Code <> 0 then
    begin ErrorText := 'Cannot resolve host: ' + Text + ' (error ' + IntToStr(Code) + ')'; Exit; end;
    Current := Request^.Addresses;
    while Current <> nil do
    begin
      if (Current^.Family = AF_INET) and (Current^.Address <> nil) and
        (Current^.AddressLength >= SizeOf(TDNSIPv4Address)) then
      begin Address := PDNSIPv4Address(Current^.Address)^.Address; Exit(True); end;
      Current := Current^.Next;
    end;
    ErrorText := 'No IPv4 address for host: ' + Text;
  finally
    ReleaseAsyncContext(Request, not Pending);
  end;
end;

function ResolveIPv4(const Host: string; TimeoutMs: Integer; CancelEvent: THandle;
  out Address: DWORD; out ErrorText: string): Boolean;
begin
  if not WinsockReady then
  begin
    Address := 0; ErrorText := 'Windows Sockets initialization failed'; Exit(False);
  end;
  Result := DefaultResolver.Resolve(Host, TimeoutMs, CancelEvent, Address, ErrorText);
end;

initialization
  WinsockData := Default(TWSAData);
  WinsockReady := WSAStartup($0202, WinsockData) = 0;
  DefaultResolver := TIPv4Resolver.Create;
finalization
  DefaultResolver.Free;
  // Deferred callbacks use only WinAPI, process-heap storage and saved native
  // function pointers. Keep Winsock alive if an uncooperative provider remains.
  if WinsockReady and (PendingAsyncCompletions = 0) then WSACleanup;
end.
