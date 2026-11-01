unit PingHelper;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Windows;

type
  TPingResult = record
    Success: Boolean;
    ResponseTime: Integer;
    ErrorMessage: string;
  end;
  TICMPCreate = function: THandle; stdcall;
  TICMPClose = function(Handle: THandle): BOOL; stdcall;
  TICMPSend = function(Handle, Event: THandle; Apc, Context: Pointer; Address: DWORD;
    Data: Pointer; DataSize: Word; Options, Reply: Pointer; ReplySize, Timeout: DWORD): DWORD; stdcall;
  TICMPParse = function(Reply: Pointer; Size: DWORD): DWORD; stdcall;
  TICMPAPI = record
    CreateHandle: TICMPCreate;
    CloseHandle: TICMPClose;
    Send: TICMPSend;
    Parse: TICMPParse;
  end;

function PingHost(const Host: string; TimeoutMs: Integer): TPingResult;
function PingHostCancelable(const Host: string; TimeoutMs: Integer; CancelEvent: THandle): TPingResult;
function PingAddress(const API: TICMPAPI; Address: DWORD; TimeoutMs: Integer; CancelEvent: THandle): TPingResult;

implementation

uses PingCompare, DNSResolver, AsyncCompletion;

function IcmpCreateFile: THandle; stdcall; external 'iphlpapi.dll';
function IcmpCloseHandle(Handle: THandle): BOOL; stdcall; external 'iphlpapi.dll';
function IcmpSendEcho2(Handle, Event: THandle; Apc, Context: Pointer; Address: DWORD;
  Data: Pointer; DataSize: Word; Options, Reply: Pointer; ReplySize, Timeout: DWORD): DWORD;
  stdcall; external 'iphlpapi.dll';
function IcmpParseReplies(Reply: Pointer; Size: DWORD): DWORD; stdcall; external 'iphlpapi.dll';

type
  PICMPRequest = ^TICMPRequest;
  TICMPRequest = record
    Async: TAsyncContext;
    API: TICMPAPI;
    Handle: THandle;
    Data: array[0..31] of Byte;
    Reply: array[0..127] of Byte;
  end;
  PICMPReplyPrefix = ^TICMPReplyPrefix;
  TICMPReplyPrefix = record
    Address, Status, RoundTripTime: DWORD;
  end;

procedure DestroyICMPRequest(Context: Pointer); stdcall;
var Request: PICMPRequest;
begin
  Request := PICMPRequest(Context);
  if (Request^.Handle <> 0) and (Request^.Handle <> INVALID_HANDLE_VALUE) then
    Request^.API.CloseHandle(Request^.Handle);
end;

function PingAddress(const API: TICMPAPI; Address: DWORD; TimeoutMs: Integer; CancelEvent: THandle): TPingResult;
var Request: PICMPRequest; Count, Code, WaitResult, HandleCount, CompletionIndex: DWORD;
    Handles: array[0..1] of THandle; Pending: Boolean; Reply: PICMPReplyPrefix;
begin
  Result := Default(TPingResult); Result.ResponseTime := -1;
  if TimeoutMs <= 0 then begin Result.ErrorMessage := 'Invalid timeout'; Exit; end;
  if (CancelEvent <> 0) and (WaitForSingleObject(CancelEvent, 0) = WAIT_OBJECT_0) then
  begin Result.ErrorMessage := 'Ping cancelled'; Exit; end;
  Request := CreateAsyncContext(SizeOf(TICMPRequest), @DestroyICMPRequest);
  if Request = nil then begin Result.ErrorMessage := 'Cannot create ICMP request context'; Exit; end;
  Pending := False;
  try
    Request^.API := API;
    Request^.Handle := API.CreateHandle();
    if Request^.Handle = INVALID_HANDLE_VALUE then
    begin Result.ErrorMessage := 'Failed to create ICMP handle'; Exit; end;
    FillChar(Request^.Data, SizeOf(Request^.Data), Ord('a'));
    Count := API.Send(Request^.Handle, Request^.Async.CompletionEvent, nil, nil, Address,
      @Request^.Data[0], SizeOf(Request^.Data), nil, @Request^.Reply[0], SizeOf(Request^.Reply), TimeoutMs);
    if Count = 0 then
    begin
      Code := GetLastError;
      if Code <> ERROR_IO_PENDING then
      begin Result.ErrorMessage := 'Ping failed: ' + IntToStr(Code); Exit; end;
      Pending := True;
      Handles[0] := Request^.Async.CompletionEvent; HandleCount := 1; CompletionIndex := 0;
      if CancelEvent <> 0 then
      begin Handles[0] := CancelEvent; Handles[1] := Request^.Async.CompletionEvent; HandleCount := 2; CompletionIndex := 1; end;
      WaitResult := WaitForMultipleObjects(HandleCount, @Handles[0], False, TimeoutMs);
      if WaitResult <> WAIT_OBJECT_0 + CompletionIndex then
      begin
        if (CancelEvent <> 0) and (WaitResult = WAIT_OBJECT_0) then Result.ErrorMessage := 'Ping cancelled'
        else if WaitResult = WAIT_TIMEOUT then Result.ErrorMessage := 'Request timed out'
        else Result.ErrorMessage := 'ICMP wait failed: ' + IntToStr(GetLastError);
        Exit;
      end;
      Pending := False;
      Count := API.Parse(@Request^.Reply[0], SizeOf(Request^.Reply));
    end;
    if (CancelEvent <> 0) and (WaitForSingleObject(CancelEvent, 0) = WAIT_OBJECT_0) then
    begin Result.ErrorMessage := 'Ping cancelled'; Exit; end;
    if Count = 0 then
    begin Result.ErrorMessage := 'Ping failed: ' + IntToStr(GetLastError); Exit; end;
    // The first three DWORD fields have identical layout in ECHO_REPLY and ECHO_REPLY32.
    Reply := PICMPReplyPrefix(@Request^.Reply[0]);
    if Reply^.Status = 0 then
    begin Result.Success := True; Result.ResponseTime := Reply^.RoundTripTime; end
    else Result.ErrorMessage := 'Ping status: ' + IntToStr(Reply^.Status);
  finally
    // A pending request retains its handle and buffers until Windows signals completion.
    ReleaseAsyncContext(Request, not Pending);
  end;
end;

function PingHost(const Host: string; TimeoutMs: Integer): TPingResult;
begin
  Result := PingHostCancelable(Host, TimeoutMs, 0);
end;

function PingHostCancelable(const Host: string; TimeoutMs: Integer; CancelEvent: THandle): TPingResult;
var Address: DWORD; API: TICMPAPI; Comparison: TPingComparison; AddressText: string;
begin
  Result := Default(TPingResult); Result.ResponseTime := -1;
  if TimeoutMs <= 0 then begin Result.ErrorMessage := 'Invalid timeout'; Exit; end;
  if not ResolveIPv4(Host, DNSLookupTimeoutMs, CancelEvent, Address, Result.ErrorMessage) then Exit;
  API.CreateHandle := @IcmpCreateFile; API.CloseHandle := @IcmpCloseHandle;
  API.Send := @IcmpSendEcho2; API.Parse := @IcmpParseReplies;
  Comparison := nil;
  if ComparisonEnabled then
  begin
    AddressText := Format('%d.%d.%d.%d', [Address and $FF, (Address shr 8) and $FF,
      (Address shr 16) and $FF, Address shr 24]);
    try Comparison := TPingComparison.Create(Host, AddressText, TimeoutMs, CancelEvent);
    except end;
  end;
  try
    Result := PingAddress(API, Address, TimeoutMs, CancelEvent);
  finally
    if Comparison <> nil then
    begin
      try Comparison.Finish(Result.Success, Result.ResponseTime, Result.ErrorMessage);
      finally Comparison.Free; end;
    end;
  end;
end;

end.
