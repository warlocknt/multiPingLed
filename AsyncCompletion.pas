unit AsyncCompletion;

{$mode objfpc}{$H+}

interface

uses Windows;

type
  TAsyncDestroy = procedure(Context: Pointer); stdcall;
  PAsyncContext = ^TAsyncContext;
  TAsyncContext = record
    CompletionEvent, ReleaseEvent, DoneEvent, CallerDoneEvent, WaitHandle: THandle;
    Destroy: TAsyncDestroy;
  end;

function CreateAsyncContext(Size: PtrUInt; Destroy: TAsyncDestroy): Pointer;
procedure ReleaseAsyncContext(Context: Pointer; Completed: Boolean);
function DrainAsyncCompletions(TimeoutMs: DWORD): Boolean;
function PendingAsyncCompletions: LongInt;

implementation

const WT_EXECUTEONLYONCE = $00000008;
type TWaitCallback = procedure(Context: Pointer; TimerFired: Byte); stdcall;
function RegisterWaitForSingleObject(WaitHandle: PHANDLE; Event: THandle; Callback: TWaitCallback;
  Context: Pointer; Milliseconds, Flags: DWORD): BOOL; stdcall; external 'kernel32.dll';
function UnregisterWaitEx(WaitHandle, CompletionEvent: THandle): BOOL; stdcall; external 'kernel32.dll';

var PendingCount: LongInt;

procedure CompletionCallback(Context: Pointer; TimerFired: Byte); stdcall;
var Header: PAsyncContext; DoneEvent: THandle;
begin
  Header := PAsyncContext(Context);
  // The caller may still be reading the completed result.
  WaitForSingleObject(Header^.ReleaseEvent, INFINITE);
  UnregisterWaitEx(Header^.WaitHandle, 0);
  DoneEvent := Header^.DoneEvent;
  if Assigned(Header^.Destroy) then Header^.Destroy(Context);
  CloseHandle(Header^.CompletionEvent);
  CloseHandle(Header^.ReleaseEvent);
  HeapFree(GetProcessHeap, 0, Context);
  SetEvent(DoneEvent);
  CloseHandle(DoneEvent);
  InterlockedDecrement(PendingCount);
end;

function CreateAsyncContext(Size: PtrUInt; Destroy: TAsyncDestroy): Pointer;
var Header: PAsyncContext;
begin
  Result := nil;
  if Size < SizeOf(TAsyncContext) then Exit;
  Header := HeapAlloc(GetProcessHeap, HEAP_ZERO_MEMORY, Size);
  if Header = nil then Exit;
  Header^.Destroy := Destroy;
  Header^.CompletionEvent := CreateEventW(nil, True, False, nil);
  Header^.ReleaseEvent := CreateEventW(nil, True, False, nil);
  Header^.DoneEvent := CreateEventW(nil, True, False, nil);
  if (Header^.CompletionEvent <> 0) and (Header^.ReleaseEvent <> 0) and
    (Header^.DoneEvent <> 0) and DuplicateHandle(GetCurrentProcess, Header^.DoneEvent,
      GetCurrentProcess, @Header^.CallerDoneEvent, 0, False, DUPLICATE_SAME_ACCESS) then
  begin
    InterlockedIncrement(PendingCount);
    if RegisterWaitForSingleObject(@Header^.WaitHandle, Header^.CompletionEvent,
      @CompletionCallback, Header, INFINITE, WT_EXECUTEONLYONCE) then Exit(Header);
    InterlockedDecrement(PendingCount);
  end;
  if Header^.CompletionEvent <> 0 then CloseHandle(Header^.CompletionEvent);
  if Header^.ReleaseEvent <> 0 then CloseHandle(Header^.ReleaseEvent);
  if Header^.DoneEvent <> 0 then CloseHandle(Header^.DoneEvent);
  if Header^.CallerDoneEvent <> 0 then CloseHandle(Header^.CallerDoneEvent);
  HeapFree(GetProcessHeap, 0, Header);
end;

procedure ReleaseAsyncContext(Context: Pointer; Completed: Boolean);
var Header: PAsyncContext; DoneEvent: THandle;
begin
  if Context = nil then Exit;
  Header := PAsyncContext(Context);
  DoneEvent := Header^.CallerDoneEvent;
  if Completed then SetEvent(Header^.CompletionEvent);
  // After this signal the context belongs exclusively to CompletionCallback.
  SetEvent(Header^.ReleaseEvent);
  if Completed then WaitForSingleObject(DoneEvent, 250);
  CloseHandle(DoneEvent);
end;

function PendingAsyncCompletions: LongInt;
begin
  Result := InterlockedCompareExchange(PendingCount, 0, 0);
end;

function DrainAsyncCompletions(TimeoutMs: DWORD): Boolean;
var Started: QWord;
begin
  Started := GetTickCount64;
  repeat
    if PendingAsyncCompletions = 0 then Exit(True);
    if GetTickCount64 - Started >= TimeoutMs then Exit(False);
    Sleep(1);
  until False;
end;

end.
