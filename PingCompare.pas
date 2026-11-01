unit PingCompare;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Windows;

type
  TPingComparison = class(TThread)
  private
    FHost, FAddress, FOutput: string;
    FTimeout: Integer;
    FStarted: TDateTime;
    FExitCode: DWORD;
    FElapsed: QWord;
  protected
    procedure Execute; override;
  public
    constructor Create(const Host, Address: string; Timeout: Integer);
    procedure Finish(Success: Boolean; RTT: Integer; const ErrorText: string);
  end;

function ComparisonEnabled: Boolean;

implementation

uses LogWriter;

function ComparisonEnabled: Boolean;
begin
  Result := ComparisonLoggingEnabled;
end;

constructor TPingComparison.Create(const Host, Address: string; Timeout: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FHost := Host;
  FAddress := Address;
  FTimeout := Timeout;
  FStarted := Now;
  FExitCode := DWORD(-1);
  Start;
end;

procedure TPingComparison.Execute;
type TCaptureBuffer = array[0..1023] of AnsiChar;
var
  SA: TSecurityAttributes;
  SI: TStartupInfoW;
  PI: TProcessInformation;
  ReadPipe, WritePipe: THandle;
  Available, Count: DWORD;
  Buffer: TCaptureBuffer;
  Command: UnicodeString;
  Started: QWord;
  Done: Boolean;
begin
  ReadPipe := 0;
  WritePipe := 0;
  PI := Default(TProcessInformation);
  Count := 0;
  Buffer := Default(TCaptureBuffer);
  Started := GetTickCount64;
  try
    try
      SA := Default(TSecurityAttributes);
      SA.nLength := SizeOf(SA);
      SA.bInheritHandle := True;
      if not CreatePipe(ReadPipe, WritePipe, @SA, 0) then RaiseLastOSError;
      if not SetHandleInformation(ReadPipe, HANDLE_FLAG_INHERIT, 0) then RaiseLastOSError;
      SI := Default(TStartupInfoW);
      SI.cb := SizeOf(SI);
      SI.dwFlags := STARTF_USESTDHANDLES;
      SI.hStdOutput := WritePipe;
      SI.hStdError := WritePipe;
      SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
      Command := '"' + UnicodeString(SysUtils.GetEnvironmentVariable('SystemRoot')) +
        '\System32\ping.exe" -4 -n 1 -w ' + UnicodeString(IntToStr(FTimeout)) +
        ' -l 32 ' + UnicodeString(FAddress);
      UniqueString(Command);
      if not CreateProcessW(nil, PWideChar(Command), nil, nil, True,
        CREATE_NO_WINDOW, nil, nil, SI, PI) then RaiseLastOSError;
      CloseHandle(WritePipe);
      WritePipe := 0;
      repeat
        Done := WaitForSingleObject(PI.hProcess, 10) = WAIT_OBJECT_0;
        while PeekNamedPipe(ReadPipe, nil, 0, nil, @Available, nil) and (Available > 0) do
        begin
          if not ReadFile(ReadPipe, Buffer, SizeOf(Buffer), Count, nil) or (Count = 0) then Break;
          // Bound captured output, but always drain the pipe.
          if Length(FOutput) < 65536 then
          begin
            Available := Length(FOutput);
            SetLength(FOutput, Available + Count);
            Move(Buffer[0], FOutput[Available + 1], Count);
          end;
        end;
        if not Done and (GetTickCount64 - Started > QWord(FTimeout) + 3000) then
        begin
          TerminateProcess(PI.hProcess, 255);
          WaitForSingleObject(PI.hProcess, INFINITE);
          FOutput := FOutput + LineEnding + '[diagnostic watchdog timeout]';
          Done := True;
        end;
      until Done;
      GetExitCodeProcess(PI.hProcess, FExitCode);
    except
      on E: Exception do FOutput := FOutput + LineEnding + '[diagnostic error: ' + E.Message + ']';
    end;
  finally
    if PI.hProcess <> 0 then
    begin
      if WaitForSingleObject(PI.hProcess, 0) <> WAIT_OBJECT_0 then
      begin
        TerminateProcess(PI.hProcess, 255);
        WaitForSingleObject(PI.hProcess, INFINITE);
      end;
      CloseHandle(PI.hProcess);
    end;
    if PI.hThread <> 0 then CloseHandle(PI.hThread);
    if ReadPipe <> 0 then CloseHandle(ReadPipe);
    if WritePipe <> 0 then CloseHandle(WritePipe);
    FElapsed := GetTickCount64 - Started;
  end;
end;

procedure TPingComparison.Finish(Success: Boolean; RTT: Integer; const ErrorText: string);
var Header: string; Entry: RawByteString; OutputText: UnicodeString;
begin
  WaitFor;
  if not LoggingEnabled then Exit;
  OutputText := '';
  SetLength(OutputText, MultiByteToWideChar(CP_OEMCP, 0,
    PAnsiChar(FOutput), Length(FOutput), nil, 0));
  if FOutput <> '' then
    MultiByteToWideChar(CP_OEMCP, 0, PAnsiChar(FOutput), Length(FOutput),
      PWideChar(OutputText), Length(OutputText));
  Header := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', FStarted) +
    ' host=' + FHost + ' ipv4=' + FAddress + ' timeout=' + IntToStr(FTimeout) +
    ' payload=32' + LineEnding + 'API success=' + BoolToStr(Success, True) +
    ' rtt=' + IntToStr(RTT) + ' error=' + ErrorText + LineEnding +
    'PING exit=' + IntToStr(FExitCode) + ' elapsed=' + IntToStr(FElapsed) +
    'ms' + LineEnding;
  Entry := UTF8Encode(UTF8Decode(Header) + OutputText + LineEnding + LineEnding);
  WriteLog('ping-compare.log', Entry);
end;

end.
