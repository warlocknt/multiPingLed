unit LogWriter;

{$mode objfpc}{$H+}

interface

procedure ConfigureDiagnostics(EnableLogging, EnableComparison: Boolean);
function ComparisonLoggingEnabled: Boolean;
function LoggingEnabled: Boolean;
procedure WriteLog(const FileName: string; const Text: RawByteString);
procedure ApplicationLog(const MessageText: string);

implementation

uses Classes, SysUtils;

var
  LogLock: TRTLCriticalSection;
  FLoggingEnabled: Boolean = False;
  FComparisonEnabled: Boolean = False;

procedure ConfigureDiagnostics(EnableLogging, EnableComparison: Boolean);
begin
  EnterCriticalSection(LogLock);
  try
    FLoggingEnabled := EnableLogging;
    FComparisonEnabled := EnableComparison;
  finally LeaveCriticalSection(LogLock); end;
end;

function LoggingEnabled: Boolean;
begin
  EnterCriticalSection(LogLock);
  try Result := FLoggingEnabled;
  finally LeaveCriticalSection(LogLock); end;
end;

function ComparisonLoggingEnabled: Boolean;
begin
  EnterCriticalSection(LogLock);
  try Result := FLoggingEnabled and FComparisonEnabled;
  finally LeaveCriticalSection(LogLock); end;
end;

procedure WriteLog(const FileName: string; const Text: RawByteString);
var Directory, Path: string; Stream: TFileStream; Rotate: Boolean;
begin
  EnterCriticalSection(LogLock);
  try
    try
      if not LoggingEnabled then Exit;
      Directory := ExtractFilePath(ExpandFileName(ParamStr(0))) + 'log';
      if not ForceDirectories(Directory) then Exit;
      Path := IncludeTrailingPathDelimiter(Directory) + ExtractFileName(FileName);
      if FileExists(Path) then
      begin
        Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
        try Rotate := Stream.Size >= 5 * 1024 * 1024;
        finally Stream.Free; end;
        if Rotate then
        begin
          if FileExists(Path + '.1') and not DeleteFile(Path + '.1') then Exit;
          if not RenameFile(Path, Path + '.1') then Exit;
        end;
      end;
      if FileExists(Path) then
        Stream := TFileStream.Create(Path, fmOpenWrite or fmShareDenyWrite)
      else Stream := TFileStream.Create(Path, fmCreate);
      try
        Stream.Seek(0, soEnd);
        if Text <> '' then Stream.WriteBuffer(Text[1], Length(Text));
      finally Stream.Free; end;
    except
      // Logging failures must not interrupt monitoring or shutdown.
    end;
  finally LeaveCriticalSection(LogLock); end;
end;

procedure ApplicationLog(const MessageText: string);
var Entry: RawByteString;
begin
  if not LoggingEnabled then Exit;
  Entry := UTF8Encode(UnicodeString(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)) +
    ' ' + UTF8Decode(MessageText) + LineEnding);
  WriteLog('application.log', Entry);
end;

initialization
  InitCriticalSection(LogLock);
finalization
  DoneCriticalSection(LogLock);
end.
