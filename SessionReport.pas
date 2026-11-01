unit SessionReport;
{$mode objfpc}{$H+}
interface
uses SQLiteNative;
function GenerateSessionReport(DB: TSQLiteDB; SessionID: Int64;
  const Root, Language: string): string;
function EscapeHtml(const Text: string): UnicodeString;
implementation
uses Classes, SysUtils, DateUtils, Windows, BuiltInTranslations;
function EscapeHtml(const Text: string): UnicodeString;
var U: UnicodeString; C: WideChar;
begin
  Result := ''; U := UTF8Decode(Text);
  for C in U do
    case C of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
      #39: Result := Result + '&#39;';
    else
      if Ord(C) >= 32 then Result := Result + C else Result := Result + ' ';
    end;
end;
function LocalTimeText(MS: Int64): string;
begin Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', UnixToDateTime(MS div 1000, False)); end;
function DurationText(MS: Int64): string;
var Seconds: Int64;
begin
  Seconds := MS div 1000;
  Result := Format('%.2d:%.2d:%.2d', [Seconds div 3600, (Seconds div 60) mod 60, Seconds mod 60]);
end;
function GenerateSessionReport(DB: TSQLiteDB; SessionID: Int64;
  const Root, Language: string): string;
var
  Labels: TStringList; Stream: TUTF8FileStream; S: Pointer;
  StartMS, EndMS, Samples, Checks, Losses: Int64;
  Path, Temp, Directory, ID: string; Suffix: Integer;
  function L(const Key: string): UnicodeString;
  begin Result := EscapeHtml(Labels.Values[Key]); end;
  procedure Line(const Text: UnicodeString);
  var Bytes: RawByteString;
  begin
    Bytes := UTF8Encode(Text + LineEnding);
    if Bytes <> '' then Stream.WriteBuffer(Bytes[1], Length(Bytes));
  end;
  function Cell(const Text: string): UnicodeString;
  begin Result := '<td>' + EscapeHtml(Text) + '</td>'; end;
  function StateText(State: Integer): string;
  begin
    case State of
      1: Result := Labels.Values['state_up'];
      2: Result := Labels.Values['state_down'];
    else Result := Labels.Values['report_unknown']; end;
  end;
begin
  Result := ''; Stream := nil; S := nil; Temp := '';
  Labels := TStringList.Create;
  try
    LoadBuiltInTranslations(Language, Labels); ID := IntToStr(SessionID);
    S := DB.Prepare('SELECT started_ms,ended_ms FROM sessions WHERE id=' + ID);
    try
      if not DB.Next(S) then raise Exception.Create('Session does not exist');
      StartMS := DB.Number(S,0); EndMS := DB.Number(S,1);
      if EndMS = 0 then raise Exception.Create('Session is not finalized');
    finally DB.Done(S); S := nil; end;
    Directory := IncludeTrailingPathDelimiter(Root) + 'reports';
    EnsureUTF8Directory(Directory);
    Path := IncludeTrailingPathDelimiter(Directory) + 'session-' +
      FormatDateTime('yyyy-mm-dd_hhnnss', UnixToDateTime(StartMS div 1000,False)) + '-' + ID;
    Suffix := 0;
    while UTF8PathExists(Path + '.html') do
    begin Inc(Suffix); Path := Path + '-' + IntToStr(Suffix); end;
    Path := Path + '.html'; Temp := Path + '.tmp';
    Stream := TUTF8FileStream.Create(Temp, True);
    Line('<!doctype html><html lang="' + EscapeHtml(Labels.Values['report_lang']) + '"><meta charset="utf-8">');
    Line('<meta name="viewport" content="width=device-width,initial-scale=1">');
    Line('<title>' + L('report_title') + ' #' + UTF8Decode(ID) + '</title>');
    Line('<style>body{font:15px system-ui,sans-serif;margin:2rem;color:#18212b;background:#f5f7fa}main{max-width:1400px;margin:auto}h1,h2{font-weight:600}table{border-collapse:collapse;background:white;width:100%;margin:1rem 0}th,td{text-align:left;padding:.6rem;border-bottom:1px solid #dde3ea}th{background:#e6edf5}td{overflow-wrap:anywhere}.note{color:#526273}small{color:#526273}@media print{body{margin:0;background:white}th{background:#eee}}</style><main>');
    Line('<h1>' + L('report_title') + ' #' + UTF8Decode(ID) + '</h1>');
    Line('<p>' + L('report_start') + ': ' + EscapeHtml(LocalTimeText(StartMS)) +
      ' &mdash; ' + L('report_end') + ': ' + EscapeHtml(LocalTimeText(EndMS)) + '</p>');
    Line('<p>' + L('report_duration') + ': ' + UTF8Decode(DurationText(EndMS-StartMS)) + '</p>');
    S := DB.Prepare('SELECT count(*),coalesce(sum(t.checks),0),coalesce(sum(t.losses),0),' +
      'coalesce(sum(t.outages),0) FROM session_nodes n JOIN node_totals t ON t.node_id=n.id WHERE n.session_id=' + ID);
    try
      DB.Next(S); Checks := DB.Number(S,1); Losses := DB.Number(S,2);
      Line('<p>' + L('report_nodes') + ': ' + UTF8Decode(IntToStr(DB.Number(S,0))) + ' | ' +
        L('report_checks') + ': ' + UTF8Decode(IntToStr(Checks)) + ' | ' + L('report_losses') + ': ' + UTF8Decode(IntToStr(Losses)) +
        ' | ' + L('report_outages') + ': ' + UTF8Decode(IntToStr(DB.Number(S,3))) + '</p>');
    finally DB.Done(S); S := nil; end;
    Line('<p class="note">' + L('report_note') + '</p>');
    Line('<h2>' + L('report_nodes') + '</h2><table><thead><tr>');
    Line('<th>' + L('column_name') + '</th><th>' + L('column_host') + '</th><th>' + L('report_checks') +
      '</th><th>' + L('report_losses') + '</th><th>' + L('report_latency') + '</th><th>' +
      L('report_outages') + '</th><th>' + L('report_up_time') + '</th><th>' + L('report_down_time') +
      '</th><th>' + L('report_unknown_time') + '</th><th>' + L('report_final_state') + '</th></tr></thead><tbody>');
    S := DB.Prepare('SELECT names.value,hosts.address,t.checks,t.losses,t.samples,t.latency_sum_ms,' +
      't.latency_max_ms,t.outages,t.up_ms,t.down_ms,t.unknown_ms,' +
      '(SELECT state FROM state_events e WHERE e.node_id=n.id ORDER BY e.at_ms DESC,e.id DESC LIMIT 1) ' +
      'FROM session_nodes n JOIN hosts ON hosts.id=n.host_id JOIN names ON names.id=n.name_id ' +
      'JOIN node_totals t ON t.node_id=n.id WHERE n.session_id=' + ID + ' ORDER BY n.id');
    try
      while DB.Next(S) do
      begin
        Samples := DB.Number(S,4); Checks := DB.Number(S,2); Losses := DB.Number(S,3);
        Line('<tr>' + Cell(DB.Text(S,0)) + Cell(DB.Text(S,1)) + Cell(IntToStr(Checks)));
        if Checks > 0 then Line(Cell(IntToStr(Losses) + '/' + IntToStr(Checks) + ' (' + FormatFloat('0.0',100.0*Losses/Checks) + '%)'))
        else Line(Cell('0/0'));
        if Samples > 0 then Line(Cell(FormatFloat('0.0',DB.Number(S,5)/Samples) + ' / ' + IntToStr(DB.Number(S,6)) + ' ' + Labels.Values['unit_ms']))
        else Line(Cell('--'));
        Line(Cell(IntToStr(DB.Number(S,7))) + Cell(DurationText(DB.Number(S,8))) +
          Cell(DurationText(DB.Number(S,9))) + Cell(DurationText(DB.Number(S,10))) + Cell(StateText(DB.Number(S,11))) + '</tr>');
      end;
    finally DB.Done(S); S := nil; end;
    Line('</tbody></table><h2>' + L('report_events') + '</h2><table><thead><tr><th>' + L('report_time') +
      '</th><th>' + L('column_name') + '</th><th>' + L('column_host') + '</th><th>' + L('report_state') + '</th></tr></thead><tbody>');
    S := DB.Prepare('SELECT e.at_ms,names.value,hosts.address,e.state FROM state_events e ' +
      'JOIN session_nodes n ON n.id=e.node_id JOIN names ON names.id=n.name_id JOIN hosts ON hosts.id=n.host_id ' +
      'WHERE n.session_id=' + ID + ' ORDER BY e.at_ms,e.id');
    try
      while DB.Next(S) do
        Line('<tr>' + Cell(LocalTimeText(DB.Number(S,0))) + Cell(DB.Text(S,1)) + Cell(DB.Text(S,2)) + Cell(StateText(DB.Number(S,3))) + '</tr>');
    finally DB.Done(S); S := nil; end;
    Line('</tbody></table><small><a href="https://github.com/warlocknt/multiPingLed">multiPingLed</a></small></main></html>');
    if not FlushFileBuffers(Stream.Handle) then RaiseLastOSError;
    FreeAndNil(Stream);
    if not MoveFileExW(PWideChar(UTF8Decode(Temp)),PWideChar(UTF8Decode(Path)),$00000008) then RaiseLastOSError;
    Temp := ''; Result := Path;
  finally
    if S <> nil then DB.Done(S);
    Stream.Free; Labels.Free;
    if Temp <> '' then DeleteUTF8File(Temp);
  end;
end;
end.
