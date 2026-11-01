unit LangManager;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, ConfigManager;

type
  { Language info record }
  TLanguageInfo = record
    Code: string;
    Name: string;
    FileName: string;
  end;

  { Language manager for multi-language support }
  TLangManager = class
  private
    FTranslations: TStringList;
    FCurrentLang: string;
    FLangDir: string;
    FAvailableLanguages: array of TLanguageInfo;

    procedure LoadLanguageFile(const FileName: string);
    procedure ScanLanguageFiles;
    function ParseLanguageFile(const FileName: string; out LangCode, LangName: string): Boolean;
    procedure CreateDefaultLanguageFiles;
  public
    constructor Create;
    destructor Destroy; override;

    // Initialize with lang dir path
    procedure Initialize(const LangPath: string);

    // Get translated string by key
    function GetString(const Key: string): string;

    // Get translated string with format
    function GetStringFmt(const Key: string; const Args: array of const): string;

    // Set current language by code
    procedure SetLanguage(const LangCode: string);

    // Get current language code
    property CurrentLanguage: string read FCurrentLang;

    // Get available languages count
    function GetLanguageCount: Integer;

    // Get language info by index
    function GetLanguageInfo(Index: Integer): TLanguageInfo;

    // Find language index by code
    function FindLanguageIndex(const LangCode: string): Integer;
  end;

var
  LangMgr: TLangManager;

// Shortcut function
function _(const Key: string): string;

implementation

uses BuiltInTranslations;

constructor TLangManager.Create;
begin
  inherited;
  FTranslations := TStringList.Create;
  LoadBuiltInTranslations('english', FTranslations);
  FCurrentLang := 'english'; // Default: English
end;

destructor TLangManager.Destroy;
begin
  FTranslations.Free;
  inherited;
end;

procedure TLangManager.Initialize(const LangPath: string);
var
  Idx: Integer;
begin
  if LangPath <> '' then
    FLangDir := LangPath
  else
    FLangDir := GlobalLangPath;

  // Ensure lang directory exists
  if not DirectoryExists(FLangDir) then
    CreateDir(FLangDir);

  // Scan for available language files
  ScanLanguageFiles;

  // If no language files found, create defaults
  if Length(FAvailableLanguages) = 0 then
  begin
    CreateDefaultLanguageFiles;
    ScanLanguageFiles;
  end;

  // Load current language
  Idx := FindLanguageIndex(FCurrentLang);
  if Idx >= 0 then
    LoadLanguageFile(FAvailableLanguages[Idx].FileName)
  else if Length(FAvailableLanguages) > 0 then
  begin
    // Load first available language
    FCurrentLang := FAvailableLanguages[0].Code;
    LoadLanguageFile(FAvailableLanguages[0].FileName);
  end;
end;

procedure TLangManager.CreateDefaultLanguageFiles;
const Codes: array[0..4] of string = ('english', 'russian', 'german', 'french', 'kazakh');
var Lines: TStringList; I: Integer;
begin
  Lines := TStringList.Create;
  try
    for I := 0 to High(Codes) do
    begin
      LoadBuiltInTranslations(Codes[I], Lines);
      Lines.SaveToFile(IncludeTrailingPathDelimiter(FLangDir) + Codes[I] + '.txt');
    end;
  finally Lines.Free; end;
end;

procedure TLangManager.ScanLanguageFiles;
var
  SearchRec: TSearchRec;
  LangCode, LangName: string;
  Count: Integer;
begin
  SetLength(FAvailableLanguages, 0);
  Count := 0;

  if FindFirst(FLangDir + '\*.txt', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Attr and faDirectory) = 0 then
        begin
          if ParseLanguageFile(FLangDir + '\' + SearchRec.Name, LangCode, LangName) then
          begin
            Inc(Count);
            SetLength(FAvailableLanguages, Count);
            FAvailableLanguages[Count - 1].Code := LangCode;
            FAvailableLanguages[Count - 1].Name := LangName;
            FAvailableLanguages[Count - 1].FileName := FLangDir + '\' + SearchRec.Name;
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

function TLangManager.ParseLanguageFile(const FileName: string; out LangCode, LangName: string): Boolean;
var
  Lines: TStringList;
  i: Integer;
  Line: string;
  PosEqual: Integer;
begin
  Result := False;
  LangCode := '';
  LangName := '';

  if not FileExists(FileName) then Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);
      if Line = '' then Continue;
      if Line[1] = '#' then Continue; // Skip comments

      PosEqual := Pos('=', Line);
      if PosEqual > 0 then
      begin
        if Copy(Line, 1, PosEqual - 1) = 'code' then
          LangCode := Copy(Line, PosEqual + 1, Length(Line));
        if Copy(Line, 1, PosEqual - 1) = 'name' then
          LangName := Copy(Line, PosEqual + 1, Length(Line));

        // If we found both, we can stop
        if (LangCode <> '') and (LangName <> '') then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TLangManager.LoadLanguageFile(const FileName: string);
var
  Lines: TStringList;
  i: Integer;
  Line: string;
  PosEqual: Integer;
  Key, Value: string;
begin
  LoadBuiltInTranslations(FCurrentLang, FTranslations);

  if not FileExists(FileName) then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);
      if Line = '' then Continue;
      if Line[1] = '#' then Continue; // Skip comments

      PosEqual := Pos('=', Line);
      if PosEqual > 0 then
      begin
        Key := Copy(Line, 1, PosEqual - 1);
        Value := Copy(Line, PosEqual + 1, Length(Line));

        // Skip metadata keys
        if (Key <> 'code') and (Key <> 'name') and (Value <> '') then
          FTranslations.Values[Key] := Value;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TLangManager.SetLanguage(const LangCode: string);
var
  Idx: Integer;
begin
  if FCurrentLang <> LangCode then
  begin
    Idx := FindLanguageIndex(LangCode);
    if Idx >= 0 then
    begin
      FCurrentLang := LangCode;
      LoadLanguageFile(FAvailableLanguages[Idx].FileName);
    end;
  end;
end;

function TLangManager.GetString(const Key: string): string;
begin
  Result := FTranslations.Values[Key];
  if Result = '' then Result := Key;
end;

function TLangManager.GetStringFmt(const Key: string; const Args: array of const): string;
begin
  Result := Format(GetString(Key), Args);
end;

function TLangManager.GetLanguageCount: Integer;
begin
  Result := Length(FAvailableLanguages);
end;

function TLangManager.GetLanguageInfo(Index: Integer): TLanguageInfo;
begin
  if (Index >= 0) and (Index < Length(FAvailableLanguages)) then
    Result := FAvailableLanguages[Index]
  else
  begin
    Result.Code := '';
    Result.Name := '';
    Result.FileName := '';
  end;
end;

function TLangManager.FindLanguageIndex(const LangCode: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FAvailableLanguages) do
  begin
    if CompareText(FAvailableLanguages[i].Code, LangCode) = 0 then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

// Global shortcut function
function _(const Key: string): string;
begin
  if Assigned(LangMgr) then
    Result := LangMgr.GetString(Key)
  else
    Result := Key;
end;

initialization
  LangMgr := TLangManager.Create;

finalization
  LangMgr.Free;

end.
