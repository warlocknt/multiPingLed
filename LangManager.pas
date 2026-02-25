unit LangManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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

    // Initialize with exe path
    procedure Initialize(const ExePath: string);

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

constructor TLangManager.Create;
begin
  inherited;
  FTranslations := TStringList.Create;
  FCurrentLang := 'english'; // Default: English
end;

destructor TLangManager.Destroy;
begin
  FTranslations.Free;
  inherited;
end;

procedure TLangManager.Initialize(const ExePath: string);
var
  Idx: Integer;
begin
  FLangDir := IncludeTrailingPathDelimiter(ExtractFilePath(ExePath)) + 'lang';

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
var
  FileName: string;
  Lines: TStringList;
begin
  // Create English language file
  FileName := FLangDir + '\english.txt';
  Lines := TStringList.Create;
  try
    Lines.Add('code=english');
    Lines.Add('name=English');
    Lines.Add('');
    Lines.Add('app_title=Multi Ping LED');
    Lines.Add('settings_title=Settings');
    Lines.Add('tab_nodes=Nodes');
    Lines.Add('tab_groups=Groups');
    Lines.Add('btn_add=Add');
    Lines.Add('btn_edit=Edit');
    Lines.Add('btn_delete=Delete');
    Lines.Add('btn_import=Import');
    Lines.Add('btn_export=Export');
    Lines.Add('btn_apply=Apply');
    Lines.Add('btn_cancel=Cancel');
    Lines.Add('column_name=Name');
    Lines.Add('column_host=Host');
    Lines.Add('column_interval=Interval');
    Lines.Add('column_timeout=Timeout');
    Lines.Add('column_type=Type');
    Lines.Add('column_nodes=Nodes');
    Lines.Add('node_title_add=Add Node');
    Lines.Add('node_title_edit=Edit Node');
    Lines.Add('label_name=Name:');
    Lines.Add('label_host=Host:');
    Lines.Add('label_interval=Interval (ms):');
    Lines.Add('label_timeout=Timeout (ms):');
    Lines.Add('group_title_add=Add Group');
    Lines.Add('group_title_edit=Edit Group');
    Lines.Add('label_type=Type:');
    Lines.Add('label_select_nodes=Select nodes:');
    Lines.Add('type_single=Single (1 node)');
    Lines.Add('type_2x2=2x2 Grid (2-4 nodes)');
    Lines.Add('type_3x3=3x3 Grid (5-9 nodes)');
    Lines.Add('selected_count=Selected: %d / %d');
    Lines.Add('error_host_empty=Host cannot be empty');
    Lines.Add('error_host_exists=Host already exists');
    Lines.Add('error_node_in_group=This node is used in a group. Remove it from all groups first.');
    Lines.Add('error_no_nodes=Create at least one node first.');
    Lines.Add('error_group_single=Single group type requires exactly 1 node');
    Lines.Add('error_group_2x2=2x2 group type requires 2-4 nodes');
    Lines.Add('error_group_3x3=3x3 group type requires 5-9 nodes');
    Lines.Add('confirm_delete_node=Delete node "%s"?');
    Lines.Add('confirm_delete_group=Delete group "%s"?');
    Lines.Add('title_confirm=Confirm');
    Lines.Add('title_error=Error');
    Lines.Add('title_success=Success');
    Lines.Add('title_cannot_delete=Cannot Delete');
    Lines.Add('title_no_nodes=No Nodes');
    Lines.Add('msg_import_success=Configuration imported successfully.');
    Lines.Add('msg_export_success=Configuration exported successfully.');
    Lines.Add('label_language=Language:');
    Lines.Add('menu_settings=Settings');
    Lines.Add('menu_exit=Exit');
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;

  // Create Russian language file
  FileName := FLangDir + '\russian.txt';
  Lines := TStringList.Create;
  try
    Lines.Add('code=russian');
    Lines.Add('name=Русский');
    Lines.Add('');
    Lines.Add('app_title=Multi Ping LED');
    Lines.Add('settings_title=Настройки');
    Lines.Add('tab_nodes=Узлы');
    Lines.Add('tab_groups=Группы');
    Lines.Add('btn_add=Добавить');
    Lines.Add('btn_edit=Изменить');
    Lines.Add('btn_delete=Удалить');
    Lines.Add('btn_import=Импорт');
    Lines.Add('btn_export=Экспорт');
    Lines.Add('btn_apply=Применить');
    Lines.Add('btn_cancel=Отмена');
    Lines.Add('column_name=Имя');
    Lines.Add('column_host=Хост');
    Lines.Add('column_interval=Интервал');
    Lines.Add('column_timeout=Таймаут');
    Lines.Add('column_type=Тип');
    Lines.Add('column_nodes=Узлы');
    Lines.Add('node_title_add=Добавить узел');
    Lines.Add('node_title_edit=Изменить узел');
    Lines.Add('label_name=Имя:');
    Lines.Add('label_host=Хост:');
    Lines.Add('label_interval=Интервал (мс):');
    Lines.Add('label_timeout=Таймаут (мс):');
    Lines.Add('group_title_add=Добавить группу');
    Lines.Add('group_title_edit=Изменить группу');
    Lines.Add('label_type=Тип:');
    Lines.Add('label_select_nodes=Выберите узлы:');
    Lines.Add('type_single=Один (1 узел)');
    Lines.Add('type_2x2=Сетка 2x2 (2-4 узла)');
    Lines.Add('type_3x3=Сетка 3x3 (5-9 узлов)');
    Lines.Add('selected_count=Выбрано: %d / %d');
    Lines.Add('error_host_empty=Хост не может быть пустым');
    Lines.Add('error_host_exists=Такой хост уже существует');
    Lines.Add('error_node_in_group=Этот узел используется в группе. Сначала удалите его из всех групп.');
    Lines.Add('error_no_nodes=Сначала создайте хотя бы один узел.');
    Lines.Add('error_group_single=Тип группы "Один" требует ровно 1 узел');
    Lines.Add('error_group_2x2=Тип группы "2x2" требует 2-4 узла');
    Lines.Add('error_group_3x3=Тип группы "3x3" требует 5-9 узлов');
    Lines.Add('confirm_delete_node=Удалить узел "%s"?');
    Lines.Add('confirm_delete_group=Удалить группу "%s"?');
    Lines.Add('title_confirm=Подтверждение');
    Lines.Add('title_error=Ошибка');
    Lines.Add('title_success=Успех');
    Lines.Add('title_cannot_delete=Невозможно удалить');
    Lines.Add('title_no_nodes=Нет узлов');
    Lines.Add('msg_import_success=Конфигурация успешно импортирована.');
    Lines.Add('msg_export_success=Конфигурация успешно экспортирована.');
    Lines.Add('label_language=Язык:');
    Lines.Add('menu_settings=Настройки');
    Lines.Add('menu_exit=Выход');
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;

  // Create German language file
  FileName := FLangDir + '\german.txt';
  Lines := TStringList.Create;
  try
    Lines.Add('code=german');
    Lines.Add('name=Deutsch');
    Lines.Add('');
    Lines.Add('app_title=Multi Ping LED');
    Lines.Add('settings_title=Einstellungen');
    Lines.Add('tab_nodes=Knoten');
    Lines.Add('tab_groups=Gruppen');
    Lines.Add('btn_add=Hinzufügen');
    Lines.Add('btn_edit=Bearbeiten');
    Lines.Add('btn_delete=Löschen');
    Lines.Add('btn_import=Importieren');
    Lines.Add('btn_export=Exportieren');
    Lines.Add('btn_apply=Anwenden');
    Lines.Add('btn_cancel=Abbrechen');
    Lines.Add('column_name=Name');
    Lines.Add('column_host=Host');
    Lines.Add('column_interval=Intervall');
    Lines.Add('column_timeout=Timeout');
    Lines.Add('column_type=Typ');
    Lines.Add('column_nodes=Knoten');
    Lines.Add('node_title_add=Knoten hinzufügen');
    Lines.Add('node_title_edit=Knoten bearbeiten');
    Lines.Add('label_name=Name:');
    Lines.Add('label_host=Host:');
    Lines.Add('label_interval=Intervall (ms):');
    Lines.Add('label_timeout=Timeout (ms):');
    Lines.Add('group_title_add=Gruppe hinzufügen');
    Lines.Add('group_title_edit=Gruppe bearbeiten');
    Lines.Add('label_type=Typ:');
    Lines.Add('label_select_nodes=Knoten auswählen:');
    Lines.Add('type_single=Einzeln (1 Knoten)');
    Lines.Add('type_2x2=2x2 Raster (2-4 Knoten)');
    Lines.Add('type_3x3=3x3 Raster (5-9 Knoten)');
    Lines.Add('selected_count=Ausgewählt: %d / %d');
    Lines.Add('error_host_empty=Host darf nicht leer sein');
    Lines.Add('error_host_exists=Host existiert bereits');
    Lines.Add('error_node_in_group=Dieser Knoten wird in einer Gruppe verwendet. Entfernen Sie ihn zuerst aus allen Gruppen.');
    Lines.Add('error_no_nodes=Erstellen Sie zuerst mindestens einen Knoten.');
    Lines.Add('error_group_single=Einzelner Gruppentyp erfordert genau 1 Knoten');
    Lines.Add('error_group_2x2=2x2 Gruppentyp erfordert 2-4 Knoten');
    Lines.Add('error_group_3x3=3x3 Gruppentyp erfordert 5-9 Knoten');
    Lines.Add('confirm_delete_node=Knoten "%s" löschen?');
    Lines.Add('confirm_delete_group=Gruppe "%s" löschen?');
    Lines.Add('title_confirm=Bestätigen');
    Lines.Add('title_error=Fehler');
    Lines.Add('title_success=Erfolg');
    Lines.Add('title_cannot_delete=Löschen nicht möglich');
    Lines.Add('title_no_nodes=Keine Knoten');
    Lines.Add('msg_import_success=Konfiguration erfolgreich importiert.');
    Lines.Add('msg_export_success=Konfiguration erfolgreich exportiert.');
    Lines.Add('label_language=Sprache:');
    Lines.Add('menu_settings=Einstellungen');
    Lines.Add('menu_exit=Beenden');
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;

  // Create French language file
  FileName := FLangDir + '\french.txt';
  Lines := TStringList.Create;
  try
    Lines.Add('code=french');
    Lines.Add('name=Français');
    Lines.Add('');
    Lines.Add('app_title=Multi Ping LED');
    Lines.Add('settings_title=Paramètres');
    Lines.Add('tab_nodes=Nœuds');
    Lines.Add('tab_groups=Groupes');
    Lines.Add('btn_add=Ajouter');
    Lines.Add('btn_edit=Modifier');
    Lines.Add('btn_delete=Supprimer');
    Lines.Add('btn_import=Importer');
    Lines.Add('btn_export=Exporter');
    Lines.Add('btn_apply=Appliquer');
    Lines.Add('btn_cancel=Annuler');
    Lines.Add('column_name=Nom');
    Lines.Add('column_host=Hôte');
    Lines.Add('column_interval=Intervalle');
    Lines.Add('column_timeout=Délai');
    Lines.Add('column_type=Type');
    Lines.Add('column_nodes=Nœuds');
    Lines.Add('node_title_add=Ajouter un nœud');
    Lines.Add('node_title_edit=Modifier le nœud');
    Lines.Add('label_name=Nom:');
    Lines.Add('label_host=Hôte:');
    Lines.Add('label_interval=Intervalle (ms):');
    Lines.Add('label_timeout=Délai (ms):');
    Lines.Add('group_title_add=Ajouter un groupe');
    Lines.Add('group_title_edit=Modifier le groupe');
    Lines.Add('label_type=Type:');
    Lines.Add('label_select_nodes=Sélectionner les nœuds:');
    Lines.Add('type_single=Simple (1 nœud)');
    Lines.Add('type_2x2=Grille 2x2 (2-4 nœuds)');
    Lines.Add('type_3x3=Grille 3x3 (5-9 nœuds)');
    Lines.Add('selected_count=Sélectionné: %d / %d');
    Lines.Add('error_host_empty=Lhôte ne peut pas être vide');
    Lines.Add('error_host_exists=Lhôte existe déjà');
    Lines.Add('error_node_in_group=Ce nœud est utilisé dans un groupe. Supprimez-le dabord de tous les groupes.');
    Lines.Add('error_no_nodes=Créez dabord au moins un nœud.');
    Lines.Add('error_group_single=Le type de groupe Simple nécessite exactement 1 nœud');
    Lines.Add('error_group_2x2=Le type de groupe 2x2 nécessite 2-4 nœuds');
    Lines.Add('error_group_3x3=Le type de groupe 3x3 nécessite 5-9 nœuds');
    Lines.Add('confirm_delete_node=Supprimer le nœud "%s"?');
    Lines.Add('confirm_delete_group=Supprimer le groupe "%s"?');
    Lines.Add('title_confirm=Confirmation');
    Lines.Add('title_error=Erreur');
    Lines.Add('title_success=Succès');
    Lines.Add('title_cannot_delete=Impossible de supprimer');
    Lines.Add('title_no_nodes=Pas de nœuds');
    Lines.Add('msg_import_success=Configuration importée avec succès.');
    Lines.Add('msg_export_success=Configuration exportée avec succès.');
    Lines.Add('label_language=Langue:');
    Lines.Add('menu_settings=Paramètres');
    Lines.Add('menu_exit=Quitter');
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;

  // Create Kazakh language file
  FileName := FLangDir + '\kazakh.txt';
  Lines := TStringList.Create;
  try
    Lines.Add('code=kazakh');
    Lines.Add('name=Қазақша');
    Lines.Add('');
    Lines.Add('app_title=Multi Ping LED');
    Lines.Add('settings_title=Параметрлер');
    Lines.Add('tab_nodes=Тұйындар');
    Lines.Add('tab_groups=Топтар');
    Lines.Add('btn_add=Қосу');
    Lines.Add('btn_edit=Өңдеу');
    Lines.Add('btn_delete=Жою');
    Lines.Add('btn_import=Импорттау');
    Lines.Add('btn_export=Экспорттау');
    Lines.Add('btn_apply=Қолдану');
    Lines.Add('btn_cancel=Бас тарту');
    Lines.Add('column_name=Атауы');
    Lines.Add('column_host=Хост');
    Lines.Add('column_interval=Аралық');
    Lines.Add('column_timeout=Күту уақыты');
    Lines.Add('column_type=Түр');
    Lines.Add('column_nodes=Тұйындар');
    Lines.Add('node_title_add=Тұйын қосу');
    Lines.Add('node_title_edit=Тұйынды өңдеу');
    Lines.Add('label_name=Атауы:');
    Lines.Add('label_host=Хост:');
    Lines.Add('label_interval=Аралық (мс):');
    Lines.Add('label_timeout=Күту уақыты (мс):');
    Lines.Add('group_title_add=Топ қосу');
    Lines.Add('group_title_edit=Топты өңдеу');
    Lines.Add('label_type=Түр:');
    Lines.Add('label_select_nodes=Тұйындарды таңдаңыз:');
    Lines.Add('type_single=Жеке (1 тұйын)');
    Lines.Add('type_2x2=2x2 тор (2-4 тұйын)');
    Lines.Add('type_3x3=3x3 тор (5-9 тұйын)');
    Lines.Add('selected_count=Таңдалған: %d / %d');
    Lines.Add('error_host_empty=Хост бос болмауы керек');
    Lines.Add('error_host_exists=Бұндай хост бар');
    Lines.Add('error_node_in_group=Бұл тұйын топта қолданылады. Оны барлық топтардан алдын ала алып тастаңыз.');
    Lines.Add('error_no_nodes=Алдымен кем дегенде бір тұйын жасаңыз.');
    Lines.Add('error_group_single=Жеке топ түрі тек 1 тұйынды қажет етеді');
    Lines.Add('error_group_2x2=2x2 топ түрі 2-4 тұйынды қажет етеді');
    Lines.Add('error_group_3x3=3x3 топ түрі 5-9 тұйынды қажет етеді');
    Lines.Add('confirm_delete_node="%s" тұйынын жою?');
    Lines.Add('confirm_delete_group="%s" тобын жою?');
    Lines.Add('title_confirm=Растау');
    Lines.Add('title_error=Қате');
    Lines.Add('title_success=Сәтті');
    Lines.Add('title_cannot_delete=Жою мүмкін емес');
    Lines.Add('title_no_nodes=Тұйындар жоқ');
    Lines.Add('msg_import_success=Конфигурация сәтті импортталды.');
    Lines.Add('msg_export_success=Конфигурация сәтті экспортталды.');
    Lines.Add('label_language=Тіл:');
    Lines.Add('menu_settings=Параметрлер');
    Lines.Add('menu_exit=Шығу');
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
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
    FindClose(SearchRec);
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
  FTranslations.Clear;

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
        if (Key <> 'code') and (Key <> 'name') then
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
  if Result = '' then
    Result := Key; // Return key if translation not found
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
