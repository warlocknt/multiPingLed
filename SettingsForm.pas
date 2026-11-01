unit SettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, Dialogs,
  ComCtrls, Menus, ConfigManager, NodeEditForm, GroupEditForm, LangManager;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    cbLogging: TCheckBox;
    cbPingComparison: TCheckBox;
    cbSessionReport: TCheckBox;
    cbSelectLanguage: TComboBox;
    cbBaloonHint: TCheckBox;
    eCommandExec: TEdit;
    lCommandExec: TLabel;
    lLang: TLabel;
    Memo1: TMemo;
    PageControl: TPageControl;
    NodesTab: TTabSheet;
    GroupsTab: TTabSheet;
    NodeMainPanel: TPanel;
    NodeListPanel: TPanel;
    NodeButtonsPanel: TPanel;
    NodesList: TListView;
    btnAddNode: TButton;
    btnEditNode: TButton;
    btnDeleteNode: TButton;
    btnImportNodes: TButton;
    btnExportNodes: TButton;
    GroupMainPanel: TPanel;
    GroupListPanel: TPanel;
    GroupButtonsPanel: TPanel;
    GroupsList: TListView;
    btnAddGroup: TButton;
    btnEditGroup: TButton;
    btnDeleteGroup: TButton;
    btnApply: TButton;
    btnCancel: TButton;
    SettingsFormButtonsPanel: TPanel;
    TabSheet1: TTabSheet;

    procedure DiagnosticsChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbSelectLanguageChange(Sender: TObject);
    procedure btnAddNodeClick(Sender: TObject);
    procedure btnEditNodeClick(Sender: TObject);
    procedure btnDeleteNodeClick(Sender: TObject);
    procedure btnAddGroupClick(Sender: TObject);
    procedure btnEditGroupClick(Sender: TObject);
    procedure btnDeleteGroupClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnImportNodesClick(Sender: TObject);
    procedure btnExportNodesClick(Sender: TObject);
    procedure GroupsListItemChecked(Sender: TObject; Item: TListItem);
    procedure NodesListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure GroupsListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    FConfig: TAppConfig;
    FOnApply: TNotifyEvent;
    FOnLanguageChanged: TNotifyEvent;
    FConfigManager: TConfigManager;
    FModified: Boolean;
    FLoading: Boolean;

    procedure SetupEvents;
    procedure LoadDiagnostics;
    procedure ApplyLocalization;
    procedure RefreshNodesList;
    procedure RefreshGroupsList;
    function IsHostExists(const Host: string; ExcludeIndex: Integer = -1): Boolean;
    function GetNextNodeId: Integer;
    function GetNextGroupId: Integer;
    procedure LoadNodeCommandFields;
    procedure SaveNodeCommandFields;
    procedure eCommandChange(Sender: TObject);
    procedure LoadLanguageSelector;
  public
    procedure SetConfig(const Config: TAppConfig);
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
    property OnLanguageChanged: TNotifyEvent read FOnLanguageChanged write FOnLanguageChanged;
    property ConfigManager: TConfigManager read FConfigManager write FConfigManager;
  end;

implementation

{$R *.lfm}

uses LogWriter;

procedure SettingsDebugLog(const Msg: string);
begin
  ApplicationLog('multiPingLed[Settings]: ' + Msg);
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  SettingsDebugLog('FormCreate');
  FLoading := True;
  SetupEvents;
  FModified := False;
  ApplyLocalization;
  FLoading := False;
  SettingsDebugLog('FormCreate done');
  PageControl.TabIndex :=0 ;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
begin
  SettingsDebugLog('FormShow');
  FLoading := True;
  try
    LoadLanguageSelector;
    LoadDiagnostics;
    if Assigned(FConfigManager) and Assigned(cbBaloonHint) then
      cbBaloonHint.Checked := FConfigManager.GetBalloonHintEnabled;
    ApplyLocalization;
  finally
    FLoading := False;
  end;
end;

procedure TSettingsForm.LoadDiagnostics;
var WasLoading: Boolean;
begin
  WasLoading := FLoading;
  FLoading := True;
  try
    cbLogging.Checked := FConfig.LoggingEnabled;
    cbPingComparison.Checked := FConfig.ComparePingEnabled;
    cbSessionReport.Checked := FConfig.CreateSessionReport;
    cbPingComparison.Enabled := cbLogging.Checked;
  finally FLoading := WasLoading; end;
end;

procedure TSettingsForm.DiagnosticsChanged(Sender: TObject);
begin
  cbPingComparison.Enabled := cbLogging.Checked;
  if FLoading then Exit;
  FConfig.LoggingEnabled := cbLogging.Checked;
  FConfig.ComparePingEnabled := cbPingComparison.Checked;
  FConfig.CreateSessionReport := cbSessionReport.Checked;
  FModified := True;
end;

procedure TSettingsForm.SetupEvents;
begin
  // Events are bound in LFM, but we need to setup OnSelectItem handlers
  if Assigned(NodesList) then
    NodesList.OnSelectItem := @NodesListSelectItem;
  if Assigned(GroupsList) then
    GroupsList.OnSelectItem := @GroupsListSelectItem;
  // Command fields
  if Assigned(eCommandExec) then
    eCommandExec.OnChange := @eCommandChange;
end;

procedure TSettingsForm.LoadLanguageSelector;
var
  i: Integer;
  LangInfo: TLanguageInfo;
  LangCount: Integer;
  CurrentIdx: Integer;
begin
  cbSelectLanguage.Items.Clear;
  
  if not Assigned(LangMgr) then Exit;
  
  LangCount := LangMgr.GetLanguageCount;
  if LangCount = 0 then Exit;
  
  CurrentIdx := -1;
  for i := 0 to LangCount - 1 do
  begin
    LangInfo := LangMgr.GetLanguageInfo(i);
    cbSelectLanguage.Items.Add(LangInfo.Name);
    if CompareText(LangInfo.Code, LangMgr.CurrentLanguage) = 0 then
      CurrentIdx := i;
  end;
  
  if CurrentIdx >= 0 then
    cbSelectLanguage.ItemIndex := CurrentIdx
  else if cbSelectLanguage.Items.Count > 0 then
    cbSelectLanguage.ItemIndex := 0;
    
  cbSelectLanguage.OnChange := @cbSelectLanguageChange;
end;

procedure TSettingsForm.cbSelectLanguageChange(Sender: TObject);
var
  Idx: Integer;
begin
  if FLoading then Exit;

  Idx := cbSelectLanguage.ItemIndex;
  if Idx < 0 then Exit;

  
  // Just mark that language was changed - will be applied on Apply
  FModified := True;
end;

procedure TSettingsForm.ApplyLocalization;
begin
  if not Assigned(LangMgr) then Exit;
  
  Caption := _('settings_title');
  cbLogging.Caption := _('option_logging');
  cbSessionReport.Caption := _('option_session_report');
  cbPingComparison.Caption := _('option_ping_comparison');
  Memo1.Lines.Text := '{host}' + #9 + _('command_host') + LineEnding +
    '{name}' + #9 + _('command_name') + LineEnding +
    '{status}' + #9 + _('command_status') + LineEnding +
    '{group}' + #9 + _('command_group') + LineEnding +
    '{ping}' + #9 + _('command_ping');

  // Tab sheets
  if Assigned(NodesTab) then
    NodesTab.Caption := _('tab_nodes');
  if Assigned(GroupsTab) then
    GroupsTab.Caption := _('tab_groups');

  // Column headers - Nodes
  if Assigned(NodesList) and (NodesList.Columns.Count > 3) then
  begin
    NodesList.Columns[0].Caption := _('column_name');
    NodesList.Columns[1].Caption := _('column_host');
    NodesList.Columns[2].Caption := _('column_interval');
    NodesList.Columns[3].Caption := _('column_timeout');
  end;

  // Column headers - Groups
  if Assigned(GroupsList) and (GroupsList.Columns.Count > 2) then
  begin
    GroupsList.Columns[0].Caption := _('column_name');
    GroupsList.Columns[1].Caption := _('column_type');
    GroupsList.Columns[2].Caption := _('column_nodes');
  end;

  // Buttons - Nodes
  if Assigned(btnAddNode) then
    btnAddNode.Caption := _('btn_add');
  if Assigned(btnEditNode) then
    btnEditNode.Caption := _('btn_edit');
  if Assigned(btnDeleteNode) then
    btnDeleteNode.Caption := _('btn_delete');

  // Buttons - Groups
  if Assigned(btnAddGroup) then
    btnAddGroup.Caption := _('btn_add');
  if Assigned(btnEditGroup) then
    btnEditGroup.Caption := _('btn_edit');
  if Assigned(btnDeleteGroup) then
    btnDeleteGroup.Caption := _('btn_delete');

  // Bottom buttons
  if Assigned(btnImportNodes) then
    btnImportNodes.Caption := _('btn_import');
  if Assigned(btnExportNodes) then
    btnExportNodes.Caption := _('btn_export');
  if Assigned(btnApply) then
    btnApply.Caption := _('btn_apply');
  if Assigned(btnCancel) then
    btnCancel.Caption := _('btn_cancel');

  // Language selector
  if Assigned(lLang) then
    lLang.Caption := _('label_language');

  // Settings tab (3rd tab)
  if Assigned(TabSheet1) then
    TabSheet1.Caption := _('tab_settings');
  if Assigned(lCommandExec) then
    lCommandExec.Caption := _('label_command_on_change');
  if Assigned(cbBaloonHint) then
    cbBaloonHint.Caption := _('label_balloon_hint');
end;

procedure TSettingsForm.SetConfig(const Config: TAppConfig);
begin
  SettingsDebugLog('SetConfig: ' + IntToStr(Length(Config.Nodes)) + ' nodes, ' + IntToStr(Length(Config.Groups)) + ' groups');
  FConfig := CloneConfig(Config);
  FLoading := True;
  try
    RefreshNodesList;
    RefreshGroupsList;
    LoadDiagnostics;
    ApplyLocalization;
  finally
    FLoading := False;
  end;
  SettingsDebugLog('SetConfig done');
end;

procedure TSettingsForm.RefreshNodesList;
var
  I: Integer;
  Item: TListItem;
  WasLoading: Boolean;
begin
  WasLoading := FLoading;
  FLoading := True;
  try
    NodesList.Items.Clear;

    for I := 0 to High(FConfig.Nodes) do
    begin
      Item := NodesList.Items.Add;
      Item.Caption := FConfig.Nodes[I].Name;
      Item.SubItems.Add(FConfig.Nodes[I].Host);
      Item.SubItems.Add(IntToStr(FConfig.Nodes[I].IntervalMs) + ' ' + _('unit_ms'));
      Item.SubItems.Add(IntToStr(FConfig.Nodes[I].TimeoutMs) + ' ' + _('unit_ms'));
    end;
  finally
    FLoading := WasLoading;
  end;
end;

procedure TSettingsForm.RefreshGroupsList;
var
  I, J: Integer;
  Item: TListItem;
  NodeIdsStr: string;
  TypeStr: string;
  Count: Integer;
  WasLoading: Boolean;
begin
  WasLoading := FLoading;
  FLoading := True;
  try
    GroupsList.Items.Clear;

    for I := 0 to High(FConfig.Groups) do
    begin
      Item := GroupsList.Items.Add;
      Item.Caption := FConfig.Groups[I].Name;
      Item.Checked := FConfig.Groups[I].Enabled;

      case FConfig.Groups[I].GroupType of
        gtSingle: TypeStr := _('type_single');
        gt2x2: TypeStr := _('type_2x2');
        gt3x3: TypeStr := _('type_3x3');
      end;
      Item.SubItems.Add(TypeStr);

      NodeIdsStr := '';
      Count := 0;
      for J := 0 to High(FConfig.Groups[I].NodeIds) do
      begin
        if FConfig.Groups[I].NodeIds[J] > 0 then
        begin
          Inc(Count);
          if NodeIdsStr <> '' then NodeIdsStr := NodeIdsStr + ', ';
          NodeIdsStr := NodeIdsStr + IntToStr(FConfig.Groups[I].NodeIds[J]);
        end;
      end;
      Item.SubItems.Add(Format('%d (%s)', [Count, NodeIdsStr]));
    end;
  finally
    FLoading := WasLoading;
  end;
end;

procedure TSettingsForm.NodesListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  btnEditNode.Enabled := Selected;
  btnDeleteNode.Enabled := Selected;
  // Загружаем команды выделенного узла
  LoadNodeCommandFields;
end;

procedure TSettingsForm.LoadNodeCommandFields;
var
  SelIndex: Integer;
  WasLoading: Boolean;
begin
  WasLoading := FLoading;
  FLoading := True;
  try
    if (NodesList.Selected = nil) or (NodesList.Selected.Index < 0) then
    begin
      if Assigned(eCommandExec) then eCommandExec.Text := '';
      Exit;
    end;
    SelIndex := NodesList.Selected.Index;
    if (SelIndex >= 0) and (SelIndex < Length(FConfig.Nodes)) then
    begin
      if Assigned(eCommandExec) then eCommandExec.Text := FConfig.Nodes[SelIndex].Command;
    end;
  finally
    FLoading := WasLoading;
  end;
end;

procedure TSettingsForm.SaveNodeCommandFields;
var
  SelIndex: Integer;
begin
  if (NodesList.Selected = nil) or (NodesList.Selected.Index < 0) then Exit;
  SelIndex := NodesList.Selected.Index;
  if (SelIndex >= 0) and (SelIndex < Length(FConfig.Nodes)) then
  begin
    if Assigned(eCommandExec) then FConfig.Nodes[SelIndex].Command := Trim(eCommandExec.Text);
  end;
end;

procedure TSettingsForm.eCommandChange(Sender: TObject);
begin
  if FLoading then Exit;
  SaveNodeCommandFields;
  FModified := True;
end;

procedure TSettingsForm.GroupsListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  btnEditGroup.Enabled := Selected;
  btnDeleteGroup.Enabled := Selected;
end;

function TSettingsForm.GetNextNodeId: Integer;
var
  I: Integer;
begin
  Result := 1;
  for I := 0 to High(FConfig.Nodes) do
  begin
    if FConfig.Nodes[I].Id >= Result then
      Result := FConfig.Nodes[I].Id + 1;
  end;
end;

function TSettingsForm.GetNextGroupId: Integer;
var
  I: Integer;
begin
  Result := 1;
  for I := 0 to High(FConfig.Groups) do
  begin
    if FConfig.Groups[I].Id >= Result then
      Result := FConfig.Groups[I].Id + 1;
  end;
end;

function TSettingsForm.IsHostExists(const Host: string; ExcludeIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(FConfig.Nodes) do
  begin
    if (I <> ExcludeIndex) and (CompareText(FConfig.Nodes[I].Host, Host) = 0) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TSettingsForm.btnAddNodeClick(Sender: TObject);
var
  NewIndex: Integer;
  NewNode: TNodeConfig;
  ExistingHosts: array of string;
  I: Integer;
  Dlg: TNodeEditForm;
begin
  // Prepare list of existing hosts
  ExistingHosts := nil;
  SetLength(ExistingHosts, Length(FConfig.Nodes));
  for I := 0 to High(FConfig.Nodes) do
    ExistingHosts[I] := FConfig.Nodes[I].Host;

  // Initialize new node
  NewNode := Default(TNodeConfig);
  NewNode.Name := _('default_node_name');
  NewNode.Host := '';
  NewNode.IntervalMs := 5000;
  NewNode.TimeoutMs := 2000;
  NewNode.State := nsUnknown;

  Dlg := TNodeEditForm.Create(nil);
  try
    Dlg.ApplyLocalization;
    Dlg.Caption := _('node_title_add');
    Dlg.SetData(NewNode, ExistingHosts, -1);

    if Dlg.ShowModal = mrOK then
    begin
      NewNode := Dlg.GetData;
      NewIndex := Length(FConfig.Nodes);
      SetLength(FConfig.Nodes, NewIndex + 1);

      NewNode.Id := GetNextNodeId;
      FConfig.Nodes[NewIndex] := NewNode;

      FModified := True;
      RefreshNodesList;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TSettingsForm.btnEditNodeClick(Sender: TObject);
var
  SelIndex: Integer;
  EditedNode: TNodeConfig;
  ExistingHosts: array of string;
  I: Integer;
  Dlg: TNodeEditForm;
begin
  if NodesList.Selected = nil then Exit;

  SelIndex := NodesList.Selected.Index;
  if (SelIndex < 0) or (SelIndex > High(FConfig.Nodes)) then Exit;

  // Prepare list of existing hosts (excluding current)
  ExistingHosts := nil;
  SetLength(ExistingHosts, Length(FConfig.Nodes));
  for I := 0 to High(FConfig.Nodes) do
    ExistingHosts[I] := FConfig.Nodes[I].Host;

  EditedNode := FConfig.Nodes[SelIndex];

  Dlg := TNodeEditForm.Create(nil);
  try
    Dlg.ApplyLocalization;
    Dlg.Caption := _('node_title_edit');
    Dlg.SetData(EditedNode, ExistingHosts, SelIndex);

    if Dlg.ShowModal = mrOK then
    begin
      FConfig.Nodes[SelIndex] := Dlg.GetData;
      FModified := True;
      RefreshNodesList;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TSettingsForm.btnDeleteNodeClick(Sender: TObject);
var
  SelIndex: Integer;
  I: Integer;
  NodeId: Integer;
  CanDelete: Boolean;
  J: Integer;
begin
  if NodesList.Selected = nil then Exit;

  SelIndex := NodesList.Selected.Index;
  if (SelIndex < 0) or (SelIndex > High(FConfig.Nodes)) then Exit;

  NodeId := FConfig.Nodes[SelIndex].Id;

  // Check if node is used in any group
  CanDelete := True;
  for I := 0 to High(FConfig.Groups) do
  begin
    for J := 0 to High(FConfig.Groups[I].NodeIds) do
    begin
      if FConfig.Groups[I].NodeIds[J] = NodeId then
      begin
        CanDelete := False;
        Break;
      end;
    end;
    if not CanDelete then Break;
  end;

  if not CanDelete then
  begin
    MessageDlg(_('title_cannot_delete'), _('error_node_in_group'), mtError, [mbOK], 0);
    Exit;
  end;

  if MessageDlg(_('title_confirm'), Format(_('confirm_delete_node'), [FConfig.Nodes[SelIndex].Name]),
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Remove node from array
    for I := SelIndex to High(FConfig.Nodes) - 1 do
      FConfig.Nodes[I] := FConfig.Nodes[I + 1];
    SetLength(FConfig.Nodes, Length(FConfig.Nodes) - 1);

    FModified := True;
    RefreshNodesList;
  end;
end;

procedure TSettingsForm.btnAddGroupClick(Sender: TObject);
var
  NewIndex: Integer;
  NewGroup: TNodeGroup;
  Dlg: TGroupEditForm;
begin
  if Length(FConfig.Nodes) = 0 then
  begin
    MessageDlg(_('title_no_nodes'), _('error_no_nodes'), mtError, [mbOK], 0);
    Exit;
  end;

  NewGroup := Default(TNodeGroup);
  NewGroup.Name := _('default_group_name');
  NewGroup.GroupType := gtSingle;
  NewGroup.Enabled := True;
  SetLength(NewGroup.NodeIds, 1);
  if Length(FConfig.Nodes) > 0 then
    NewGroup.NodeIds[0] := FConfig.Nodes[0].Id;

  Dlg := TGroupEditForm.Create(nil);
  try
    Dlg.ApplyLocalization;
    Dlg.Caption := _('group_title_add');
    Dlg.SetData(NewGroup, FConfig.Nodes);

    if Dlg.ShowModal = mrOK then
    begin
      if Dlg.ValidateInput then
      begin
        NewGroup := Dlg.GetData;
        NewIndex := Length(FConfig.Groups);
        SetLength(FConfig.Groups, NewIndex + 1);

        NewGroup.Id := GetNextGroupId;
        FConfig.Groups[NewIndex] := NewGroup;

        FModified := True;
        RefreshGroupsList;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TSettingsForm.btnEditGroupClick(Sender: TObject);
var
  SelIndex: Integer;
  EditedGroup: TNodeGroup;
  Dlg: TGroupEditForm;
begin
  if GroupsList.Selected = nil then Exit;

  SelIndex := GroupsList.Selected.Index;
  if (SelIndex < 0) or (SelIndex > High(FConfig.Groups)) then Exit;

  EditedGroup := FConfig.Groups[SelIndex];

  Dlg := TGroupEditForm.Create(nil);
  try
    Dlg.ApplyLocalization;
    Dlg.Caption := _('group_title_edit');
    Dlg.SetData(EditedGroup, FConfig.Nodes);

    if Dlg.ShowModal = mrOK then
    begin
      if Dlg.ValidateInput then
      begin
        FConfig.Groups[SelIndex] := Dlg.GetData;
        FModified := True;
        RefreshGroupsList;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TSettingsForm.btnDeleteGroupClick(Sender: TObject);
var
  SelIndex: Integer;
  I: Integer;
begin
  if GroupsList.Selected = nil then Exit;

  SelIndex := GroupsList.Selected.Index;
  if (SelIndex < 0) or (SelIndex > High(FConfig.Groups)) then Exit;

  if MessageDlg(_('title_confirm'), Format(_('confirm_delete_group'), [FConfig.Groups[SelIndex].Name]),
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    for I := SelIndex to High(FConfig.Groups) - 1 do
      FConfig.Groups[I] := FConfig.Groups[I + 1];
    SetLength(FConfig.Groups, Length(FConfig.Groups) - 1);

    FModified := True;
    RefreshGroupsList;
  end;
end;

procedure TSettingsForm.btnApplyClick(Sender: TObject);
var
  ErrorMsg: string;
  Idx: Integer;
  LangInfo: TLanguageInfo;
  LangCode: string;
  Validator: TConfigManager;
  OldConfig: TAppConfig;
  Balloon: Integer;
begin
  SettingsDebugLog('btnApplyClick - applying config');
  // Use external ConfigManager if assigned, otherwise create new
  if Assigned(FConfigManager) then
  begin
    Validator := TConfigManager.Create('');
    try
      Validator.SetConfig(FConfig);
      if not Validator.ValidateConfig(ErrorMsg) then
      begin
        MessageDlg(_('title_error'), ErrorMsg, mtError, [mbOK], 0);
        Exit;
      end;
    finally
      Validator.Free;
    end;
    LangCode := LangMgr.CurrentLanguage;
    Idx := cbSelectLanguage.ItemIndex;
    if Idx >= 0 then
    begin
      LangInfo := LangMgr.GetLanguageInfo(Idx);
      LangCode := LangInfo.Code;
    end;
    Balloon := -1;
    if Assigned(cbBaloonHint) then Balloon := Ord(cbBaloonHint.Checked);
    OldConfig := FConfigManager.GetConfig;
    FConfigManager.SetConfig(FConfig);
    try
      FConfigManager.SaveConfig(LangCode, Balloon);
    except
      on E: Exception do
      begin
        FConfigManager.SetConfig(OldConfig);
        MessageDlg(_('title_error'), E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
    LangMgr.SetLanguage(LangCode);
    if Assigned(FOnLanguageChanged) then FOnLanguageChanged(Self);

    if Assigned(FOnApply) then
    begin
      SettingsDebugLog('Calling OnApply handler');
      FOnApply(Self);
    end;

    FModified := False;
    ModalResult := mrOK;
    SettingsDebugLog('Apply done');
  end;
end;

procedure TSettingsForm.btnCancelClick(Sender: TObject);
begin
  SettingsDebugLog('btnCancelClick');
  ModalResult := mrCancel;
end;

procedure TSettingsForm.btnImportNodesClick(Sender: TObject);
var
  OpenDlg: TOpenDialog;
  ConfigMgr: TConfigManager;
  ErrorMsg: string;
  I: Integer;
  LangInfo: TLanguageInfo;
begin
  OpenDlg := TOpenDialog.Create(nil);
  try
    OpenDlg.Filter := _('filter_config_files');
    OpenDlg.DefaultExt := 'ini';

    if OpenDlg.Execute then
    begin
      ConfigMgr := TConfigManager.Create('');
      try
        if ConfigMgr.ImportConfig(OpenDlg.FileName, ErrorMsg) then
        begin
          FConfig := ConfigMgr.GetConfig;
          LoadDiagnostics;
          if Assigned(FConfigManager) then FConfig.Version := FConfigManager.GetVersion;
          cbBaloonHint.Checked := ConfigMgr.GetBalloonHintEnabled;
          for I := 0 to LangMgr.GetLanguageCount - 1 do
          begin
            LangInfo := LangMgr.GetLanguageInfo(I);
            if SameText(LangInfo.Code, ConfigMgr.GetLanguage) then
              cbSelectLanguage.ItemIndex := I;
          end;
          RefreshNodesList;
          RefreshGroupsList;
          FModified := True;
          MessageDlg(_('title_success'), _('msg_import_success'), mtInformation, [mbOK], 0);
        end
        else
        begin
          MessageDlg(_('title_error'), ErrorMsg, mtError, [mbOK], 0);
        end;
      finally
        ConfigMgr.Free;
      end;
    end;
  finally
    OpenDlg.Free;
  end;
end;

procedure TSettingsForm.btnExportNodesClick(Sender: TObject);
var
  SaveDlg: TSaveDialog;
  ConfigMgr: TConfigManager;
begin
  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.Filter := _('filter_config_files');
    SaveDlg.DefaultExt := 'ini';

    if SaveDlg.Execute then
    begin
      ConfigMgr := TConfigManager.Create('');
      try
        ConfigMgr.SetConfig(FConfig);
        ConfigMgr.ExportConfig(SaveDlg.FileName);
        MessageDlg(_('title_success'), _('msg_export_success'), mtInformation, [mbOK], 0);
      finally
        ConfigMgr.Free;
      end;
    end;
  finally
    SaveDlg.Free;
  end;
end;

procedure TSettingsForm.GroupsListItemChecked(Sender: TObject; Item: TListItem);
var
  Idx: Integer;
begin
  if FLoading then Exit;

  Idx := Item.Index;
  if (Idx >= 0) and (Idx <= High(FConfig.Groups)) then
  begin
    FConfig.Groups[Idx].Enabled := Item.Checked;
    FModified := True;
  end;
end;

end.
