unit SettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, Dialogs,
  ComCtrls, Menus, FGL, NodeManager, ConfigManager, NodeEditForm, GroupEditForm, LangManager;

type

  { TSettingsForm }

  TApplyConfiguration = procedure(Sender: TObject; Persist: TNotifyEvent) of object;
  TMonitoringRows = specialize TFPGMap<Integer, TListItem>;
  TSnapshotIndex = specialize TFPGMap<Integer, Integer>;

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
    SettingsTab: TTabSheet;
    MonitoringTab: TTabSheet;
    MonitoringList: TListView;
    MonitoringLegend: TLabel;
    MonitoringTimer: TTimer;

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
    FBaseline: TAppConfig;
    FBaselineLanguage: string;
    FBaselineBalloon: Boolean;
    FRows: TMonitoringRows;
    FSnapshotIndex: TSnapshotIndex;
    FNodeManager: TNodeManager;
    FMonitoring: TMonitoringSnapshot;
    FConfig: TAppConfig;
    FPendingLanguage: string;
    FPendingPersisted: Boolean;
    FOnApply: TApplyConfiguration;
    FOnLanguageChanged: TNotifyEvent;
    FConfigManager: TConfigManager;
    FModified: Boolean;
    FLoading: Boolean;
    procedure PersistPending(Sender: TObject);
    procedure SetConfigManager(Value: TConfigManager);
    function SelectedLanguage: string;
    procedure PreferenceChanged(Sender: TObject);
    procedure UpdateMonitoringHint;
    procedure MonitoringTick(Sender: TObject);
    procedure MonitoringPageChanged(Sender: TObject);
    procedure MonitoringMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MonitoringDrawSubItem(Sender: TCustomListView; Item: TListItem;
      SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);


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
    destructor Destroy; override;
    procedure UpdateApplyState;
    function TryApplyConfig(out ErrorText: string): Boolean;
    procedure ExportDraft(const FileName: string);
    procedure SetConfig(const Config: TAppConfig);
    procedure RefreshMonitoring;
    procedure OpenPage(Monitoring: Boolean);
    property NodeManager: TNodeManager read FNodeManager write FNodeManager;
    property OnApply: TApplyConfiguration read FOnApply write FOnApply;
    property OnLanguageChanged: TNotifyEvent read FOnLanguageChanged write FOnLanguageChanged;
    property ConfigManager: TConfigManager read FConfigManager write SetConfigManager;
  end;

implementation

{$R *.lfm}

uses LogWriter;

procedure SettingsDebugLog(const Msg: string);
begin
  ApplicationLog('multiPingLed[Settings]: ' + Msg);
end;

procedure TSettingsForm.OpenPage(Monitoring: Boolean);
begin
  if Monitoring then PageControl.ActivePage := MonitoringTab
  else PageControl.ActivePage := SettingsTab;
  MonitoringPageChanged(Self);
end;

procedure TSettingsForm.MonitoringPageChanged(Sender: TObject);
begin
  MonitoringTimer.Enabled := Visible and (PageControl.ActivePage = MonitoringTab);
  if PageControl.ActivePage = MonitoringTab then RefreshMonitoring;
end;

procedure TSettingsForm.MonitoringTick(Sender: TObject);
begin
  if Visible and (PageControl.ActivePage = MonitoringTab) then RefreshMonitoring;
end;

procedure TSettingsForm.RefreshMonitoring;
var I, J, K, Id: Integer; Item: TListItem; Values: array[0..4] of string; N: TNodeConfig;
begin
  if FNodeManager = nil then Exit;
  FMonitoring := FNodeManager.GetMonitoringSnapshot;
  FSnapshotIndex.Clear; FSnapshotIndex.Sorted := False;
  for I := 0 to High(FMonitoring) do
    if FMonitoring[I].Active then FSnapshotIndex.Add(FMonitoring[I].Node.Id, I);
  FSnapshotIndex.Sorted := True;
  MonitoringList.Items.BeginUpdate;
  try
    for J := MonitoringList.Items.Count - 1 downto 0 do
    begin
      Id := PtrUInt(MonitoringList.Items[J].Data);
      if FSnapshotIndex.IndexOf(Id) < 0 then
      begin
        K := FRows.IndexOf(Id); if K >= 0 then FRows.Delete(K);
        MonitoringList.Items.Delete(J);
      end;
    end;
    for I := 0 to High(FMonitoring) do
    begin
      if not FMonitoring[I].Active then Continue;
      N := FMonitoring[I].Node;
      J := FRows.IndexOf(N.Id);
      if J >= 0 then Item := FRows.Data[J]
      else
      begin
        Item := MonitoringList.Items.Add;
        Item.Data := Pointer(PtrUInt(N.Id));
        for K := 0 to 4 do Item.SubItems.Add('');
        FRows.Add(N.Id, Item);
      end;
      if Item.Caption <> N.Name then Item.Caption := N.Name;
      Values[0] := N.Host;
      case N.State of
        nsUp: Values[1] := _('state_up');
        nsDown: Values[1] := _('state_down');
      else Values[1] := _('monitor_unknown'); end;
      Values[2] := '-';
      if (N.LastPingTime > 0) and (N.LastPingMs >= 0) then
        Values[2] := IntToStr(N.LastPingMs) + ' ' + _('unit_ms');
      Values[3] := IntToStr(N.ProbeLosses) + ' / ' + IntToStr(N.ProbeCount);
      Values[4] := '-';
      if N.LastStateChange > 0 then Values[4] := FormatDateTime('dd.mm hh:nn:ss', N.LastStateChange);
      for K := 0 to 4 do
        if Item.SubItems[K] <> Values[K] then Item.SubItems[K] := Values[K];
    end;
  finally MonitoringList.Items.EndUpdate; end;
  UpdateMonitoringHint;
end;

procedure TSettingsForm.UpdateMonitoringHint;
var P: TPoint;
begin
  P := MonitoringList.ScreenToClient(Mouse.CursorPos);
  MonitoringMouseMove(MonitoringList, [], P.X, P.Y);
end;

procedure TSettingsForm.MonitoringMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var Item: TListItem; I: Integer; HintText: string;
begin
  HintText := '';
  Item := MonitoringList.GetItemAt(X, Y);
  if Item <> nil then
  begin
    I := FSnapshotIndex.IndexOf(PtrUInt(Item.Data));
    if I >= 0 then HintText := FMonitoring[FSnapshotIndex.Data[I]].Node.LastPingError;
  end;
  if MonitoringList.Hint <> HintText then
  begin
    Application.CancelHint;
    MonitoringList.Hint := HintText;
  end;
end;

procedure TSettingsForm.MonitoringDrawSubItem(Sender: TCustomListView; Item: TListItem;
  SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
var I: Integer;
begin
  DefaultDraw := True;
  if cdsSelected in State then Exit;
  Sender.Canvas.Font.Color := MonitoringList.Font.Color;
  if SubItem <> 2 then Exit;
  I := FSnapshotIndex.IndexOf(PtrUInt(Item.Data));
  if I < 0 then Exit;
  I := FSnapshotIndex.Data[I];
  if not FMonitoring[I].Active then Sender.Canvas.Font.Color := clGray
  else case FMonitoring[I].Node.State of
    nsUp: Sender.Canvas.Font.Color := RGBToColor(47, 225, 119);
    nsDown: Sender.Canvas.Font.Color := RGBToColor(245, 69, 83);
  else Sender.Canvas.Font.Color := RGBToColor(129, 140, 155); end;
end;

procedure TSettingsForm.SetConfigManager(Value: TConfigManager);
var WasLoading: Boolean;
begin
  FConfigManager := Value;
  if Value <> nil then
  begin
    FBaseline := Value.GetConfig;
    FBaselineLanguage := Value.GetLanguage;
    FBaselineBalloon := Value.GetBalloonHintEnabled;
    WasLoading := FLoading; FLoading := True;
    try cbBaloonHint.Checked := FBaselineBalloon;
    finally FLoading := WasLoading; end;
  end;
  UpdateApplyState;
end;

function TSettingsForm.SelectedLanguage: string;
var Info: TLanguageInfo;
begin
  Result := FBaselineLanguage;
  if cbSelectLanguage.ItemIndex >= 0 then
  begin
    Info := LangMgr.GetLanguageInfo(cbSelectLanguage.ItemIndex);
    if Info.Code <> '' then Result := Info.Code;
  end;
end;

procedure TSettingsForm.PreferenceChanged(Sender: TObject);
begin
  if not FLoading then UpdateApplyState;
end;

procedure TSettingsForm.UpdateApplyState;
begin
  if FLoading then Exit;
  FModified := not SameEditableConfig(FConfig, FBaseline) or
    (SelectedLanguage <> FBaselineLanguage) or (cbBaloonHint.Checked <> FBaselineBalloon);
  btnApply.Enabled := FModified and (FConfigManager <> nil);
end;

destructor TSettingsForm.Destroy;
begin
  if MonitoringTimer <> nil then MonitoringTimer.Enabled := False;
  FRows.Free;
  FSnapshotIndex.Free;
  inherited Destroy;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  FRows := TMonitoringRows.Create; FRows.Sorted := True;
  FSnapshotIndex := TSnapshotIndex.Create; FSnapshotIndex.Sorted := True;
  SettingsDebugLog('FormCreate');
  FLoading := True;
  SetupEvents;
  FModified := False;
  btnApply.Enabled := False;
  ApplyLocalization;
  FLoading := False;
  SettingsDebugLog('FormCreate done');
  PageControl.ActivePage := MonitoringTab;
  PageControl.OnChange := @MonitoringPageChanged;
  MonitoringTimer.OnTimer := @MonitoringTick;
  MonitoringList.OnMouseMove := @MonitoringMouseMove;
  MonitoringList.OnCustomDrawSubItem := @MonitoringDrawSubItem;
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
  MonitoringPageChanged(Self);
  UpdateApplyState;
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
  UpdateApplyState;
end;

procedure TSettingsForm.SetupEvents;
begin
  cbBaloonHint.OnChange := @PreferenceChanged;
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
  UpdateApplyState;
end;

procedure TSettingsForm.ApplyLocalization;
begin
  if not Assigned(LangMgr) then Exit;
  
  Caption := _('settings_title');
  MonitoringTab.Caption := _('tab_monitoring');
  MonitoringLegend.Caption := _('monitor_legend');
  MonitoringList.Columns[0].Caption := _('column_name');
  MonitoringList.Columns[1].Caption := _('column_host');
  MonitoringList.Columns[2].Caption := _('monitor_status');
  MonitoringList.Columns[3].Caption := _('monitor_latency');
  MonitoringList.Columns[4].Caption := _('monitor_losses');
  MonitoringList.Columns[5].Caption := _('monitor_changed');
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
    btnCancel.Caption := _('btn_discard_changes');

  // Language selector
  if Assigned(lLang) then
    lLang.Caption := _('label_language');

  // Settings tab (3rd tab)
  if Assigned(SettingsTab) then
    SettingsTab.Caption := _('tab_settings');
  if Assigned(lCommandExec) then
    lCommandExec.Caption := _('label_command_on_change');
  if Assigned(cbBaloonHint) then
    cbBaloonHint.Caption := _('label_balloon_hint');
end;

procedure TSettingsForm.SetConfig(const Config: TAppConfig);
begin
  SettingsDebugLog('SetConfig: ' + IntToStr(Length(Config.Nodes)) + ' nodes, ' + IntToStr(Length(Config.Groups)) + ' groups');
  FConfig := CloneConfig(Config);
  FBaseline := CloneConfig(Config);
  FBaselineLanguage := LangMgr.CurrentLanguage;
  FBaselineBalloon := True;
  if FConfigManager <> nil then
  begin
    FBaseline := FConfigManager.GetConfig;
    FBaselineLanguage := FConfigManager.GetLanguage;
    FBaselineBalloon := FConfigManager.GetBalloonHintEnabled;
  end;
  FLoading := True;
  try
    RefreshNodesList;
    RefreshGroupsList;
    LoadDiagnostics;
    ApplyLocalization;
  finally
    FLoading := False;
  end;
  UpdateApplyState;
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
  UpdateApplyState;
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

      UpdateApplyState;
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
      UpdateApplyState;
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

    UpdateApplyState;
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

        UpdateApplyState;
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
        UpdateApplyState;
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

    UpdateApplyState;
    RefreshGroupsList;
  end;
end;

procedure TSettingsForm.PersistPending(Sender: TObject);
begin
  FConfigManager.SaveConfig(FPendingLanguage, Ord(cbBaloonHint.Checked));
  FPendingPersisted := True;
end;

function TSettingsForm.TryApplyConfig(out ErrorText: string): Boolean;
var Validator: TConfigManager; OldConfig: TAppConfig;
    LangCode, OldLanguage: string; OldBalloon: Integer; RuntimeAttempted: Boolean;
begin
  Result := False; ErrorText := '';
  UpdateApplyState;
  if (FConfigManager = nil) or not FModified then Exit(True);
  Validator := TConfigManager.Create('');
  try
    Validator.SetConfig(FConfig);
    if not Validator.ValidateConfig(ErrorText) then Exit;
  finally Validator.Free; end;
  LangCode := SelectedLanguage;
  OldConfig := FConfigManager.GetConfig;
  OldLanguage := FConfigManager.GetLanguage;
  OldBalloon := Ord(FConfigManager.GetBalloonHintEnabled);
  FPendingLanguage := LangCode; FPendingPersisted := False;
  RuntimeAttempted := False;
  FConfigManager.SetConfig(FConfig);
  try
    if Assigned(FOnApply) then
    begin
      RuntimeAttempted := True;
      FOnApply(Self, @PersistPending);
    end;
    if not FPendingPersisted then PersistPending(Self);
  except
    on E: Exception do
    begin
      ErrorText := E.Message;
      FConfigManager.SetConfig(OldConfig);
      if FPendingPersisted then
        try FConfigManager.SaveConfig(OldLanguage, OldBalloon);
        except on R: Exception do ErrorText := ErrorText + LineEnding + R.Message; end;
      if RuntimeAttempted then
        try FOnApply(Self, nil);
        except on R: Exception do ErrorText := ErrorText + LineEnding + R.Message; end;
      UpdateApplyState;
      Exit;
    end;
  end;
  LangMgr.SetLanguage(LangCode);
  if Assigned(FOnLanguageChanged) then FOnLanguageChanged(Self);
  FBaseline := CloneConfig(FConfig);
  FBaselineLanguage := LangCode;
  FBaselineBalloon := cbBaloonHint.Checked;
  ApplyLocalization;
  RefreshMonitoring;
  UpdateApplyState;
  Result := True;
end;

procedure TSettingsForm.btnApplyClick(Sender: TObject);
var ErrorText: string;
begin
  if not TryApplyConfig(ErrorText) then
    MessageDlg(_('title_error'), ErrorText, mtError, [mbOK], 0);
end;

procedure TSettingsForm.btnCancelClick(Sender: TObject);
begin
  SettingsDebugLog('btnCancelClick');
  if FConfigManager <> nil then
  begin
    SetConfig(FConfigManager.GetConfig);
    LoadLanguageSelector;
    cbBaloonHint.Checked := FConfigManager.GetBalloonHintEnabled;
    UpdateApplyState;
  end;
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
          UpdateApplyState;
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

procedure TSettingsForm.ExportDraft(const FileName: string);
var Manager: TConfigManager;
begin
  Manager := TConfigManager.Create('');
  try
    Manager.SetConfig(FConfig);
    Manager.ExportConfig(FileName, SelectedLanguage, Ord(cbBaloonHint.Checked));
  finally Manager.Free; end;
end;

procedure TSettingsForm.btnExportNodesClick(Sender: TObject);
var
  SaveDlg: TSaveDialog;
begin
  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.Filter := _('filter_config_files');
    SaveDlg.DefaultExt := 'ini';

    if SaveDlg.Execute then
    begin
      ExportDraft(SaveDlg.FileName);
      MessageDlg(_('title_success'), _('msg_export_success'), mtInformation, [mbOK], 0);
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
    UpdateApplyState;
  end;
end;

end.
