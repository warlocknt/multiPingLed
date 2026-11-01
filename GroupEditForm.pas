unit GroupEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst,
  ConfigManager, LangManager;

type

  { TGroupEditForm }

  TGroupEditForm = class(TForm)
    lblName: TLabel;
    lblType: TLabel;
    lblNodes: TLabel;
    CounterLbl: TLabel;
    edName: TEdit;
    cbType: TComboBox;
    clbNodes: TCheckListBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure OnNodeCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FGroup: TNodeGroup;
    FAllNodes: array of TNodeConfig;
    procedure UpdateCounter;
    function GetRequiredCount: Integer;
    function GetMinCount: Integer;
    function GetSelectedCount: Integer;
    procedure FillNodeList;
    procedure OnTypeChange(Sender: TObject);
  public
    procedure SetData(const Group: TNodeGroup; const AllNodes: array of TNodeConfig);
    function GetData: TNodeGroup;
    function ValidateInput: Boolean;
    procedure ApplyLocalization;
  end;

implementation

{$R *.lfm}

procedure TGroupEditForm.FormCreate(Sender: TObject);
begin
  ApplyLocalization;
  clbNodes.OnClickCheck := @OnNodeCheckClick;
  cbType.OnChange := @OnTypeChange;
end;


procedure TGroupEditForm.OnNodeCheckClick(Sender: TObject);
begin
  UpdateCounter;

end;

procedure TGroupEditForm.OnTypeChange(Sender: TObject);
begin
  UpdateCounter;
end;

procedure TGroupEditForm.ApplyLocalization;
begin
  lblName.Caption := _('label_name');
  lblType.Caption := _('label_type');
  lblNodes.Caption := _('label_select_nodes');
  btnOK.Caption := _('btn_apply');
  btnCancel.Caption := _('btn_cancel');

  // Update type combo items
  cbType.Items.Clear;
  cbType.Items.Add(_('type_single'));
  cbType.Items.Add(_('type_2x2'));
  cbType.Items.Add(_('type_3x3'));
end;

procedure TGroupEditForm.SetData(const Group: TNodeGroup; const AllNodes: array of TNodeConfig);
var
  I: Integer;
begin
  FGroup := Group;
  SetLength(FAllNodes, Length(AllNodes));
  for I := 0 to High(AllNodes) do
    FAllNodes[I] := AllNodes[I];

  edName.Text := Group.Name;
  case Group.GroupType of
    gtSingle: cbType.ItemIndex := 0;
    gt2x2: cbType.ItemIndex := 1;
    gt3x3: cbType.ItemIndex := 2;
  else
    cbType.ItemIndex := 0;
  end;

  FillNodeList;
  UpdateCounter;
end;

function TGroupEditForm.GetData: TNodeGroup;
var I, J, Slot, Capacity, KeepLength: Integer;
  function Selected(Id: Integer): Boolean;
  var K: Integer;
  begin
    for K := 0 to High(FAllNodes) do
      if FAllNodes[K].Id = Id then Exit(clbNodes.Checked[K]);
    Result := False;
  end;
begin
  Result := FGroup; Result.Name := Trim(edName.Text);
  case cbType.ItemIndex of
    0: begin Result.GroupType := gtSingle; Capacity := 1; end;
    1: begin Result.GroupType := gt2x2; Capacity := 4; end;
  else begin Result.GroupType := gt3x3; Capacity := 9; end; end;
  Result.NodeIds := nil; SetLength(Result.NodeIds, Capacity);
  KeepLength := Length(FGroup.NodeIds);
  if KeepLength > Capacity then KeepLength := Capacity;
  // Preserve occupied positions, including holes between existing hosts.
  for I := 0 to KeepLength - 1 do
    if (FGroup.NodeIds[I] > 0) and Selected(FGroup.NodeIds[I]) then
      Result.NodeIds[I] := FGroup.NodeIds[I];
  for I := 0 to High(FAllNodes) do
    if clbNodes.Checked[I] then
    begin
      J := 0;
      while (J < Capacity) and (Result.NodeIds[J] <> FAllNodes[I].Id) do Inc(J);
      if J < Capacity then Continue;
      Slot := 0;
      while (Slot < Capacity) and (Result.NodeIds[Slot] <> 0) do Inc(Slot);
      if Slot >= Capacity then Continue;
      Result.NodeIds[Slot] := FAllNodes[I].Id;
      if KeepLength <= Slot then KeepLength := Slot + 1;
    end;
  SetLength(Result.NodeIds, KeepLength);
end;

procedure TGroupEditForm.UpdateCounter;
var
  Count, MinCount, MaxCount: Integer;
begin
  Count := GetSelectedCount;
  // Grow automatically when necessary; never shrink an existing grid on a rename.
  if Count > GetRequiredCount then
    if Count <= 4 then cbType.ItemIndex := 1 else cbType.ItemIndex := 2;

  MinCount := GetMinCount;
  MaxCount := GetRequiredCount;

  CounterLbl.Caption := Format('%d (%d-%d)', [Count, MinCount, MaxCount]);

  if (Count >= MinCount) and (Count <= MaxCount) then
    CounterLbl.Font.Color := clGreen
  else
    CounterLbl.Font.Color := clRed;
end;

function TGroupEditForm.GetRequiredCount: Integer;
begin
  case cbType.ItemIndex of
    0: Result := 1;
    1: Result := 4;  // 2x2: max 4, min 1
    2: Result := 9;  // 3x3: max 9, min 1
  else
    Result := 0;
  end;
end;

function TGroupEditForm.GetMinCount: Integer;
begin
  Result := 1;
end;

function TGroupEditForm.GetSelectedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to clbNodes.Count - 1 do
    if clbNodes.Checked[I] then
      Inc(Result);
end;

procedure TGroupEditForm.FillNodeList;
var
  I: Integer;
  NodeName: string;
  NodeId, J: Integer;
  IsInGroup: Boolean;
begin
  clbNodes.Items.Clear;

  for I := 0 to High(FAllNodes) do
  begin
    NodeId := FAllNodes[I].Id;
    NodeName := Format('%d: %s (%s)', [NodeId, FAllNodes[I].Name, FAllNodes[I].Host]);
    clbNodes.Items.Add(NodeName);

    // Check if this node is already in the group
    IsInGroup := False;
    for J := 0 to High(FGroup.NodeIds) do
    begin
      if FGroup.NodeIds[J] = NodeId then
      begin
        IsInGroup := True;
        Break;
      end;
    end;

    if IsInGroup then
      clbNodes.Checked[I] := True;
  end;
end;

function TGroupEditForm.ValidateInput: Boolean;
var
  Count, MinCount, MaxCount: Integer;
begin
  Result := False;

  Count := GetSelectedCount;
  MinCount := GetMinCount;
  MaxCount := GetRequiredCount;

  if (Count < MinCount) or (Count > MaxCount) then
  begin
    case cbType.ItemIndex of
      0: ShowMessage(_('error_group_single'));
      1: ShowMessage(_('error_group_2x2'));
      2: ShowMessage(_('error_group_3x3'));
    end;
    Exit;
  end;

  Result := True;
end;

end.
