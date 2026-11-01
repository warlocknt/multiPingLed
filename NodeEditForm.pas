unit NodeEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ConfigManager, LangManager;

type

  { TNodeEditForm }

  TNodeEditForm = class(TForm)
    NodeNotifications: TCheckBox;
    NodeFailureThreshold: TEdit;
    NodeFailureLabel: TLabel;
    cbOnPingFail: TCheckBox;
    cbOnPingRestore: TCheckBox;
    eRunOnPingFail: TEdit;
    eRunOnPingResore: TEdit;
    lblName: TLabel;
    lblHost: TLabel;
    lblInterval: TLabel;
    lblTimeout: TLabel;
    edName: TEdit;
    edHost: TEdit;
    edInterval: TEdit;
    edTimeout: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    procedure cbOnPingFailClick(Sender: TObject);
    procedure cbOnPingRestoreClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FNode: TNodeConfig;
    FExistingHosts: array of string;
    FExcludeIndex: Integer;
    function ValidateInput: Boolean;
    procedure ValidateClose(Sender: TObject; var CanClose: Boolean);
  public
    procedure SetData(const Node: TNodeConfig; const ExistingHosts: array of string; ExcludeIndex: Integer);
    function GetData: TNodeConfig;
    procedure ApplyLocalization;
  end;

implementation

{$R *.lfm}

procedure TNodeEditForm.FormCreate(Sender: TObject);
begin
  ApplyLocalization;
end;

procedure TNodeEditForm.cbOnPingFailClick(Sender: TObject);
begin
  eRunOnPingFail.Enabled := cbOnPingFail.Checked;
end;

procedure TNodeEditForm.cbOnPingRestoreClick(Sender: TObject);
begin
  eRunOnPingResore.Enabled := cbOnPingRestore.Checked;
end;

procedure TNodeEditForm.ApplyLocalization;
begin
  if Assigned(NodeNotifications) then NodeNotifications.Caption := _('node_notifications');
  if Assigned(NodeFailureLabel) then NodeFailureLabel.Caption := _('node_failure_threshold');
  lblName.Caption := _('label_name');
  lblHost.Caption := _('label_host');
  lblInterval.Caption := _('label_interval');
  lblTimeout.Caption := _('label_timeout');
  btnOK.Caption := _('btn_apply');
  btnCancel.Caption := _('btn_cancel');
  if Assigned(cbOnPingFail) then
    cbOnPingFail.Caption := _('label_run_on_ping_fail');
  if Assigned(cbOnPingRestore) then
    cbOnPingRestore.Caption := _('label_run_on_ping_restore');
end;

procedure TNodeEditForm.SetData(const Node: TNodeConfig; const ExistingHosts: array of string; ExcludeIndex: Integer);
var
  I: Integer;
begin
  OnCloseQuery := @ValidateClose;
  FNode := Node;
  NodeNotifications.Checked := not Node.NotificationsMuted;
  if Node.FailThreshold <= 0 then NodeFailureThreshold.Text := '3'
  else NodeFailureThreshold.Text := IntToStr(Node.FailThreshold);
  FExcludeIndex := ExcludeIndex;
  SetLength(FExistingHosts, Length(ExistingHosts));
  for I := 0 to High(ExistingHosts) do
    FExistingHosts[I] := ExistingHosts[I];

  edName.Text := Node.Name;
  edHost.Text := Node.Host;
  edInterval.Text := IntToStr(Node.IntervalMs);
  edTimeout.Text := IntToStr(Node.TimeoutMs);
  cbOnPingFail.Checked := Node.DownCommand <> '';
  cbOnPingRestore.Checked := Node.UpCommand <> '';
  eRunOnPingFail.Text := Node.DownCommand;
  eRunOnPingResore.Text := Node.UpCommand;
  cbOnPingFailClick(Self);
  cbOnPingRestoreClick(Self);
end;

function TNodeEditForm.GetData: TNodeConfig;
begin
  Result := FNode;
  Result.NotificationsMuted := not NodeNotifications.Checked;
  Result.FailThreshold := StrToIntDef(NodeFailureThreshold.Text, 3);
  Result.Name := Trim(edName.Text);
  Result.Host := Trim(edHost.Text);
  Result.IntervalMs := StrToIntDef(edInterval.Text, 5000);
  Result.TimeoutMs := StrToIntDef(edTimeout.Text, 2000);
  if cbOnPingFail.Checked then Result.DownCommand := Trim(eRunOnPingFail.Text)
  else Result.DownCommand := '';
  if cbOnPingRestore.Checked then Result.UpCommand := Trim(eRunOnPingResore.Text)
  else Result.UpCommand := '';
end;

function TNodeEditForm.ValidateInput: Boolean;
var
  Host: string;
  I, Value: Integer;
begin
  Result := False;
  if not TryStrToInt(NodeFailureThreshold.Text, Value) or (Value < 1) or (Value > 20) then
  begin
    ShowMessage(_('error_failure_range'));
    Exit;
  end;

  if not TryStrToInt(edInterval.Text, Value) or (Value <= 0) then
  begin
    ShowMessage(_('error_interval_positive'));
    Exit;
  end;
  if not TryStrToInt(edTimeout.Text, Value) or (Value <= 0) or (Value > 60000) then
  begin
    ShowMessage(_('error_timeout_range'));
    Exit;
  end;

  Host := Trim(edHost.Text);
  if Host = '' then
  begin
    ShowMessage(_('error_host_empty'));
    Exit;
  end;

  // Check for duplicate host
  for I := 0 to High(FExistingHosts) do
  begin
    if (I <> FExcludeIndex) and (CompareText(FExistingHosts[I], Host) = 0) then
    begin
      ShowMessage(_('error_host_exists'));
      Exit;
    end;
  end;

  Result := True;
end;

procedure TNodeEditForm.ValidateClose(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOK then CanClose := ValidateInput;
end;

end.
