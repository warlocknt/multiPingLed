unit NodeEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ConfigManager, LangManager;

type
  TNodeEditForm = class(TForm)
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
    procedure FormCreate(Sender: TObject);
  private
    FNode: TNodeConfig;
    FExistingHosts: array of string;
    FExcludeIndex: Integer;
    function ValidateInput: Boolean;
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

procedure TNodeEditForm.ApplyLocalization;
begin
  lblName.Caption := _('label_name');
  lblHost.Caption := _('label_host');
  lblInterval.Caption := _('label_interval');
  lblTimeout.Caption := _('label_timeout');
  btnOK.Caption := _('btn_apply');
  btnCancel.Caption := _('btn_cancel');
end;

procedure TNodeEditForm.SetData(const Node: TNodeConfig; const ExistingHosts: array of string; ExcludeIndex: Integer);
var
  I: Integer;
begin
  FNode := Node;
  FExcludeIndex := ExcludeIndex;
  SetLength(FExistingHosts, Length(ExistingHosts));
  for I := 0 to High(ExistingHosts) do
    FExistingHosts[I] := ExistingHosts[I];

  edName.Text := Node.Name;
  edHost.Text := Node.Host;
  edInterval.Text := IntToStr(Node.IntervalMs);
  edTimeout.Text := IntToStr(Node.TimeoutMs);
end;

function TNodeEditForm.GetData: TNodeConfig;
begin
  Result := FNode;
  Result.Name := Trim(edName.Text);
  Result.Host := Trim(edHost.Text);
  Result.IntervalMs := StrToIntDef(edInterval.Text, 5000);
  Result.TimeoutMs := StrToIntDef(edTimeout.Text, 2000);
end;

function TNodeEditForm.ValidateInput: Boolean;
var
  Host: string;
  I: Integer;
begin
  Result := False;

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

end.
