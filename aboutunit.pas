unit AboutUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Buttons, ExtCtrls, JvJanLED, Windows, ShellApi, LangManager, AppConstants;

type
  TUpdateStatus = (
    updAvailable,
    updCheckFailed,
    updNone
    );

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Image1: TImage;
    LUpdateStatus: TLabel;
    LabelTg: TLabel;
    LabelTgLNK: TLabel;
    LabelGit: TLabel;
    LabelGitLNK: TLabel;
    StatusBarAbout: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure LabelGitMouseLeave(Sender: TObject);
    procedure LabelGitMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure LabelTgLNKClick(Sender: TObject);
    procedure LabelMouseEnter(Sender: TObject);
    procedure LabelMouseLeave(Sender: TObject);
    procedure LabelGitLNKClick(Sender: TObject);
    procedure LabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure LabelTgMouseLeave(Sender: TObject);
    procedure LabelTgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure LUpdateStatusClick(Sender: TObject);
    procedure LUpdateStatusMouseLeave(Sender: TObject);
    procedure LUpdateStatusMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure OpenURL(const URL: string);
  private

  public
    AppVersion: string;
    procedure ApplyLocalization;
    procedure SetUpdateStatus(UpdateStatus: TUpdateStatus);

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.ApplyLocalization;
begin
  Caption := 'multiPingLed v.' + AppVersion;
end;

procedure TAboutForm.SetUpdateStatus(UpdateStatus: TUpdateStatus);
begin
  case UpdateStatus of
    updAvailable:
    begin
      LUpdateStatus.OnMouseLeave := @LUpdateStatusMouseLeave;
      LUpdateStatus.OnMouseMove := @LUpdateStatusMouseMove;
      LUpdateStatus.OnClick := @LUpdateStatusClick;
      LUpdateStatus.Cursor := crHandPoint;
    end;

    updCheckFailed, updNone:
    begin
      LUpdateStatus.OnMouseLeave := nil;
      LUpdateStatus.OnMouseMove := nil;
      LUpdateStatus.OnClick := nil;
      LUpdateStatus.Font.Color := clDefault;
      LUpdateStatus.Cursor := crDefault;
    end;
  end;

end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  //ApplyLocalization;
end;

procedure TAboutForm.LabelGitMouseLeave(Sender: TObject);
begin
  LabelGitLNK.Font.Color := clDefault;
end;

procedure TAboutForm.LabelGitMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  LabelGitLNK.Font.Color := clHighlight;
end;

procedure TAboutForm.LabelTgLNKClick(Sender: TObject);
begin
  OpenURL('https://t.me/'+REPO_OWNER);
end;

procedure TAboutForm.LabelMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Color := clHighlight;
end;

procedure TAboutForm.LabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  (Sender as TLabel).Font.Color := clHighlight;
end;

procedure TAboutForm.LabelTgMouseLeave(Sender: TObject);
begin
  LabelTgLNK.Font.Color := clDefault;
end;

procedure TAboutForm.LabelTgMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  LabelTgLNK.Font.Color := clHighlight;
end;

procedure TAboutForm.LUpdateStatusClick(Sender: TObject);
begin
  OpenURL(URL_RELEASES_LATEST);
end;

procedure TAboutForm.LUpdateStatusMouseLeave(Sender: TObject);
begin
  LUpdateStatus.Font.Color := clDefault;
end;

procedure TAboutForm.LUpdateStatusMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  LUpdateStatus.Font.Color := clHighlight;
end;

procedure TAboutForm.LabelMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Color := clDefault;
end;

procedure TAboutForm.LabelGitLNKClick(Sender: TObject);
begin
  OpenURL(URL_REPO);
end;

procedure TAboutForm.OpenURL(const URL: string);
begin
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

end.
