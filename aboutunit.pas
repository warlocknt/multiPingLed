unit AboutUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Buttons, JvJanLED, Windows, ShellApi, LangManager;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    LUpdateStatus: TLabel;
    LabelTg: TLabel;
    LabelTgLNK: TLabel;
    LabelGit: TLabel;
    LabelGitLNK: TLabel;
    StatusBarAbout: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure LabelGitMouseLeave(Sender: TObject);
    procedure LabelGitMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LabelTgLNKClick(Sender: TObject);
    procedure LabelMouseEnter(Sender: TObject);
    procedure LabelMouseLeave(Sender: TObject);
    procedure LabelGitLNKClick(Sender: TObject);
    procedure LabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure LabelTgMouseLeave(Sender: TObject);
    procedure LabelTgMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OpenURL(const URL: string);
  private

  public
    AppVersion:String;
    procedure ApplyLocalization;

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.ApplyLocalization;
begin
  Caption := 'Multi Ping LED ver. ' + AppVersion;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  //ApplyLocalization;
end;

procedure TAboutForm.LabelGitMouseLeave(Sender: TObject);
begin
  LabelGitLNK.Font.Color:=clDefault;
end;

procedure TAboutForm.LabelGitMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  LabelGitLNK.Font.Color := clHighlight;
end;

procedure TAboutForm.LabelTgLNKClick(Sender: TObject);
begin
  OpenURL('https://t.me/warlocknt');
end;

procedure TAboutForm.LabelMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Color:= clHighlight;
end;

procedure TAboutForm.LabelMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
    (Sender as TLabel).Font.Color:= clHighlight;
end;

procedure TAboutForm.LabelTgMouseLeave(Sender: TObject);
begin
  LabelTgLNK.Font.Color:=clDefault;
end;

procedure TAboutForm.LabelTgMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
    LabelTgLNK.Font.Color := clHighlight;
end;

procedure TAboutForm.LabelMouseLeave(Sender: TObject);
begin
    (Sender as TLabel).Font.Color:= clDefault;
end;

procedure TAboutForm.LabelGitLNKClick(Sender: TObject);
begin
  OpenURL('https://github.com/warlocknt/multiPingLed');
end;



procedure TAboutForm.OpenURL(const URL: string);
begin
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

end.

