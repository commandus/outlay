unit settings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, VCL.Themes, dm;

type
  TFormSettings = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cbStyle: TComboBox;
    EDbName: TEdit;
    EHostAddress: TEdit;
    EUser: TEdit;
    EPassword: TEdit;
    BOK: TButton;
    BCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
  private
    procedure load();
    procedure save();
  public
    { Public declarations }
  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.dfm}

procedure TFormSettings.BCancelClick(Sender: TObject);
begin
  load();
  Close();
end;

procedure TFormSettings.BOKClick(Sender: TObject);
begin
  save();
  if not dm.dmOutlay.connect(dm.dmOutlay.IBDatabase) then begin
    ShowMessage('Нет соединения с СУБД');
    Exit;
  end;
  Close();
end;

procedure TFormSettings.FormCreate(Sender: TObject);
begin
  load();
end;

procedure TFormSettings.load();
begin
  dm.dmOutlay.loadSettings();
  cbStyle.Items:= dm.dmOutlay.styles;
  cbStyle.ItemIndex:= dm.dmOutlay.getStyleIndex();
  EDbName.Text:= dm.dmOutlay.dbName;
  EHostAddress.Text:= dm.dmOutlay.hostAddress;
  EUser.Text:= dm.dmOutlay.userName;
  EPassword.Text:= dm.dmOutlay.userPassword;
end;

procedure TFormSettings.save();
begin
  dm.dmOutlay.style:= cbStyle.Items[cbStyle.ItemIndex];
  dm.dmOutlay.setStyle(dm.dmOutlay.style);
  dm.dmOutlay.dbName:= EDbName.Text;
  dm.dmOutlay.hostAddress:= EHostAddress.Text;
  dm.dmOutlay.userName:= EUser.Text;
  dm.dmOutlay.userPassword:= EPassword.Text;
  dm.dmOutlay.saveSettings();
end;

end.
