unit about;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, ShellApi;

type
  TFormAbout = class(TForm)
    ImageAbout: TImage;
    Button1: TButton;
    LabelVersion: TLabel;
    LabelURLUpdates: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure LabelURLUpdatesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.dfm}

const URL_UPDATES = 'https://outlay.commandus.com/download/';

procedure GetBuildInfo(var V1, V2, V3, V4: Word);
var
  VerInfoSize, VerValueSize, Dummy : DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  With VerValue^ do begin
    V1:= dwFileVersionMS shr 16;
    V2:= dwFileVersionMS and $FFFF;
    V3:= dwFileVersionLS shr 16;
    V4:= dwFileVersionLS and $FFFF;
  end;
  FreeMem(VerInfo, VerInfoSize);
end;

procedure TFormAbout.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
var
  V1, V2, V3, V4: Word;
begin
  //
  GetBuildInfo(V1, V2, V3, V4);
  LabelVersion.Caption:= 'Версия '
    + IntToStr(v1) + '.'
    + IntToStr(v2) + '.'
    + IntToStr(v3) + '.'
    + IntToStr(v4);
end;

procedure TFormAbout.LabelURLUpdatesClick(Sender: TObject);
begin
   ShellExecuteA(Application.Handle, PAnsiChar('open'),
     PAnsiChar(URL_UPDATES),
     Nil, Nil, SW_SHOW);
end;

end.
