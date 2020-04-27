unit rpt;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  formoutlay;

type
  TFormReports = class(TFormOutlay)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormReports: TFormReports;

implementation

{$R *.dfm}

end.
