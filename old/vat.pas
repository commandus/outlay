unit vat;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  formoutlay, dm, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.Menus, DBGridEhGrouping,
  ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh, EhLibVCL, GridsEh, DBAxisGridsEh,
  DBGridEh;

type
  TFormVat = class(TFormOutlay)
    DBNavigatorVAT: TDBNavigator;
    DBGVAT: TDBGridEh;
    procedure MenuOrgTypesClick(Sender: TObject);
    procedure ShowOrgTypes();
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormVat: TFormVat;

implementation

{$R *.dfm}

procedure TFormVat.MenuOrgTypesClick(Sender: TObject);
begin
  // ShowOrgTypes();
end;

procedure TFormVat.ShowOrgTypes();
begin
  // Show;
end;

end.
