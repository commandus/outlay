unit person;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  formoutlay, dm, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.Menus, DBAxisGridsEh,
  DBGridEh, DBGridEhGrouping, ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh,
  EhLibVCL, GridsEh;

type
  TFormPersons = class(TFormOutlay)
    DBGridPersonList: TDBGridEh;
    DBNavigatorOrg: TDBNavigator;
    MainMenuOrg: TMainMenu;
    MenuOrgTypes: TMenuItem;
    procedure MenuOrgTypesClick(Sender: TObject);
    procedure ShowOrgTypes();
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormPersons: TFormPersons;

implementation

{$R *.dfm}

procedure TFormPersons.MenuOrgTypesClick(Sender: TObject);
begin
  // ShowOrgTypes();
end;

procedure TFormPersons.ShowOrgTypes();
begin
  // Show;
end;

end.
