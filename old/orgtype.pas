unit orgtype;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  formoutlay, dm, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.Menus, DBAxisGridsEh,
  DBGridEh, DBGridEhGrouping, ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh,
  EhLibVCL, GridsEh;

type
  TFormOrgTypes = class(TFormOutlay)
    DBGridProjectList: TDBGridEh;
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
  FormOrgTypes: TFormOrgTypes;

implementation

{$R *.dfm}

procedure TFormOrgTypes.MenuOrgTypesClick(Sender: TObject);
begin
  // ShowOrgTypes();
end;

procedure TFormOrgTypes.ShowOrgTypes();
begin
  // Show;
end;

end.
