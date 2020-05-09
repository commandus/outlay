unit org;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  dm, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.Menus, GridsEh, DBAxisGridsEh,
  DBGridEh,
  formoutlay, orgtype, orgposition, DBGridEhGrouping, ToolCtrlsEh,
  DBGridEhToolCtrls, DynVarsEh, EhLibVCL, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TFormOrgs = class(TFormOutlay)
    DBGridProjectList: TDBGridEh;
    DBNavigatorOrg: TDBNavigator;
    MainMenuOrg: TMainMenu;
    MenuOrgTypes: TMenuItem;
    MenuOrgPosition: TMenuItem;
    MenuPerson: TMenuItem;
    dsOrg: TDataSource;
    FDTOrgType: TFDTable;
    FDTOrg: TFDTable;
    procedure MenuOrgTypesClick(Sender: TObject);
    procedure ShowOrgTypes();
    procedure MenuOrgPositionClick(Sender: TObject);
    procedure ShowOrgPositions();
    procedure ShowPersons();
  private
  { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOrgs: TFormOrgs;

implementation

{$R *.dfm}

procedure TFormOrgs.MenuOrgPositionClick(Sender: TObject);
begin
  ShowOrgPositions();
end;

procedure TFormOrgs.MenuOrgTypesClick(Sender: TObject);
begin
  ShowOrgTypes();
end;

procedure TFormOrgs.ShowOrgTypes();
begin
  FormOrgTypes.Show();
end;

procedure TFormOrgs.ShowOrgPositions();
begin
  FormOrgPositions.Show();
end;

procedure TFormOrgs.ShowPersons();
begin
  FormOrgPositions.Show();
end;

end.
