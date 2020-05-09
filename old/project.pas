unit project;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  dm, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.Menus, GridsEh, DBAxisGridsEh,
  DBGridEh,
  formoutlay, org, DBGridEhGrouping, ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh,
  EhLibVCL;

type
  TFormProjects = class(TFormOutlay)
    DBGridProjectList: TDBGridEh;
    DBNavigatorProjects: TDBNavigator;
    MainMenuProjects: TMainMenu;
    MenuOrg: TMenuItem;
    procedure MenuOrgClick(Sender: TObject);
    procedure ShowOrgs();
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormProjects: TFormProjects;

implementation

{$R *.dfm}

procedure TFormProjects.MenuOrgClick(Sender: TObject);
begin
   ShowOrgs();
end;

procedure TFormProjects.ShowOrgs();
begin
   FormOrgs.Show;
end;

end.
