unit dict;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB,
  Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.Menus, DBAxisGridsEh,
  DBGridEh, DBGridEhGrouping, ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh, EhLibVCL,
  GridsEh, Vcl.StdCtrls, VCL.Themes, EhLibIBX, DbUtilsEh, Vcl.ComCtrls,
  dm, rpt, settings, myorg;

type
  TFormDict = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    DBGridProjectList: TDBGridEh;
    DBNavigatorRequest: TDBNavigator;
    Организации: TTabSheet;
    DBGridEh1: TDBGridEh;
    DBNavigatorOrg: TDBNavigator;
    НДС: TTabSheet;
    DBGVAT: TDBGridEh;
    DBNavigatorVAT: TDBNavigator;
    DBGridEh2: TDBGridEh;
    DBNavigatorProjects: TDBNavigator;
    DBGridPersonList: TDBGridEh;
    DBNavigator1: TDBNavigator;
    DBGridEh3: TDBGridEh;
    DBNavigator2: TDBNavigator;
    DBGridEh4: TDBGridEh;
    DBNavigator3: TDBNavigator;
    TabSheet7: TTabSheet;
    DBNavigator4: TDBNavigator;
    DBGridEHMeasureUnit: TDBGridEh;
    TabSheet8: TTabSheet;
    DBNavigator5: TDBNavigator;
    DBGridEhSaleType: TDBGridEh;
    TabSheet9: TTabSheet;
    DBGridEh6: TDBGridEh;
    DBNavigator6: TDBNavigator;
    TabSheet10: TTabSheet;
    DBGridEh7: TDBGridEh;
    DBNavigator7: TDBNavigator;
    DBGridEh8: TDBGridEh;
    DBNavigator8: TDBNavigator;
    TabSheet11: TTabSheet;
    DBGridEh5: TDBGridEh;
    DBNavigator9: TDBNavigator;
    TabSheet12: TTabSheet;
    DBGridEh9: TDBGridEh;
    DBNavigator10: TDBNavigator;
    TabSheet13: TTabSheet;
    DBGridEh10: TDBGridEh;
    DBNavigator11: TDBNavigator;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
  end;

var
  FormDict: TFormDict;

implementation

{$R *.dfm}

uses main;

procedure TFormDict.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // refresh
  dmOutlay.IBTransaction.Commit();
end;

procedure TFormDict.FormCreate(Sender: TObject);
begin
  dm.dmOutlay.ActivateDbControls(Self);
end;

end.
