unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB,
  Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.Menus, DBAxisGridsEh,
  DBGridEh, DBGridEhGrouping, ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh, EhLibVCL,
  GridsEh, Vcl.StdCtrls, VCL.Themes, EhLibIBX, DbUtilsEh, Vcl.ComCtrls,
  dm, rpt, dict, settings, myorg;

type
  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    MenuOptions: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet4: TTabSheet;
    DBGridProjectList: TDBGridEh;
    DBNavigatorRequest: TDBNavigator;
    DBGridEh2: TDBGridEh;
    DBNavigatorProjects: TDBNavigator;
    DBGridEh8: TDBGridEh;
    DBNavigator8: TDBNavigator;
    MenuSep1: TMenuItem;
    MenuExit: TMenuItem;
    MenuReport: TMenuItem;
    MenuOrg: TMenuItem;
    MenuDict: TMenuItem;
    TabSheet3: TTabSheet;
    DBGridEh1: TDBGridEh;
    Splitter1: TSplitter;
    Panel1: TPanel;
    DBGridEh3: TDBGridEh;
    Splitter2: TSplitter;
    DBGridEh4: TDBGridEh;
    MenuSave: TMenuItem;
    MenuCancel: TMenuItem;
    procedure ShowOptions();
    procedure ShowMyOrg();
    procedure ShowDict();
    procedure MenuOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuOrgClick(Sender: TObject);
    procedure MenuDictClick(Sender: TObject);
    procedure RefreshData();
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuCancelClick(Sender: TObject);
    procedure MenuReportClick(Sender: TObject);
  private
    procedure Rollback3();
    procedure Commit3();
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  if not dm.dmOutlay.IBDatabase.Connected then begin
    ShowOptions();
  end;
  DbUtilsEh.SQLFilterMarker:= '/*Filter*/';
end;

procedure TFormMain.MenuCancelClick(Sender: TObject);
begin
  Rollback3();
end;

procedure TFormMain.MenuDictClick(Sender: TObject);
begin
  ShowDict();
end;

procedure TFormMain.MenuExitClick(Sender: TObject);
begin
  Close();
end;

procedure TFormMain.MenuOptionsClick(Sender: TObject);
begin
   ShowOptions();
end;

procedure TFormMain.MenuOrgClick(Sender: TObject);
begin
  ShowMyOrg();
end;

procedure TFormMain.MenuReportClick(Sender: TObject);
begin

  if FormReports = Nil then
    FormReports:= TFormReports.Create(Self);
  FormReports.Show();
end;

procedure TFormMain.MenuSaveClick(Sender: TObject);
begin
  Commit3();
end;

procedure TFormMain.ShowOptions();
begin
  FormMain.Hide();
  FormSettings:= TFormSettings.Create(Self);
  FormSettings.ShowModal();
  FreeAndNil(FormSettings);
  FormMain.Show();
end;

procedure TFormMain.ShowMyOrg();
begin
  FormMain.Hide();
  FormMyOrg:= TFormMyOrg.Create(Self);
  FormMyOrg.ShowModal();
  FormMain.RefreshData();
  FormMain.Show();
  FreeAndNil(FormMyOrg);
end;

procedure TFormMain.RefreshData();
var
  c: Integer;
begin
  try
    for c:= 0 to ComponentCount - 1 do begin
      if (Components[c] is TDBGridEh) then with TDBGridEh(Components[c]) do begin
        if DataSource = dmOutlay.dslProject then
          SaveVertPos('NAME');
        DataSource.DataSet.Close;
        DataSource.DataSet.Open();
        if DataSource = dmOutlay.dslProject then
          RestoreVertPos('NAME');
        // DataSource.DataSet.Refresh();
        // Refresh();
      end;
    end;
  finally
  end;
end;

procedure TFormMain.ShowDict();
begin
  FormMain.Hide();
  FormDict:= TFormDict.Create(Self);
  FormDict.ShowModal();
  FormMain.RefreshData();
  FormMain.Show();
  FreeAndNil(FormDict);
end;

procedure TFormMain.Rollback3();
begin
  if dm.dmOutlay.IBLProject.Modified then
    dm.dmOutlay.IBLProject.Cancel;
  if dm.dmOutlay.IBLRequest.Modified then
    dm.dmOutlay.IBLRequest.Cancel;
  if dm.dmOutlay.IBLSpecification.Modified then
    dm.dmOutlay.IBLSpecification.Cancel;
end;

procedure TFormMain.Commit3();
begin
  if dm.dmOutlay.IBLProject.Modified then
    dm.dmOutlay.IBLProject.Post;
  if dm.dmOutlay.IBLRequest.Modified then
    dm.dmOutlay.IBLRequest.Post;
  if dm.dmOutlay.IBLSpecification.Modified then
    dm.dmOutlay.IBLSpecification.Post;
end;

end.
