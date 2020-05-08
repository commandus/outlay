unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB,
  Vcl.Menus, DBAxisGridsEh,
  DBGridEh, DBGridEhGrouping, ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh, EhLibVCL,
  GridsEh, Vcl.StdCtrls, VCL.Themes, EhLibIBX, DbUtilsEh, Vcl.ComCtrls,
  IBX.IBCustomDataSet, ShellApi,
  dm, rpt, dict, settings, myorg, about, Vcl.PlatformDefaultStyleActnCtrls,
  System.Actions, Vcl.ActnList, Vcl.ActnMan, Vcl.ToolWin, Vcl.ActnCtrls,
  System.ImageList, Vcl.ImgList, Vcl.ValEdit, Vcl.Grids, Vcl.DBCtrls,
  Vcl.ExtCtrls;

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
    PanelRight: TPanel;
    DBGridEhRequest: TDBGridEh;
    Splitter2: TSplitter;
    DBGridEhSpec: TDBGridEh;
    MenuSep2: TMenuItem;
    ManuHelp: TMenuItem;
    MenuHelpAbout: TMenuItem;
    MenuHelpUserGuide: TMenuItem;
    PanelRequest: TPanel;
    PanelRequestProp: TPanel;
    Splitter3: TSplitter;
    PageControlRightTop: TPageControl;
    TabSheet5: TTabSheet;
    PanelCurrencyControl: TPanel;
    DBGridEhRequestCurrency: TDBGridEh;
    TabSheet6: TTabSheet;
    ActionToolBar1: TActionToolBar;
    ActionManagerMain: TActionManager;
    ActionSetRequestDiscount: TAction;
    actSave: TAction;
    actRollback: TAction;
    ImageList1: TImageList;
    VLERequestSums: TValueListEditor;
    DBGridEhPayment: TDBGridEh;
    procedure ShowOptions();
    procedure ShowMyOrg();
    procedure ShowDict();
    procedure MenuOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuOrgClick(Sender: TObject);
    procedure MenuDictClick(Sender: TObject);
    procedure RefreshData();
    procedure MenuReportClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure MenuHelpUserGuideClick(Sender: TObject);
    procedure ActionSetRequestDiscountExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actRollbackExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    procedure Rollback3();
    procedure Commit3();
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

const URL_USER_GUIDE = 'https://docs.google.com/document/d/1ilBTF2z5BMg_kmHjyQnYWb5hFhUK5fvylxXYMa8Zv7I/edit?usp=sharing';

procedure TFormMain.FormActivate(Sender: TObject);
begin
  dm.dmOutlay.IBDatabase.Connected:= false;
  try
    if not dm.dmOutlay.IBDatabase.Connected then begin
      dm.dmOutlay.IBDatabase.Connected:= true;
    end;
  except on E: Exception do
    ShowOptions();
  end;
  if dm.dmOutlay.IBDatabase.Connected then begin
    dm.dmOutlay.ActivateDbControls(dm.dmOutlay);
    dm.dmOutlay.ActivateDbControls(Self);
    DbUtilsEh.SQLFilterMarker:= '/*Filter*/';
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  dm.dmOutlay.requestSumList:= VLERequestSums.Strings;
end;

procedure TFormMain.MenuDictClick(Sender: TObject);
begin
  ShowDict();
end;

procedure TFormMain.MenuExitClick(Sender: TObject);
begin
  Close();
end;

procedure TFormMain.MenuHelpAboutClick(Sender: TObject);
begin
  FormAbout:= TFormAbout.Create(Self);
  FormAbout.ShowModal();
  FreeAndNil(FormAbout);
end;

procedure TFormMain.MenuHelpUserGuideClick(Sender: TObject);
begin
   ShellExecuteA(Application.Handle, PAnsiChar('open'),
     PAnsiChar(URL_USER_GUIDE),
     Nil, Nil, SW_SHOW);
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

procedure TFormMain.ShowOptions();
begin
  FormMain.Hide();
  FormSettings:= TFormSettings.Create(Self);
  FormSettings.ShowModal();
  FreeAndNil(FormSettings);
  dm.dmOutlay.connect(dm.dmOutlay.IBDatabase);
  if dm.dmOutlay.IBDatabase.Connected then begin
    dm.dmOutlay.ActivateDbControls(Self);
    DbUtilsEh.SQLFilterMarker:= '/*Filter*/';
  end;
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
  p: boolean;
begin
  try
    for c:= 0 to ComponentCount - 1 do begin
      if (Components[c] is TDBGridEh) then with TDBGridEh(Components[c]) do begin
        p:= DataSource = dmOutlay.dslProject;
        if DataSource.DataSet.Active then begin
          if p then begin
            SaveVertPos('NAME');
          end;
          DataSource.DataSet.Close;
        end;
        DataSource.DataSet.Open();
        if p then
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
  // dm.dmOutlay.IBDatabase.DefaultTransaction.Rollback;
  // dm.dmOutlay.IBDatabase.DefaultTransaction.StartTransaction;
end;

procedure TFormMain.Commit3();
begin
  if dm.dmOutlay.IBLProject.Modified then
    dm.dmOutlay.IBLProject.Post;
  if dm.dmOutlay.IBLRequest.Modified then
    dm.dmOutlay.IBLRequest.Post;
  if dm.dmOutlay.IBLSpecification.Modified then
    dm.dmOutlay.IBLSpecification.Post;
  // dm.dmOutlay.IBDatabase.DefaultTransaction.Commit;
  // dm.dmOutlay.IBDatabase.DefaultTransaction.StartTransaction;
end;

procedure TFormMain.ActionSetRequestDiscountExecute(Sender: TObject);
begin
  dm.dmOutlay.setRequestDiscount2All(dm.dmOutlay.IBLRequest.FieldByName('DISCOUNT').AsFloat);
end;

procedure TFormMain.actRollbackExecute(Sender: TObject);
begin
  Rollback3;
end;

procedure TFormMain.actSaveExecute(Sender: TObject);
begin
  Commit3;
end;

end.
