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
    MenuSep1: TMenuItem;
    MenuExit: TMenuItem;
    MenuReport: TMenuItem;
    MenuOrg: TMenuItem;
    MenuDict: TMenuItem;
    MenuSep2: TMenuItem;
    ManuHelp: TMenuItem;
    MenuHelpAbout: TMenuItem;
    MenuHelpUserGuide: TMenuItem;
    ActionManagerMain: TActionManager;
    ActionSetRequestDiscount: TAction;
    actSave: TAction;
    actRollback: TAction;
    ImageList1: TImageList;
    Splitter1: TSplitter;
    DBGridEh1: TDBGridEh;
    PanelRight: TPanel;
    Splitter2: TSplitter;
    PanelRequest: TPanel;
    Splitter3: TSplitter;
    DBGridEhRequest: TDBGridEh;
    PanelRequestProp: TPanel;
    PageControlRightTop: TPageControl;
    TabSheet5: TTabSheet;
    PanelCurrencyControl: TPanel;
    DBGridEhRequestCurrency: TDBGridEh;
    ActionToolBar1: TActionToolBar;
    DBGridEhPayment: TDBGridEh;
    TabSheet6: TTabSheet;
    VLERequestSums: TValueListEditor;
    DBGridEhSpec: TDBGridEh;
    procedure ShowOptions();
    procedure ShowMyOrg();
    procedure ShowDict();
    procedure MenuOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuOrgClick(Sender: TObject);
    procedure MenuDictClick(Sender: TObject);
    procedure MenuReportClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure MenuHelpUserGuideClick(Sender: TObject);
    procedure ActionSetRequestDiscountExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actRollbackExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    dictDatasets: array of TIBDataset;
    mainDatasets: array of TIBDataset;
    procedure RefreshDicts();
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
  dictDatasets:= [dmOutlay.IBOrg,
    dmOutlay.IBSaleType,
    dmOutlay.IBStage,
    dmOutlay.IBVAT,
    dmOutlay.IBCurrency,
    dmOutlay.IBPaymentState,
    dmOutlay.IBPaymentType,
    dmOutlay.IBPart
  ];
  mainDatasets:= [
    dm.dmOutlay.IBLProject,
    dm.dmOutlay.IBLRequest,
    dm.dmOutlay.IBLSpecification,
    dm.dmOutlay.IBLPayment,
    dm.dmOutlay.IBLRequestCurrencyRate
  ];
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
  FormMain.Refresh();
  FormMain.Show();
  FreeAndNil(FormMyOrg);
end;

procedure TFormMain.ShowDict();
begin
  FormMain.Hide();
  FormDict:= TFormDict.Create(Self);
  FormDict.ShowModal();
  FormMain.Refresh();
  FormMain.Show();
  FreeAndNil(FormDict);
end;

procedure TFormMain.RefreshDicts();
var
  ds: TIBDataSet;
  bookmark: TBookmark;
begin
  for ds in dictDatasets do begin
    bookmark:= ds.GetBookmark;
    ds.Close;
    ds.Open;
    ds.GotoBookmark(bookmark);
  end;
end;

procedure TFormMain.Rollback3();
var
  ds: TIBDataSet;
  bookmark: TBookmark;
begin
  RefreshDicts;
  for ds in mainDatasets do begin
  if ds.Modified then
    ds.Cancel;
  end;
  bookmark:= ds.GetBookmark;
  ds.Close;
  ds.Open;
  ds.GotoBookmark(bookmark);
end;

procedure TFormMain.Commit3();
var
  ds: TIBDataSet;
  bookmark: TBookmark;
begin
  RefreshDicts;
  for ds in mainDatasets do begin
  if ds.Modified then
    ds.Post;
  end;
  bookmark:= ds.GetBookmark;
  ds.Close;
  ds.Open;
  ds.GotoBookmark(bookmark);
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
