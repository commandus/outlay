unit rpt;
(**
  * db_name=localhost:c:\empty.fdb;
  * password=masterkey;
  * user_name=SYSDBA;
  * lc_ctype=win1251;
  * sql_role_name=ADMIN;
  * sql_dialect=3;
  * clientlib="c:\program files\firebird\bin\fbclient.dll"
  *)

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  formoutlay, DBGridEhGrouping, ToolCtrlsEh, DBGridEhToolCtrls, DynVarsEh,
  Vcl.StdCtrls, Vcl.ExtCtrls, EhLibVCL, GridsEh, DBAxisGridsEh, DBGridEh,
  Vcl.ComCtrls,
  dm, Data.DB, IBX.IBCustomDataSet, IBX.IBQuery;

type
  TFormReports = class(TFormOutlay)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    DBGridEh1: TDBGridEh;
    Panel1: TPanel;
    Splitter2: TSplitter;
    DBGridEh3: TDBGridEh;
    Panel2: TPanel;
    BReportExec: TButton;
    Splitter1: TSplitter;
    TabSheet2: TTabSheet;
    MemoErrors: TMemo;
    dsReports: TDataSource;
    IBQueryReports: TIBQuery;
    DBGridEh4: TDBGridEh;
    IBQueryReportsNAME: TIBStringField;
    IBQueryReportsSOURCE: TBlobField;
    procedure BReportExecClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure doReport();
    function reportPDF(const reportName: String): AnsiString;
  public
    { Public declarations }
    reportFileName: AnsiString;
  end;

var
  FormReports: TFormReports;

implementation

{$R *.dfm}

uses
  ShellApi;

type
  TConnectErrorCallbackFunc = function (AErrorMessage: PAnsiChar) : integer;  stdcall;
  TScriptErrorCallbackFunc = function (AStmtText, AErrMessage: PAnsiChar): integer; stdcall;
  TScriptBeforeExecStatementFunc = function (AStmtText, AText: PAnsiChar): integer; stdcall;
  TScriptAfterExecStatementFunc = function (AStmtText: PAnsiChar; Success: integer): integer; stdcall;
  TScriptIBEBlockProgressFunc = function (AProgressMessage: PAnsiChar): integer; stdcall;

  TExecuteScriptProc = procedure (AScriptFile: PAnsiChar;
    AErrorCallbackFunc : TScriptErrorCallbackFunc;
    ABeforeCallbackFunc : TScriptBeforeExecStatementFunc;
    AAfterCallbackFunc : TScriptAfterExecStatementFunc); stdcall;

  TExecuteScriptProc2 = procedure (AScriptFile: PAnsiChar;
    AErrorCallbackFunc : TScriptErrorCallbackFunc;
    ABeforeCallbackFunc : TScriptBeforeExecStatementFunc;
    AAfterCallbackFunc : TScriptAfterExecStatementFunc;
    AIBEBlockProgressFunc : TScriptIBEBlockProgressFunc); stdcall;

  TConnectDBProc = function (AConnectParams: PAnsiChar;
    AConnectErrorCallbacFunc : TConnectErrorCallbackFunc): integer; stdcall;


function HandleError(AStmtText, AErrMessage: PAnsiChar): integer; stdcall;
begin
  Result:= 0;
  // FormReports.MemoErrors.Lines.Add(AStmtText);
  FormReports.MemoErrors.Lines.Add(AErrMessage);
end;

function BeforeExec(AStmtText, AText: PAnsiChar): integer; stdcall;
begin
  Result:= 0;
  FormReports.MemoErrors.Lines.Add(AText);
end;

function AfterExec(AStmtText: PAnsiChar; Success: integer): integer; stdcall;
begin
  Result:= 0;
  if Success <> 1 then
  begin
    FormReports.MemoErrors.Lines.Add('Отчет не выполнен из-за ошибки.');
    Result:= 1;
  end else
  begin
    FormReports.MemoErrors.Lines.Add('Выполнен успешно ');
    ShellExecuteA(FormReports.Handle, Nil, PAnsiChar(FormReports.reportFileName),
      nil, nil, SW_SHOWNORMAL);
  end;
end;

function CEH(AErrorMessage: PAnsiChar): integer;  stdcall;
begin
  ShowMessage(AErrorMessage);
end;

procedure TFormReports.BReportExecClick(Sender: TObject);
begin
  doReport();
end;

function TFormReports.reportPdf(const reportName: String): AnsiString;
begin
  reportFileName:= reportName + '.pdf';
  Result:= 'execute ibeblock as begin'#13#10 +
  ' Params[''stage''] = ''Проект'';'#13#10 +
  ' SELECT SOURCE FROM fastreport where name = ''' + reportName + ''''#13#10 +
  ' into :RepSrc;'#13#10 +
  ' Report = ibec_CreateReport(RepSrc, Params, null);'#13#10 +
  ' Res = ibec_ExportReport(Report, ''' + reportFileName +
  ''', __erPDF, '''');'#13#10 +
  'end'#13#10;
  // FormReports.MemoErrors.Lines.Add(Result);
end;

{
PropData Parameters Name="stage" DataType="ftString" Expression="[paramstage1]"
0A506172616D657465727301010C3C000000204E616D653D227374616765222044617461547970653D226674537472696E67222045787072657373696F6E3D225B706172616D7374616765315D220000
}

procedure TFormReports.doReport();
var
  Hndl: THandle;
  ESP: TExecuteScriptProc;
  CP: TConnectDBProc;
  script: AnsiString;
  dbconnstr: AnsiString;
begin
  FormReports.MemoErrors.Clear;
  Hndl:= LoadLibraryA(PAnsiChar('IBEScript.dll'));
  try
    if (Hndl > HINSTANCE_ERROR) then
    begin
      CP:= GetProcAddress(Hndl, PAnsiChar('Connect'));
      if @CP <> nil then
      begin
        dbconnstr:= dm.dmOutlay.connectionString();
        MemoErrors.Lines.Add(dbconnstr);
        CP(PAnsiChar(dbconnstr), @HandleError);
      end else begin
        CEH('В динамически загружаемой библиотеке IBEScript нет Connect()');
      end;
      ESP:= GetProcAddress(Hndl, PAnsiChar('ExecScriptText'));
      if @ESP <> nil then
      begin
        script:= reportPDF(IBQueryReports.FieldByName('NAME').AsAnsiString);
        ESP(PAnsiChar(script), @HandleError, @BeforeExec, @AfterExec);
      end else begin
        CEH('В динамически загружаемой библиотеке IBEScript нет ExecScriptText()');
      end;
    end;
  finally
    if Hndl > HINSTANCE_ERROR then
      FreeLibrary(Hndl);
  end;

end;

procedure TFormReports.FormCreate(Sender: TObject);
var
  c: Integer;
begin
  try
    for c:= 0 to ComponentCount - 1 do begin
      if (Components[c] is TIBQuery) then
        TIBQuery(Components[c]).Active:= true;
      if (Components[c] is TIBDataset) then
        TIBDataset(Components[c]).Active:= true;
    end;
  finally
  end;
end;

end.
