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
  Vcl.ComCtrls, Vcl.Grids, Vcl.ValEdit,
  dm, Data.DB, IBX.IBCustomDataSet, IBX.IBQuery;

type
  TReportType = (
    rptPDF = 0,
    rptXLS = 1,
    rptXLSX = 2,
    rptRTF = 3
  );

  TFormReports = class(TFormOutlay)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    DBGridEh1: TDBGridEh;
    Panel1: TPanel;
    Splitter2: TSplitter;
    Panel2: TPanel;
    BReportExec: TButton;
    Splitter1: TSplitter;
    MemoErrors: TMemo;
    dsReports: TDataSource;
    IBQueryReports: TIBQuery;
    IBQueryReportsNAME: TIBStringField;
    IBQueryReportsSOURCE: TBlobField;
    VLEParameters: TValueListEditor;
    IBParameters: TIBDataSet;
    IBParametersNAME: TIBStringField;
    IBParametersREPORT: TIBStringField;
    IBParametersVAL: TIBStringField;
    ButtonXLS: TButton;
    ButtonXLSX: TButton;
    ButtonRTF: TButton;
    procedure BReportExecClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IBQueryReportsAfterScroll(DataSet: TDataSet);
    procedure ButtonXLSXClick(Sender: TObject);
    procedure ButtonXLSClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonRTFClick(Sender: TObject);
  private
    paramList: TStringList;
    procedure doReport(reportType: TReportType);
    procedure loadParameters();
    procedure saveParameters();
    function report2File(reportType: TReportType): AnsiString;
    class function ExtractFr3(const fr3: String): TStringList;
  public
    { Public declarations }
    reportFileName: AnsiString;
    reportName: AnsiString;
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
    FormReports.saveParameters();
  end;
end;

function CEH(AErrorMessage: PAnsiChar): integer;  stdcall;
begin
  ShowMessage(AErrorMessage);
end;

procedure TFormReports.BReportExecClick(Sender: TObject);
begin
  doReport(rptPDF);
end;

procedure TFormReports.ButtonRTFClick(Sender: TObject);
begin
  doReport(rptRTF);
end;

procedure TFormReports.ButtonXLSClick(Sender: TObject);
begin
  doReport(rptXLS);
end;

procedure TFormReports.ButtonXLSXClick(Sender: TObject);
begin
  doReport(rptXLSX);
end;


function TFormReports.report2File(reportType: TReportType): AnsiString;
var
  k, t, v, paramstr: String;
  params: AnsiString;
  i: Integer;
  ibecFmt: AnsiString;
begin
  reportName:= IBQueryReports.FieldByName('NAME').AsString;
  params:= '';
  for i:= 0 to paramList.Count - 1 do begin
    k:= paramList.Names[i];
    t:= paramList.ValueFromIndex[i];
    v:= VLEParameters.Values[k];
    if ((t = 'ftString') or (t = 'ftDate') or (t = 'ftTime') or (t = 'ftDateTime')) then
    begin
      v:= '''' + v + '''';
    end;
    params:= params+ 'Params[''' + k + '''] = ' + v + ';'#13#10;
  end;

  case reportType of
    rptXLS: begin
      reportFileName:= reportName + '.xls';
      ibecFmt:= '__erXLS';
    end;
    rptXLSX: begin
      reportFileName:= reportName + '.xlsx';
      ibecFmt:= '__erXML_XLS';
    end;
    rptRTF: begin
      reportFileName:= reportName + '.rtf';
      ibecFmt:= '__erRTF';
    end
    else begin
      reportFileName:= reportName + '.pdf';
      ibecFmt:= '__erPDF';
    end;
  end;
  Result:= 'execute ibeblock as begin'#13#10 +
  params +
  ' SELECT SOURCE FROM fastreport where name = ''' + reportName + ''''#13#10 +
  ' into :RepSrc;'#13#10 +
  ' Report = ibec_CreateReport(RepSrc, Params, null);'#13#10 +
  ' Res = ibec_ExportReport(Report, ''' + reportFileName +
  ''', ' + ibecFmt + ', '''');'#13#10 +
  'end'#13#10;
  // FormReports.MemoErrors.Lines.Add(Result);
end;

{
PropData Parameters Name="stage" DataType="ftString" Expression="[paramstage1]"
0A506172616D657465727301010C3C000000204E616D653D227374616765222044617461547970653D226674537472696E67222045787072657373696F6E3D225B706172616D7374616765315D220000
}
procedure TFormReports.doReport(reportType: TReportType);
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
        MemoErrors.Lines.Add(String(dbconnstr));
        CP(PAnsiChar(dbconnstr), @HandleError);
      end else begin
        CEH('В динамически загружаемой библиотеке IBEScript нет Connect()');
      end;
      ESP:= GetProcAddress(Hndl, PAnsiChar('ExecScriptText'));
      if @ESP <> nil then
      begin
        try
          script:= report2File(reportType);
          ESP(PAnsiChar(script), @HandleError, @BeforeExec, @AfterExec);
        except on E: Exception do
          CEH('Ошибка генерации отчета. Возможно, не установлен Excel.');
        end;
      end else begin
        CEH('В динамически загружаемой библиотеке IBEScript нет ExecScriptText()');
      end;
    end;
  finally
    if Hndl > HINSTANCE_ERROR then
      FreeLibrary(Hndl);
  end;
end;

procedure TFormReports.FormActivate(Sender: TObject);
begin
  dm.dmOutlay.ActivateDbControls(Self);
end;

procedure TFormReports.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IBParameters.Close;
  IBQueryReports.Close;
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

procedure TFormReports.IBQueryReportsAfterScroll(DataSet: TDataSet);
begin
  reportName:= Dataset.FieldByName('NAME').AsString;
  paramList:= ExtractFr3(Dataset.FieldByName('SOURCE').AsString);
  loadParameters();
end;

{
  1234567890123456
  PropData="044C6"
  p = 1
  p+10=11
  p2=16
  p2 - p - 10 = 16 - 11 = 5


  Parameters
  01010C3C 000000
   Name="stage" DataType="ftString" Expression="[paramstage1]"
   Name="stage" DataType="ftString" Expression="[paramstage1]"
}
class function TFormReports.ExtractFr3(const fr3: String): TStringList;
var
  p, p1, p2: Integer;
  hex, bin: AnsiString;
  datatype, paramname: AnsiString;
begin
  Result:= TStringList.Create;
  Result.Duplicates:= dupIgnore;
  p:= 0;
  while true do begin
    Inc(p);
    p:= StringSearch('PropData="', fr3, true, false, p);
    if (p <= 0) then Break;
    p2:= StringSearch('"', fr3, true, false, p + 10);
    if (p2 <= 0) then Continue;
    hex:= Copy(fr3, p + 10, p2 - p - 10);
    SetLength(bin, Length(hex) div 2);
    HexToBin(PAnsiChar(hex), PAnsiChar(bin), Length(bin));

    p1:= StringSearch('Parameters', bin, true, false, 1);
    if (p1 <> 2) then Continue;

    p1:= StringSearch('DataType="', bin, true, false, p1 + 18);
    if (p1 <= 0) then Continue;
    p2:= StringSearch('"', bin, true, false, p1 + 10);
    if (p2 <= 0) then Continue;
    datatype:= Copy(bin, p1 + 10, p2 - p1 - 10);

    p1:= StringSearch('Expression="[', bin, true, false, p2);
    if (p1 <= 0) then Continue;
    p2:= StringSearch(']"', bin, true, false, p1 + 13);
    if (p1 <= 0) then Continue;
    paramname:= Copy(bin, p1 + 13, p2 - p1 - 13);
    Result.AddPair(paramname, datatype);
  end;
end;

procedure TFormReports.loadParameters();
var
  n, v: String;
  i: Integer;
begin
  VLEParameters.Strings.Clear;
  for i:= 0 to paramList.Count - 1 do begin
    VLEParameters.Values[paramList.Names[i]]:= '';
  end;
  IBParameters.Params.ByName('REPORT').AsString:= reportName;
  IBParameters.Active:= True;
  while not IBParameters.Eof do
  begin
    n:= IBParameters.FieldByName('NAME').AsString;
    v:= IBParameters.FieldByName('VAL').AsString;
    if (paramList.IndexOfName(n) >= 0) then
    begin
      VLEParameters.Values[n]:= v;
    end;
    IBParameters.Next;
  end;
end;

procedure TFormReports.saveParameters();
var
  k, v: String;
  i: Integer;
begin
  for i:= 0 to paramList.Count - 1 do begin
    k:= paramList.Names[i];
    v:= VLEParameters.Values[k];
    if IBParameters.Locate('NAME', VarArrayOf([k]), []) then
    begin
      IBParameters.Edit;
      IBParameters.FieldByName('VAL').AsString:= v;
    end else begin
      IBParameters.Insert;
      IBParameters.FieldByName('REPORT').AsString:= reportName;
      IBParameters.FieldByName('NAME').AsString:= k;
      IBParameters.FieldByName('VAL').AsString:= v;
    end;
  end;
  IBParameters.Post;
end;

end.
