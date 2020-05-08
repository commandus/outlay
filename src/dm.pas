unit dm;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Data.FMTBcd, Data.DBXInterBase,
  Data.SqlExpr, VCL.Themes, Vcl.ValEdit, IniFiles,
  Vcl.Forms, EhLibIBX, DbUtilsEh, IBX.IBDatabase,
  IBX.IBCustomDataSet, IBX.IBQuery;

type
  TdmOutlay = class(TDataModule)
    dsProject: TDataSource;
    dsOrgType: TDataSource;
    dsOrgPosition: TDataSource;
    dsPerson: TDataSource;
    dsSaleType: TDataSource;
    dsVAT: TDataSource;
    dsRequest: TDataSource;
    dsOrg: TDataSource;
    IBDatabase: TIBDatabase;
    IBTransaction: TIBTransaction;
    IBVAT: TIBDataSet;
    IBVATVAL: TLargeintField;
    IBVATNAME: TIBStringField;
    IBProject: TIBDataSet;
    IBProjectNAME: TIBStringField;
    IBProjectSELLERORG: TIBStringField;
    IBProjectDESCRIPTION: TIBStringField;
    IBProjectVAT: TLargeintField;
    IBPerson: TIBDataSet;
    IBPersonID: TLargeintField;
    IBPersonORGNAME: TIBStringField;
    IBPersonPOSITIONNAME: TIBStringField;
    IBPersonPERSONNAME: TIBStringField;
    IBPersonEMAIL: TIBStringField;
    IBPersonPHONE: TIBStringField;
    IBPersonSITE: TIBStringField;
    IBPersonTAG: TIBStringField;
    IBPersonDESCRIPTION: TIBStringField;
    IBPersonNOTES: TWideMemoField;
    IBRequest: TIBDataSet;
    IBRequestID: TLargeintField;
    IBRequestPROJECTNAME: TIBStringField;
    IBRequestNAME: TIBStringField;
    IBRequestDESCRIPTION: TIBStringField;
    IBRequestSALETYPE: TIBStringField;
    IBRequestORG: TIBStringField;
    IBRequestVAT: TLargeintField;
    IBOrgType: TIBDataSet;
    IBOrgTypeVAL: TIBStringField;
    IBOrgTypeDESCRIPTION: TIBStringField;
    IBOrgPosition: TIBDataSet;
    IBOrgPositionNAME: TIBStringField;
    IBOrgPositionDESCRIPTION: TIBStringField;
    IBSaleType: TIBDataSet;
    IBSaleTypeNAME: TIBStringField;
    IBSaleTypeDESCRIPTION: TIBStringField;
    IBOrg: TIBDataSet;
    IBOrgORGTYPE: TIBStringField;
    IBOrgORGNAME: TIBStringField;
    IBOrgFULLNAME: TIBStringField;
    IBOrgALTNAME: TIBStringField;
    IBOrgINN: TLargeintField;
    IBOrgDESCRIPTION: TIBStringField;
    IBOrgTAG: TIBStringField;
    IBOrgCREATED: TDateTimeField;
    IBOrgMODIFIED: TDateTimeField;
    IBOrgLOGO: TBlobField;
    IBCategory: TIBDataSet;
    IBCategoryNAME: TIBStringField;
    IBCategoryPARENT: TIBStringField;
    IBCategoryDESCRIPTION: TIBStringField;
    dsCategory: TDataSource;
    IBMeasureUnit: TIBDataSet;
    IBMeasureUnitNAME: TIBStringField;
    IBMeasureUnitFULLNAME: TIBStringField;
    dsMeasureUnit: TDataSource;
    IBPart: TIBDataSet;
    IBPartID: TLargeintField;
    IBPartCATEGORY: TIBStringField;
    IBPartVENDOR: TIBStringField;
    IBPartPARTNO: TIBStringField;
    IBPartNAME: TIBStringField;
    IBPartDESCRIPTION: TIBStringField;
    IBPartMEASUREUNIT: TIBStringField;
    IBPartWEIGHT: TFloatField;
    IBPartWIDTH: TFloatField;
    IBPartHEIGHT: TFloatField;
    IBPartLENGTH: TFloatField;
    dsPart: TDataSource;
    ibPrice: TIBDataSet;
    ibPriceID: TLargeintField;
    ibPricePARTNAME: TIBStringField;
    ibPriceORGNAME: TIBStringField;
    ibPriceCURRENCY: TIBStringField;
    ibPricePRICE: TFloatField;
    ibPriceSRC: TIBStringField;
    ibPriceNOTES: TIBStringField;
    dsPrice: TDataSource;
    IBLRequestCurrencyRate: TIBDataSet;
    IBLRequestCurrencyRateID: TLargeintField;
    IBLRequestCurrencyRateREQUESTID: TLargeintField;
    IBLRequestCurrencyRateCURRENCY: TIBStringField;
    IBLRequestCurrencyRateVAL: TFloatField;
    dsRequestCurrencyRate: TDataSource;
    IBSpecification: TIBDataSet;
    dsSpecification: TDataSource;
    IBCurrency: TIBDataSet;
    IBCurrencyCURRENCYSYMBOL: TIBStringField;
    IBCurrencyNAME: TIBStringField;
    IBCurrencyDEFAULTRATE: TFloatField;
    dsValidCurrency: TDataSource;
    IBVendor: TIBDataSet;
    IBVendorNAME: TIBStringField;
    IBVendorDESCRIPTION: TIBStringField;
    dsVendor: TDataSource;
    IBRequestSTAGE: TIBStringField;
    IBProjectSTAGE: TIBStringField;
    IBStage: TIBDataSet;
    IBStageNAME: TIBStringField;
    IBStageDESCRIPTION: TIBStringField;
    IBOrgOWNER: TIBStringField;
    IBLProject: TIBDataSet;
    IBStringField1: TIBStringField;
    IBStringField2: TIBStringField;
    IBStringField3: TIBStringField;
    LargeintField1: TLargeintField;
    IBStringField4: TIBStringField;
    IBLRequest: TIBDataSet;
    LargeintField2: TLargeintField;
    IBStringField5: TIBStringField;
    IBStringField6: TIBStringField;
    IBStringField7: TIBStringField;
    IBStringField8: TIBStringField;
    IBStringField9: TIBStringField;
    LargeintField3: TLargeintField;
    IBStringField10: TIBStringField;
    dslProject: TDataSource;
    dslRequest: TDataSource;
    IBLSpecification: TIBDataSet;
    LargeintField4: TLargeintField;
    LargeintField5: TLargeintField;
    LargeintField6: TLargeintField;
    IBStringField11: TIBStringField;
    FloatField1: TFloatField;
    LargeintField7: TLargeintField;
    FloatField2: TFloatField;
    FloatField3: TFloatField;
    LargeintField8: TLargeintField;
    FloatField4: TFloatField;
    IBStringField12: TIBStringField;
    IBStringField13: TIBStringField;
    dslSpecification: TDataSource;
    IBSpecPrice: TIBDataSet;
    LargeintField9: TLargeintField;
    IBStringField14: TIBStringField;
    IBStringField15: TIBStringField;
    IBStringField16: TIBStringField;
    FloatField5: TFloatField;
    IBStringField17: TIBStringField;
    IBStringField18: TIBStringField;
    IBSpecificationCOST: TFloatField;
    IBSpecificationDISCOUNT: TFloatField;
    IBSpecificationID: TLargeintField;
    IBSpecificationLINENO: TLargeintField;
    IBSpecificationNOTES: TIBStringField;
    IBSpecificationPARTNAME: TIBStringField;
    IBSpecificationPRICE: TFloatField;
    IBSpecificationPRICEID: TLargeintField;
    IBSpecificationQTY: TFloatField;
    IBSpecificationREQUESTID: TLargeintField;
    IBSpecificationTAG: TIBStringField;
    IBSpecificationVAT: TLargeintField;
    IBLSpecificationPRICEVAL: TStringField;
    IBSpecPriceVAL: TIBStringField;
    IBLSpecificationPRICEORGNAME: TStringField;
    IBLSpecificationPRICEPRICE: TCurrencyField;
    IBLSpecificationPRICECURRENCY: TStringField;
    IBLSpecificationPRICERUB: TCurrencyField;
    IBCurrencyISLOCAL: TLargeintField;
    IBLSpecificationCOSTLIST: TCurrencyField;
    IBLSpecificationCOSTDISCOUNT: TCurrencyField;
    IBSpecPart: TIBDataSet;
    IBSpecPartCATEGORY: TIBStringField;
    IBSpecPartDESCRIPTION: TIBStringField;
    IBSpecPartHEIGHT: TFloatField;
    IBSpecPartID: TLargeintField;
    IBSpecPartLENGTH: TFloatField;
    IBSpecPartMEASUREUNIT: TIBStringField;
    IBSpecPartNAME: TIBStringField;
    IBSpecPartPARTNO: TIBStringField;
    IBSpecPartVENDOR: TIBStringField;
    IBSpecPartWEIGHT: TFloatField;
    IBSpecPartWIDTH: TFloatField;
    IBLSpecificationPARTID: TLongWordField;
    IBLSpecificationPARTVOL: TFloatField;
    IBSpecPartVOL: TFloatField;
    IBLSpecificationPARTVOLSUM: TFloatField;
    IBLSpecificationPARTWEIGHTSUM: TFloatField;
    IBPartVOL: TFloatField;
    IBRequestDISCOUNT: TFloatField;
    IBProjectDISCOUNT: TFloatField;
    IBLRequestDISCOUNT: TFloatField;
    IBLProjectDISCOUNT: TFloatField;
    IBLSpecificationPRICENDISCOUNT: TCurrencyField;
    IBLSpecificationCOSTNDISCOUNT: TCurrencyField;
    IBLSpecificationREBATE: TFloatField;
    IBSpecificationREBATE: TFloatField;
    dsPaymentType: TDataSource;
    IBPaymentType: TIBDataSet;
    IBPaymentTypeNAME: TIBStringField;
    IBPaymentTypeCODE: TLargeintField;
    dsPaymentState: TDataSource;
    IBPaymentState: TIBDataSet;
    IBStringField19: TIBStringField;
    LargeintField10: TLargeintField;
    dsPayment: TDataSource;
    IBLPayment: TIBDataSet;
    IBLPaymentDOCID: TIBStringField;
    IBLPaymentNOTES: TIBStringField;
    IBLPaymentPAYDAY: TDateTimeField;
    IBLPaymentPSTATE: TIBStringField;
    IBLPaymentPTYPE: TIBStringField;
    IBLPaymentREQUESTID: TLargeintField;
    IBLPaymentVAL: TFloatField;
    IBLPaymentID: TLargeintField;
    procedure DataModuleCreate(Sender: TObject);
    procedure IBLRequestAfterInsert(DataSet: TDataSet);
    procedure IBLProjectAfterInsert(DataSet: TDataSet);
    procedure IBLSpecificationAfterInsert(DataSet: TDataSet);
    procedure IBLSpecificationCalcFields(DataSet: TDataSet);
    procedure IBLRequestCurrencyRateAfterInsert(DataSet: TDataSet);
    procedure IBLSpecificationAfterPost(DataSet: TDataSet);
    procedure IBLSpecificationAfterOpen(DataSet: TDataSet);
    procedure IBLPaymentAfterInsert(DataSet: TDataSet);
    procedure IBLRequestCurrencyRateAfterOpen(DataSet: TDataSet);
    procedure IBLRequestCurrencyRateAfterPost(DataSet: TDataSet);
    procedure IBLSpecificationAfterDelete(DataSet: TDataSet);
  private
    function getStyles: TStringList;
    function GetSelectedProjectName: String;
    function GetSelectedProjectVAT: Int64;
    function GetSelectedProjectDiscount: Int64;
    function GetSelectedRequestVAT: Int64;
    function GetSelectedRequestName: String;
    function GetSelectedRequestDiscount: Int64;
    function GetSelectedSpecificationPartName: String;
    function GetDefaultProjectSellerOrg(): String;
    function GetDefaultProjectStage(): String;
    function GetDefaultProjectVAT(): Int64;
    function GetSelectedRequestId(): LongWord;
    function calcSums(): TStrings;
  public
    requestSumList: TStrings;
    inifilename: String;
    style: String;
    styles: TStringList;
    dbName: String;
    hostAddress: String;
    userName: String;
    userPassword: String;
    function connect(ibDatabase: TIBDatabase): Boolean;
    class procedure loadLastStyle();
    procedure setStyle(const style: String);
    function getStyleIndex: Integer;
    procedure loadSettings();
    procedure saveSettings();
    function connectionString: AnsiString;
    function getCurrencyRateDefault(const curr: string): Double;
    function getCurrencyRate(const curr: string): Double;
    procedure setRequestDiscount2All(const val: double);
    procedure updateSums();
    procedure updatedCurrencyRate();
    class procedure ActivateDbControls(component: TComponent);
  end;

var
  dmOutlay: TdmOutlay;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  DBGridEh, Generics.Collections;

procedure TdmOutlay.DataModuleCreate(Sender: TObject);
begin
  inifilename:= ChangeFileExt(Application.ExeName, '.ini');
  loadSettings();
  connect(IBDatabase);
end;

{
  FDConnectionOutlay.ConnectionString:=
    'DriverID=FB;Protocol=TCPIP;CharacterSet=WIN1251;Server=' + hostAddress
      + ';Database=' + dbName
      + ';User_name=' + userName
      + ';Password=' + userPassword;

}
function TdmOutlay.connect(IBDatabase: TIBDatabase): Boolean;
var
  c: Integer;
begin
  IBDatabase.Connected:= False;
  IBDatabase.DatabaseName:= hostAddress + ':' + dbName;
  with IBDatabase.Params do begin
    Clear;
    Add('lc_ctype=WIN1251');
    Add('USER_NAME=' + userName);
    Add('PASSWORD=' + userPassword);
  end;
  try
    IBDatabase.Connected:= true;
    for c:= 0 to ComponentCount - 1 do begin
      if (Components[c] is TIBQuery) then
        TIBQuery(Components[c]).Active:= true;
      if (Components[c] is TIBDataset) then
        TIBDataset(Components[c]).Active:= true;
    end;
  finally
  end;
  Result:= IBDatabase.Connected;
end;

{
  * db_name=localhost:c:\empty.fdb;
  * password=masterkey;
  * user_name=SYSDBA;
  * lc_ctype=win1251;
  * sql_role_name=ADMIN;
  * sql_dialect=3;
  * clientlib="c:\program files\firebird\bin\fbclient.dll"
}
function TdmOutlay.connectionString: AnsiString;
begin
  Result:= 'lc_ctype=WIN1251;user_name=' + userName
    + ';password=' + userPassword
    + ';db_name=' + hostAddress + ':' + dbName + ';';
end;

function TdmOutlay.getStyles: TStringList;
var
  i: Integer;
  styles: TStringList;
begin
  styles:= TStringList.Create;
  for i := 0 to Length(TStyleManager.StyleNames) - 1 do begin
    styles.Add(TStyleManager.StyleNames[i]);
  end;
  styles.Sort();
  Result:= styles;
end;

procedure TdmOutlay.IBLPaymentAfterInsert(DataSet: TDataSet);
begin
  if (DataSet.State in [dsEdit, dsInsert]) then
  begin
    DataSet.FieldByName('REQUESTID').AsLongWord:= GetSelectedRequestId();
  end;
end;

procedure TdmOutlay.IBLProjectAfterInsert(DataSet: TDataSet);
begin
  if (DataSet.State in [dsEdit, dsInsert]) then
  begin
    DataSet.FieldByName('SELLERORG').AsString:= GetDefaultProjectSellerOrg();
    DataSet.FieldByName('STAGE').AsString:= GetDefaultProjectStage();
    DataSet.FieldByName('VAT').AsLargeInt:= GetDefaultProjectVAT();
  end;
end;

procedure TdmOutlay.IBLRequestAfterInsert(DataSet: TDataSet);
begin
  if (DataSet.State in [dsEdit, dsInsert]) then
  begin
    DataSet.FieldByName('PROJECTNAME').AsString:= GetSelectedProjectName();
    DataSet.FieldByName('VAT').AsLargeInt:= GetSelectedProjectVAT();
    DataSet.FieldByName('DISCOUNT').AsLargeInt:= GetSelectedProjectDiscount();
    IBLRequestCurrencyRate.Refresh;
  end;
end;

procedure TdmOutlay.IBLSpecificationAfterDelete(DataSet: TDataSet);
begin
  updateSums();
end;

procedure TdmOutlay.IBLSpecificationAfterInsert(DataSet: TDataSet);
begin
  if (DataSet.State in [dsEdit, dsInsert]) then
  begin
    DataSet.FieldByName('REQUESTID').AsLongWord:= GetSelectedRequestId();
    DataSet.FieldByName('VAT').AsLargeInt:= GetSelectedRequestVAT();
    DataSet.FieldByName('DISCOUNT').AsLargeInt:= GetSelectedRequestDiscount();
  end;
end;

procedure TdmOutlay.IBLSpecificationAfterOpen(DataSet: TDataSet);
begin
  updateSums();
end;

procedure TdmOutlay.IBLSpecificationAfterPost(DataSet: TDataSet);
begin
  updateSums();
end;

procedure TdmOutlay.updateSums();
var
  sl: TStrings;
begin
  sl:= dm.dmOutlay.calcSums();
  if requestSumList <> Nil then begin
    requestSumList.Assign(sl);
  end;
  sl.Free;
end;

procedure TdmOutlay.updatedCurrencyRate();
begin
  if IBLSpecification.Active then
  begin
    IBLSpecification.Close;
    IBLSpecification.Open;
  end;
end;

function TdmOutlay.getCurrencyRate(const curr: string): Double;
var
  bookmark: TBookmark;
begin
  bookmark:= IBLRequestCurrencyRate.GetBookmark;
  IBLRequestCurrencyRate.DisableControls;
  IBLRequestCurrencyRate.First;
  while not IBLRequestCurrencyRate.Eof do
  begin
    if curr = IBLRequestCurrencyRate.FieldByName('CURRENCY').AsString  then begin
      Result:= IBLRequestCurrencyRate.FieldByName('VAL').AsCurrency;
      IBLRequestCurrencyRate.EnableControls;
      Exit;
    end;
    IBLRequestCurrencyRate.Next;
  end;
  IBLRequestCurrencyRate.GotoBookmark(bookmark);
  IBLRequestCurrencyRate.EnableControls;
  Result:= getCurrencyRateDefault(curr);
end;

function TdmOutlay.getCurrencyRateDefault(const curr: string): Double;
var
  bookmark: TBookmark;
begin
  bookmark:= IBCurrency.GetBookmark;
  IBCurrency.DisableControls;
  IBCurrency.First;
  while not IBCurrency.Eof do
  begin
    if curr = IBCurrency.FieldByName('CURRENCYSYMBOL').AsString  then begin
      Result:= IBCurrency.FieldByName('DEFAULTRATE').AsCurrency;
      IBCurrency.EnableControls;
      Exit;
    end;
    IBCurrency.Next;
  end;
  Result:= 1.0;
  IBCurrency.GotoBookmark(bookmark);
  IBCurrency.EnableControls;
end;

procedure TdmOutlay.IBLSpecificationCalcFields(DataSet: TDataSet);
var
  priceCurrency, price: Currency;
  c: String;
  rate: double;
  qty, vol, weight, rebate, discount, pricerebate: double;
begin
  priceCurrency:= DataSet.FieldByName('PRICEPRICE').AsCurrency;
  if (Abs(priceCurrency) < 0.001) then Exit;
  c:= DataSet.FieldByName('PRICECURRENCY').AsString;
  rebate:= Dataset.FieldByName('REBATE').AsCurrency;
  discount:= DataSet.FieldByName('DISCOUNT').AsCurrency;
  qty:= DataSet.FieldByName('QTY').AsCurrency;
  rate:= getCurrencyRate(c);
  price:= rate * priceCurrency;
  pricerebate:= price - (price * rebate / 100);

  DataSet.FieldByName('PRICERUB').AsCurrency:= pricerebate;
  DataSet.FieldByName('PRICENDISCOUNT').AsCurrency:= pricerebate + (pricerebate * discount / 100);

  if (Abs(qty) > 0.0001) then begin
    vol:= DataSet.FieldByName('PARTVOL').AsFloat;
    weight:= DataSet.FieldByName('PARTWEIGHT').AsFloat;

    DataSet.FieldByName('COSTLIST').AsCurrency:= pricerebate* qty;
    DataSet.FieldByName('COSTNDISCOUNT').AsCurrency:= qty * (pricerebate + (pricerebate* discount / 100));
    DataSet.FieldByName('PARTVOLSUM').AsFloat:= vol * qty;
    DataSet.FieldByName('PARTWEIGHTSUM').AsFloat:= weight * qty;
  end;
end;

procedure TdmOutlay.IBLRequestCurrencyRateAfterInsert(DataSet: TDataSet);
begin
  Dataset.FieldByName('REQUESTID').AsLongWord:= IBLRequest.FieldByName('ID').AsLongWord;
end;

procedure TdmOutlay.IBLRequestCurrencyRateAfterOpen(DataSet: TDataSet);
begin
  updatedCurrencyRate();
end;

procedure TdmOutlay.IBLRequestCurrencyRateAfterPost(DataSet: TDataSet);
begin
  updatedCurrencyRate();
end;

class procedure TdmOutlay.loadLastStyle();
var
  ini: TIniFile;
  s: String;
begin
   ini:= TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
   try
    s:= ini.ReadString('ui', 'style', 'Windows10');
    TStyleManager.TrySetStyle(s);
   finally
     ini.Free;
   end;
end;

procedure TdmOutlay.setStyle(const style: String);
begin
   try
     TStyleManager.TrySetStyle(style);
   finally
   end;
end;

procedure TdmOutlay.loadSettings();
var
  ini: TIniFile;
begin
   styles:= getStyles();
   ini:= TIniFile.Create(inifilename);
   try
    style:= ini.ReadString('ui', 'style', 'Windows10');
    dbName:= ini.ReadString('db', 'name', 'outlay');
    hostAddress:= ini.ReadString('db', 'host', '127.0.0.1');
    userName:= ini.ReadString('db', 'user', 'SYSDBA');
    userPassword:= ini.ReadString('db', 'password', 'masterkey');
   finally
     ini.Free;
   end;
end;

procedure TdmOutlay.saveSettings();
var
  ini: TIniFile;
begin
   ini:= TIniFile.Create(inifilename);
   try
    ini.WriteString('ui', 'style', style);
    ini.WriteString('db', 'name', dbName);
    ini.WriteString('db', 'host', hostAddress);
    ini.WriteString('db', 'user', userName);
    ini.WriteString('db', 'password', userPassword);
   finally
     ini.Free;
   end;
end;

function TdmOutlay.getStyleIndex: Integer;
var
  r: Integer;
begin
  r:= -1;
  styles.Find(style, r);
  Result:= r;
end;

function TdmOutlay.GetSelectedProjectName: String;
begin
  Result:= IBLProject.FieldByName('NAME').AsString;
end;

function TdmOutlay.GetSelectedProjectVAT: Int64;
begin
  Result:= IBLProject.FieldByName('VAT').AsLargeInt;
end;

function TdmOutlay.GetSelectedProjectDiscount: Int64;
begin
  Result:= IBLProject.FieldByName('DISCOUNT').AsLargeInt;
end;

function TdmOutlay.GetSelectedRequestVAT: Int64;
begin
  Result:= IBLRequest.FieldByName('VAT').AsLargeInt;
end;

function TdmOutlay.GetSelectedRequestDiscount: Int64;
begin
  Result:= IBLRequest.FieldByName('DISCOUNT').AsLargeInt;
end;

function TdmOutlay.GetSelectedRequestName: String;
begin
  Result:= IBLRequest.FieldByName('NAME').AsString;
end;

function TdmOutlay.GetSelectedSpecificationPartName: String;
begin
  Result:= IBLSpecification.FieldByName('PARTNAME').AsString;
end;

function TdmOutlay.GetDefaultProjectSellerOrg(): String;
begin
  Result:= IBOrg.FieldByName('ORGNAME').AsString;
end;

function TdmOutlay.GetDefaultProjectStage(): String;
begin
  Result:= IBStage.FieldByName('NAME').AsString;
end;

function TdmOutlay.GetDefaultProjectVAT(): Int64;
begin
  Result:= IBVAT.FieldByName('VAL').AsLargeInt;
end;

function TdmOutlay.GetSelectedRequestId(): LongWord;
begin
  Result:= IBLRequest.FieldByName('ID').AsLongWord;
end;

class procedure TdmOutlay.ActivateDbControls(component: TComponent);
var
  c: Integer;
begin
  try
    for c:= 0 to component.ComponentCount - 1 do begin
      if (component.Components[c] is TIBQuery) then
        TIBQuery(component.Components[c]).Active:= true;
      if (component.Components[c] is TIBDataset) then
        TIBDataset(component.Components[c]).Active:= true;
    end;
  finally
  end;
end;


procedure TdmOutlay.setRequestDiscount2All(const val: double);
var
  bookmark: TBookmark;
begin
  bookmark:= IBLSpecification.GetBookmark;
  IBLSpecification.DisableControls;
  IBLSpecification.First;
  while not IBLSpecification.Eof do
  begin
    IBLSpecification.Edit;
    IBLSpecification.FieldByName('DISCOUNT').AsFloat:= val;
    IBLSpecification.Post;
    IBLSpecification.Next;
  end;
  IBLSpecification.GotoBookmark(bookmark);
  IBLSpecification.EnableControls;
end;

function TdmOutlay.calcSums(): TStrings;
var
  i: Integer;
  bookmark: TBookmark;
  c, k: String;
  rebate, price, priceRUR,
  rebateRUR, discountRUR, rebateCurr, discountCurr: Currency;
  rate, qty, vol, vat, weight, discount,
  sumWeight, sumVol: double;
  sumIn, sumRebate, sumDiscount: Currency;
  sumVATIn, sumVATRebate, sumVATDiscount: Currency;
  currCostIn, currCostRebate, currCostDiscount: TDictionary<String, Currency>;
begin
  bookmark:= IBLSpecification.GetBookmark;
  IBLSpecification.DisableControls;
  IBLSpecification.First;

  sumIn:= 0.0;
  sumRebate:= 0.0;
  sumDiscount:= 0.0;
  sumVATIn:= 0.0;
  sumVATRebate:= 0.0;
  sumVATDiscount:= 0.0;
  sumWeight:= 0;
  sumVol:= 0;

  currCostIn:= TDictionary<String, Currency>.Create();
  currCostRebate:= TDictionary<String, Currency>.Create();
  currCostDiscount:= TDictionary<String, Currency>.Create();

  while not IBLSpecification.Eof do
  begin
    price:= IBLSpecification.FieldByName('PRICEPRICE').AsCurrency;
    if (Abs(price) < 0.001) then Continue;
    qty:= IBLSpecification.FieldByName('QTY').AsCurrency;
    if (Abs(qty) < 0.0001) then Continue;

    c:= IBLSpecification.FieldByName('PRICECURRENCY').AsString;
    rate:= getCurrencyRate(c);
    rebate:= IBLSpecification.FieldByName('REBATE').AsCurrency;
    discount:= IBLSpecification.FieldByName('DISCOUNT').AsCurrency;
    vat:= IBLSpecification.FieldByName('VAT').AsFloat;
    vol:= IBLSpecification.FieldByName('PARTVOL').AsFloat;
    weight:= IBLSpecification.FieldByName('PARTWEIGHT').AsFloat;

    if not currCostIn.ContainsKey(c) then
      currCostIn.Add(c, 0.0);
    if not currCostRebate.ContainsKey(c) then
      currCostRebate.Add(c, 0.0);
    if not currCostDiscount.ContainsKey(c) then
      currCostDiscount.Add(c, 0.0);

    currCostIn[c]:= currCostIn[c] + qty * price;
    priceRUR:= rate * price;
    sumIn:= sumIn + qty * priceRUR;
    sumVATIn:= sumVATIn + (qty * priceRUR * vat / 100);
    rebateRUR:= (priceRUR * rebate / 100);
    sumRebate:= sumRebate + qty * (priceRUR - rebateRUR);
    sumVATRebate:= sumVATRebate + qty * (priceRUR - rebateRUR) * vat / 100;
    discountRUR:= (priceRUR - rebateRUR) * discount / 100;

    sumDiscount:= sumDiscount + qty * (priceRUR - rebateRUR + discountRUR);

    sumVATDiscount:= sumVATDiscount + qty * (priceRUR - rebateRUR + discountRUR) * vat / 100;

    rebateCurr:= (price * rebate / 100);
    currCostRebate[c]:= currCostRebate[c] + qty * (price - rebateCurr);
    discountCurr:= (rebateCurr * discount/ 100);
    currCostDiscount[c]:= currCostDiscount[c] + qty * (price - discountCurr);

    sumWeight:= sumWeight + weight * qty;
    sumVol:= sumVol + Vol * qty;
    IBLSpecification.Next;
  end;

  IBLSpecification.GotoBookmark(bookmark);
  IBLSpecification.EnableControls;

  Result:= TStringList.Create;

  for k in currCostIn.Keys do begin
    Result.Values['Вход ' + k]:= CurrToStr(currCostIn.Items[k]);
  end;
  Result.Values['Итого ']:= CurrToStr(sumIn);

  for k in currCostRebate.Keys do begin
    Result.Values['С рибейтом ' + k]:= CurrToStr(currCostRebate.Items[k]);
  end;
  Result.Values['Итого ']:= CurrToStr(sumRebate);

  for k in currCostDiscount.Keys do begin
    Result.Values['С наценкой ' + k]:= CurrToStr(currCostDiscount.Items[k]);
  end;
  Result.Values['Продажа ']:= CurrToStr(sumDiscount);
  Result.Values['Вал. прибыль ']:= CurrToStr(sumDiscount - sumRebate);

  Result.Values['Вход. налог ']:= CurrToStr(sumVATIn);
  Result.Values['С рибейтом налог ']:= CurrToStr(sumVATRebate);
  Result.Values['Выход. налог ']:= CurrToStr(sumVATDiscount);

  Result.Values['Вес ']:= FloatToStrF(sumWeight, ffFixed, 18, 2);
  Result.Values['Объем ']:= FloatToStrF(sumVol, ffFixed, 18, 2);

  currCostIn.Free;
  currCostRebate.Free;
  currCostDiscount.Free;

end;

end.
