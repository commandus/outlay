unit dm;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Data.FMTBcd, Data.DBXInterBase,
  Data.SqlExpr, VCL.Themes, IniFiles,
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
    IBRequestCREATED: TDateTimeField;
    IBRequestMODIFIED: TDateTimeField;
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
    IBPartCREATED: TDateTimeField;
    IBPartMODIFIED: TDateTimeField;
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
    IBRequestCurrencyRate: TIBDataSet;
    IBRequestCurrencyRateID: TLargeintField;
    IBRequestCurrencyRateREQUESTID: TLargeintField;
    IBRequestCurrencyRateCURRENCY: TIBStringField;
    IBRequestCurrencyRateVAL: TFloatField;
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
    DateTimeField3: TDateTimeField;
    DateTimeField4: TDateTimeField;
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
    IBPriceOrg: TIBDataSet;
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
    IBPriceOrgVAL: TIBStringField;
    IBLSpecificationPRICEORGNAME: TStringField;
    IBLSpecificationPRICEPRICE: TCurrencyField;
    IBLSpecificationPRICECURRENCY: TStringField;
    IBLSpecificationPRICERUB: TCurrencyField;
    IBCurrencyISLOCAL: TLargeintField;
    IBLSpecificationCOSTLIST: TCurrencyField;
    IBLSpecificationCOSTDISCOUNT: TCurrencyField;
    procedure DataModuleCreate(Sender: TObject);
    procedure IBLRequestAfterInsert(DataSet: TDataSet);
    procedure IBLProjectAfterInsert(DataSet: TDataSet);
    procedure IBLSpecificationAfterInsert(DataSet: TDataSet);
    procedure IBLSpecificationCalcFields(DataSet: TDataSet);
  private
    function getStyles: TStringList;
    function GetSelectedProjectName: String;
    function GetSelectedProjectVAT: Int64;
    function GetSelectedRequestVAT: Int64;
    function GetSelectedRequestName: String;
    function GetSelectedSpecificationPartName: String;
    function GetDefaultProjectSellerOrg(): String;
    function GetDefaultProjectStage(): String;
    function GetDefaultProjectVAT(): Int64;
    function GetSelectedRequestId(): LongWord;
  public
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
    function getRate(const curr: string): Double;
    class procedure ActivateDbControls(component: TComponent);
  end;

var
  dmOutlay: TdmOutlay;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

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
  end;
end;

procedure TdmOutlay.IBLSpecificationAfterInsert(DataSet: TDataSet);
begin
  if (DataSet.State in [dsEdit, dsInsert]) then
  begin
    DataSet.FieldByName('REQUESTID').AsLongWord:= GetSelectedRequestId();
     DataSet.FieldByName('VAT').AsLargeInt:= GetSelectedRequestVAT();
  end;
end;

function TdmOutlay.getRate(const curr: string): Double;
begin
  IBCurrency.First;
  while not IBCurrency.Eof do
  begin
    if curr = IBCurrency.FieldByName('CURRENCYSYMBOL').AsString  then begin
      Result:= IBCurrency.FieldByName('DEFAULTRATE').AsCurrency;
      Exit;
    end;
    IBCurrency.Next;
  end;
  Result:= 1.0;
end;

procedure TdmOutlay.IBLSpecificationCalcFields(DataSet: TDataSet);
var
  p, price: Currency;
  c: String;
  rate: double;
  qty: double;
begin
  p:= DataSet.FieldByName('PRICEPRICE').AsCurrency;
  if (Abs(p) < 0.001) then Exit;
  c:= DataSet.FieldByName('PRICECURRENCY').AsString;
  rate:= getRate(c);
  price:= rate * p;
  DataSet.FieldByName('PRICERUB').AsCurrency:= price;

  qty:= DataSet.FieldByName('QTY').AsCurrency;
  if (Abs(qty) > 0.001) then begin
    DataSet.FieldByName('COSTLIST').AsCurrency:= price * qty;
  end else begin
    // DataSet.FieldByName('COSTLIST').Clear;
  end;
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

function TdmOutlay.GetSelectedRequestVAT: Int64;
begin
  Result:= IBLRequest.FieldByName('VAT').AsLargeInt;
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

end.
