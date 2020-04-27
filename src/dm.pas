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
    ibPriceCREATED: TDateTimeField;
    ibPriceMODIFIED: TDateTimeField;
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
    IBSpecificationID: TLargeintField;
    IBSpecificationREQUESTID: TLargeintField;
    IBSpecificationLINENO: TLargeintField;
    IBSpecificationPARTNAME: TIBStringField;
    IBSpecificationQTY: TFloatField;
    IBSpecificationPRICEID: TLargeintField;
    IBSpecificationPRICE: TFloatField;
    IBSpecificationDISCOUNT: TFloatField;
    IBSpecificationVAT: TLargeintField;
    IBSpecificationCOST: TFloatField;
    IBSpecificationTAG: TIBStringField;
    IBSpecificationNOTES: TIBStringField;
    IBSpecificationCREATED: TDateTimeField;
    IBSpecificationMODIFIED: TDateTimeField;
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
    DateTimeField1: TDateTimeField;
    DateTimeField2: TDateTimeField;
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
    DateTimeField5: TDateTimeField;
    DateTimeField6: TDateTimeField;
    dslSpecification: TDataSource;
    procedure DataModuleCreate(Sender: TObject);
    procedure dslRequestUpdateData(Sender: TObject);
    procedure IBLRequestAfterInsert(DataSet: TDataSet);
  private
    function getStyles: TStringList;
    function GetSelectedProjectName: String;
    function GetSelectedProjectVAT: Int64;
    function GetSelectedRequestName: String;
    function GetSelectedSpecificationPartName: String;
  public
    inifilename: String;
    style: String;
    styles: TStringList;
    dbName: String;
    hostAddress: String;
    userName: String;
    userPassword: String;
    function connect: Boolean;
    class procedure loadLastStyle();
    procedure setStyle(const style: String);
    function getStyleIndex: Integer;
    procedure loadSettings();
    procedure saveSettings();
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
  connect();
end;

procedure TdmOutlay.dslRequestUpdateData(Sender: TObject);
begin

end;

{
  FDConnectionOutlay.ConnectionString:=
    'DriverID=FB;Protocol=TCPIP;CharacterSet=WIN1251;Server=' + hostAddress
      + ';Database=' + dbName
      + ';User_name=' + userName
      + ';Password=' + userPassword;

}
function TdmOutlay.connect: Boolean;
var
  c: Integer;
begin
  IBDatabase.Connected:= False;
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

procedure TdmOutlay.IBLRequestAfterInsert(DataSet: TDataSet);
begin
  if (DataSet.State in [dsEdit, dsInsert]) then
  begin
    DataSet.FieldByName('PROJECTNAME').AsString:= GetSelectedProjectName();
    DataSet.FieldByName('VAT').AsLargeInt:= GetSelectedProjectVAT();
    DataSet.Modified:= false;
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

end.
