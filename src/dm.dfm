object dmOutlay: TdmOutlay
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 524
  Width = 821
  object dsProject: TDataSource
    DataSet = IBProject
    Left = 56
    Top = 232
  end
  object dsOrgType: TDataSource
    DataSet = IBOrgType
    Left = 172
    Top = 112
  end
  object dsOrgPosition: TDataSource
    DataSet = IBOrgPosition
    Left = 220
    Top = 112
  end
  object dsPerson: TDataSource
    DataSet = IBPerson
    Left = 76
    Top = 112
  end
  object dsSaleType: TDataSource
    DataSet = IBSaleType
    Left = 268
    Top = 112
  end
  object dsVAT: TDataSource
    DataSet = IBVAT
    Left = 324
    Top = 112
  end
  object dsRequest: TDataSource
    DataSet = IBRequest
    Left = 116
    Top = 232
  end
  object dsOrg: TDataSource
    DataSet = IBOrg
    Left = 380
    Top = 112
  end
  object IBDatabase: TIBDatabase
    Connected = True
    DatabaseName = '127.0.0.1:outlay'
    Params.Strings = (
      'lc_ctype=WIN1251'
      'USER_NAME=SYSDBA'
      'PASSWORD=masterkey')
    LoginPrompt = False
    DefaultTransaction = IBTransaction
    ServerType = 'IBServer'
    Left = 16
    Top = 16
  end
  object IBTransaction: TIBTransaction
    Active = True
    AutoStopAction = saCommit
    Left = 76
    Top = 16
  end
  object IBVAT: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from VAT'
      'where'
      '  VAL = :OLD_VAL and'
      '  NAME = :OLD_NAME')
    InsertSQL.Strings = (
      'insert into VAT'
      '  (VAL, NAME)'
      'values'
      '  (:VAL, :NAME)')
    RefreshSQL.Strings = (
      'Select '
      '  VAL,'
      '  NAME'
      'from VAT '
      'where'
      '  VAL = :VAL and'
      '  NAME = :NAME')
    SelectSQL.Strings = (
      'select VAL, NAME from VAT where /*Filter*/ 1=1')
    ModifySQL.Strings = (
      'update VAT'
      'set'
      '  VAL = :VAL,'
      '  NAME = :NAME'
      'where'
      '  VAL = :OLD_VAL and'
      '  NAME = :OLD_NAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 324
    Top = 64
    object IBVATVAL: TLargeintField
      FieldName = 'VAL'
      Origin = '"VAT"."VAL"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object IBVATNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"VAT"."NAME"'
      Size = 1024
    end
  end
  object IBProject: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from PROJECT'
      'where'
      '  NAME = :OLD_NAME')
    InsertSQL.Strings = (
      'insert into PROJECT'
      '  (NAME, SELLERORG, DESCRIPTION, VAT, STAGE, DISCOUNT)'
      'values'
      '  (:NAME, :SELLERORG, :DESCRIPTION, :VAT, :STAGE, :DISCOUNT)')
    RefreshSQL.Strings = (
      'Select '
      '  NAME,'
      '  SELLERORG,'
      '  DESCRIPTION,'
      '  CREATED,'
      '  MODIFIED,'
      '  VAT,'
      '  STAGE,'
      '  DISCOUNT'
      'from PROJECT '
      'where'
      '  NAME = :NAME')
    SelectSQL.Strings = (
      'select  NAME,SELLERORG,DESCRIPTION,VAT, STAGE,DISCOUNT '
      'from "PROJECT" where /*Filter*/ 1=1'
      '')
    ModifySQL.Strings = (
      'update PROJECT'
      'set'
      '  NAME = :NAME,'
      '  SELLERORG = :SELLERORG,'
      '  DESCRIPTION = :DESCRIPTION,'
      '  VAT = :VAT,'
      '  STAGE = :STAGE,'
      '  DISCOUNT = :DISCOUNT'
      'where'
      '  NAME = :OLD_NAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 56
    Top = 184
    object IBProjectNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"PROJECT"."NAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 1024
    end
    object IBProjectSELLERORG: TIBStringField
      FieldName = 'SELLERORG'
      Origin = '"PROJECT"."SELLERORG"'
      Size = 1024
    end
    object IBProjectDESCRIPTION: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"PROJECT"."DESCRIPTION"'
      Size = 4096
    end
    object IBProjectVAT: TLargeintField
      FieldName = 'VAT'
      Origin = '"PROJECT"."VAT"'
    end
    object IBProjectSTAGE: TIBStringField
      FieldName = 'STAGE'
      Origin = '"PROJECT"."STAGE"'
      Size = 16
    end
    object IBProjectDISCOUNT: TFloatField
      FieldName = 'DISCOUNT'
      Origin = '"PROJECT"."DISCOUNT"'
    end
  end
  object IBPerson: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from PERSON'
      'where'
      '  ID = :OLD_ID')
    InsertSQL.Strings = (
      'insert into PERSON'
      
        '  (ID, ORGNAME, POSITIONNAME, PERSONNAME, EMAIL, PHONE, SITE, TA' +
        'G, '
      '  DESCRIPTION, NOTES)'
      'values'
      
        '  (:ID, :ORGNAME, :POSITIONNAME, :PERSONNAME, :EMAIL, :PHONE, :S' +
        'ITE, :TAG, '
      '   :DESCRIPTION, :NOTES)')
    RefreshSQL.Strings = (
      'Select '
      '  ID,'
      '  ORGNAME,'
      '  POSITIONNAME,'
      '  PERSONNAME,'
      '  EMAIL,'
      '  PHONE,'
      '  SITE,'
      '  TAG,'
      '  DESCRIPTION,'
      '  NOTES'
      'from PERSON '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      'select  ID,ORGNAME,POSITIONNAME,PERSONNAME,EMAIL,PHONE,'
      'SITE,TAG,DESCRIPTION,NOTES from PERSON')
    ModifySQL.Strings = (
      'update PERSON'
      'set'
      '  ID = :ID,'
      '  ORGNAME = :ORGNAME,'
      '  POSITIONNAME = :POSITIONNAME,'
      '  PERSONNAME = :PERSONNAME,'
      '  EMAIL = :EMAIL,'
      '  PHONE = :PHONE,'
      '  SITE = :SITE,'
      '  TAG = :TAG,'
      '  DESCRIPTION = :DESCRIPTION,'
      '  NOTES = :NOTES'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    GeneratorField.Field = 'ID'
    GeneratorField.Generator = 'GEN_PERSON_ID'
    Left = 76
    Top = 64
    object IBPersonID: TLargeintField
      FieldName = 'ID'
      Origin = '"PERSON"."ID"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object IBPersonORGNAME: TIBStringField
      FieldName = 'ORGNAME'
      Origin = '"PERSON"."ORGNAME"'
      Size = 1024
    end
    object IBPersonPOSITIONNAME: TIBStringField
      FieldName = 'POSITIONNAME'
      Origin = '"PERSON"."POSITIONNAME"'
      Size = 1024
    end
    object IBPersonPERSONNAME: TIBStringField
      FieldName = 'PERSONNAME'
      Origin = '"PERSON"."PERSONNAME"'
      Required = True
      Size = 1024
    end
    object IBPersonEMAIL: TIBStringField
      FieldName = 'EMAIL'
      Origin = '"PERSON"."EMAIL"'
      Size = 255
    end
    object IBPersonPHONE: TIBStringField
      FieldName = 'PHONE'
      Origin = '"PERSON"."PHONE"'
      Size = 255
    end
    object IBPersonSITE: TIBStringField
      FieldName = 'SITE'
      Origin = '"PERSON"."SITE"'
      Size = 255
    end
    object IBPersonTAG: TIBStringField
      FieldName = 'TAG'
      Origin = '"PERSON"."TAG"'
      Size = 16
    end
    object IBPersonDESCRIPTION: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"PERSON"."DESCRIPTION"'
      Size = 4096
    end
    object IBPersonNOTES: TWideMemoField
      FieldName = 'NOTES'
      Origin = '"PERSON"."NOTES"'
      ProviderFlags = [pfInUpdate]
      BlobType = ftWideMemo
      Size = 8
    end
  end
  object IBRequest: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from REQUEST'
      'where'
      '  ID = :OLD_ID')
    InsertSQL.Strings = (
      'insert into REQUEST'
      
        '  (ID, PROJECTNAME, NAME, DESCRIPTION, SALETYPE, ORG, VAT, STAGE' +
        ', DISCOUNT)'
      'values'
      
        '  (:ID, :PROJECTNAME, :NAME, :DESCRIPTION, :SALETYPE, :ORG, :VAT' +
        ', :STAGE, '
      '   :DISCOUNT)')
    RefreshSQL.Strings = (
      'Select '
      '  ID,'
      '  PROJECTNAME,'
      '  NAME,'
      '  DESCRIPTION,'
      '  CREATED,'
      '  MODIFIED,'
      '  SALETYPE,'
      '  ORG,'
      '  VAT,'
      '  STAGE,'
      '  DISCOUNT'
      'from REQUEST '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      'select ID,STAGE,PROJECTNAME,NAME,DESCRIPTION,'
      'SALETYPE,ORG,VAT,DISCOUNT'
      'from REQUEST')
    ModifySQL.Strings = (
      'update REQUEST'
      'set'
      '  ID = :ID,'
      '  PROJECTNAME = :PROJECTNAME,'
      '  NAME = :NAME,'
      '  DESCRIPTION = :DESCRIPTION,'
      '  SALETYPE = :SALETYPE,'
      '  ORG = :ORG,'
      '  VAT = :VAT,'
      '  STAGE = :STAGE,'
      '  DISCOUNT = :DISCOUNT'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    GeneratorField.Field = 'ID'
    GeneratorField.Generator = 'GEN_REQUEST_ID'
    Active = True
    Left = 116
    Top = 183
    object IBRequestID: TLargeintField
      FieldName = 'ID'
      Origin = '"REQUEST"."ID"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object IBRequestPROJECTNAME: TIBStringField
      FieldName = 'PROJECTNAME'
      Origin = '"REQUEST"."PROJECTNAME"'
      Size = 1024
    end
    object IBRequestNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"REQUEST"."NAME"'
      Size = 1024
    end
    object IBRequestDESCRIPTION: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"REQUEST"."DESCRIPTION"'
      Size = 4096
    end
    object IBRequestSALETYPE: TIBStringField
      FieldName = 'SALETYPE'
      Origin = '"REQUEST"."SALETYPE"'
      Size = 1024
    end
    object IBRequestORG: TIBStringField
      FieldName = 'ORG'
      Origin = '"REQUEST"."ORG"'
      Size = 1024
    end
    object IBRequestVAT: TLargeintField
      FieldName = 'VAT'
      Origin = '"REQUEST"."VAT"'
    end
    object IBRequestSTAGE: TIBStringField
      FieldName = 'STAGE'
      Origin = '"REQUEST"."STAGE"'
      Size = 16
    end
    object IBRequestDISCOUNT: TFloatField
      FieldName = 'DISCOUNT'
      Origin = '"REQUEST"."DISCOUNT"'
    end
  end
  object IBOrgType: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from ORGTYPE'
      'where'
      '  VAL = :OLD_VAL')
    InsertSQL.Strings = (
      'insert into ORGTYPE'
      '  (VAL, DESCRIPTION)'
      'values'
      '  (:VAL, :DESCRIPTION)')
    RefreshSQL.Strings = (
      'Select '
      '  VAL,'
      '  DESCRIPTION'
      'from ORGTYPE '
      'where'
      '  VAL = :VAL')
    SelectSQL.Strings = (
      'select VAL,DESCRIPTION'
      'from ORGTYPE')
    ModifySQL.Strings = (
      'update ORGTYPE'
      'set'
      '  VAL = :VAL,'
      '  DESCRIPTION = :DESCRIPTION'
      'where'
      '  VAL = :OLD_VAL')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 172
    Top = 64
    object IBOrgTypeVAL: TIBStringField
      FieldName = 'VAL'
      Origin = '"ORGTYPE"."VAL"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 16
    end
    object IBOrgTypeDESCRIPTION: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"ORGTYPE"."DESCRIPTION"'
      Size = 4096
    end
  end
  object IBOrgPosition: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from ORGPOSITION'
      'where'
      '  NAME = :OLD_NAME')
    InsertSQL.Strings = (
      'insert into ORGPOSITION'
      '  (NAME, DESCRIPTION)'
      'values'
      '  (:NAME, :DESCRIPTION)')
    RefreshSQL.Strings = (
      'Select '
      '  NAME,'
      '  DESCRIPTION'
      'from ORGPOSITION '
      'where'
      '  NAME = :NAME')
    SelectSQL.Strings = (
      'select NAME, DESCRIPTION from ORGPOSITION')
    ModifySQL.Strings = (
      'update ORGPOSITION'
      'set'
      '  NAME = :NAME,'
      '  DESCRIPTION = :DESCRIPTION'
      'where'
      '  NAME = :OLD_NAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 220
    Top = 64
    object IBOrgPositionNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"ORGPOSITION"."NAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 1024
    end
    object IBOrgPositionDESCRIPTION: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"ORGPOSITION"."DESCRIPTION"'
      Size = 4096
    end
  end
  object IBSaleType: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from SALETYPE'
      'where'
      '  NAME = :OLD_NAME')
    InsertSQL.Strings = (
      'insert into SALETYPE'
      '  (NAME, DESCRIPTION)'
      'values'
      '  (:NAME, :DESCRIPTION)')
    RefreshSQL.Strings = (
      'Select '
      '  NAME,'
      '  DESCRIPTION'
      'from SALETYPE '
      'where'
      '  NAME = :NAME')
    SelectSQL.Strings = (
      'select NAME, DESCRIPTION from SALETYPE')
    ModifySQL.Strings = (
      'update SALETYPE'
      'set'
      '  NAME = :NAME,'
      '  DESCRIPTION = :DESCRIPTION'
      'where'
      '  NAME = :OLD_NAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 268
    Top = 64
    object IBSaleTypeNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"SALETYPE"."NAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 1024
    end
    object IBSaleTypeDESCRIPTION: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"SALETYPE"."DESCRIPTION"'
      Size = 4096
    end
  end
  object IBOrg: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from ORG'
      'where'
      '  ORGNAME = :OLD_ORGNAME')
    InsertSQL.Strings = (
      'insert into ORG'
      
        '  (ORGTYPE, ORGNAME, FULLNAME, ALTNAME, INN, DESCRIPTION, TAG, C' +
        'REATED, '
      '   MODIFIED, LOGO, OWNER)'
      'values'
      
        '  (:ORGTYPE, :ORGNAME, :FULLNAME, :ALTNAME, :INN, :DESCRIPTION, ' +
        ':TAG, :CREATED, '
      '   :MODIFIED, :LOGO, :OWNER)')
    RefreshSQL.Strings = (
      'Select '
      '  ORGTYPE,'
      '  ORGNAME,'
      '  FULLNAME,'
      '  ALTNAME,'
      '  INN,'
      '  DESCRIPTION,'
      '  TAG,'
      '  CREATED,'
      '  MODIFIED,'
      '  LOGO,'
      '  OWNER'
      'from ORG '
      'where'
      '  ORGNAME = :ORGNAME')
    SelectSQL.Strings = (
      
        'select ORGTYPE,ORGNAME,FULLNAME,ALTNAME,INN,DESCRIPTION,TAG,CREA' +
        'TED,MODIFIED,LOGO,OWNER'
      'from ORG')
    ModifySQL.Strings = (
      'update ORG'
      'set'
      '  ORGTYPE = :ORGTYPE,'
      '  ORGNAME = :ORGNAME,'
      '  FULLNAME = :FULLNAME,'
      '  ALTNAME = :ALTNAME,'
      '  INN = :INN,'
      '  DESCRIPTION = :DESCRIPTION,'
      '  TAG = :TAG,'
      '  CREATED = :CREATED,'
      '  MODIFIED = :MODIFIED,'
      '  LOGO = :LOGO,'
      '  OWNER = :OWNER'
      'where'
      '  ORGNAME = :OLD_ORGNAME')
    ParamCheck = True
    UniDirectional = False
    Left = 380
    Top = 64
    object IBOrgORGTYPE: TIBStringField
      FieldName = 'ORGTYPE'
      Origin = '"ORG"."ORGTYPE"'
      Size = 16
    end
    object IBOrgORGNAME: TIBStringField
      FieldName = 'ORGNAME'
      Origin = '"ORG"."ORGNAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 1024
    end
    object IBOrgFULLNAME: TIBStringField
      FieldName = 'FULLNAME'
      Origin = '"ORG"."FULLNAME"'
      Size = 1024
    end
    object IBOrgALTNAME: TIBStringField
      FieldName = 'ALTNAME'
      Origin = '"ORG"."ALTNAME"'
      Size = 1024
    end
    object IBOrgINN: TLargeintField
      FieldName = 'INN'
      Origin = '"ORG"."INN"'
    end
    object IBOrgDESCRIPTION: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"ORG"."DESCRIPTION"'
      Size = 4096
    end
    object IBOrgTAG: TIBStringField
      FieldName = 'TAG'
      Origin = '"ORG"."TAG"'
      Size = 16
    end
    object IBOrgCREATED: TDateTimeField
      FieldName = 'CREATED'
      Origin = '"ORG"."CREATED"'
    end
    object IBOrgMODIFIED: TDateTimeField
      FieldName = 'MODIFIED'
      Origin = '"ORG"."MODIFIED"'
    end
    object IBOrgLOGO: TBlobField
      FieldName = 'LOGO'
      Origin = '"ORG"."LOGO"'
      ProviderFlags = [pfInUpdate]
      Size = 8
    end
    object IBOrgOWNER: TIBStringField
      FieldName = 'OWNER'
      Origin = '"ORG"."OWNER"'
      Size = 1024
    end
  end
  object IBCategory: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from CATEGORY'
      'where'
      '  NAME = :OLD_NAME')
    InsertSQL.Strings = (
      'insert into CATEGORY'
      '  (NAME, PARENT, DESCRIPTION)'
      'values'
      '  (:NAME, :PARENT, :DESCRIPTION)')
    RefreshSQL.Strings = (
      'Select '
      '  NAME,'
      '  PARENT,'
      '  DESCRIPTION'
      'from CATEGORY '
      'where'
      '  NAME = :NAME')
    SelectSQL.Strings = (
      'select NAME, PARENT, DESCRIPTION from CATEGORY')
    ModifySQL.Strings = (
      'update CATEGORY'
      'set'
      '  NAME = :NAME,'
      '  PARENT = :PARENT,'
      '  DESCRIPTION = :DESCRIPTION'
      'where'
      '  NAME = :OLD_NAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 444
    Top = 64
    object IBCategoryNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"CATEGORY"."NAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 1024
    end
    object IBCategoryPARENT: TIBStringField
      FieldName = 'PARENT'
      Origin = '"CATEGORY"."PARENT"'
      Size = 1024
    end
    object IBCategoryDESCRIPTION: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"CATEGORY"."DESCRIPTION"'
      Size = 4096
    end
  end
  object dsCategory: TDataSource
    DataSet = IBCategory
    Left = 436
    Top = 112
  end
  object IBMeasureUnit: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from MEASUREUNIT'
      'where'
      '  NAME = :OLD_NAME')
    InsertSQL.Strings = (
      'insert into MEASUREUNIT'
      '  (NAME, FULLNAME)'
      'values'
      '  (:NAME, :FULLNAME)')
    RefreshSQL.Strings = (
      'Select '
      '  NAME,'
      '  FULLNAME'
      'from MEASUREUNIT '
      'where'
      '  NAME = :NAME')
    SelectSQL.Strings = (
      'select NAME, FULLNAME from MEASUREUNIT')
    ModifySQL.Strings = (
      'update MEASUREUNIT'
      'set'
      '  NAME = :NAME,'
      '  FULLNAME = :FULLNAME'
      'where'
      '  NAME = :OLD_NAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 500
    Top = 64
    object IBMeasureUnitNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"MEASUREUNIT"."NAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 1024
    end
    object IBMeasureUnitFULLNAME: TIBStringField
      FieldName = 'FULLNAME'
      Origin = '"MEASUREUNIT"."FULLNAME"'
      Size = 1024
    end
  end
  object dsMeasureUnit: TDataSource
    DataSet = IBMeasureUnit
    Left = 500
    Top = 112
  end
  object IBPart: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from PART'
      'where'
      '  ID = :OLD_ID')
    InsertSQL.Strings = (
      'insert into PART'
      
        '  (ID, CATEGORY, VENDOR, PARTNO, NAME, DESCRIPTION, MEASUREUNIT,' +
        ' VOL, WEIGHT, '
      '   WIDTH, HEIGHT, LENGTH)'
      'values'
      
        '  (:ID, :CATEGORY, :VENDOR, :PARTNO, :NAME, :DESCRIPTION, :MEASU' +
        'REUNIT, '
      '   :VOL, :WEIGHT, :WIDTH, :HEIGHT, :LENGTH)')
    RefreshSQL.Strings = (
      'Select '
      '  ID,'
      '  CATEGORY,'
      '  VENDOR,'
      '  PARTNO,'
      '  NAME,'
      '  DESCRIPTION,'
      '  MEASUREUNIT,'
      '  CREATED,'
      '  MODIFIED,'
      '  WEIGHT,'
      '  WIDTH,'
      '  HEIGHT,'
      '  LENGTH,'
      '  VOL'
      'from PART '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      
        'select ID,CATEGORY,VENDOR,PARTNO,NAME,DESCRIPTION,MEASUREUNIT,VO' +
        'L,WEIGHT,WIDTH,HEIGHT,"LENGTH"'
      'from PART')
    ModifySQL.Strings = (
      'update PART'
      'set'
      '  ID = :ID,'
      '  CATEGORY = :CATEGORY,'
      '  VENDOR = :VENDOR,'
      '  PARTNO = :PARTNO,'
      '  NAME = :NAME,'
      '  DESCRIPTION = :DESCRIPTION,'
      '  MEASUREUNIT = :MEASUREUNIT,'
      '  VOL = :VOL,'
      '  WEIGHT = :WEIGHT,'
      '  WIDTH = :WIDTH,'
      '  HEIGHT = :HEIGHT,'
      '  LENGTH = :LENGTH'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    GeneratorField.Field = 'ID'
    GeneratorField.Generator = 'GEN_PART_ID'
    Active = True
    Left = 568
    Top = 64
    object IBPartID: TLargeintField
      FieldName = 'ID'
      Origin = '"PART"."ID"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object IBPartCATEGORY: TIBStringField
      FieldName = 'CATEGORY'
      Origin = '"PART"."CATEGORY"'
      Size = 1024
    end
    object IBPartVENDOR: TIBStringField
      FieldName = 'VENDOR'
      Origin = '"PART"."VENDOR"'
      Size = 1024
    end
    object IBPartPARTNO: TIBStringField
      FieldName = 'PARTNO'
      Origin = '"PART"."PARTNO"'
      Required = True
      Size = 1024
    end
    object IBPartNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"PART"."NAME"'
      Required = True
      Size = 1024
    end
    object IBPartDESCRIPTION: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"PART"."DESCRIPTION"'
      Size = 4096
    end
    object IBPartMEASUREUNIT: TIBStringField
      FieldName = 'MEASUREUNIT'
      Origin = '"PART"."MEASUREUNIT"'
      Size = 16
    end
    object IBPartVOL: TFloatField
      FieldName = 'VOL'
      Origin = '"PART"."VOL"'
    end
    object IBPartWEIGHT: TFloatField
      FieldName = 'WEIGHT'
      Origin = '"PART"."WEIGHT"'
    end
    object IBPartWIDTH: TFloatField
      FieldName = 'WIDTH'
      Origin = '"PART"."WIDTH"'
    end
    object IBPartHEIGHT: TFloatField
      FieldName = 'HEIGHT'
      Origin = '"PART"."HEIGHT"'
    end
    object IBPartLENGTH: TFloatField
      FieldName = 'LENGTH'
      Origin = '"PART"."LENGTH"'
    end
  end
  object dsPart: TDataSource
    DataSet = IBPart
    Left = 564
    Top = 112
  end
  object ibPrice: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from PRICE'
      'where'
      '  ID = :OLD_ID')
    InsertSQL.Strings = (
      'insert into PRICE'
      '  (PARTNAME, ORGNAME, CURRENCY, PRICE, SRC, NOTES)'
      'values'
      '  (:PARTNAME, :ORGNAME, :CURRENCY, :PRICE, :SRC, :NOTES)')
    RefreshSQL.Strings = (
      'Select '
      '  ID,'
      '  PARTNAME,'
      '  ORGNAME,'
      '  CURRENCY,'
      '  PRICE,'
      '  CREATED,'
      '  MODIFIED,'
      '  SRC,'
      '  NOTES'
      'from PRICE '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      'select ID,PARTNAME,ORGNAME,CURRENCY,PRICE,SRC,NOTES'
      'from PRICE')
    ModifySQL.Strings = (
      'update PRICE'
      'set'
      '  PARTNAME = :PARTNAME,'
      '  ORGNAME = :ORGNAME,'
      '  CURRENCY = :CURRENCY,'
      '  PRICE = :PRICE,'
      '  SRC = :SRC,'
      '  NOTES = :NOTES'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    GeneratorField.Field = 'ID'
    GeneratorField.Generator = 'GEN_PRICE_ID'
    Active = True
    Left = 624
    Top = 64
    object ibPriceID: TLargeintField
      FieldName = 'ID'
      Origin = '"PRICE"."ID"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object ibPricePARTNAME: TIBStringField
      FieldName = 'PARTNAME'
      Origin = '"PRICE"."PARTNAME"'
      Size = 1024
    end
    object ibPriceORGNAME: TIBStringField
      FieldName = 'ORGNAME'
      Origin = '"PRICE"."ORGNAME"'
      Size = 1024
    end
    object ibPriceCURRENCY: TIBStringField
      FieldName = 'CURRENCY'
      Origin = '"PRICE"."CURRENCY"'
      Size = 1024
    end
    object ibPricePRICE: TFloatField
      FieldName = 'PRICE'
      Origin = '"PRICE"."PRICE"'
    end
    object ibPriceSRC: TIBStringField
      FieldName = 'SRC'
      Origin = '"PRICE"."SRC"'
      Size = 255
    end
    object ibPriceNOTES: TIBStringField
      FieldName = 'NOTES'
      Origin = '"PRICE"."NOTES"'
      Size = 4096
    end
  end
  object dsPrice: TDataSource
    DataSet = ibPrice
    Left = 620
    Top = 112
  end
  object IBLRequestCurrencyRate: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    AfterInsert = IBLRequestCurrencyRateAfterInsert
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from REQUESTCURRENCYRATE'
      'where'
      '  ID = :OLD_ID')
    InsertSQL.Strings = (
      'insert into REQUESTCURRENCYRATE'
      '  (ID, REQUESTID, CURRENCY, VAL)'
      'values'
      '  (:ID, :REQUESTID, :CURRENCY, :VAL)')
    RefreshSQL.Strings = (
      'Select '
      '  ID,'
      '  REQUESTID,'
      '  CURRENCY,'
      '  VAL'
      'from REQUESTCURRENCYRATE '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      'select ID, REQUESTID, CURRENCY, VAL from REQUESTCURRENCYRATE '
      'WHERE REQUESTID = :ID')
    ModifySQL.Strings = (
      'update REQUESTCURRENCYRATE'
      'set'
      '  ID = :ID,'
      '  REQUESTID = :REQUESTID,'
      '  CURRENCY = :CURRENCY,'
      '  VAL = :VAL'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    GeneratorField.Field = 'ID'
    GeneratorField.Generator = 'GEN_REQUESTCURRENCYRATE_ID'
    Active = True
    DataSource = dslRequest
    Left = 528
    Top = 312
    object IBLRequestCurrencyRateID: TLargeintField
      FieldName = 'ID'
      Origin = '"REQUESTCURRENCYRATE"."ID"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object IBLRequestCurrencyRateREQUESTID: TLargeintField
      FieldName = 'REQUESTID'
      Origin = '"REQUESTCURRENCYRATE"."REQUESTID"'
      Required = True
    end
    object IBLRequestCurrencyRateCURRENCY: TIBStringField
      FieldName = 'CURRENCY'
      Origin = '"REQUESTCURRENCYRATE"."CURRENCY"'
      Size = 1024
    end
    object IBLRequestCurrencyRateVAL: TFloatField
      FieldName = 'VAL'
      Origin = '"REQUESTCURRENCYRATE"."VAL"'
    end
  end
  object dsRequestCurrencyRate: TDataSource
    DataSet = IBLRequestCurrencyRate
    Left = 532
    Top = 360
  end
  object IBSpecification: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from SPECIFICATION'
      'where'
      '  ID = :OLD_ID')
    InsertSQL.Strings = (
      'insert into SPECIFICATION'
      
        '  (DISCOUNT, ID, LINENO, NOTES, PARTNAME, PRICE, PRICEID, QTY, R' +
        'EQUESTID, '
      '   TAG, VAT, REBATE)'
      'values'
      
        '  (:DISCOUNT, :ID, :LINENO, :NOTES, :PARTNAME, :PRICE, :PRICEID,' +
        ' :QTY, '
      '   :REQUESTID, :TAG, :VAT, :REBATE)')
    RefreshSQL.Strings = (
      'Select '
      '  ID,'
      '  REQUESTID,'
      '  LINENO,'
      '  PARTNAME,'
      '  QTY,'
      '  PRICEID,'
      '  PRICE,'
      '  DISCOUNT,'
      '  VAT,'
      '  COST,'
      '  TAG,'
      '  NOTES,'
      '  CREATED,'
      '  MODIFIED,'
      '  REBATE'
      'from SPECIFICATION '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      
        'select COST, DISCOUNT, ID, LINENO, NOTES, PARTNAME, PRICE, PRICE' +
        'ID, QTY, REQUESTID, TAG, VAT, REBATE'
      'from SPECIFICATION')
    ModifySQL.Strings = (
      'update SPECIFICATION'
      'set'
      '  DISCOUNT = :DISCOUNT,'
      '  ID = :ID,'
      '  LINENO = :LINENO,'
      '  NOTES = :NOTES,'
      '  PARTNAME = :PARTNAME,'
      '  PRICE = :PRICE,'
      '  PRICEID = :PRICEID,'
      '  QTY = :QTY,'
      '  REQUESTID = :REQUESTID,'
      '  TAG = :TAG,'
      '  VAT = :VAT,'
      '  REBATE = :REBATE'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    GeneratorField.Field = 'ID'
    GeneratorField.Generator = 'GEN_SPECIFICATION_ID'
    Active = True
    Left = 208
    Top = 184
    object IBSpecificationID: TLargeintField
      FieldName = 'ID'
      Origin = '"SPECIFICATION"."ID"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object IBSpecificationREQUESTID: TLargeintField
      FieldName = 'REQUESTID'
      Origin = '"SPECIFICATION"."REQUESTID"'
      Required = True
    end
    object IBSpecificationPARTNAME: TIBStringField
      FieldName = 'PARTNAME'
      Origin = '"SPECIFICATION"."PARTNAME"'
      Required = True
      Size = 1024
    end
    object IBSpecificationPRICEID: TLargeintField
      FieldName = 'PRICEID'
      Origin = '"SPECIFICATION"."PRICEID"'
    end
    object IBSpecificationPRICE: TFloatField
      FieldName = 'PRICE'
      Origin = '"SPECIFICATION"."PRICE"'
    end
    object IBSpecificationLINENO: TLargeintField
      FieldName = 'LINENO'
      Origin = '"SPECIFICATION"."LINENO"'
    end
    object IBSpecificationDISCOUNT: TFloatField
      FieldName = 'DISCOUNT'
      Origin = '"SPECIFICATION"."DISCOUNT"'
    end
    object IBSpecificationQTY: TFloatField
      FieldName = 'QTY'
      Origin = '"SPECIFICATION"."QTY"'
    end
    object IBSpecificationCOST: TFloatField
      FieldKind = fkInternalCalc
      FieldName = 'COST'
      Origin = '"SPECIFICATION"."COST"'
      ProviderFlags = []
      ReadOnly = True
    end
    object IBSpecificationVAT: TLargeintField
      FieldName = 'VAT'
      Origin = '"SPECIFICATION"."VAT"'
      Required = True
    end
    object IBSpecificationNOTES: TIBStringField
      FieldName = 'NOTES'
      Origin = '"SPECIFICATION"."NOTES"'
      Size = 4096
    end
    object IBSpecificationTAG: TIBStringField
      FieldName = 'TAG'
      Origin = '"SPECIFICATION"."TAG"'
      Size = 16
    end
    object IBSpecificationREBATE: TFloatField
      FieldName = 'REBATE'
      Origin = '"SPECIFICATION"."REBATE"'
    end
  end
  object dsSpecification: TDataSource
    DataSet = IBSpecification
    Left = 212
    Top = 240
  end
  object IBCurrency: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from VALIDCURRENCY'
      'where'
      '  CURRENCYSYMBOL = :OLD_CURRENCYSYMBOL')
    InsertSQL.Strings = (
      'insert into VALIDCURRENCY'
      '  (CURRENCYSYMBOL, NAME, DEFAULTRATE, ISLOCAL)'
      'values'
      '  (:CURRENCYSYMBOL, :NAME, :DEFAULTRATE, :ISLOCAL)')
    RefreshSQL.Strings = (
      'Select '
      '  CURRENCYSYMBOL,'
      '  NAME,'
      '  DEFAULTRATE,'
      '  ISLOCAL'
      'from VALIDCURRENCY '
      'where'
      '  CURRENCYSYMBOL = :CURRENCYSYMBOL')
    SelectSQL.Strings = (
      
        'select CURRENCYSYMBOL, NAME, DEFAULTRATE, ISLOCAL from VALIDCURR' +
        'ENCY')
    ModifySQL.Strings = (
      'update VALIDCURRENCY'
      'set'
      '  CURRENCYSYMBOL = :CURRENCYSYMBOL,'
      '  NAME = :NAME,'
      '  DEFAULTRATE = :DEFAULTRATE,'
      '  ISLOCAL = :ISLOCAL'
      'where'
      '  CURRENCYSYMBOL = :OLD_CURRENCYSYMBOL')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 680
    Top = 64
    object IBCurrencyCURRENCYSYMBOL: TIBStringField
      FieldName = 'CURRENCYSYMBOL'
      Origin = '"VALIDCURRENCY"."CURRENCYSYMBOL"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 16
    end
    object IBCurrencyNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"VALIDCURRENCY"."NAME"'
      Required = True
      Size = 16
    end
    object IBCurrencyDEFAULTRATE: TFloatField
      FieldName = 'DEFAULTRATE'
      Origin = '"VALIDCURRENCY"."DEFAULTRATE"'
    end
    object IBCurrencyISLOCAL: TLargeintField
      FieldName = 'ISLOCAL'
      Origin = '"VALIDCURRENCY"."ISLOCAL"'
    end
  end
  object dsValidCurrency: TDataSource
    DataSet = IBCurrency
    Left = 684
    Top = 112
  end
  object IBVendor: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from VENDOR'
      'where'
      '  NAME = :OLD_NAME')
    InsertSQL.Strings = (
      'insert into VENDOR'
      '  (NAME, DESCRIPTION)'
      'values'
      '  (:NAME, :DESCRIPTION)')
    RefreshSQL.Strings = (
      'Select '
      '  NAME,'
      '  DESCRIPTION'
      'from VENDOR '
      'where'
      '  NAME = :NAME')
    SelectSQL.Strings = (
      'select NAME, DESCRIPTION from VENDOR')
    ModifySQL.Strings = (
      'update VENDOR'
      'set'
      '  NAME = :NAME,'
      '  DESCRIPTION = :DESCRIPTION'
      'where'
      '  NAME = :OLD_NAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 736
    Top = 64
    object IBVendorNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"VENDOR"."NAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 1024
    end
    object IBVendorDESCRIPTION: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"VENDOR"."DESCRIPTION"'
      Size = 4096
    end
  end
  object dsVendor: TDataSource
    DataSet = IBVendor
    Left = 740
    Top = 112
  end
  object IBStage: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from STAGE'
      'where'
      '  NAME = :OLD_NAME')
    InsertSQL.Strings = (
      'insert into STAGE'
      '  (NAME, DESCRIPTION)'
      'values'
      '  (:NAME, :DESCRIPTION)')
    RefreshSQL.Strings = (
      'Select '
      '  NAME,'
      '  DESCRIPTION'
      'from STAGE '
      'where'
      '  NAME = :NAME')
    SelectSQL.Strings = (
      'select NAME, DESCRIPTION from STAGE')
    ModifySQL.Strings = (
      'update STAGE'
      'set'
      '  NAME = :NAME,'
      '  DESCRIPTION = :DESCRIPTION'
      'where'
      '  NAME = :OLD_NAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 264
    Top = 184
    object IBStageNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"STAGE"."NAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 16
    end
    object IBStageDESCRIPTION: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"STAGE"."DESCRIPTION"'
      Size = 4096
    end
  end
  object IBLProject: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    AfterInsert = IBLProjectAfterInsert
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from PROJECT'
      'where'
      '  NAME = :OLD_NAME')
    InsertSQL.Strings = (
      'insert into PROJECT'
      '  (NAME, SELLERORG, DESCRIPTION, VAT, STAGE, DISCOUNT)'
      'values'
      '  (:NAME, :SELLERORG, :DESCRIPTION, :VAT, :STAGE, :DISCOUNT)')
    RefreshSQL.Strings = (
      'Select '
      '  NAME,'
      '  SELLERORG,'
      '  DESCRIPTION,'
      '  CREATED,'
      '  MODIFIED,'
      '  VAT,'
      '  STAGE,'
      '  DISCOUNT'
      'from PROJECT '
      'where'
      '  NAME = :NAME')
    SelectSQL.Strings = (
      'select  NAME,SELLERORG,DESCRIPTION,VAT, STAGE, DISCOUNT'
      'from "PROJECT"'
      '')
    ModifySQL.Strings = (
      'update PROJECT'
      'set'
      '  NAME = :NAME,'
      '  SELLERORG = :SELLERORG,'
      '  DESCRIPTION = :DESCRIPTION,'
      '  VAT = :VAT,'
      '  STAGE = :STAGE,'
      '  DISCOUNT = :DISCOUNT'
      'where'
      '  NAME = :OLD_NAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 432
    Top = 184
    object IBStringField1: TIBStringField
      FieldName = 'NAME'
      Origin = '"PROJECT"."NAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 1024
    end
    object IBStringField2: TIBStringField
      FieldName = 'SELLERORG'
      Origin = '"PROJECT"."SELLERORG"'
      Size = 1024
    end
    object IBStringField3: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"PROJECT"."DESCRIPTION"'
      Size = 4096
    end
    object LargeintField1: TLargeintField
      FieldName = 'VAT'
      Origin = '"PROJECT"."VAT"'
    end
    object IBStringField4: TIBStringField
      FieldName = 'STAGE'
      Origin = '"PROJECT"."STAGE"'
      Size = 16
    end
    object IBLProjectDISCOUNT: TFloatField
      FieldName = 'DISCOUNT'
      Origin = '"PROJECT"."DISCOUNT"'
    end
  end
  object IBLRequest: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    AfterInsert = IBLRequestAfterInsert
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from REQUEST'
      'where'
      '  ID = :OLD_ID')
    InsertSQL.Strings = (
      'insert into REQUEST'
      
        '  (ID, STAGE, PROJECTNAME, NAME, DESCRIPTION, SALETYPE, ORG, VAT' +
        ', DISCOUNT)'
      'values'
      
        '  (:ID, :STAGE, :PROJECTNAME, :NAME, :DESCRIPTION, :SALETYPE, :O' +
        'RG, :VAT, '
      '   :DISCOUNT)')
    RefreshSQL.Strings = (
      'Select '
      '  ID,'
      '  PROJECTNAME,'
      '  NAME,'
      '  DESCRIPTION,'
      '  CREATED,'
      '  MODIFIED,'
      '  SALETYPE,'
      '  ORG,'
      '  VAT,'
      '  STAGE,'
      '  DISCOUNT'
      'from REQUEST '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      'select ID,STAGE,PROJECTNAME,NAME,DESCRIPTION,'
      'SALETYPE,ORG,VAT, DISCOUNT'
      'from REQUEST where PROJECTNAME = :NAME')
    ModifySQL.Strings = (
      'update REQUEST'
      'set'
      '  ID = :ID,'
      '  STAGE = :STAGE,'
      '  PROJECTNAME = :PROJECTNAME,'
      '  NAME = :NAME,'
      '  DESCRIPTION = :DESCRIPTION,'
      '  SALETYPE = :SALETYPE,'
      '  ORG = :ORG,'
      '  VAT = :VAT,'
      '  DISCOUNT = :DISCOUNT'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    GeneratorField.Field = 'ID'
    GeneratorField.Generator = 'GEN_REQUEST_ID'
    Active = True
    DataSource = dslProject
    Left = 524
    Top = 183
    object LargeintField2: TLargeintField
      FieldName = 'ID'
      Origin = '"REQUEST"."ID"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object IBStringField5: TIBStringField
      FieldName = 'PROJECTNAME'
      Origin = '"REQUEST"."PROJECTNAME"'
      Size = 1024
    end
    object IBStringField6: TIBStringField
      FieldName = 'NAME'
      Origin = '"REQUEST"."NAME"'
      Size = 1024
    end
    object IBStringField7: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"REQUEST"."DESCRIPTION"'
      Size = 4096
    end
    object IBStringField8: TIBStringField
      FieldName = 'SALETYPE'
      Origin = '"REQUEST"."SALETYPE"'
      Size = 1024
    end
    object IBStringField9: TIBStringField
      FieldName = 'ORG'
      Origin = '"REQUEST"."ORG"'
      Size = 1024
    end
    object LargeintField3: TLargeintField
      FieldName = 'VAT'
      Origin = '"REQUEST"."VAT"'
    end
    object IBStringField10: TIBStringField
      FieldName = 'STAGE'
      Origin = '"REQUEST"."STAGE"'
      Size = 16
    end
    object IBLRequestDISCOUNT: TFloatField
      FieldName = 'DISCOUNT'
      Origin = '"REQUEST"."DISCOUNT"'
    end
  end
  object dslProject: TDataSource
    DataSet = IBLProject
    Left = 432
    Top = 248
  end
  object dslRequest: TDataSource
    DataSet = IBLRequest
    Left = 528
    Top = 248
  end
  object IBLSpecification: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    AfterInsert = IBLSpecificationAfterInsert
    AfterOpen = IBLSpecificationAfterOpen
    AfterPost = IBLSpecificationAfterPost
    OnCalcFields = IBLSpecificationCalcFields
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from SPECIFICATION'
      'where'
      '  ID = :OLD_ID')
    InsertSQL.Strings = (
      'insert into SPECIFICATION'
      
        '  (ID, REQUESTID, LINENO, PARTNAME, QTY, PRICEID, PRICE, DISCOUN' +
        'T, VAT, '
      '   TAG, NOTES, REBATE)'
      'values'
      
        '  (:ID, :REQUESTID, :LINENO, :PARTNAME, :QTY, :PRICEID, :PRICE, ' +
        ':DISCOUNT, '
      '   :VAT, :TAG, :NOTES, :REBATE)')
    RefreshSQL.Strings = (
      'Select '
      '  ID,'
      '  REQUESTID,'
      '  LINENO,'
      '  PARTNAME,'
      '  QTY,'
      '  PRICEID,'
      '  PRICE,'
      '  DISCOUNT,'
      '  VAT,'
      '  COST,'
      '  TAG,'
      '  NOTES,'
      '  CREATED,'
      '  MODIFIED,'
      '  REBATE'
      'from SPECIFICATION '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      
        'select  ID,REQUESTID,LINENO,PARTNAME,QTY,PRICEID,PRICE,DISCOUNT,' +
        'VAT,COST,TAG,NOTES,REBATE'
      'from SPECIFICATION where REQUESTID = :ID')
    ModifySQL.Strings = (
      'update SPECIFICATION'
      'set'
      '  ID = :ID,'
      '  REQUESTID = :REQUESTID,'
      '  LINENO = :LINENO,'
      '  PARTNAME = :PARTNAME,'
      '  QTY = :QTY,'
      '  PRICEID = :PRICEID,'
      '  PRICE = :PRICE,'
      '  DISCOUNT = :DISCOUNT,'
      '  VAT = :VAT,'
      '  TAG = :TAG,'
      '  NOTES = :NOTES,'
      '  REBATE = :REBATE'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    GeneratorField.Field = 'ID'
    GeneratorField.Generator = 'GEN_SPECIFICATION_ID'
    Active = True
    DataSource = dslRequest
    Left = 600
    Top = 184
    object LargeintField4: TLargeintField
      FieldName = 'ID'
      Origin = '"SPECIFICATION"."ID"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object LargeintField5: TLargeintField
      FieldName = 'REQUESTID'
      Origin = '"SPECIFICATION"."REQUESTID"'
      Required = True
    end
    object LargeintField6: TLargeintField
      FieldName = 'LINENO'
      Origin = '"SPECIFICATION"."LINENO"'
    end
    object IBStringField11: TIBStringField
      FieldName = 'PARTNAME'
      Origin = '"SPECIFICATION"."PARTNAME"'
      Required = True
      Size = 1024
    end
    object FloatField1: TFloatField
      FieldName = 'QTY'
      Origin = '"SPECIFICATION"."QTY"'
    end
    object LargeintField7: TLargeintField
      FieldName = 'PRICEID'
      Origin = '"SPECIFICATION"."PRICEID"'
    end
    object FloatField2: TFloatField
      FieldName = 'PRICE'
      Origin = '"SPECIFICATION"."PRICE"'
    end
    object FloatField3: TFloatField
      FieldName = 'DISCOUNT'
      Origin = '"SPECIFICATION"."DISCOUNT"'
    end
    object LargeintField8: TLargeintField
      FieldName = 'VAT'
      Origin = '"SPECIFICATION"."VAT"'
      Required = True
    end
    object FloatField4: TFloatField
      FieldKind = fkInternalCalc
      FieldName = 'COST'
      Origin = '"SPECIFICATION"."COST"'
      ProviderFlags = []
      ReadOnly = True
    end
    object IBStringField12: TIBStringField
      FieldName = 'TAG'
      Origin = '"SPECIFICATION"."TAG"'
      Size = 16
    end
    object IBStringField13: TIBStringField
      FieldName = 'NOTES'
      Origin = '"SPECIFICATION"."NOTES"'
      Size = 4096
    end
    object IBLSpecificationREBATE: TFloatField
      FieldName = 'REBATE'
      Origin = '"SPECIFICATION"."REBATE"'
    end
    object IBLSpecificationPRICEVAL: TStringField
      FieldKind = fkLookup
      FieldName = 'PRICEVAL'
      LookupDataSet = IBSpecPrice
      LookupKeyFields = 'ID'
      LookupResultField = 'VAL'
      KeyFields = 'PRICEID'
      Size = 1024
      Lookup = True
    end
    object IBLSpecificationPRICEORGNAME: TStringField
      FieldKind = fkLookup
      FieldName = 'PRICEORGNAME'
      LookupDataSet = IBSpecPrice
      LookupKeyFields = 'ID'
      LookupResultField = 'ORGNAME'
      KeyFields = 'PRICEID'
      Size = 1024
      Lookup = True
    end
    object IBLSpecificationPRICEPRICE: TCurrencyField
      FieldKind = fkLookup
      FieldName = 'PRICEPRICE'
      LookupDataSet = IBSpecPrice
      LookupKeyFields = 'ID'
      LookupResultField = 'PRICE'
      KeyFields = 'PRICEID'
      Lookup = True
    end
    object IBLSpecificationPRICECURRENCY: TStringField
      FieldKind = fkLookup
      FieldName = 'PRICECURRENCY'
      LookupDataSet = IBSpecPrice
      LookupKeyFields = 'ID'
      LookupResultField = 'CURRENCY'
      KeyFields = 'PRICEID'
      Size = 1024
      Lookup = True
    end
    object IBLSpecificationPRICESRC: TStringField
      FieldKind = fkLookup
      FieldName = 'PRICESRC'
      LookupDataSet = IBSpecPrice
      LookupKeyFields = 'ID'
      LookupResultField = 'SRC'
      KeyFields = 'PRICEID'
      Size = 255
      Lookup = True
    end
    object IBLSpecificationPRICENOTES: TStringField
      FieldKind = fkLookup
      FieldName = 'PRICENOTES'
      LookupDataSet = IBSpecPrice
      LookupKeyFields = 'ID'
      LookupResultField = 'NOTES'
      KeyFields = 'PRICEID'
      Size = 4096
      Lookup = True
    end
    object IBLSpecificationPRICEPARTNAME: TStringField
      FieldKind = fkLookup
      FieldName = 'PRICEPARTNAME'
      LookupDataSet = IBSpecPrice
      LookupKeyFields = 'ID'
      LookupResultField = 'PARTNAME'
      KeyFields = 'PRICEID'
      Size = 1024
      Lookup = True
    end
    object IBLSpecificationPRICERUB: TCurrencyField
      FieldKind = fkCalculated
      FieldName = 'PRICERUB'
      Calculated = True
    end
    object IBLSpecificationCOSTLIST: TCurrencyField
      FieldKind = fkCalculated
      FieldName = 'COSTLIST'
      Calculated = True
    end
    object IBLSpecificationCOSTDISCOUNT: TCurrencyField
      FieldKind = fkCalculated
      FieldName = 'COSTDISCOUNT'
      Calculated = True
    end
    object IBLSpecificationPARTID: TLongWordField
      FieldKind = fkLookup
      FieldName = 'PARTID'
      LookupDataSet = IBSpecPart
      LookupKeyFields = 'NAME'
      LookupResultField = 'ID'
      KeyFields = 'PARTNAME'
      Lookup = True
    end
    object IBLSpecificationPARTCATEGORY: TStringField
      FieldKind = fkLookup
      FieldName = 'PARTCATEGORY'
      LookupDataSet = IBSpecPart
      LookupKeyFields = 'NAME'
      LookupResultField = 'CATEGORY'
      KeyFields = 'PARTNAME'
      Size = 1024
      Lookup = True
    end
    object IBLSpecificationPARTVENDOR: TStringField
      FieldKind = fkLookup
      FieldName = 'PARTVENDOR'
      LookupDataSet = IBSpecPart
      LookupKeyFields = 'NAME'
      LookupResultField = 'VENDOR'
      KeyFields = 'PARTNAME'
      Size = 1024
      Lookup = True
    end
    object IBLSpecificationPARTPARTNO: TStringField
      FieldKind = fkLookup
      FieldName = 'PARTNO'
      LookupDataSet = IBSpecPart
      LookupKeyFields = 'NAME'
      LookupResultField = 'PARTNO'
      KeyFields = 'PARTNAME'
      Size = 1024
      Lookup = True
    end
    object IBLSpecificationPARTDESCRIPTION: TStringField
      FieldKind = fkLookup
      FieldName = 'PARTDESCRIPTION'
      LookupDataSet = IBSpecPart
      LookupKeyFields = 'NAME'
      LookupResultField = 'DESCRIPTION'
      KeyFields = 'PARTNAME'
      Size = 1024
      Lookup = True
    end
    object IBLSpecificationPARTMEASUREUNIT: TStringField
      FieldKind = fkLookup
      FieldName = 'PARTMEASUREUNIT'
      LookupDataSet = IBSpecPart
      LookupKeyFields = 'NAME'
      LookupResultField = 'MEASUREUNIT'
      KeyFields = 'PARTNAME'
      Size = 16
      Lookup = True
    end
    object IBLSpecificationPARTWEIGHT: TFloatField
      FieldKind = fkLookup
      FieldName = 'PARTWEIGHT'
      LookupDataSet = IBSpecPart
      LookupKeyFields = 'NAME'
      LookupResultField = 'WEIGHT'
      KeyFields = 'PARTNAME'
      Lookup = True
    end
    object IBLSpecificationPARTLENGTH: TFloatField
      FieldKind = fkLookup
      FieldName = 'PARTLENGTH'
      LookupDataSet = IBSpecPart
      LookupKeyFields = 'NAME'
      LookupResultField = 'LENGTH'
      KeyFields = 'PARTNAME'
      Lookup = True
    end
    object IBLSpecificationPARTWIDTH: TFloatField
      FieldKind = fkLookup
      FieldName = 'PARTWIDTH'
      LookupDataSet = IBSpecPart
      LookupKeyFields = 'NAME'
      LookupResultField = 'WIDTH'
      KeyFields = 'PARTNAME'
      Lookup = True
    end
    object IBLSpecificationPARTHEIGHT: TFloatField
      FieldKind = fkLookup
      FieldName = 'PARTHEIGHT'
      LookupDataSet = IBSpecPart
      LookupKeyFields = 'NAME'
      LookupResultField = 'HEIGHT'
      KeyFields = 'PARTNAME'
      Lookup = True
    end
    object IBLSpecificationPARTVOL: TFloatField
      FieldKind = fkLookup
      FieldName = 'PARTVOL'
      LookupDataSet = IBSpecPart
      LookupKeyFields = 'NAME'
      LookupResultField = 'VOL'
      KeyFields = 'PARTNAME'
      Lookup = True
    end
    object IBLSpecificationPARTVOLSUM: TFloatField
      FieldKind = fkCalculated
      FieldName = 'PARTVOLSUM'
      Calculated = True
    end
    object IBLSpecificationPARTWEIGHTSUM: TFloatField
      FieldKind = fkCalculated
      FieldName = 'PARTWEIGHTSUM'
      Calculated = True
    end
    object IBLSpecificationPRICENDISCOUNT: TCurrencyField
      FieldKind = fkCalculated
      FieldName = 'PRICENDISCOUNT'
      Calculated = True
    end
    object IBLSpecificationCOSTNDISCOUNT: TCurrencyField
      FieldKind = fkCalculated
      FieldName = 'COSTNDISCOUNT'
      Calculated = True
    end
  end
  object dslSpecification: TDataSource
    DataSet = IBLSpecification
    OnDataChange = dslSpecificationDataChange
    Left = 604
    Top = 248
  end
  object IBSpecPrice: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from PRICE'
      'where'
      '  ID = :OLD_ID')
    InsertSQL.Strings = (
      'insert into PRICE'
      '  (ID, PARTNAME, ORGNAME, CURRENCY, PRICE, SRC, NOTES)'
      'values'
      '  (:ID, :PARTNAME, :ORGNAME, :CURRENCY, :PRICE, :SRC, :NOTES)')
    RefreshSQL.Strings = (
      'Select '
      '  ID,'
      '  PARTNAME,'
      '  ORGNAME,'
      '  CURRENCY,'
      '  PRICE,'
      '  CREATED,'
      '  MODIFIED,'
      '  SRC,'
      '  NOTES'
      'from PRICE '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      
        'select ID,PARTNAME,ORGNAME,CURRENCY,PRICE,CREATED,MODIFIED,SRC,N' +
        'OTES,'
      'ORGNAME || '#39' '#39' || round(PRICE, 2) || CURRENCY VAL'
      'from PRICE')
    ModifySQL.Strings = (
      'update PRICE'
      'set'
      '  ID = :ID,'
      '  PARTNAME = :PARTNAME,'
      '  ORGNAME = :ORGNAME,'
      '  CURRENCY = :CURRENCY,'
      '  PRICE = :PRICE,'
      '  SRC = :SRC,'
      '  NOTES = :NOTES'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    GeneratorField.Field = 'ID'
    GeneratorField.Generator = 'GEN_PRICE_ID'
    Active = True
    Left = 600
    Top = 304
    object LargeintField9: TLargeintField
      FieldName = 'ID'
      Origin = '"PRICE"."ID"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object IBStringField14: TIBStringField
      FieldName = 'PARTNAME'
      Origin = '"PRICE"."PARTNAME"'
      Size = 1024
    end
    object IBStringField15: TIBStringField
      FieldName = 'ORGNAME'
      Origin = '"PRICE"."ORGNAME"'
      Size = 1024
    end
    object IBStringField16: TIBStringField
      FieldName = 'CURRENCY'
      Origin = '"PRICE"."CURRENCY"'
      Size = 1024
    end
    object FloatField5: TFloatField
      FieldName = 'PRICE'
      Origin = '"PRICE"."PRICE"'
    end
    object IBStringField17: TIBStringField
      FieldName = 'SRC'
      Origin = '"PRICE"."SRC"'
      Size = 255
    end
    object IBStringField18: TIBStringField
      FieldName = 'NOTES'
      Origin = '"PRICE"."NOTES"'
      Size = 4096
    end
    object IBSpecPriceVAL: TIBStringField
      FieldName = 'VAL'
      ProviderFlags = []
      ReadOnly = True
      Size = 2074
    end
  end
  object IBSpecPart: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from PART'
      'where'
      '  ID = :OLD_ID')
    InsertSQL.Strings = (
      'insert into PART'
      
        '  (CATEGORY, DESCRIPTION, HEIGHT, ID, LENGTH, MEASUREUNIT, NAME,' +
        ' PARTNO, '
      '   VENDOR, WEIGHT, WIDTH, VOL)'
      'values'
      
        '  (:CATEGORY, :DESCRIPTION, :HEIGHT, :ID, :LENGTH, :MEASUREUNIT,' +
        ' :NAME, '
      '   :PARTNO, :VENDOR, :WEIGHT, :WIDTH, :VOL)')
    RefreshSQL.Strings = (
      'Select '
      '  ID,'
      '  CATEGORY,'
      '  VENDOR,'
      '  PARTNO,'
      '  NAME,'
      '  DESCRIPTION,'
      '  MEASUREUNIT,'
      '  CREATED,'
      '  MODIFIED,'
      '  WEIGHT,'
      '  WIDTH,'
      '  HEIGHT,'
      '  LENGTH,'
      '  VOL'
      'from PART '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      
        'select CATEGORY, DESCRIPTION, HEIGHT, ID, LENGTH, MEASUREUNIT, N' +
        'AME, PARTNO, VENDOR, WEIGHT, WIDTH, VOL from PART')
    ModifySQL.Strings = (
      'update PART'
      'set'
      '  CATEGORY = :CATEGORY,'
      '  DESCRIPTION = :DESCRIPTION,'
      '  HEIGHT = :HEIGHT,'
      '  ID = :ID,'
      '  LENGTH = :LENGTH,'
      '  MEASUREUNIT = :MEASUREUNIT,'
      '  NAME = :NAME,'
      '  PARTNO = :PARTNO,'
      '  VENDOR = :VENDOR,'
      '  WEIGHT = :WEIGHT,'
      '  WIDTH = :WIDTH,'
      '  VOL = :VOL'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    GeneratorField.Field = 'ID'
    GeneratorField.Generator = 'GEN_PRICE_ID'
    Active = True
    Left = 680
    Top = 304
    object IBSpecPartID: TLargeintField
      FieldName = 'ID'
      Origin = '"PART"."ID"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object IBSpecPartPARTNO: TIBStringField
      FieldName = 'PARTNO'
      Origin = '"PART"."PARTNO"'
      Required = True
      Size = 1024
    end
    object IBSpecPartNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"PART"."NAME"'
      Required = True
      Size = 1024
    end
    object IBSpecPartCATEGORY: TIBStringField
      FieldName = 'CATEGORY'
      Origin = '"PART"."CATEGORY"'
      Size = 1024
    end
    object IBSpecPartVENDOR: TIBStringField
      FieldName = 'VENDOR'
      Origin = '"PART"."VENDOR"'
      Size = 1024
    end
    object IBSpecPartDESCRIPTION: TIBStringField
      FieldName = 'DESCRIPTION'
      Origin = '"PART"."DESCRIPTION"'
      Size = 4096
    end
    object IBSpecPartLENGTH: TFloatField
      FieldName = 'LENGTH'
      Origin = '"PART"."LENGTH"'
    end
    object IBSpecPartWIDTH: TFloatField
      FieldName = 'WIDTH'
      Origin = '"PART"."WIDTH"'
    end
    object IBSpecPartHEIGHT: TFloatField
      FieldName = 'HEIGHT'
      Origin = '"PART"."HEIGHT"'
    end
    object IBSpecPartWEIGHT: TFloatField
      FieldName = 'WEIGHT'
      Origin = '"PART"."WEIGHT"'
    end
    object IBSpecPartMEASUREUNIT: TIBStringField
      FieldName = 'MEASUREUNIT'
      Origin = '"PART"."MEASUREUNIT"'
      Size = 16
    end
    object IBSpecPartVOL: TFloatField
      FieldName = 'VOL'
      Origin = '"PART"."VOL"'
    end
  end
end
