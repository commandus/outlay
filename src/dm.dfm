object dmOutlay: TdmOutlay
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 524
  Width = 821
  object dsProject: TDataSource
    DataSet = IBProject
    Left = 96
    Top = 240
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
    Left = 252
    Top = 240
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
      '  (NAME, SELLERORG, DESCRIPTION, VAT, STAGE)'
      'values'
      '  (:NAME, :SELLERORG, :DESCRIPTION, :VAT, :STAGE)')
    RefreshSQL.Strings = (
      'Select '
      '  NAME,'
      '  SELLERORG,'
      '  DESCRIPTION,'
      '  VAT,'
      '  STAGE'
      'from PROJECT '
      'where'
      '  NAME = :NAME')
    SelectSQL.Strings = (
      'select  NAME,SELLERORG,DESCRIPTION,VAT, STAGE '
      'from "PROJECT" where /*Filter*/ 1=1'
      '')
    ModifySQL.Strings = (
      'update PROJECT'
      'set'
      '  NAME = :NAME,'
      '  SELLERORG = :SELLERORG,'
      '  DESCRIPTION = :DESCRIPTION,'
      '  VAT = :VAT,'
      '  STAGE = :STAGE'
      'where'
      '  NAME = :OLD_NAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 96
    Top = 192
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
      
        '  (ID, PROJECTNAME, NAME, DESCRIPTION, CREATED, MODIFIED, SALETY' +
        'PE, ORG, '
      '   VAT, STAGE)'
      'values'
      
        '  (:ID, :PROJECTNAME, :NAME, :DESCRIPTION, :CREATED, :MODIFIED, ' +
        ':SALETYPE, '
      '   :ORG, :VAT, :STAGE)')
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
      '  STAGE'
      'from REQUEST '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      'select ID,STAGE,PROJECTNAME,NAME,DESCRIPTION,CREATED,MODIFIED,'
      'SALETYPE,ORG,VAT'
      'from REQUEST')
    ModifySQL.Strings = (
      'update REQUEST'
      'set'
      '  ID = :ID,'
      '  PROJECTNAME = :PROJECTNAME,'
      '  NAME = :NAME,'
      '  DESCRIPTION = :DESCRIPTION,'
      '  CREATED = :CREATED,'
      '  MODIFIED = :MODIFIED,'
      '  SALETYPE = :SALETYPE,'
      '  ORG = :ORG,'
      '  VAT = :VAT,'
      '  STAGE = :STAGE'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    GeneratorField.Field = 'ID'
    GeneratorField.Generator = 'GEN_REQUEST_ID'
    Active = True
    Left = 252
    Top = 191
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
    object IBRequestCREATED: TDateTimeField
      FieldName = 'CREATED'
      Origin = '"REQUEST"."CREATED"'
    end
    object IBRequestMODIFIED: TDateTimeField
      FieldName = 'MODIFIED'
      Origin = '"REQUEST"."MODIFIED"'
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
        ' CREATED, '
      '   MODIFIED, WEIGHT, WIDTH, HEIGHT, LENGTH)'
      'values'
      
        '  (:ID, :CATEGORY, :VENDOR, :PARTNO, :NAME, :DESCRIPTION, :MEASU' +
        'REUNIT, '
      '   :CREATED, :MODIFIED, :WEIGHT, :WIDTH, :HEIGHT, :LENGTH)')
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
      '  LENGTH'
      'from PART '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      
        'select ID,CATEGORY,VENDOR,PARTNO,NAME,DESCRIPTION,MEASUREUNIT,CR' +
        'EATED,MODIFIED,WEIGHT,WIDTH,HEIGHT,"LENGTH"'
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
      '  CREATED = :CREATED,'
      '  MODIFIED = :MODIFIED,'
      '  WEIGHT = :WEIGHT,'
      '  WIDTH = :WIDTH,'
      '  HEIGHT = :HEIGHT,'
      '  LENGTH = :LENGTH'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
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
    object IBPartCREATED: TDateTimeField
      FieldName = 'CREATED'
      Origin = '"PART"."CREATED"'
    end
    object IBPartMODIFIED: TDateTimeField
      FieldName = 'MODIFIED'
      Origin = '"PART"."MODIFIED"'
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
      
        '  (ID, PARTNAME, ORGNAME, CURRENCY, PRICE, CREATED, MODIFIED, SR' +
        'C, NOTES)'
      'values'
      
        '  (:ID, :PARTNAME, :ORGNAME, :CURRENCY, :PRICE, :CREATED, :MODIF' +
        'IED, :SRC, '
      '   :NOTES)')
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
        'OTES'
      'from PRICE')
    ModifySQL.Strings = (
      'update PRICE'
      'set'
      '  ID = :ID,'
      '  PARTNAME = :PARTNAME,'
      '  ORGNAME = :ORGNAME,'
      '  CURRENCY = :CURRENCY,'
      '  PRICE = :PRICE,'
      '  CREATED = :CREATED,'
      '  MODIFIED = :MODIFIED,'
      '  SRC = :SRC,'
      '  NOTES = :NOTES'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
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
    object ibPriceCREATED: TDateTimeField
      FieldName = 'CREATED'
      Origin = '"PRICE"."CREATED"'
    end
    object ibPriceMODIFIED: TDateTimeField
      FieldName = 'MODIFIED'
      Origin = '"PRICE"."MODIFIED"'
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
  object IBRequestCurrencyRate: TIBDataSet
    Database = IBDatabase
    Transaction = IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    SelectSQL.Strings = (
      'select ID, REQUESTID, CURRENCY, VAL from REQUESTCURRENCYRATE')
    ParamCheck = True
    UniDirectional = False
    Active = True
    DataSource = dsRequest
    Left = 248
    Top = 296
    object IBRequestCurrencyRateID: TLargeintField
      FieldName = 'ID'
      Origin = '"REQUESTCURRENCYRATE"."ID"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object IBRequestCurrencyRateREQUESTID: TLargeintField
      FieldName = 'REQUESTID'
      Origin = '"REQUESTCURRENCYRATE"."REQUESTID"'
      Required = True
    end
    object IBRequestCurrencyRateCURRENCY: TIBStringField
      FieldName = 'CURRENCY'
      Origin = '"REQUESTCURRENCYRATE"."CURRENCY"'
      Size = 1024
    end
    object IBRequestCurrencyRateVAL: TFloatField
      FieldName = 'VAL'
      Origin = '"REQUESTCURRENCYRATE"."VAL"'
    end
  end
  object dsRequestCurrencyRate: TDataSource
    DataSet = IBRequestCurrencyRate
    Left = 252
    Top = 344
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
      
        '  (ID, REQUESTID, LINENO, PARTNAME, QTY, PRICEID, PRICE, DISCOUN' +
        'T, VAT, '
      '   TAG, NOTES, CREATED, MODIFIED)'
      'values'
      
        '  (:ID, :REQUESTID, :LINENO, :PARTNAME, :QTY, :PRICEID, :PRICE, ' +
        ':DISCOUNT, '
      '   :VAT, :TAG, :NOTES, :CREATED, :MODIFIED)')
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
      '  MODIFIED'
      'from SPECIFICATION '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      
        'select  ID,REQUESTID,LINENO,PARTNAME,QTY,PRICEID,PRICE,DISCOUNT,' +
        'VAT,COST,TAG,NOTES,CREATED,MODIFIED'
      'from SPECIFICATION')
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
      '  CREATED = :CREATED,'
      '  MODIFIED = :MODIFIED'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    Left = 344
    Top = 192
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
    object IBSpecificationLINENO: TLargeintField
      FieldName = 'LINENO'
      Origin = '"SPECIFICATION"."LINENO"'
    end
    object IBSpecificationPARTNAME: TIBStringField
      FieldName = 'PARTNAME'
      Origin = '"SPECIFICATION"."PARTNAME"'
      Required = True
      Size = 1024
    end
    object IBSpecificationQTY: TFloatField
      FieldName = 'QTY'
      Origin = '"SPECIFICATION"."QTY"'
    end
    object IBSpecificationPRICEID: TLargeintField
      FieldName = 'PRICEID'
      Origin = '"SPECIFICATION"."PRICEID"'
    end
    object IBSpecificationPRICE: TFloatField
      FieldName = 'PRICE'
      Origin = '"SPECIFICATION"."PRICE"'
    end
    object IBSpecificationDISCOUNT: TFloatField
      FieldName = 'DISCOUNT'
      Origin = '"SPECIFICATION"."DISCOUNT"'
    end
    object IBSpecificationVAT: TLargeintField
      FieldName = 'VAT'
      Origin = '"SPECIFICATION"."VAT"'
      Required = True
    end
    object IBSpecificationCOST: TFloatField
      FieldKind = fkInternalCalc
      FieldName = 'COST'
      Origin = '"SPECIFICATION"."COST"'
      ProviderFlags = []
      ReadOnly = True
    end
    object IBSpecificationTAG: TIBStringField
      FieldName = 'TAG'
      Origin = '"SPECIFICATION"."TAG"'
      Size = 16
    end
    object IBSpecificationNOTES: TIBStringField
      FieldName = 'NOTES'
      Origin = '"SPECIFICATION"."NOTES"'
      Size = 4096
    end
    object IBSpecificationCREATED: TDateTimeField
      FieldName = 'CREATED'
      Origin = '"SPECIFICATION"."CREATED"'
    end
    object IBSpecificationMODIFIED: TDateTimeField
      FieldName = 'MODIFIED'
      Origin = '"SPECIFICATION"."MODIFIED"'
    end
  end
  object dsSpecification: TDataSource
    DataSet = IBSpecification
    Left = 348
    Top = 248
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
      '  (CURRENCYSYMBOL, NAME, DEFAULTRATE)'
      'values'
      '  (:CURRENCYSYMBOL, :NAME, :DEFAULTRATE)')
    RefreshSQL.Strings = (
      'Select '
      '  CURRENCYSYMBOL,'
      '  NAME,'
      '  DEFAULTRATE'
      'from VALIDCURRENCY '
      'where'
      '  CURRENCYSYMBOL = :CURRENCYSYMBOL')
    SelectSQL.Strings = (
      'select CURRENCYSYMBOL, NAME, DEFAULTRATE from VALIDCURRENCY')
    ModifySQL.Strings = (
      'update VALIDCURRENCY'
      'set'
      '  CURRENCYSYMBOL = :CURRENCYSYMBOL,'
      '  NAME = :NAME,'
      '  DEFAULTRATE = :DEFAULTRATE'
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
    Left = 576
    Top = 192
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
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from PROJECT'
      'where'
      '  NAME = :OLD_NAME')
    InsertSQL.Strings = (
      'insert into PROJECT'
      '  (NAME, SELLERORG, DESCRIPTION, CREATED, MODIFIED, VAT, STAGE)'
      'values'
      
        '  (:NAME, :SELLERORG, :DESCRIPTION, :CREATED, :MODIFIED, :VAT, :' +
        'STAGE)')
    RefreshSQL.Strings = (
      'Select '
      '  NAME,'
      '  SELLERORG,'
      '  DESCRIPTION,'
      '  CREATED,'
      '  MODIFIED,'
      '  VAT,'
      '  STAGE'
      'from PROJECT '
      'where'
      '  NAME = :NAME')
    SelectSQL.Strings = (
      'select  NAME,SELLERORG,DESCRIPTION,CREATED,MODIFIED,VAT, STAGE '
      'from "PROJECT" where /*Filter*/ 1=1'
      '')
    ModifySQL.Strings = (
      'update PROJECT'
      'set'
      '  NAME = :NAME,'
      '  SELLERORG = :SELLERORG,'
      '  DESCRIPTION = :DESCRIPTION,'
      '  CREATED = :CREATED,'
      '  MODIFIED = :MODIFIED,'
      '  VAT = :VAT,'
      '  STAGE = :STAGE'
      'where'
      '  NAME = :OLD_NAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 512
    Top = 296
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
    object DateTimeField1: TDateTimeField
      FieldName = 'CREATED'
      Origin = '"PROJECT"."CREATED"'
      Required = True
    end
    object DateTimeField2: TDateTimeField
      FieldName = 'MODIFIED'
      Origin = '"PROJECT"."MODIFIED"'
      Required = True
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
      
        '  (ID, PROJECTNAME, NAME, DESCRIPTION, CREATED, MODIFIED, SALETY' +
        'PE, ORG, '
      '   VAT, STAGE)'
      'values'
      
        '  (:ID, :PROJECTNAME, :NAME, :DESCRIPTION, :CREATED, :MODIFIED, ' +
        ':SALETYPE, '
      '   :ORG, :VAT, :STAGE)')
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
      '  STAGE'
      'from REQUEST '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      'select ID,STAGE,PROJECTNAME,NAME,DESCRIPTION,CREATED,MODIFIED,'
      'SALETYPE,ORG,VAT'
      'from REQUEST where PROJECTNAME = :NAME')
    ModifySQL.Strings = (
      'update REQUEST'
      'set'
      '  ID = :ID,'
      '  PROJECTNAME = :PROJECTNAME,'
      '  NAME = :NAME,'
      '  DESCRIPTION = :DESCRIPTION,'
      '  CREATED = :CREATED,'
      '  MODIFIED = :MODIFIED,'
      '  SALETYPE = :SALETYPE,'
      '  ORG = :ORG,'
      '  VAT = :VAT,'
      '  STAGE = :STAGE'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    GeneratorField.Field = 'ID'
    GeneratorField.Generator = 'GEN_REQUEST_ID'
    Active = True
    DataSource = dslProject
    Left = 604
    Top = 295
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
    object DateTimeField3: TDateTimeField
      FieldName = 'CREATED'
      Origin = '"REQUEST"."CREATED"'
    end
    object DateTimeField4: TDateTimeField
      FieldName = 'MODIFIED'
      Origin = '"REQUEST"."MODIFIED"'
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
  end
  object dslProject: TDataSource
    DataSet = IBLProject
    Left = 512
    Top = 360
  end
  object dslRequest: TDataSource
    DataSet = IBLRequest
    OnUpdateData = dslRequestUpdateData
    Left = 608
    Top = 360
  end
  object IBLSpecification: TIBDataSet
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
      
        '  (ID, REQUESTID, LINENO, PARTNAME, QTY, PRICEID, PRICE, DISCOUN' +
        'T, VAT, '
      '   TAG, NOTES, CREATED, MODIFIED)'
      'values'
      
        '  (:ID, :REQUESTID, :LINENO, :PARTNAME, :QTY, :PRICEID, :PRICE, ' +
        ':DISCOUNT, '
      '   :VAT, :TAG, :NOTES, :CREATED, :MODIFIED)')
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
      '  MODIFIED'
      'from SPECIFICATION '
      'where'
      '  ID = :ID')
    SelectSQL.Strings = (
      
        'select  ID,REQUESTID,LINENO,PARTNAME,QTY,PRICEID,PRICE,DISCOUNT,' +
        'VAT,COST,TAG,NOTES,CREATED,MODIFIED'
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
      '  CREATED = :CREATED,'
      '  MODIFIED = :MODIFIED'
      'where'
      '  ID = :OLD_ID')
    ParamCheck = True
    UniDirectional = False
    Active = True
    DataSource = dslRequest
    Left = 680
    Top = 296
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
    object DateTimeField5: TDateTimeField
      FieldName = 'CREATED'
      Origin = '"SPECIFICATION"."CREATED"'
    end
    object DateTimeField6: TDateTimeField
      FieldName = 'MODIFIED'
      Origin = '"SPECIFICATION"."MODIFIED"'
    end
  end
  object dslSpecification: TDataSource
    DataSet = IBLSpecification
    Left = 684
    Top = 360
  end
end
