object FormMyOrg: TFormMyOrg
  Left = 0
  Top = 0
  Caption = #1054#1088#1075#1072#1085#1080#1079#1072#1094#1080#1080
  ClientHeight = 480
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    624
    480)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 5
    Width = 96
    Height = 13
    Caption = '&'#1042#1072#1096#1077' '#1087#1088#1077#1076#1087#1088#1080#1103#1090#1080#1077
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 162
    Height = 13
    Caption = '&'#1042#1099#1073#1077#1088#1080#1090#1077' '#1076#1086#1095#1077#1088#1085#1080#1077' '#1080' '#1087#1086#1084#1077#1090#1100#1090#1077
  end
  object DBGridOrgList: TDBGridEh
    Left = 8
    Top = 67
    Width = 425
    Height = 386
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColumnDefValues.Title.TitleButton = True
    DataSource = dsOrg
    DynProps = <>
    OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
    TabOrder = 0
    Columns = <
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'ORGNAME'
        Footers = <>
        Title.Caption = #1054#1088#1075#1072#1085#1080#1079#1072#1094#1080#1103
        Width = 199
      end
      item
        CellButtons = <>
        DropDownBox.ListSource = dmOutlay.dsOrgType
        DynProps = <>
        EditButtons = <>
        FieldName = 'OWNER'
        Footers = <>
        LookupParams.KeyFieldNames = 'ORGNAME'
        LookupParams.LookupDisplayFieldName = 'ORGNAME'
        LookupParams.LookupKeyFieldNames = 'ORGNAME'
        Title.Caption = #1043#1086#1083#1086#1074#1085#1072#1103' '#1086#1088#1075#1072#1085#1080#1079#1072#1094#1080#1103
        Width = 196
      end>
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object DBCBEhMyOrg: TDBLookupComboboxEh
    Left = 8
    Top = 21
    Width = 425
    Height = 21
    DynProps = <>
    DataField = 'ORGNAME'
    DataSource = dsMyOrg
    EditButtons = <>
    KeyField = 'ORGNAME'
    ListField = 'ORGNAME'
    ListSource = dsOrg
    TabOrder = 1
    Visible = True
  end
  object BSetOwner: TButton
    Left = 439
    Top = 64
    Width = 177
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&'#1055#1086#1084#1077#1090#1080#1090#1100' '#1082#1072#1082' '#1076#1086#1095#1077#1088#1085#1077#1077
    TabOrder = 2
    OnClick = BSetOwnerClick
  end
  object BSave: TButton
    Left = 439
    Top = 447
    Width = 177
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&'#1057#1086#1093#1088#1072#1085#1080#1090#1100
    Default = True
    TabOrder = 3
    OnClick = BSaveClick
  end
  object dsMyOrg: TDataSource
    DataSet = IBMyOrg
    Left = 464
    Top = 280
  end
  object IBOrg: TIBDataSet
    Database = dmOutlay.IBDatabase
    Transaction = dmOutlay.IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from ORG'
      'where'
      '  ORGNAME = :OLD_ORGNAME')
    InsertSQL.Strings = (
      'insert into ORG'
      '  (ORGNAME, OWNER)'
      'values'
      '  (:ORGNAME, :OWNER)')
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
      'select ORGNAME, OWNER FROM ORG')
    ModifySQL.Strings = (
      'update ORG'
      'set'
      '  ORGNAME = :ORGNAME,'
      '  OWNER = :OWNER'
      'where'
      '  ORGNAME = :OLD_ORGNAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 532
    Top = 232
    object IBOrgORGNAME: TIBStringField
      FieldName = 'ORGNAME'
      Origin = '"ORG"."ORGNAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 1024
    end
    object IBOrgOWNER: TIBStringField
      FieldName = 'OWNER'
      Origin = '"ORG"."OWNER"'
      Size = 1024
    end
  end
  object dsOrg: TDataSource
    DataSet = IBOrg
    Left = 532
    Top = 280
  end
  object IBMyOrg: TIBDataSet
    Database = dmOutlay.IBDatabase
    Transaction = dmOutlay.IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from ORG'
      'where'
      '  ORGNAME = :OLD_ORGNAME')
    InsertSQL.Strings = (
      'insert into ORG'
      '  (ORGNAME)'
      'values'
      '  (:ORGNAME)')
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
      'select ORGNAME FROM ORG')
    ModifySQL.Strings = (
      'update ORG'
      'set'
      '  ORGNAME = :ORGNAME'
      'where'
      '  ORGNAME = :OLD_ORGNAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 468
    Top = 232
    object IBStringField2: TIBStringField
      FieldName = 'ORGNAME'
      Origin = '"ORG"."ORGNAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 1024
    end
  end
end
