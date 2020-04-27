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
  object DBGridProjectList: TDBGridEh
    Left = 8
    Top = 64
    Width = 425
    Height = 386
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = dsMyOrg
    DynProps = <>
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
        LookupParams.LookupDataSet = IBQueryLookup
        LookupParams.LookupDisplayFieldName = 'ORGNAME'
        LookupParams.LookupKeyFieldNames = 'ORGNAME'
        Title.Caption = #1043#1086#1083#1086#1074#1085#1072#1103' '#1086#1088#1075#1072#1085#1080#1079#1072#1094#1080#1103
        Width = 196
      end>
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object DBComboboxEh1: TDBComboBoxEh
    Left = 8
    Top = 21
    Width = 425
    Height = 21
    DataField = 'ORGNAME'
    DataSource = dsMyOrg
    DynProps = <>
    EditButtons = <>
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
  object IBQMyOrg: TIBQuery
    Database = dmOutlay.IBDatabase
    Transaction = dmOutlay.IBTransaction
    Active = True
    BufferChunks = 1000
    CachedUpdates = False
    ParamCheck = True
    SQL.Strings = (
      'select ORGNAME from ORG')
    Left = 456
    Top = 192
    object IBQMyOrgORGNAME: TIBStringField
      FieldName = 'ORGNAME'
      Origin = '"ORG"."ORGNAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 1024
    end
  end
  object dsMyOrg: TDataSource
    DataSet = IBQMyOrg
    Left = 456
    Top = 240
  end
  object IBQueryOrg: TIBQuery
    Database = dmOutlay.IBDatabase
    Transaction = dmOutlay.IBTransaction
    Active = True
    BufferChunks = 1000
    CachedUpdates = False
    ParamCheck = True
    SQL.Strings = (
      'select ORGNAME, OWNER from ORG')
    Left = 456
    Top = 120
    object IBQueryOrgORGNAME: TIBStringField
      FieldName = 'ORGNAME'
      Origin = '"ORG"."ORGNAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 1024
    end
    object IBQueryOrgOWNER: TIBStringField
      FieldName = 'OWNER'
      Origin = '"ORG"."OWNER"'
      Size = 1024
    end
  end
  object IBQueryLookup: TIBQuery
    Database = dmOutlay.IBDatabase
    Transaction = dmOutlay.IBTransaction
    Active = True
    BufferChunks = 1000
    CachedUpdates = False
    ParamCheck = True
    SQL.Strings = (
      'select ORGNAME from ORG')
    Left = 504
    Top = 120
    object IBStringField1: TIBStringField
      FieldName = 'ORGNAME'
      Origin = '"ORG"."ORGNAME"'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 1024
    end
  end
end
