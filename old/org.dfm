object FormOrgs: TFormOrgs
  Left = 0
  Top = 0
  Caption = #1054#1088#1075#1072#1085#1080#1079#1072#1094#1080#1080
  ClientHeight = 460
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenuOrg
  OldCreateOrder = False
  DesignSize = (
    624
    460)
  PixelsPerInch = 96
  TextHeight = 13
  object DBGridProjectList: TDBGridEh
    Left = 0
    Top = -6
    Width = 624
    Height = 430
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = dsOrg
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
        Width = 100
      end
      item
        CellButtons = <>
        DropDownBox.ListSource = dmOutlay.dsOrgType
        DynProps = <>
        EditButtons = <>
        FieldName = 'ORGTYPE'
        Footers = <>
        LookupParams.LookupDataSet = FDTOrgType
        LookupParams.LookupDisplayFieldName = 'VAL'
        LookupParams.LookupKeyFieldNames = 'VAL'
        Title.Caption = #1058#1080#1087
        Width = 80
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'INN'
        Footers = <>
        Title.Caption = #1048#1053#1053
        Width = 80
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'ALTNAME'
        Footers = <>
        Title.Caption = #1057#1086#1082#1088#1072#1097#1077#1085#1085#1086
        Width = 68
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'FULLNAME'
        Footers = <>
        Title.Caption = #1055#1086#1083#1085#1086#1077' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
        Width = 154
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'ISSELLER'
        Footers = <>
        Title.Caption = #1084#1086#1077
        Width = 25
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'ISACTIVE'
        Footers = <>
        Title.Caption = #1040#1082#1090#1080#1074#1085#1086
        Width = 25
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'DESCRIPTION'
        Footers = <>
        Title.Caption = #1047#1072#1084#1077#1090#1082#1072
      end>
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object DBNavigatorOrg: TDBNavigator
    Left = 0
    Top = 430
    Width = 320
    Height = 30
    DataSource = dsOrg
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
    ExplicitTop = 431
  end
  object MainMenuOrg: TMainMenu
    Left = 272
    Top = 120
    object MenuPerson: TMenuItem
      Caption = '&'#1057#1086#1090#1088#1091#1076#1085#1080#1082#1080' Ctrl+E'
      ShortCut = 16453
    end
    object MenuOrgTypes: TMenuItem
      Caption = '&'#1058#1080#1087#1099' '#1086#1088#1075#1072#1085#1080#1079#1072#1094#1080#1081' Ctrl+T'
      ShortCut = 16468
      OnClick = MenuOrgTypesClick
    end
    object MenuOrgPosition: TMenuItem
      Caption = '&'#1044#1086#1083#1078#1085#1086#1089#1090#1080' Ctrl+P'
      Hint = #1056#1086#1083#1100' '#1089#1086#1090#1088#1091#1076#1085#1080#1082#1086#1074
      ShortCut = 16464
      OnClick = MenuOrgPositionClick
    end
  end
  object dsOrg: TDataSource
    DataSet = FDTOrg
    Left = 408
    Top = 368
  end
  object FDTOrgType: TFDTable
    Active = True
    IndexName = 'PK_ORGTYPE'
    Connection = dmOutlay.FDConnectionOutlay
    UpdateOptions.UpdateTableName = 'ORGTYPE'
    TableName = 'ORGTYPE'
    Left = 480
    Top = 304
  end
  object FDTOrg: TFDTable
    Active = True
    IndexName = 'PK_ORG'
    Connection = dmOutlay.FDConnectionOutlay
    UpdateOptions.UpdateTableName = 'ORG'
    TableName = 'ORG'
    Left = 416
    Top = 304
  end
end
