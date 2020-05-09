object FormPersons: TFormPersons
  Left = 0
  Top = 0
  Caption = #1057#1086#1090#1088#1091#1076#1085#1080#1082#1080
  ClientHeight = 461
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
    461)
  PixelsPerInch = 96
  TextHeight = 13
  object DBGridPersonList: TDBGridEh
    Left = 0
    Top = 0
    Width = 624
    Height = 431
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = dmOutlay.dsPerson
    DynProps = <>
    TabOrder = 0
    Columns = <
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'PERSONNAME'
        Footers = <>
        Title.Caption = #1060'.'#1048'.'#1054'.'
        Width = 100
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'ID'
        Footers = <>
        Title.Caption = #8470
        Width = 40
      end
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
        DynProps = <>
        EditButtons = <>
        FieldName = 'POSITIONNAME'
        Footers = <>
        Title.Caption = #1044#1086#1083#1078#1085#1086#1089#1090#1100
        Width = 100
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'PHONE'
        Footers = <>
        Title.Caption = #1058#1077#1083
        Width = 100
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'EMAIL'
        Footers = <>
        Title.Caption = 'e-mail'
        Width = 100
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'SITE'
        Footers = <>
        Title.Caption = #1057#1072#1081#1090
        Width = 60
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'DESCRIPTION'
        Footers = <>
        Title.Caption = #1047#1072#1084#1077#1090#1082#1072
        Width = 100
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'TAG'
        Footers = <>
        Title.Caption = #1058#1077#1075
        Width = 60
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'ISACTIVE'
        Footers = <>
        Title.Caption = #1040#1082#1090#1080#1074#1077#1085
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'MODIFIED'
        Footers = <>
        Title.Caption = #1048#1079#1084#1077#1085#1077#1085
        Width = 60
      end>
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object DBNavigatorOrg: TDBNavigator
    Left = 0
    Top = 432
    Width = 320
    Height = 30
    DataSource = dmOutlay.dsPerson
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
  end
  object MainMenuOrg: TMainMenu
    Left = 32
    Top = 320
    object MenuOrgTypes: TMenuItem
      Caption = '&'#1058#1080#1087#1099' '#1086#1088#1075#1072#1085#1080#1079#1072#1094#1080#1081' Ctrl+T'
      ShortCut = 16468
      OnClick = MenuOrgTypesClick
    end
  end
end
