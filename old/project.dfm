object FormProjects: TFormProjects
  Left = 0
  Top = 0
  Caption = #1055#1088#1086#1077#1082#1090#1099
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenuProjects
  OldCreateOrder = False
  DesignSize = (
    624
    441)
  PixelsPerInch = 96
  TextHeight = 13
  object DBGridProjectList: TDBGridEh
    Left = 0
    Top = 0
    Width = 624
    Height = 411
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = dmOutlay.dsProject
    DynProps = <>
    TabOrder = 0
    Columns = <
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'NAME'
        Footers = <>
        Title.Caption = #1055#1088#1086#1077#1082#1090
        Width = 80
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'SELLERORG'
        Footers = <>
        Title.Caption = #1055#1088#1086#1076#1072#1074#1077#1094
        Width = 153
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'DESCRIPTION'
        Footers = <>
        Title.Caption = #1047#1072#1084#1077#1090#1082#1072
        Width = 256
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'VAT'
        Footers = <>
        Title.Caption = #1053#1072#1083#1086#1075
        Width = 30
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
  object DBNavigatorProjects: TDBNavigator
    Left = 0
    Top = 411
    Width = 320
    Height = 30
    DataSource = dmOutlay.dsProject
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
  end
  object MainMenuProjects: TMainMenu
    Left = 144
    Top = 216
    object MenuOrg: TMenuItem
      Caption = '&'#1054#1088#1075#1072#1085#1080#1079#1072#1094#1080#1080' Ctrl+O'
      Hint = #1057#1087#1080#1089#1086#1082' '#1086#1088#1075#1072#1085#1080#1079#1072#1094#1080#1081
      ShortCut = 16463
      OnClick = MenuOrgClick
    end
  end
end
