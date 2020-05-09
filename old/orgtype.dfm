object FormOrgTypes: TFormOrgTypes
  Left = 0
  Top = 0
  Caption = #1058#1080#1087#1099' '#1086#1088#1075#1072#1085#1080#1079#1072#1094#1080#1081
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
  object DBGridProjectList: TDBGridEh
    Left = 0
    Top = 0
    Width = 624
    Height = 431
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = dmOutlay.dsOrgType
    DynProps = <>
    TabOrder = 0
    Columns = <
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'VAL'
        Footers = <>
        Title.Caption = #1054#1088#1075#1072#1085#1080#1079#1072#1094#1080#1103
      end
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'DESCRIPTION'
        Footers = <>
        Title.Caption = #1047#1085#1072#1095#1077#1085#1080#1077
        Width = 487
      end>
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object DBNavigatorOrg: TDBNavigator
    Left = 0
    Top = 432
    Width = 320
    Height = 30
    DataSource = dmOutlay.dsOrgType
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
