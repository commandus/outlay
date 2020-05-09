object FormVat: TFormVat
  Left = 0
  Top = 0
  Caption = #1057#1090#1072#1074#1082#1080' '#1053#1044#1057
  ClientHeight = 481
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    624
    481)
  PixelsPerInch = 96
  TextHeight = 13
  object DBNavigatorVAT: TDBNavigator
    Left = 0
    Top = 451
    Width = 320
    Height = 30
    DataSource = dmOutlay.dsOrgType
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 0
  end
  object DBGVAT: TDBGridEh
    Left = 0
    Top = 0
    Width = 623
    Height = 451
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = dmOutlay.dsVAT
    DynProps = <>
    TabOrder = 1
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
end
