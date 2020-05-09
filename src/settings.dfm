object FormSettings: TFormSettings
  Left = 0
  Top = 0
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 327
  ClientWidth = 224
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    224
    327)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 17
    Width = 58
    Height = 13
    Caption = '&'#1057#1090#1080#1083#1100' '#1086#1082#1086#1085
  end
  object Label2: TLabel
    Left = 24
    Top = 63
    Width = 89
    Height = 13
    Caption = '&'#1048#1084#1103' '#1073#1072#1079#1099' '#1076#1072#1085#1085#1099#1093
  end
  object Label3: TLabel
    Left = 24
    Top = 111
    Width = 111
    Height = 13
    Caption = '&'#1040#1076#1088#1077#1089' '#1080#1083#1080' '#1080#1103' '#1089#1077#1088#1074#1077#1088#1072
  end
  object Label4: TLabel
    Left = 24
    Top = 159
    Width = 103
    Height = 13
    Caption = '&'#1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100' '#1057#1059#1041#1044
  end
  object Label5: TLabel
    Left = 24
    Top = 207
    Width = 37
    Height = 13
    Caption = '&'#1055#1072#1088#1086#1083#1100
  end
  object cbStyle: TComboBox
    Left = 24
    Top = 36
    Width = 171
    Height = 21
    TabOrder = 0
    Text = 'cbStyle'
  end
  object EDbName: TEdit
    Left = 24
    Top = 82
    Width = 171
    Height = 21
    Hint = #1051#1072#1090#1080#1085#1089#1082#1080#1077' '#1073#1091#1082#1074#1099
    TabOrder = 1
    Text = 'outlay'
    TextHint = #1080#1084#1103' '#1073#1072#1079#1099' '#1076#1072#1085#1085#1099#1093
  end
  object EHostAddress: TEdit
    Left = 24
    Top = 130
    Width = 171
    Height = 21
    Hint = #1051#1072#1090#1080#1085#1089#1082#1080#1077' '#1073#1091#1082#1074#1099
    TabOrder = 2
    Text = '127.0.0.1'
    TextHint = #1080#1084#1103' '#1073#1072#1079#1099' '#1076#1072#1085#1085#1099#1093
  end
  object EUser: TEdit
    Left = 24
    Top = 178
    Width = 171
    Height = 21
    Hint = #1051#1072#1090#1080#1085#1089#1082#1080#1077' '#1073#1091#1082#1074#1099
    TabOrder = 3
    Text = 'SYSDBA'
    TextHint = #1080#1084#1103' '#1073#1072#1079#1099' '#1076#1072#1085#1085#1099#1093
  end
  object BOK: TButton
    Left = 120
    Top = 278
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&'#1057#1086#1093#1088#1072#1085#1080#1090#1100
    Default = True
    TabOrder = 5
    OnClick = BOKClick
    ExplicitLeft = 520
    ExplicitTop = 392
  end
  object BCancel: TButton
    Left = 24
    Top = 278
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&'#1054#1090#1084#1077#1085#1072
    TabOrder = 6
    OnClick = BCancelClick
    ExplicitLeft = 424
    ExplicitTop = 392
  end
  object EPassword: TEdit
    Left = 24
    Top = 226
    Width = 171
    Height = 21
    Hint = #1051#1072#1090#1080#1085#1089#1082#1080#1077' '#1073#1091#1082#1074#1099
    PasswordChar = '*'
    TabOrder = 4
    Text = 'masterkey'
    TextHint = #1080#1084#1103' '#1073#1072#1079#1099' '#1076#1072#1085#1085#1099#1093
  end
end
