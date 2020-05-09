object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = #1057#1087#1077#1094#1080#1092#1080#1082#1072#1094#1080#1080' 1.0'
  ClientHeight = 542
  ClientWidth = 1001
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 153
    Top = 0
    Height = 542
    ExplicitLeft = 248
    ExplicitTop = 120
    ExplicitHeight = 100
  end
  object DBGridEh1: TDBGridEh
    Left = 0
    Top = 0
    Width = 153
    Height = 542
    Align = alLeft
    Border.EdgeBorders = []
    BorderStyle = bsNone
    ColumnDefValues.Title.TitleButton = True
    DataSource = dmOutlay.dslProject
    DynProps = <>
    OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
    TabOrder = 0
    Columns = <
      item
        CellButtons = <>
        DynProps = <>
        EditButtons = <>
        FieldName = 'NAME'
        Footers = <>
        Title.Caption = #1055#1088#1086#1077#1082#1090
        Width = 119
      end>
    object RowDetailData: TRowDetailPanelControlEh
    end
  end
  object PanelRight: TPanel
    Left = 156
    Top = 0
    Width = 845
    Height = 542
    Align = alClient
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 0
      Top = 200
      Width = 845
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitLeft = 1
      ExplicitTop = 121
      ExplicitWidth = 293
    end
    object PanelRequest: TPanel
      Left = 0
      Top = 0
      Width = 845
      Height = 200
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Splitter3: TSplitter
        Left = 500
        Top = 0
        Height = 200
        ExplicitLeft = 829
      end
      object DBGridEhRequest: TDBGridEh
        Left = 0
        Top = 0
        Width = 500
        Height = 200
        Align = alLeft
        Border.EdgeBorders = []
        BorderStyle = bsNone
        ColumnDefValues.Title.TitleButton = True
        DataSource = dmOutlay.dslRequest
        DynProps = <>
        OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
        TabOrder = 0
        Columns = <
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'NAME'
            Footers = <>
            Title.Caption = #1047#1072#1087#1088#1086#1089
            Width = 122
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'ORG'
            Footers = <>
            LookupParams.KeyFieldNames = 'ORG'
            LookupParams.LookupDataSet = dmOutlay.IBOrg
            LookupParams.LookupDisplayFieldName = 'ORGNAME'
            LookupParams.LookupKeyFieldNames = 'ORGNAME'
            Title.Caption = #1054#1088#1075#1072#1085#1080#1079#1072#1094#1080#1103
            Width = 99
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'SALETYPE'
            Footers = <>
            LookupParams.KeyFieldNames = 'SALETYPE'
            LookupParams.LookupDataSet = dmOutlay.IBSaleType
            LookupParams.LookupDisplayFieldName = 'NAME'
            LookupParams.LookupKeyFieldNames = 'NAME'
            Title.Caption = #1058#1080#1087' '#1087#1088#1086#1076#1072#1078#1080
            Width = 73
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'STAGE'
            Footers = <>
            LookupParams.KeyFieldNames = 'STAGE'
            LookupParams.LookupDataSet = dmOutlay.IBStage
            LookupParams.LookupDisplayFieldName = 'NAME'
            LookupParams.LookupKeyFieldNames = 'NAME'
            Title.Caption = #1057#1086#1089#1090#1086#1103#1085#1080#1077
            Width = 61
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'VAT'
            Footers = <>
            LookupParams.KeyFieldNames = 'VAT'
            LookupParams.LookupDataSet = dmOutlay.IBVAT
            LookupParams.LookupDisplayFieldName = 'VAL'
            LookupParams.LookupKeyFieldNames = 'VAL'
            Title.Caption = #1053#1044#1057
            Width = 30
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'PROJECTNAME'
            Footers = <>
            Title.Caption = #1055#1088#1086#1077#1082#1090
            Visible = False
            Width = 30
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'DISCOUNT'
            Footers = <>
            Title.Caption = #1053#1072#1094#1077#1085#1082#1072
            Width = 39
          end>
        object RowDetailData: TRowDetailPanelControlEh
        end
      end
      object PanelRequestProp: TPanel
        Left = 503
        Top = 0
        Width = 342
        Height = 200
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object PageControlRightTop: TPageControl
          Left = 0
          Top = 0
          Width = 342
          Height = 200
          ActivePage = TabSheet5
          Align = alClient
          TabOrder = 0
          object TabSheet5: TTabSheet
            Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1085#1080#1077
            object PanelCurrencyControl: TPanel
              Left = 0
              Top = 26
              Width = 88
              Height = 146
              Align = alLeft
              BevelOuter = bvNone
              TabOrder = 0
              object DBGridEhRequestCurrency: TDBGridEh
                Left = 0
                Top = 0
                Width = 88
                Height = 146
                Align = alClient
                Border.EdgeBorders = []
                BorderStyle = bsNone
                ColumnDefValues.Title.TitleButton = True
                DataSource = dmOutlay.dsRequestCurrencyRate
                DynProps = <>
                OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
                TabOrder = 0
                VertScrollBar.VisibleMode = sbNeverShowEh
                Columns = <
                  item
                    CellButtons = <>
                    DynProps = <>
                    EditButtons = <>
                    FieldName = 'CURRENCY'
                    Footers = <>
                    LookupParams.KeyFieldNames = 'CURRENCY'
                    LookupParams.LookupDataSet = dmOutlay.IBCurrency
                    LookupParams.LookupDisplayFieldName = 'CURRENCYSYMBOL'
                    LookupParams.LookupKeyFieldNames = 'CURRENCYSYMBOL'
                    Title.Caption = #1042#1072#1083'.'
                    Width = 29
                  end
                  item
                    CellButtons = <>
                    DynProps = <>
                    EditButtons = <>
                    FieldName = 'VAL'
                    Footers = <>
                    Title.Caption = #1050#1091#1088#1089
                    Width = 36
                  end>
                object RowDetailData: TRowDetailPanelControlEh
                end
              end
            end
            object ActionToolBar1: TActionToolBar
              Left = 0
              Top = 0
              Width = 334
              Height = 26
              ActionManager = ActionManagerMain
              Caption = 'ActionToolBar1'
              Color = clMenuBar
              ColorMap.DisabledFontColor = 7171437
              ColorMap.HighlightColor = clWhite
              ColorMap.BtnSelectedFont = clBlack
              ColorMap.UnusedColor = clWhite
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBlack
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              Spacing = 0
            end
            object DBGridEhPayment: TDBGridEh
              Left = 88
              Top = 26
              Width = 246
              Height = 146
              Align = alClient
              Border.EdgeBorders = []
              BorderStyle = bsNone
              ColumnDefValues.Title.TitleButton = True
              DataSource = dmOutlay.dsPayment
              DynProps = <>
              OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
              TabOrder = 2
              VertScrollBar.VisibleMode = sbNeverShowEh
              Columns = <
                item
                  CellButtons = <>
                  DisplayFormat = 'DD-MM-YYYY'
                  DynProps = <>
                  EditButtons = <>
                  FieldName = 'PAYDAY'
                  Footers = <>
                  Title.Caption = #1044#1072#1090#1072
                  Width = 65
                end
                item
                  CellButtons = <>
                  DynProps = <>
                  EditButtons = <>
                  FieldName = 'PSTATE'
                  Footers = <>
                  LookupParams.KeyFieldNames = 'PSTATE'
                  LookupParams.LookupDataSet = dmOutlay.IBPaymentState
                  LookupParams.LookupDisplayFieldName = 'NAME'
                  LookupParams.LookupKeyFieldNames = 'NAME'
                  Title.Caption = #1057#1086#1089#1090'.'
                  Width = 36
                end
                item
                  CellButtons = <>
                  DynProps = <>
                  EditButtons = <>
                  FieldName = 'PTYPE'
                  Footers = <>
                  LookupParams.KeyFieldNames = 'PTYPE'
                  LookupParams.LookupDataSet = dmOutlay.IBPaymentType
                  LookupParams.LookupDisplayFieldName = 'NAME'
                  LookupParams.LookupKeyFieldNames = 'NAME'
                  Title.Caption = #1058#1080#1087
                  Width = 30
                end
                item
                  CellButtons = <>
                  DynProps = <>
                  EditButtons = <>
                  FieldName = 'VAL'
                  Footers = <>
                  Title.Caption = #1057#1091#1084#1084#1072
                  Width = 69
                end>
              object RowDetailData: TRowDetailPanelControlEh
              end
            end
          end
          object TabSheet6: TTabSheet
            Caption = #1057#1074#1086#1076#1082#1072
            ImageIndex = 1
            object VLERequestSums: TValueListEditor
              Left = 0
              Top = 0
              Width = 334
              Height = 172
              Align = alClient
              BorderStyle = bsNone
              TabOrder = 0
              TitleCaptions.Strings = (
                #1048#1090#1086#1075
                #1047#1085#1072#1095#1077#1085#1080#1077)
              ColWidths = (
                150
                182)
            end
          end
        end
      end
    end
    object DBGridEhSpec: TDBGridEh
      Left = 0
      Top = 203
      Width = 845
      Height = 339
      Align = alClient
      Border.EdgeBorders = []
      BorderStyle = bsNone
      ColumnDefValues.Title.TitleButton = True
      DataSource = dmOutlay.dslSpecification
      DynProps = <>
      FooterRowCount = 1
      OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
      SumList.Active = True
      TabOrder = 1
      Columns = <
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'REQUESTID'
          Footers = <>
          LookupParams.LookupKeyFieldNames = 'ID'
          Title.Caption = #1047#1072#1087#1088#1086#1089
          Visible = False
          Width = 80
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PARTNO'
          Footer.FieldName = 'ID'
          Footer.ValueType = fvtCount
          Footers = <>
          Title.Caption = #1040#1088#1090#1080#1082#1091#1083
          Width = 100
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PARTNAME'
          Footers = <>
          LookupParams.KeyFieldNames = 'PARTNAME'
          LookupParams.LookupDataSet = dmOutlay.IBPart
          LookupParams.LookupDisplayFieldName = 'NAME'
          LookupParams.LookupKeyFieldNames = 'NAME'
          Title.Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1090#1086#1074#1072#1088#1072
          Width = 121
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'QTY'
          Footer.ValueType = fvtSum
          Footers = <>
          Title.Caption = #1050#1086#1083'-'#1074#1086
          Width = 39
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PARTMEASUREUNIT'
          Footers = <>
          Title.Caption = #1045#1076'.'#1080#1079#1084'.'
          Width = 42
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PRICEPRICE'
          Footers = <>
          Title.Caption = #1042#1093#1086#1076
          Width = 70
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PRICECURRENCY'
          Footers = <>
          Title.Caption = #1042#1072#1083'.'
          Width = 32
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'REBATE'
          Footers = <>
          Title.Caption = #1057#1082#1080#1076#1082#1072',%'
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PRICERUB'
          Footers = <>
          Title.Caption = #1042#1093#1086#1076' '#1080#1090#1086#1075
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'COSTLIST'
          Footer.ValueType = fvtSum
          Footers = <>
          Title.Caption = #1057#1091#1084#1084#1072', '#1088#1091#1073'.'
          Width = 63
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'DISCOUNT'
          Footers = <>
          Title.Caption = #1053#1072#1082#1088#1091#1090#1082#1072', %'
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PRICENDISCOUNT'
          Footer.ValueType = fvtSum
          Footers = <>
          Title.Caption = #1062#1077#1085#1072'+%'
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'COSTNDISCOUNT'
          Footer.ValueType = fvtSum
          Footers = <>
          Title.Caption = #1057#1091#1084#1084#1072'+%'
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          Footers = <>
          Title.Caption = 'MSRP GPL/Fixed'
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          Footers = <>
          Title.Caption = #1056#1077#1085#1090#1072#1073#1077#1083#1100#1085#1086#1089#1090#1100', %'
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'VAT'
          Footers = <>
          Title.Caption = #1042#1093#1086#1076'.'#1053#1044#1057
          Width = 48
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PRICEORGNAME'
          Footers = <>
          Title.Caption = #1055#1086#1089#1090#1072#1074#1097#1080#1082
          Width = 120
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PARTVOL'
          Footers = <>
          Title.Caption = #1054#1073#1098#1077#1084' '#1077#1076'.'
          Width = 60
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PARTWEIGHT'
          Footers = <>
          Title.Caption = #1042#1077#1089' '#1077#1076'.'
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PARTVOLSUM'
          Footer.ValueType = fvtSum
          Footers = <>
          Title.Caption = #1054#1073#1098#1077#1084
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PARTWEIGHTSUM'
          Footer.ValueType = fvtSum
          Footers = <>
          Title.Caption = #1042#1077#1089
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PRICEVAL'
          Footers = <>
          Title.Caption = #1055#1088#1072#1081#1089'-'#1083#1080#1089#1090
          Width = 136
        end
        item
          CellButtons = <>
          DynProps = <>
          EditButtons = <>
          FieldName = 'PRICERUB'
          Footers = <>
          Title.Caption = #1050#1091#1088#1089#1086#1074#1072#1103
        end>
      object RowDetailData: TRowDetailPanelControlEh
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 488
    Top = 384
    object MenuSettings: TMenuItem
      Caption = '&'#1060#1072#1081#1083
      object MenuReport: TMenuItem
        Caption = '&'#1054#1090#1095#1077#1090#1099
        ShortCut = 32850
        OnClick = MenuReportClick
      end
      object MenuDict: TMenuItem
        Caption = '&'#1057#1087#1088#1072#1074#1086#1095#1085#1080#1082#1080
        Hint = #1057#1087#1088#1072#1074#1086#1095#1085#1080#1082#1080
        ShortCut = 32836
        OnClick = MenuDictClick
      end
      object MenuSep2: TMenuItem
        Caption = '-'
      end
      object MenuOrg: TMenuItem
        Caption = '&'#1052#1086#1103' '#1086#1088#1075#1072#1085#1080#1079#1072#1094#1080#1103
        Hint = #1059#1082#1072#1079#1072#1090#1100' '#1084#1086#1102' '#1080' '#1089#1074#1103#1079#1072#1085#1099#1077' '#1089' '#1085#1077#1081' '#1086#1088#1075#1072#1085#1080#1079#1072#1094#1080#1080' ('#1087#1088#1086#1076#1072#1074#1094#1099')'
        ShortCut = 32845
        OnClick = MenuOrgClick
      end
      object MenuOptions: TMenuItem
        Caption = '&'#1053#1072#1089#1090#1088#1086#1081#1082#1080
        ShortCut = 32847
        OnClick = MenuOptionsClick
      end
      object MenuSep1: TMenuItem
        Caption = '-'
      end
      object MenuExit: TMenuItem
        Caption = '&'#1047#1072#1074#1077#1088#1096#1080#1090#1100' '#1088#1072#1073#1086#1090#1091
        ShortCut = 32856
        OnClick = MenuExitClick
      end
    end
    object TMenuItem
    end
    object ManuHelp: TMenuItem
      Caption = '&'#1057#1087#1088#1072#1074#1082#1072
      object MenuHelpUserGuide: TMenuItem
        Caption = '&'#1056#1091#1082#1086#1074#1086#1076#1089#1090#1074#1086' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
        OnClick = MenuHelpUserGuideClick
      end
      object MenuHelpAbout: TMenuItem
        Caption = '&'#1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
        OnClick = MenuHelpAboutClick
      end
    end
  end
  object ActionManagerMain: TActionManager
    ActionBars = <
      item
        Items.CaptionOptions = coAll
        Items = <
          item
            Action = actRollback
            Caption = #1054#1090#1084#1077#1085#1080#1090#1100' F&4'
            ImageIndex = 1
            ShortCut = 115
          end
          item
            Action = ActionSetRequestDiscount
            Caption = #1053#1072#1094#1077#1085#1080#1090#1100' '#1074#1089#1077' F&9'
            ImageIndex = 2
            ShortCut = 120
          end>
      end
      item
        Items = <
          item
            Action = actSave
            Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' &F5'
            ImageIndex = 0
            ShortCut = 116
          end
          item
            Action = actRollback
            Caption = #1054#1090#1084#1077#1085#1080#1090#1100' F&4'
            ImageIndex = 1
            ShortCut = 115
          end
          item
            Action = ActionSetRequestDiscount
            Caption = #1053#1072#1094#1077#1085#1080#1090#1100' '#1074#1089#1077' F&9'
            ImageIndex = 2
            ShortCut = 120
          end>
        ActionBar = ActionToolBar1
      end>
    Images = ImageList1
    Left = 604
    Top = 380
    StyleName = 'Platform Default'
    object ActionSetRequestDiscount: TAction
      Category = 'Request'
      Caption = #1053#1072#1094#1077#1085#1080#1090#1100' '#1074#1089#1077' F9'
      Hint = #1059#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1085#1072#1094#1077#1085#1082#1091' '#1085#1072' '#1074#1089#1077' '#1087#1086#1079#1080#1094#1080#1080
      ImageIndex = 2
      ShortCut = 120
      OnExecute = ActionSetRequestDiscountExecute
    end
    object actSave: TAction
      Category = 'Command'
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' F5'
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1080#1079#1084#1077#1085#1077#1085#1080#1103
      ImageIndex = 0
      ShortCut = 116
      OnExecute = actSaveExecute
    end
    object actRollback: TAction
      Category = 'Command'
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100' F4'
      Hint = #1054#1090#1084#1077#1085#1080#1090#1100' '#1080#1079#1084#1077#1085#1077#1085#1080#1103
      ImageIndex = 1
      ShortCut = 115
      OnExecute = actRollbackExecute
    end
  end
  object ImageList1: TImageList
    Left = 556
    Top = 380
    Bitmap = {
      494C010103000800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000C0C0C000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000080808000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080808000C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C0000000
      0000000000000000000000000000000000008080800000000000000000000000
      0000000000000000000080808000000000000000000000000000000000000000
      000000000000C0C0C00000000000808080000000000000000000000000000000
      0000000000000000000000000000C0C0C000C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000008080800000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000C0C0C000C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C000C0C0C000808080008080800080808000000000000000
      0000000000000000000080808000C0C0C0008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000000000
      000080808000C0C0C000C0C0C0000000000000000000C0C0C000C0C0C0008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C000C0C0C00080808000808080008080800000000000000000000000
      0000000000000000000080808000000000008080800000000000000000000000
      00000000000080808000C0C0C0000000000000000000C0C0C000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      000080808000C0C0C000C0C0C0000000000000000000C0C0C000C0C0C0008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C000000000000000000000000000808080000000000080808000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000C0C0C0000000
      0000000000000000000000000000000000008080800080808000000000000000
      000000000000808080008080800000000000000000000000000080808000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      000000000000000000000000000000000000C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080808000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF00003FFFFFFF80010000
      0FFFFFFF8001000003FFFFFF80010000007FFFFF80010000001F01F880010000
      000703F880010000F80007F181810000F0010183818100000007000780010000
      001F380F8001000000FF7FFF8001000003FFFFFF800100000FFFFFFF80010000
      7FFFFFFF80030000FFFFFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
end
