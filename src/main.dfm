object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = #1057#1087#1077#1094#1080#1092#1080#1082#1072#1094#1080#1080' 1.0'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 624
    Height = 441
    ActivePage = TabSheet4
    Align = alClient
    MultiLine = True
    TabOrder = 0
    TabPosition = tpBottom
    object TabSheet3: TTabSheet
      Caption = #1047#1072#1087#1088#1086#1089
      ImageIndex = 3
      object Splitter1: TSplitter
        Left = 153
        Top = 0
        Height = 415
        ExplicitLeft = 248
        ExplicitTop = 120
        ExplicitHeight = 100
      end
      object DBGridEh1: TDBGridEh
        Left = 0
        Top = 0
        Width = 153
        Height = 415
        Align = alLeft
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
          end>
        object RowDetailData: TRowDetailPanelControlEh
        end
      end
      object Panel1: TPanel
        Left = 156
        Top = 0
        Width = 460
        Height = 415
        Align = alClient
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
        object Splitter2: TSplitter
          Left = 1
          Top = 121
          Width = 458
          Height = 3
          Cursor = crVSplit
          Align = alTop
          ExplicitWidth = 293
        end
        object DBGridEh3: TDBGridEh
          Left = 1
          Top = 1
          Width = 458
          Height = 120
          Align = alTop
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
              Width = 267
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
              Width = 150
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
              Width = 30
            end>
          object RowDetailData: TRowDetailPanelControlEh
          end
        end
        object DBGridEh4: TDBGridEh
          Left = 1
          Top = 124
          Width = 458
          Height = 290
          Align = alClient
          ColumnDefValues.Title.TitleButton = True
          DataSource = dmOutlay.dslSpecification
          DynProps = <>
          OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
          TabOrder = 1
          Columns = <
            item
              CellButtons = <>
              DynProps = <>
              EditButtons = <>
              FieldName = 'REQUESTID'
              Footers = <>
              LookupParams.KeyFieldNames = 'REQUESTID'
              LookupParams.LookupDataSet = dmOutlay.IBRequest
              LookupParams.LookupKeyFieldNames = 'ID'
              Title.Caption = #1047#1072#1087#1088#1086#1089
              Width = 80
            end
            item
              CellButtons = <>
              DynProps = <>
              EditButtons = <>
              FieldName = 'PARTNAME'
              Footers = <>
              LookupParams.KeyFieldNames = 'PARTNAME'
              LookupParams.LookupDataSet = dmOutlay.IBPart
              LookupParams.LookupKeyFieldNames = 'NAME'
              Title.Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
              Width = 153
            end
            item
              CellButtons = <>
              DynProps = <>
              EditButtons = <>
              FieldName = 'PRICEID'
              Footers = <>
              LookupParams.KeyFieldNames = 'PRICEID'
              LookupParams.LookupDataSet = dmOutlay.ibPrice
              LookupParams.LookupKeyFieldNames = 'ID'
              Title.Caption = #1062#1077#1085#1072' '#1082#1072#1090#1072#1083'.'
              Width = 86
            end
            item
              CellButtons = <>
              DynProps = <>
              EditButtons = <>
              FieldName = 'PRICE'
              Footers = <>
              Title.Caption = #1062#1077#1085#1072
            end
            item
              CellButtons = <>
              DynProps = <>
              EditButtons = <>
              FieldName = 'QTY'
              Footers = <>
              Title.Caption = #1050#1086#1083'-'#1074#1086
            end
            item
              CellButtons = <>
              DynProps = <>
              EditButtons = <>
              FieldName = 'DISCOUNT'
              Footers = <>
              Title.Caption = #1044#1080#1089#1082#1086#1085#1090', %'
            end
            item
              CellButtons = <>
              DynProps = <>
              EditButtons = <>
              FieldName = 'VAT'
              Footers = <>
              Title.Caption = #1053#1044#1057
              Width = 48
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
      end
    end
    object TabSheet4: TTabSheet
      Caption = #1055#1088#1086#1077#1082#1090#1099
      ImageIndex = 4
      DesignSize = (
        616
        415)
      object DBGridEh2: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 384
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColumnDefValues.Title.TitleButton = True
        Ctl3D = True
        DataSource = dmOutlay.dsProject
        DynProps = <>
        OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
        ParentCtl3D = False
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
            FieldName = 'STAGE'
            Footers = <>
            LookupParams.KeyFieldNames = 'STAGE'
            LookupParams.LookupDataSet = dmOutlay.IBStage
            LookupParams.LookupDisplayFieldName = 'NAME'
            LookupParams.LookupKeyFieldNames = 'NAME'
            Title.Caption = #1057#1086#1089#1090#1086#1103#1085#1080#1077
            Width = 80
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'SELLERORG'
            Footers = <>
            LookupParams.KeyFieldNames = 'SELLERORG'
            LookupParams.LookupDataSet = dmOutlay.IBOrg
            LookupParams.LookupDisplayFieldName = 'ORGNAME'
            LookupParams.LookupKeyFieldNames = 'ORGNAME'
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
            Width = 171
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
        Top = 384
        Width = 320
        Height = 30
        DataSource = dmOutlay.dsProject
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
    end
    object TabSheet1: TTabSheet
      Caption = #1047#1072#1087#1088#1086#1089#1099
      DesignSize = (
        616
        415)
      object DBGridProjectList: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 384
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColumnDefValues.Title.TitleButton = True
        DataSource = dmOutlay.dsRequest
        DynProps = <>
        OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
        TabOrder = 0
        Columns = <
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'PROJECTNAME'
            Footers = <>
            LookupParams.KeyFieldNames = 'PROJECTNAME'
            LookupParams.LookupDataSet = dmOutlay.IBProject
            LookupParams.LookupDisplayFieldName = 'NAME'
            LookupParams.LookupKeyFieldNames = 'NAME'
            Title.Caption = #1055#1088#1086#1077#1082#1090
            Width = 80
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
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'NAME'
            Footers = <>
            Title.Caption = #1047#1072#1087#1088#1086#1089
            Width = 153
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'DESCRIPTION'
            Footers = <>
            Title.Caption = #1047#1072#1084#1077#1090#1082#1072
            Width = 64
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
            Width = 97
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'MODIFIED'
            Footers = <>
            Title.Caption = #1048#1079#1084#1077#1085#1077#1085
            Width = 58
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
            Width = 40
          end>
        object RowDetailData: TRowDetailPanelControlEh
        end
      end
      object DBNavigatorRequest: TDBNavigator
        Left = 0
        Top = 384
        Width = 320
        Height = 30
        DataSource = dmOutlay.dsRequest
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
    end
    object TabSheet2: TTabSheet
      Caption = #1057#1087#1077#1094#1080#1092#1080#1082#1072#1094#1080#1080
      ImageIndex = 2
      DesignSize = (
        616
        415)
      object DBGridEh8: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 384
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColumnDefValues.Title.TitleButton = True
        DataSource = dmOutlay.dsSpecification
        DynProps = <>
        OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
        TabOrder = 0
        Columns = <
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'REQUESTID'
            Footers = <>
            LookupParams.KeyFieldNames = 'REQUESTID'
            LookupParams.LookupDataSet = dmOutlay.IBRequest
            LookupParams.LookupKeyFieldNames = 'ID'
            Title.Caption = #1047#1072#1087#1088#1086#1089
            Width = 80
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'PARTNAME'
            Footers = <>
            LookupParams.KeyFieldNames = 'PARTNAME'
            LookupParams.LookupDataSet = dmOutlay.IBPart
            LookupParams.LookupKeyFieldNames = 'NAME'
            Title.Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
            Width = 153
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'PRICEID'
            Footers = <>
            LookupParams.KeyFieldNames = 'PRICEID'
            LookupParams.LookupDataSet = dmOutlay.ibPrice
            LookupParams.LookupKeyFieldNames = 'ID'
            Title.Caption = #1062#1077#1085#1072' '#1082#1072#1090#1072#1083'.'
            Width = 86
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'PRICE'
            Footers = <>
            Title.Caption = #1062#1077#1085#1072
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'QTY'
            Footers = <>
            Title.Caption = #1050#1086#1083'-'#1074#1086
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'DISCOUNT'
            Footers = <>
            Title.Caption = #1044#1080#1089#1082#1086#1085#1090', %'
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'VAT'
            Footers = <>
            Title.Caption = #1053#1044#1057
            Width = 48
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
      object DBNavigator8: TDBNavigator
        Left = 0
        Top = 384
        Width = 320
        Height = 30
        DataSource = dmOutlay.dsSpecification
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 488
    Top = 368
    object MenuSettings: TMenuItem
      Caption = '&'#1060#1072#1081#1083
      object MenuOrg: TMenuItem
        Caption = '&'#1047#1072#1076#1072#1090#1100' '#1084#1086#1102' '#1086#1088#1075#1072#1085#1080#1079#1072#1094#1080#1102
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
    object MenuReport: TMenuItem
      Caption = '&'#1054#1090#1095#1077#1090#1099
    end
    object MenuDict: TMenuItem
      Caption = '&'#1057#1087#1088#1072#1074#1086#1095#1085#1080#1082#1080
      Hint = #1057#1087#1088#1072#1074#1086#1095#1085#1080#1082#1080
      ShortCut = 32836
      OnClick = MenuDictClick
    end
    object MenuSave: TMenuItem
      Caption = '&'#1057#1086#1093#1088#1072#1085#1080#1090#1100
      ShortCut = 116
      OnClick = MenuSaveClick
    end
    object MenuCancel: TMenuItem
      Caption = '&'#1054#1090#1084#1077#1085#1080#1090#1100
      ShortCut = 115
      OnClick = MenuCancelClick
    end
  end
end
