object FormDict: TFormDict
  Left = 0
  Top = 0
  Caption = #1057#1087#1088#1072#1074#1086#1095#1085#1080#1082#1080
  ClientHeight = 461
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 624
    Height = 461
    ActivePage = TabSheet1
    Align = alClient
    MultiLine = True
    TabOrder = 0
    TabPosition = tpBottom
    object TabSheet4: TTabSheet
      Caption = #1055#1088#1086#1077#1082#1090#1099
      ImageIndex = 4
      DesignSize = (
        616
        417)
      object DBGridEh2: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 386
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColumnDefValues.Title.TitleButton = True
        DataSource = dmOutlay.dsProject
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
        Top = 386
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
        417)
      object DBGridProjectList: TDBGridEh
        Left = 0
        Top = -6
        Width = 616
        Height = 386
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
            FieldName = 'ORG'
            Footers = <>
            LookupParams.KeyFieldNames = 'ORG'
            LookupParams.LookupDataSet = dmOutlay.IBOrg
            LookupParams.LookupDisplayFieldName = 'ORGNAME'
            LookupParams.LookupKeyFieldNames = 'ORGNAME'
            Title.Caption = #1054#1088#1075#1072#1085#1080#1079#1072#1094#1080#1103
            Width = 120
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
        Top = 386
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
        417)
      object DBGridEh8: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 386
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
        Top = 386
        Width = 320
        Height = 30
        DataSource = dmOutlay.dsSpecification
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
    end
    object Организации: TTabSheet
      Caption = #1054#1088#1075#1072#1085#1080#1079#1072#1094#1080#1080
      ImageIndex = 1
      DesignSize = (
        616
        417)
      object DBGridEh1: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 386
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColumnDefValues.Title.TitleButton = True
        DataSource = dmOutlay.dsOrg
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
            Width = 100
          end
          item
            CellButtons = <>
            DropDownBox.ListSource = dmOutlay.dsOrgType
            DynProps = <>
            EditButtons = <>
            FieldName = 'ORGTYPE'
            Footers = <>
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
        Top = 386
        Width = 320
        Height = 30
        DataSource = dmOutlay.dsOrg
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
    end
    object TabSheet5: TTabSheet
      Caption = #1057#1086#1090#1088#1091#1076#1085#1080#1082#1080
      ImageIndex = 5
      DesignSize = (
        616
        417)
      object DBGridPersonList: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 386
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColumnDefValues.Title.TitleButton = True
        DataSource = dmOutlay.dsPerson
        DynProps = <>
        OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
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
            LookupParams.KeyFieldNames = 'ORGNAME'
            LookupParams.LookupDataSet = dmOutlay.IBOrg
            LookupParams.LookupDisplayFieldName = 'ORGNAME'
            LookupParams.LookupKeyFieldNames = 'ORGNAME'
            Title.Caption = #1054#1088#1075#1072#1085#1080#1079#1072#1094#1080#1103
            Width = 100
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'POSITIONNAME'
            Footers = <>
            LookupParams.KeyFieldNames = 'POSITIONNAME'
            LookupParams.LookupDataSet = dmOutlay.IBOrgPosition
            LookupParams.LookupDisplayFieldName = 'NAME'
            LookupParams.LookupKeyFieldNames = 'NAME'
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
          end>
        object RowDetailData: TRowDetailPanelControlEh
        end
      end
      object DBNavigator1: TDBNavigator
        Left = 0
        Top = 386
        Width = 320
        Height = 30
        DataSource = dmOutlay.dsPerson
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
    end
    object НДС: TTabSheet
      Caption = #1053#1044#1057
      ImageIndex = 3
      DesignSize = (
        616
        417)
      object DBGVAT: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 386
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColumnDefValues.Title.TitleButton = True
        DataSource = dmOutlay.dsVAT
        DynProps = <>
        OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
        TabOrder = 0
        Columns = <
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'VAL'
            Footers = <>
            Title.Caption = #1057#1090#1072#1074#1082#1072', %'
            Width = 68
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'NAME'
            Footers = <>
            Title.Caption = #1053#1072#1083#1086#1075
            Width = 255
          end>
        object RowDetailData: TRowDetailPanelControlEh
        end
      end
      object DBNavigatorVAT: TDBNavigator
        Left = 0
        Top = 386
        Width = 320
        Height = 30
        DataSource = dmOutlay.dsVAT
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
    end
    object TabSheet6: TTabSheet
      Caption = #1058#1080#1087#1099' '#1086#1088#1075'.'
      ImageIndex = 6
      DesignSize = (
        616
        417)
      object DBGridEh3: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 386
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColumnDefValues.Title.TitleButton = True
        DataSource = dmOutlay.dsOrgType
        DynProps = <>
        OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
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
      object DBNavigator2: TDBNavigator
        Left = 0
        Top = 386
        Width = 320
        Height = 30
        DataSource = dmOutlay.dsOrgType
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
    end
    object TabSheet3: TTabSheet
      Caption = #1044#1086#1083#1078#1085#1086#1089#1090#1080
      ImageIndex = 7
      DesignSize = (
        616
        417)
      object DBGridEh4: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 386
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColumnDefValues.Title.TitleButton = True
        DataSource = dmOutlay.dsOrgPosition
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
            Title.Caption = #1044#1086#1083#1078#1085#1086#1089#1090#1100
            Width = 100
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'DESCRIPTION'
            Footers = <>
            Title.Caption = #1055#1086#1083#1085#1086#1077' '#1085#1072#1079#1074#1072#1085#1080#1077
            Width = 487
          end>
        object RowDetailData: TRowDetailPanelControlEh
        end
      end
      object DBNavigator3: TDBNavigator
        Left = 0
        Top = 386
        Width = 320
        Height = 30
        DataSource = dmOutlay.dsOrgPosition
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
    end
    object TabSheet7: TTabSheet
      Caption = #1045#1076'.'#1080#1079#1084'.'
      ImageIndex = 8
      DesignSize = (
        616
        417)
      object DBNavigator4: TDBNavigator
        Left = 0
        Top = 386
        Width = 320
        Height = 30
        DataSource = dmOutlay.dsMeasureUnit
        Anchors = [akLeft, akBottom]
        TabOrder = 0
      end
      object DBGridEHMeasureUnit: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 386
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColumnDefValues.Title.TitleButton = True
        DataSource = dmOutlay.dsMeasureUnit
        DynProps = <>
        OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
        TabOrder = 1
        Columns = <
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'NAME'
            Footers = <>
            Title.Caption = #1054#1073#1086#1079#1085#1072#1095#1077#1085#1080#1077
            Width = 100
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'FULLNAME'
            Footers = <>
            Title.Caption = #1045#1076#1080#1085#1080#1094#1072' '#1080#1079#1084#1077#1088#1077#1085#1080#1103
            Width = 487
          end>
        object RowDetailData: TRowDetailPanelControlEh
        end
      end
    end
    object TabSheet8: TTabSheet
      Caption = #1058#1080#1087' '#1087#1088#1086#1076#1072#1078
      ImageIndex = 9
      DesignSize = (
        616
        417)
      object DBNavigator5: TDBNavigator
        Left = 0
        Top = 386
        Width = 320
        Height = 30
        DataSource = dmOutlay.dsSaleType
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 0
      end
      object DBGridEhSaleType: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 386
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColumnDefValues.Title.TitleButton = True
        DataSource = dmOutlay.dsSaleType
        DynProps = <>
        OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
        TabOrder = 1
        Columns = <
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'NAME'
            Footers = <>
            Title.Caption = #1058#1080#1087' '#1087#1088#1086#1076#1072#1078#1080
            Width = 100
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'DESCRIPTION'
            Footers = <>
            Title.Caption = #1055#1086#1083#1085#1086#1077' '#1085#1072#1079#1074#1072#1085#1080#1077
            Width = 487
          end>
        object RowDetailData: TRowDetailPanelControlEh
        end
      end
    end
    object TabSheet9: TTabSheet
      Caption = #1042#1072#1083'.'
      ImageIndex = 10
      DesignSize = (
        616
        417)
      object DBGridEh6: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 386
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColumnDefValues.Title.TitleButton = True
        DataSource = dmOutlay.dsValidCurrency
        DynProps = <>
        OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
        TabOrder = 0
        Columns = <
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'CURRENCYSYMBOL'
            Footers = <>
            Title.Caption = #1054#1073#1086#1079#1085#1072#1095#1077#1085#1080#1077
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'NAME'
            Footers = <>
            Title.Caption = #1042#1072#1083#1102#1090#1072
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'DEFAULTRATE'
            Footers = <>
            Title.Caption = #1050#1091#1088#1089' '#1082' RUR'
            Width = 80
          end>
        object RowDetailData: TRowDetailPanelControlEh
        end
      end
      object DBNavigator6: TDBNavigator
        Left = 0
        Top = 386
        Width = 320
        Height = 30
        DataSource = dmOutlay.dsValidCurrency
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
    end
    object TabSheet10: TTabSheet
      Caption = #1055#1086#1089#1090#1072#1074#1097#1080#1082#1080
      ImageIndex = 11
      DesignSize = (
        616
        417)
      object DBGridEh7: TDBGridEh
        Left = 0
        Top = 0
        Width = 616
        Height = 386
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColumnDefValues.Title.TitleButton = True
        DataSource = dmOutlay.dsVendor
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
            Title.Caption = #1057#1086#1082#1088'. '#1085#1072#1079#1074#1072#1085#1080#1077
            Width = 88
          end
          item
            CellButtons = <>
            DynProps = <>
            EditButtons = <>
            FieldName = 'DESCRIPTION'
            Footers = <>
            Title.Caption = #1055#1086#1089#1090#1072#1074#1097#1080#1082
            Width = 490
          end>
        object RowDetailData: TRowDetailPanelControlEh
        end
      end
      object DBNavigator7: TDBNavigator
        Left = 0
        Top = 386
        Width = 320
        Height = 30
        DataSource = dmOutlay.dsVendor
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
    end
  end
end
