object FormReports: TFormReports
  Left = 0
  Top = 0
  Caption = #1054#1090#1095#1077#1090#1099
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 624
    Height = 441
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    TabPosition = tpBottom
    object TabSheet1: TTabSheet
      Caption = #1054#1090#1095#1077#1090
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
        DataSource = dsReports
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
            Title.Caption = #1054#1090#1095#1077#1090
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
        DesignSize = (
          460
          415)
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
              Visible = False
              Width = 30
            end>
          object RowDetailData: TRowDetailPanelControlEh
          end
        end
        object Panel2: TPanel
          Left = 1
          Top = 373
          Width = 458
          Height = 41
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
        end
        object BReportExec: TButton
          Left = 376
          Top = 382
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&'#1042#1099#1087#1086#1083#1085#1080#1090#1100
          Default = True
          TabOrder = 2
          OnClick = BReportExecClick
        end
        object DBGridEh4: TDBGridEh
          Left = 1
          Top = 124
          Width = 458
          Height = 249
          Align = alClient
          ColumnDefValues.Title.TitleButton = True
          DataSource = dmOutlay.dslSpecification
          DynProps = <>
          OptionsEh = [dghFixed3D, dghHighlightFocus, dghClearSelection, dghAutoSortMarking, dghMultiSortMarking, dghDialogFind, dghColumnResize, dghColumnMove, dghExtendVertLines]
          TabOrder = 3
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
              Visible = False
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
              LookupParams.LookupDisplayFieldName = 'NAME'
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
              Title.Caption = #1055#1088#1072#1081#1089
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
    object TabSheet2: TTabSheet
      Caption = #1054#1096#1080#1073#1082#1080
      ImageIndex = 1
      object MemoErrors: TMemo
        Left = 0
        Top = 0
        Width = 616
        Height = 415
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object dsReports: TDataSource
    DataSet = IBQueryReports
    Left = 496
    Top = 352
  end
  object IBQueryReports: TIBQuery
    Database = dmOutlay.IBDatabase
    Transaction = dmOutlay.IBTransaction
    Active = True
    BufferChunks = 1000
    CachedUpdates = False
    ParamCheck = True
    SQL.Strings = (
      'select NAME, SOURCE from FASTREPORT order by NAME')
    Left = 480
    Top = 292
    object IBQueryReportsNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"FASTREPORT"."NAME"'
      Size = 100
    end
    object IBQueryReportsSOURCE: TBlobField
      FieldName = 'SOURCE'
      Origin = '"FASTREPORT"."SOURCE"'
      ProviderFlags = [pfInUpdate]
      Size = 8
    end
  end
end
