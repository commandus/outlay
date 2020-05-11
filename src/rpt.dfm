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
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 624
    Height = 441
    ActivePage = TabSheet1
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
        BorderStyle = bsNone
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
          Top = 169
          Width = 458
          Height = 3
          Cursor = crVSplit
          Align = alTop
          ExplicitTop = 121
          ExplicitWidth = 293
        end
        object VLEParameters: TValueListEditor
          Left = 1
          Top = 1
          Width = 458
          Height = 168
          Align = alTop
          BorderStyle = bsNone
          TabOrder = 3
          TitleCaptions.Strings = (
            #1055#1072#1088#1072#1084#1077#1090#1088
            #1047#1085#1072#1095#1077#1085#1080#1077)
          ColWidths = (
            150
            306)
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
          Left = 368
          Top = 379
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&PDF'
          Default = True
          TabOrder = 2
          OnClick = BReportExecClick
        end
        object MemoErrors: TMemo
          Left = 1
          Top = 172
          Width = 458
          Height = 201
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object ButtonXLS: TButton
          Left = 287
          Top = 379
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&XLS'
          TabOrder = 4
          OnClick = ButtonXLSClick
        end
        object ButtonXLSX: TButton
          Left = 206
          Top = 379
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&XLSX'
          TabOrder = 5
          OnClick = ButtonXLSXClick
        end
        object ButtonRTF: TButton
          Left = 125
          Top = 379
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&RTF'
          TabOrder = 6
          OnClick = ButtonRTFClick
        end
      end
    end
  end
  object dsReports: TDataSource
    DataSet = IBQueryReports
    Left = 504
    Top = 296
  end
  object IBQueryReports: TIBQuery
    Database = dmOutlay.IBDatabase
    Transaction = dmOutlay.IBTransaction
    AfterScroll = IBQueryReportsAfterScroll
    Active = True
    BufferChunks = 1000
    CachedUpdates = False
    ParamCheck = True
    SQL.Strings = (
      'select NAME, SOURCE from FASTREPORT order by NAME')
    Left = 504
    Top = 236
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
  object IBParameters: TIBDataSet
    Database = dmOutlay.IBDatabase
    Transaction = dmOutlay.IBTransaction
    BufferChunks = 1000
    CachedUpdates = False
    DeleteSQL.Strings = (
      'delete from PARAMVALUE'
      'where'
      '  REPORT = :OLD_REPORT and'
      '  NAME = :OLD_NAME')
    InsertSQL.Strings = (
      'insert into PARAMVALUE'
      '  (REPORT, NAME, VAL)'
      'values'
      '  (:REPORT, :NAME, :VAL)')
    RefreshSQL.Strings = (
      'Select '
      '  REPORT,'
      '  NAME,'
      '  VAL'
      'from PARAMVALUE '
      'where'
      '  REPORT = :REPORT and'
      '  NAME = :NAME')
    SelectSQL.Strings = (
      'select NAME, REPORT, VAL from PARAMVALUE WHERE REPORT = :REPORT')
    ModifySQL.Strings = (
      'update PARAMVALUE'
      'set'
      '  REPORT = :REPORT,'
      '  NAME = :NAME,'
      '  VAL = :VAL'
      'where'
      '  REPORT = :OLD_REPORT and'
      '  NAME = :OLD_NAME')
    ParamCheck = True
    UniDirectional = False
    Active = True
    Left = 424
    Top = 240
    object IBParametersNAME: TIBStringField
      FieldName = 'NAME'
      Origin = '"PARAMVALUE"."NAME"'
      Size = 1024
    end
    object IBParametersREPORT: TIBStringField
      FieldName = 'REPORT'
      Origin = '"PARAMVALUE"."REPORT"'
      Size = 1024
    end
    object IBParametersVAL: TIBStringField
      FieldName = 'VAL'
      Origin = '"PARAMVALUE"."VAL"'
      Size = 1024
    end
  end
end
