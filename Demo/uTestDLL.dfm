object Form1: TForm1
  Left = 330
  Top = 222
  Width = 555
  Height = 454
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 539
    Height = 418
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Execute from file'
      object Label1: TLabel
        Left = 8
        Top = 5
        Width = 47
        Height = 13
        Caption = 'File Name'
      end
      object fne: TEdit
        Left = 8
        Top = 23
        Width = 361
        Height = 21
        TabOrder = 0
      end
      object Button1: TButton
        Left = 392
        Top = 22
        Width = 129
        Height = 25
        Caption = 'Execute!'
        TabOrder = 1
        OnClick = Button1Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Execute text'
      ImageIndex = 1
      object mScript: TMemo
        Left = 0
        Top = 41
        Width = 531
        Height = 349
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          '')
        ParentFont = False
        TabOrder = 0
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 531
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label2: TLabel
          Left = 0
          Top = 24
          Width = 51
          Height = 13
          Caption = 'Script Text'
        end
        object Button2: TButton
          Left = 400
          Top = 8
          Width = 129
          Height = 25
          Caption = 'Execute!'
          TabOrder = 0
          OnClick = Button2Click
        end
      end
    end
    object tsExec2: TTabSheet
      Caption = 'Execute text (IBEBlock with progress)'
      ImageIndex = 3
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 531
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label3: TLabel
          Left = 0
          Top = 24
          Width = 51
          Height = 13
          Caption = 'Script Text'
        end
        object Button3: TButton
          Left = 416
          Top = 8
          Width = 113
          Height = 25
          Caption = 'Execute'
          TabOrder = 0
          OnClick = Button3Click
        end
      end
      object Memo1: TMemo
        Left = 0
        Top = 41
        Width = 531
        Height = 349
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'execute ibeblock'
          'as'
          'begin'
          '  i = 0;'
          '  while (i < 1000) do'
          '  begin'
          '    ibec_progress(i);'
          '    i = i + 1;'
          '  end'
          'end')
        ParentFont = False
        TabOrder = 1
      end
    end
    object tsOutput: TTabSheet
      Caption = 'Output'
      ImageIndex = 2
      object mLog: TMemo
        Left = 0
        Top = 0
        Width = 531
        Height = 390
        Align = alClient
        Lines.Strings = (
          '')
        TabOrder = 0
      end
    end
  end
end
