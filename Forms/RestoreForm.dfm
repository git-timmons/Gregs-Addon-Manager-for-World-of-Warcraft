object Restore: TForm3
  Left = 0
  Top = 0
  Caption = 'Restore / Switch Versions'
  ClientHeight = 500
  ClientWidth = 650
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 650
    Height = 70
    Align = alTop
    BevelOuter = bvNone
    Color = 2039583
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      Left = 16
      Top = 12
      Width = 198
      Height = 21
      Caption = 'Version History / Switcher'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblAddonName: TLabel
      Left = 16
      Top = 39
      Width = 82
      Height = 15
      Caption = 'Addon: [Name]'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblActiveVersion: TLabel
      Left = 240
      Top = 39
      Width = 154
      Height = 15
      Caption = 'Active Version: [timestamp]'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object ListView1: TListView
    Left = 0
    Top = 70
    Width = 650
    Height = 250
    Align = alClient
    Columns = <>
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = ListView1DblClick
    OnSelectItem = ListView1SelectItem
  end
  object pnlOptions: TPanel
    Left = 0
    Top = 320
    Width = 650
    Height = 110
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object grpRestoreType: TGroupBox
      Left = 16
      Top = 8
      Width = 400
      Height = 90
      Caption = 'Restore Type'
      TabOrder = 0
      object rbRestoreEverything: TRadioButton
        Left = 16
        Top = 24
        Width = 360
        Height = 17
        Caption = 'Everything (addon files + all settings)'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbRestoreAddonOnly: TRadioButton
        Left = 16
        Top = 47
        Width = 360
        Height = 17
        Caption = 'Addon files only (keep current settings)'
        TabOrder = 1
      end
      object rbRestoreSettingsOnly: TRadioButton
        Left = 16
        Top = 70
        Width = 360
        Height = 17
        Caption = 'Settings only (keep current addon files)'
        TabOrder = 2
      end
    end
    object chkBackupFirst: TCheckBox
      Left = 432
      Top = 32
      Width = 200
      Height = 17
      Caption = 'Create safety backup first'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 430
    Width = 650
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object btnSwitch: TButton
      Left = 360
      Top = 10
      Width = 120
      Height = 32
      Caption = 'Switch to This'
      TabOrder = 0
      OnClick = btnSwitchClick
    end
    object btnDelete: TButton
      Left = 486
      Top = 10
      Width = 70
      Height = 32
      Caption = 'Delete'
      TabOrder = 1
      OnClick = btnDeleteClick
    end
    object btnClose: TButton
      Left = 562
      Top = 10
      Width = 70
      Height = 32
      Cancel = True
      Caption = 'Close'
      TabOrder = 2
      OnClick = btnCloseClick
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 480
    Width = 650
    Height = 20
    Panels = <>
    SimplePanel = True
  end
  object ProgressBar1: TProgressBar
    Left = 16
    Top = 456
    Width = 618
    Height = 17
    TabOrder = 5
  end
end
