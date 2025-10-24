object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Greg'#39's Manager for World of Warcraft Addons'
  ClientHeight = 550
  ClientWidth = 800
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
  object lblWatcherStatus: TLabel
    Left = 500
    Top = 58
    Width = 74
    Height = 15
    Cursor = crHandPoint
    Caption = 'Not Watching'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    OnClick = lblWatcherStatusClick
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    Color = 2039583
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      Left = 16
      Top = 12
      Width = 386
      Height = 32
      Caption = 'Greg'#39's Manager for WoW Addons'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -24
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object ListView1: TListView
    Left = 0
    Top = 60
    Width = 800
    Height = 360
    Align = alClient
    Columns = <>
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = ListView1DblClick
    ExplicitTop = 54
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 420
    Width = 800
    Height = 80
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnAddAddon: TButton
      Left = 16
      Top = 16
      Width = 100
      Height = 32
      Caption = 'Add Addon'
      TabOrder = 0
      OnClick = btnAddAddonClick
    end
    object btnEditAddon: TButton
      Left = 122
      Top = 16
      Width = 100
      Height = 32
      Caption = 'Edit'
      TabOrder = 1
      OnClick = btnEditAddonClick
    end
    object btnRemoveAddon: TButton
      Left = 228
      Top = 16
      Width = 100
      Height = 32
      Caption = 'Remove'
      TabOrder = 2
      OnClick = btnRemoveAddonClick
    end
    object btnBackup: TButton
      Left = 360
      Top = 16
      Width = 100
      Height = 32
      Caption = 'Backup'
      TabOrder = 3
      OnClick = btnBackupClick
    end
    object btnRestore: TButton
      Left = 466
      Top = 16
      Width = 100
      Height = 32
      Caption = 'Restore'
      TabOrder = 4
      OnClick = btnRestoreClick
    end
    object btnCheckUpdates: TButton
      Left = 572
      Top = 16
      Width = 110
      Height = 32
      Caption = 'Check Updates'
      TabOrder = 5
      OnClick = btnCheckUpdatesClick
    end
    object btnSettings: TButton
      Left = 688
      Top = 16
      Width = 100
      Height = 32
      Caption = 'Settings'
      TabOrder = 6
      OnClick = btnSettingsClick
    end
    object ProgressBar1: TProgressBar
      Left = 16
      Top = 54
      Width = 772
      Height = 17
      TabOrder = 7
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 500
    Width = 800
    Height = 50
    Panels = <>
    SimplePanel = True
    SimpleText = 'Ready'
  end
  object btnBackupAll: TButton
    Left = 572
    Top = 16
    Width = 100
    Height = 32
    Caption = 'Backup All'
    TabOrder = 4
    OnClick = btnBackupAllClick
  end
  object PopupMenu1: TPopupMenu
    Left = 400
    Top = 200
    object mnuBackup: TMenuItem
      Caption = 'Backup'
      OnClick = mnuBackupClick
    end
    object mnuRestore: TMenuItem
      Caption = 'Restore...'
      OnClick = mnuRestoreClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuViewBackups: TMenuItem
      Caption = 'View Backups Folder'
      OnClick = mnuViewBackupsClick
    end
    object mnuEdit: TMenuItem
      Caption = 'Edit'
      OnClick = mnuEditClick
    end
    object mnuRemove: TMenuItem
      Caption = 'Remove'
      OnClick = mnuRemoveClick
    end
  end
end
