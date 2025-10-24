object Form2: TForm2
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Settings - Greg'#39's Manager'
  ClientHeight = 520
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 600
    Height = 470
    ActivePage = tabPaths
    Align = alClient
    TabOrder = 0
    object tabPaths: TTabSheet
      Caption = 'Paths'
      object lblWoWPath: TLabel
        Left = 16
        Top = 16
        Width = 170
        Height = 15
        Caption = 'World of Warcraft AddOns Path:'
      end
      object lblDownloads: TLabel
        Left = 16
        Top = 96
        Width = 98
        Height = 15
        Caption = 'Downloads Folder:'
      end
      object lblDataFolder: TLabel
        Left = 16
        Top = 176
        Width = 176
        Height = 15
        Caption = 'Data Folder (Settings && Backups):'
      end
      object edtWoWPath: TEdit
        Left = 16
        Top = 37
        Width = 449
        Height = 23
        TabOrder = 0
      end
      object btnBrowseWoW: TButton
        Left = 471
        Top = 35
        Width = 100
        Height = 25
        Caption = 'Browse...'
        TabOrder = 1
        OnClick = btnBrowseWoWClick
      end
      object btnDetectWoW: TButton
        Left = 471
        Top = 64
        Width = 100
        Height = 25
        Caption = 'Auto-Detect'
        TabOrder = 2
        OnClick = btnDetectWoWClick
      end
      object edtDownloadsPath: TEdit
        Left = 16
        Top = 117
        Width = 449
        Height = 23
        TabOrder = 3
      end
      object btnBrowseDownloads: TButton
        Left = 471
        Top = 115
        Width = 100
        Height = 25
        Caption = 'Browse...'
        TabOrder = 4
        OnClick = btnBrowseDownloadsClick
      end
      object btnDetectDownloads: TButton
        Left = 471
        Top = 144
        Width = 100
        Height = 25
        Caption = 'Auto-Detect'
        TabOrder = 5
        OnClick = btnDetectDownloadsClick
      end
      object edtDataFolder: TEdit
        Left = 16
        Top = 197
        Width = 449
        Height = 23
        TabOrder = 6
      end
      object btnBrowseDataFolder: TButton
        Left = 471
        Top = 195
        Width = 100
        Height = 25
        Caption = 'Browse...'
        TabOrder = 7
        OnClick = btnBrowseDataFolderClick
      end
    end
    object tabBackup: TTabSheet
      Caption = 'Backup'
      ImageIndex = 1
      object lblMaxBackups: TLabel
        Left = 16
        Top = 80
        Width = 131
        Height = 15
        Caption = 'Max Backups Per Addon:'
      end
      object lblBackupsInfo: TLabel
        Left = 16
        Top = 144
        Width = 161
        Height = 15
        Caption = 'Keep last 5 backups per addon'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
      end
      object chkEnableBackups: TCheckBox
        Left = 16
        Top = 16
        Width = 200
        Height = 17
        Caption = 'Enable automatic backups'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object chkKeepHistory: TCheckBox
        Left = 16
        Top = 39
        Width = 200
        Height = 17
        Caption = 'Keep backup history'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object trackMaxBackups: TTrackBar
        Left = 16
        Top = 101
        Width = 449
        Height = 37
        Position = 5
        TabOrder = 2
        OnChange = trackMaxBackupsChange
      end
      object edtMaxBackups: TEdit
        Left = 471
        Top = 104
        Width = 100
        Height = 23
        TabOrder = 3
        Text = '5'
        OnChange = edtMaxBackupsChange
      end
      object grpInstallMode: TGroupBox
        Left = 16
        Top = 176
        Width = 555
        Height = 81
        Caption = 'Installation Mode'
        TabOrder = 4
        object rbCopyOnTop: TRadioButton
          Left = 16
          Top = 24
          Width = 500
          Height = 17
          Caption = 'Copy on top (keep existing files, overwrite only new ones)'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object rbFreshInstall: TRadioButton
          Left = 16
          Top = 47
          Width = 500
          Height = 17
          Caption = 'Fresh install (delete old addon files first, then install)'
          TabOrder = 1
        end
      end
      object chkConfirmDeletions: TCheckBox
        Left = 16
        Top = 272
        Width = 400
        Height = 17
        Caption = 'Confirm before deleting backups (disable for instant deletion)'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
    end
    object tabUpdates: TTabSheet
      Caption = 'Updates'
      ImageIndex = 2
      object lblBrowserZoom: TLabel
        Left = 16
        Top = 288
        Width = 170
        Height = 15
        Caption = 'Embedded Browser Zoom Level:'
      end
      object lblZoomPercent: TLabel
        Left = 471
        Top = 312
        Width = 28
        Height = 15
        Caption = '100%'
      end
      object grpUpdateMethod: TGroupBox
        Left = 16
        Top = 16
        Width = 555
        Height = 121
        Caption = 'Update Check Method'
        TabOrder = 0
        object rbEmbeddedBrowser: TRadioButton
          Left = 16
          Top = 24
          Width = 520
          Height = 17
          Caption = 
            'Embedded Browser (legal compliance - displays pages for 5 second' +
            's each)'
          TabOrder = 0
        end
        object rbExternalBrowser: TRadioButton
          Left = 16
          Top = 56
          Width = 520
          Height = 17
          Caption = 'External Browser (opens tabs in your default browser manually)'
          TabOrder = 1
        end
        object rbManualOnly: TRadioButton
          Left = 16
          Top = 88
          Width = 520
          Height = 17
          Caption = 'Manual Only (no automatic version checking)'
          Checked = True
          TabOrder = 2
          TabStop = True
        end
      end
      object chkUseInGameTracking: TCheckBox
        Left = 16
        Top = 152
        Width = 500
        Height = 17
        Caption = 
          'Use In-Game Version Tracking (requires GregsVersionTracker addon' +
          ')'
        TabOrder = 1
      end
      object grpFileWatcher: TGroupBox
        Left = 16
        Top = 184
        Width = 555
        Height = 89
        Caption = 'FileWatcher - Downloads Monitor'
        TabOrder = 2
        object lblPollingInterval: TLabel
          Left = 192
          Top = 28
          Width = 134
          Height = 15
          Caption = 'Check every (in seconds):'
        end
        object lblPollingSeconds: TLabel
          Left = 400
          Top = 28
          Width = 43
          Height = 15
          Caption = 'seconds'
        end
        object chkEnableFileWatcher: TCheckBox
          Left = 16
          Top = 24
          Width = 160
          Height = 17
          Caption = 'Enable FileWatcher'
          TabOrder = 0
          OnClick = chkEnableFileWatcherClick
        end
        object edtPollingInterval: TEdit
          Left = 336
          Top = 24
          Width = 50
          Height = 23
          TabOrder = 1
          Text = '5'
        end
        object chkBackupBeforeInstall: TCheckBox
          Left = 16
          Top = 53
          Width = 240
          Height = 17
          Caption = 'Backup existing addons before install'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object chkDeleteZipAfterInstall: TCheckBox
          Left = 272
          Top = 53
          Width = 240
          Height = 17
          Caption = 'Delete ZIP after successful install'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
      end
      object trackBrowserZoom: TTrackBar
        Left = 16
        Top = 309
        Width = 449
        Height = 37
        Max = 200
        Min = 50
        Frequency = 10
        Position = 100
        TabOrder = 3
        OnChange = trackBrowserZoomChange
      end
    end
    object tabWindow: TTabSheet
      Caption = 'Window'
      ImageIndex = 3
      object chkAlwaysOnTop: TCheckBox
        Left = 16
        Top = 16
        Width = 300
        Height = 17
        Caption = 'Always stay on top'
        TabOrder = 0
      end
      object chkMinimizeToTray: TCheckBox
        Left = 16
        Top = 48
        Width = 300
        Height = 17
        Caption = 'Minimize to system tray'
        TabOrder = 1
        OnClick = chkMinimizeToTrayClick
      end
      object chkStartMinimized: TCheckBox
        Left = 40
        Top = 80
        Width = 300
        Height = 17
        Caption = 'Start minimized to tray'
        Enabled = False
        TabOrder = 2
      end
      object chkRememberPosition: TCheckBox
        Left = 16
        Top = 112
        Width = 300
        Height = 17
        Caption = 'Remember window position'
        TabOrder = 3
      end
    end
    object tabAppearance: TTabSheet
      Caption = 'Appearance'
      ImageIndex = 4
      object lblTheme: TLabel
        Left = 16
        Top = 16
        Width = 40
        Height = 15
        Caption = 'Theme:'
      end
      object lblThemeInfo: TLabel
        Left = 16
        Top = 72
        Width = 328
        Height = 15
        Caption = 'Theme changes will take effect after restarting the application.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
      end
      object cmbTheme: TComboBox
        Left = 16
        Top = 37
        Width = 300
        Height = 23
        Style = csDropDownList
        TabOrder = 0
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 470
    Width = 600
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TButton
      Left = 328
      Top = 12
      Width = 80
      Height = 30
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 414
      Top = 12
      Width = 80
      Height = 30
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
    object btnApply: TButton
      Left = 500
      Top = 12
      Width = 80
      Height = 30
      Caption = 'Apply'
      Enabled = False
      TabOrder = 2
      OnClick = btnApplyClick
    end
  end
end
