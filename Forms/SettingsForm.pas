unit SettingsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.FileCtrl, Vcl.Themes, System.IOUtils, System.Win.Registry,
  System.Types,
  AddonManager;

type
  TForm2 = class(TForm)
    PageControl1: TPageControl;
    tabPaths: TTabSheet;
    tabBackup: TTabSheet;
    tabUpdates: TTabSheet;
    tabWindow: TTabSheet;
    tabAppearance: TTabSheet;
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    btnDetectWoW: TButton;
    btnBrowseWoW: TButton;
    lblWoWPath: TLabel;
    edtWoWPath: TEdit;
    lblDownloads: TLabel;
    edtDownloadsPath: TEdit;
    btnBrowseDownloads: TButton;
    btnDetectDownloads: TButton;
    lblDataFolder: TLabel;
    edtDataFolder: TEdit;
    btnBrowseDataFolder: TButton;
    chkEnableBackups: TCheckBox;
    chkKeepHistory: TCheckBox;
    lblMaxBackups: TLabel;
    trackMaxBackups: TTrackBar;
    edtMaxBackups: TEdit;
    lblBackupsInfo: TLabel;
    grpInstallMode: TGroupBox;
    rbCopyOnTop: TRadioButton;
    rbFreshInstall: TRadioButton;
    chkConfirmDeletions: TCheckBox;
    grpUpdateMethod: TGroupBox;
    rbEmbeddedBrowser: TRadioButton;
    rbExternalBrowser: TRadioButton;
    rbManualOnly: TRadioButton;
    chkUseInGameTracking: TCheckBox;
    lblBrowserZoom: TLabel;
    trackBrowserZoom: TTrackBar;
    lblZoomPercent: TLabel;
    grpFileWatcher: TGroupBox;
    chkEnableFileWatcher: TCheckBox;
    lblPollingInterval: TLabel;
    edtPollingInterval: TEdit;
    lblPollingSeconds: TLabel;
    chkDeleteZipAfterInstall: TCheckBox;
    chkBackupBeforeInstall: TCheckBox;
    chkAlwaysOnTop: TCheckBox;
    chkMinimizeToTray: TCheckBox;
    chkStartMinimized: TCheckBox;
    chkRememberPosition: TCheckBox;
    lblTheme: TLabel;
    cmbTheme: TComboBox;
    lblThemeInfo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnDetectWoWClick(Sender: TObject);
    procedure btnBrowseWoWClick(Sender: TObject);
    procedure btnDetectDownloadsClick(Sender: TObject);
    procedure btnBrowseDownloadsClick(Sender: TObject);
    procedure btnBrowseDataFolderClick(Sender: TObject);
    procedure trackMaxBackupsChange(Sender: TObject);
    procedure edtMaxBackupsChange(Sender: TObject);
    procedure trackBrowserZoomChange(Sender: TObject);
    procedure chkMinimizeToTrayClick(Sender: TObject);
    procedure chkEnableFileWatcherClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
  private
    FAddonManager: TAddonManager;
    FModified: Boolean;
    
    function FindWoWInstallation: string;
    function GetDownloadsFolderPath: string;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure LoadAvailableThemes;
    procedure UpdateMaxBackupsDisplay;
    procedure UpdateZoomDisplay;
    procedure UpdateFileWatcherControls;
    procedure SetModified;
  public
    constructor Create(AOwner: TComponent; AAddonManager: TAddonManager); reintroduce;
  end;

var
  Settings: TForm2;

implementation

{$R *.dfm}


const
  // Path patterns - no drives hardcoded
  WOW_PATH_PATTERNS: array[0..3] of string = (
    'Program Files (x86)\World of Warcraft\_retail_\Interface\AddOns\',
    'Program Files\World of Warcraft\_retail_\Interface\AddOns\',
    'Games\World of Warcraft\_retail_\Interface\AddOns\',
    'Users\Public\Games\World of Warcraft\_retail_\Interface\AddOns\'
  );

constructor TForm2.Create(AOwner: TComponent; AAddonManager: TAddonManager);
begin
  inherited Create(AOwner);
  FAddonManager := AAddonManager;
  FModified := False;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  
  // Setup trackbars
  trackMaxBackups.Min := 0;
  trackMaxBackups.Max := 10;
  trackMaxBackups.Frequency := 1;
  
  trackBrowserZoom.Min := 50;
  trackBrowserZoom.Max := 200;
  trackBrowserZoom.Frequency := 10;
  
  LoadAvailableThemes;
  LoadSettings;
end;

procedure TForm2.LoadSettings;
var
  UpdateMethod: string;
begin
  // Paths
  edtWoWPath.Text := FAddonManager.WoWAddonPath;
  edtDownloadsPath.Text := FAddonManager.DownloadFolder;
  edtDataFolder.Text := FAddonManager.DataFolder;
  
  // Backup settings
  chkEnableBackups.Checked := FAddonManager.BackupEnabled;
  chkKeepHistory.Checked := FAddonManager.KeepBackupHistory;
  
  trackMaxBackups.Position := FAddonManager.MaxBackupsPerAddon;
  edtMaxBackups.Text := IntToStr(FAddonManager.MaxBackupsPerAddon);
  UpdateMaxBackupsDisplay;
  
  rbCopyOnTop.Checked := not FAddonManager.FreshInstallMode;
  rbFreshInstall.Checked := FAddonManager.FreshInstallMode;
  
  chkConfirmDeletions.Checked := FAddonManager.IniFile.ReadBool('Settings', 'ConfirmDeletions', True);
  
  // Update checking
  UpdateMethod := FAddonManager.IniFile.ReadString('Updates', 'UpdateCheckMethod', 'manual');

  if UpdateMethod = 'embedded' then
    rbEmbeddedBrowser.Checked := True
  else if UpdateMethod = 'external' then
    rbExternalBrowser.Checked := True
  else
    rbManualOnly.Checked := True;
  
  chkUseInGameTracking.Checked := FAddonManager.IniFile.ReadBool('Updates', 'UseInGameTracking', False);

  trackBrowserZoom.Position := FAddonManager.IniFile.ReadInteger('Updates', 'EmbeddedBrowserZoom', 100);
  UpdateZoomDisplay;
  
  // FileWatcher settings
  chkEnableFileWatcher.Checked := FAddonManager.IniFile.ReadBool('FileWatcher', 'Enabled', False);
  edtPollingInterval.Text := FAddonManager.IniFile.ReadString('FileWatcher', 'PollingInterval', '5');
  chkBackupBeforeInstall.Checked := FAddonManager.IniFile.ReadBool('FileWatcher', 'BackupBeforeInstall', True);
  chkDeleteZipAfterInstall.Checked := FAddonManager.IniFile.ReadBool('FileWatcher', 'DeleteZipAfterInstall', True);
  UpdateFileWatcherControls;
  
  // Window behavior
  chkAlwaysOnTop.Checked := FAddonManager.IniFile.ReadBool('UI', 'AlwaysOnTop', False);
  chkMinimizeToTray.Checked := FAddonManager.IniFile.ReadBool('UI', 'MinimizeToTray', False);
  chkStartMinimized.Checked := FAddonManager.IniFile.ReadBool('UI', 'StartMinimizedToTray', False);
  chkRememberPosition.Checked := FAddonManager.IniFile.ReadBool('UI', 'RememberWindowPosition', False);
  
  chkStartMinimized.Enabled := chkMinimizeToTray.Checked;
  
  // Appearance
  cmbTheme.ItemIndex := cmbTheme.Items.IndexOf(
    FAddonManager.IniFile.ReadString('UI', 'Theme', 'System Default')
  );
  if cmbTheme.ItemIndex = -1 then
    cmbTheme.ItemIndex := 0;
    
  FModified := False;
  btnApply.Enabled := False;
end;

procedure TForm2.SaveSettings;
var
  UpdateMethod: string;
begin
  // Paths
  FAddonManager.WoWAddonPath := edtWoWPath.Text;
  FAddonManager.DownloadFolder := edtDownloadsPath.Text;
  
  // Backup settings
  FAddonManager.BackupEnabled := chkEnableBackups.Checked;
  FAddonManager.KeepBackupHistory := chkKeepHistory.Checked;
  
  if StrToIntDef(edtMaxBackups.Text, 5) <= 0 then
    FAddonManager.MaxBackupsPerAddon := -1 // Infinite
  else
    FAddonManager.MaxBackupsPerAddon := StrToIntDef(edtMaxBackups.Text, 5);
    
  FAddonManager.FreshInstallMode := rbFreshInstall.Checked;
  
  FAddonManager.IniFile.WriteBool('Settings', 'ConfirmDeletions', chkConfirmDeletions.Checked);
  
  // Update checking
  if rbEmbeddedBrowser.Checked then
    UpdateMethod := 'embedded'
  else if rbExternalBrowser.Checked then
    UpdateMethod := 'external'
  else
    UpdateMethod := 'manual';
    
  FAddonManager.IniFile.WriteString('Updates', 'UpdateCheckMethod', UpdateMethod);
  FAddonManager.IniFile.WriteBool('Updates', 'UseInGameTracking', chkUseInGameTracking.Checked);
  FAddonManager.IniFile.WriteInteger('Updates', 'EmbeddedBrowserZoom', trackBrowserZoom.Position);
  
  // FileWatcher settings
  FAddonManager.IniFile.WriteBool('FileWatcher', 'Enabled', chkEnableFileWatcher.Checked);
  FAddonManager.IniFile.WriteString('FileWatcher', 'PollingInterval', edtPollingInterval.Text);
  FAddonManager.IniFile.WriteBool('FileWatcher', 'BackupBeforeInstall', chkBackupBeforeInstall.Checked);
  FAddonManager.IniFile.WriteBool('FileWatcher', 'DeleteZipAfterInstall', chkDeleteZipAfterInstall.Checked);
  
  // Window behavior
  FAddonManager.IniFile.WriteBool('UI', 'AlwaysOnTop', chkAlwaysOnTop.Checked);
  FAddonManager.IniFile.WriteBool('UI', 'MinimizeToTray', chkMinimizeToTray.Checked);
  FAddonManager.IniFile.WriteBool('UI', 'StartMinimizedToTray', chkStartMinimized.Checked);
  FAddonManager.IniFile.WriteBool('UI', 'RememberWindowPosition', chkRememberPosition.Checked);
  
  // Appearance
  if cmbTheme.ItemIndex >= 0 then
    FAddonManager.IniFile.WriteString('UI', 'Theme', cmbTheme.Text);
  
  FAddonManager.SaveSettings;
  
  FModified := False;
  btnApply.Enabled := False;
end;

procedure TForm2.LoadAvailableThemes;
var
  StyleName: string;
begin
  cmbTheme.Items.Clear;
  cmbTheme.Items.Add('System Default');
  
  // Add all installed VCL Styles
  for StyleName in TStyleManager.StyleNames do
    cmbTheme.Items.Add(StyleName);
end;

function TForm2.FindWoWInstallation: string;
var
  Drives: TStringDynArray;
  Drive: string;
  Pattern: string;
  TestPath: string;
  DriveList: TStringList;
begin
  Result := '';

  // Get all available drives
  Drives := TDirectory.GetLogicalDrives;

  // Sort alphabetically (C, D, E, etc)
  DriveList := TStringList.Create;
  try
    for Drive in Drives do
      DriveList.Add(Drive);
    DriveList.Sort;

    // For each drive (alphabetically)
    for Drive in DriveList do
    begin
      // Skip non-fixed drives (CD-ROM, network, etc) - only local hard drives
      if GetDriveType(PChar(Drive)) <> DRIVE_FIXED then
        Continue;

      // Try each path pattern on this drive
      for Pattern in WOW_PATH_PATTERNS do
      begin
        TestPath := TPath.Combine(Drive, Pattern);

        if TDirectory.Exists(TestPath) then
        begin
          Result := TestPath;
          Exit; // Just exit - finally will clean up!
        end;
      end;
    end;
  finally
    DriveList.Free; // This ALWAYS runs - even after Exit!
  end;
end;

function TForm2.GetDownloadsFolderPath: string;
var
  Reg: TRegistry;
begin
  Result := '';
  
  // Try registry (HKEY_CURRENT_USER - no admin needed)
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    
    if Reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders') then
    begin
      Result := Reg.ReadString('{374DE290-123F-4565-9164-39C4925E467B}');
      Reg.CloseKey;
      
      if (Result <> '') and TDirectory.Exists(Result) then
        Exit;
    end;
  finally
    Reg.Free;
  end;
  
  // Fallback: standard location
  Result := TPath.Combine(TPath.GetHomePath, 'Downloads');
  
  if not TDirectory.Exists(Result) then
    Result := '';
end;

procedure TForm2.btnDetectWoWClick(Sender: TObject);
var
  Path: string;
begin
  Path := FindWoWInstallation;
  
  if Path <> '' then
  begin
    edtWoWPath.Text := Path;
    ShowMessage('WoW installation found: ' + Path);
    SetModified;
  end
  else
    ShowMessage('Could not find WoW installation automatically. Please browse manually.');
end;

procedure TForm2.btnBrowseWoWClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := edtWoWPath.Text;
  
  if SelectDirectory('Select WoW AddOns Folder', '', Dir, [sdNewUI, sdNewFolder]) then
  begin
    edtWoWPath.Text := Dir;
    SetModified;
  end;
end;

procedure TForm2.btnDetectDownloadsClick(Sender: TObject);
var
  Path: string;
begin
  Path := GetDownloadsFolderPath;
  
  if Path <> '' then
  begin
    edtDownloadsPath.Text := Path;
    ShowMessage('Downloads folder found: ' + Path);
    SetModified;
  end
  else
    ShowMessage('Could not detect Downloads folder. Please browse manually.');
end;

procedure TForm2.btnBrowseDownloadsClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := edtDownloadsPath.Text;
  
  if SelectDirectory('Select Downloads Folder', '', Dir, [sdNewUI, sdNewFolder]) then
  begin
    edtDownloadsPath.Text := Dir;
    SetModified;
  end;
end;

procedure TForm2.btnBrowseDataFolderClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := edtDataFolder.Text;
  
  if SelectDirectory('Select Data Folder (for settings and backups)', '', Dir, [sdNewUI, sdNewFolder]) then
  begin
    edtDataFolder.Text := Dir;
    SetModified;
  end;
end;

procedure TForm2.trackMaxBackupsChange(Sender: TObject);
begin
  edtMaxBackups.Text := IntToStr(trackMaxBackups.Position);
  UpdateMaxBackupsDisplay;
  SetModified;
end;

procedure TForm2.edtMaxBackupsChange(Sender: TObject);
var
  Value: Integer;
begin
  Value := StrToIntDef(edtMaxBackups.Text, 5);
  
  if Value <= 0 then
  begin
    trackMaxBackups.Position := 0;
    UpdateMaxBackupsDisplay;
  end
  else if Value > 10 then
  begin
    edtMaxBackups.Text := '10';
    trackMaxBackups.Position := 10;
  end
  else
    trackMaxBackups.Position := Value;
    
  SetModified;
end;

procedure TForm2.UpdateMaxBackupsDisplay;
var
  Value: Integer;
begin
  Value := StrToIntDef(edtMaxBackups.Text, 5);
  
  if Value <= 0 then
    lblBackupsInfo.Caption := 'Unlimited backups (no automatic pruning)'
  else
    lblBackupsInfo.Caption := Format('Keep last %d backups per addon', [Value]);
end;

procedure TForm2.trackBrowserZoomChange(Sender: TObject);
begin
  UpdateZoomDisplay;
  SetModified;
end;

procedure TForm2.UpdateZoomDisplay;
begin
  lblZoomPercent.Caption := Format('%d%%', [trackBrowserZoom.Position]);
end;

procedure TForm2.chkMinimizeToTrayClick(Sender: TObject);
begin
  chkStartMinimized.Enabled := chkMinimizeToTray.Checked;
  if not chkMinimizeToTray.Checked then
    chkStartMinimized.Checked := False;
  SetModified;
end;

procedure TForm2.chkEnableFileWatcherClick(Sender: TObject);
begin
  UpdateFileWatcherControls;
  SetModified;
end;

procedure TForm2.UpdateFileWatcherControls;
begin
  lblPollingInterval.Enabled := chkEnableFileWatcher.Checked;
  edtPollingInterval.Enabled := chkEnableFileWatcher.Checked;
  lblPollingSeconds.Enabled := chkEnableFileWatcher.Checked;
  chkBackupBeforeInstall.Enabled := chkEnableFileWatcher.Checked;
  chkDeleteZipAfterInstall.Enabled := chkEnableFileWatcher.Checked;
end;

procedure TForm2.SetModified;
begin
  FModified := True;
  btnApply.Enabled := True;
end;

procedure TForm2.btnOKClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOk;
end;

procedure TForm2.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TForm2.btnApplyClick(Sender: TObject);
begin
  SaveSettings;
end;

end.