unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.Menus, System.DateUtils,
  AddonManager, BackupEngine, BackupEngineThreaded, FileWatcher;

type
  TForm1 = class(TForm)
    pnlTop: TPanel;
    lblTitle: TLabel;
    ListView1: TListView;
    pnlBottom: TPanel;
    StatusBar1: TStatusBar;
    ProgressBar1: TProgressBar;
    btnAddAddon: TButton;
    btnEditAddon: TButton;
    btnRemoveAddon: TButton;
    btnBackup: TButton;
    btnBackupAll: TButton;
    btnRestore: TButton;
    btnSettings: TButton;
    btnCheckUpdates: TButton;
    lblWatcherStatus: TLabel;
    PopupMenu1: TPopupMenu;
    mnuBackup: TMenuItem;
    mnuRestore: TMenuItem;
    mnuEdit: TMenuItem;
    mnuRemove: TMenuItem;
    N1: TMenuItem;
    mnuViewBackups: TMenuItem;
    // NEW BUTTON - we'll add this to the .dfm manually or comment it out
    // btnScanAddons: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddAddonClick(Sender: TObject);
    procedure btnEditAddonClick(Sender: TObject);
    procedure btnRemoveAddonClick(Sender: TObject);
    procedure btnBackupClick(Sender: TObject);
    procedure btnBackupAllClick(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnCheckUpdatesClick(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure mnuBackupClick(Sender: TObject);
    procedure mnuRestoreClick(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuRemoveClick(Sender: TObject);
    procedure mnuViewBackupsClick(Sender: TObject);
    procedure lblWatcherStatusClick(Sender: TObject);
    // NEW: Scan addons
    procedure btnScanAddonsClick(Sender: TObject);
    procedure mnuSetURLClick(Sender: TObject);
  private
    FAddonManager: TAddonManager;
    FBackupEngine: TBackupEngine;
    FFileWatcher: TFileWatcher;
    FFullBackupInProgress: Boolean;
    FFullBackupCount: Integer;
    FFullBackupTotal: Integer;
    
    procedure LoadAddonList;
    procedure AddAddonToList(const AddonInfo: TAddonInfo; Index: Integer);
    procedure UpdateStatusBar(const Msg: string);
    procedure OnBackupProgress(const Msg: string; Percent: Integer);
    procedure OnBackupComplete(const BackupPath: string);
    procedure OnBackupError(const ErrorMsg: string);
    
    procedure CheckFirstRun;
    procedure CheckPeriodicBackupReminder;
    function PromptForFullBackup: Boolean;
    procedure CreateFullSystemBackup;
    procedure OnFullBackupProgress(const Msg: string; Percent: Integer);
    procedure OnFullBackupComplete(const BackupPath: string);
    procedure OnFullBackupError(const ErrorMsg: string);
    
    procedure OnAddonDetected(Sender: TObject; const ZipInfo: TAddonZipInfo);
    procedure UpdateWatcherStatus;
    
    // NEW: Scan-related
    procedure ScanAndImportAddons;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.IOUtils, Winapi.ShellAPI, SettingsForm, RestoreForm;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize managers
  FAddonManager := TAddonManager.Create;
  
  FBackupEngine := TBackupEngine.Create(
    FAddonManager.WoWAddonPath,
    FAddonManager.DataFolder,
    FAddonManager.MaxBackupsPerAddon,
    True  // ConfirmDeletions
  );
  
  // Initialize FileWatcher
  FFileWatcher := TFileWatcher.Create(Self, FAddonManager, FBackupEngine);
  FFileWatcher.DownloadFolder := FAddonManager.DownloadFolder;
  FFileWatcher.OnAddonDetected := OnAddonDetected;
  
  FFullBackupInProgress := False;
  
  // Setup ListView columns
  ListView1.ViewStyle := vsReport;
  ListView1.Columns.Add.Caption := 'Addon Name';
  ListView1.Columns.Add.Caption := 'Folder';
  ListView1.Columns.Add.Caption := 'Last Updated';
  ListView1.Columns.Add.Caption := 'Version';
  ListView1.Columns.Add.Caption := 'URL Status';  // NEW COLUMN
  ListView1.Columns[0].Width := 200;
  ListView1.Columns[1].Width := 150;
  ListView1.Columns[2].Width := 120;
  ListView1.Columns[3].Width := 100;
  ListView1.Columns[4].Width := 100;  // URL Status column
  
  // Check if WoW path is set
  if FAddonManager.WoWAddonPath = '' then
  begin
    ShowMessage('Welcome to Greg''s Manager!' + sLineBreak + sLineBreak +
                'Let''s find your World of Warcraft installation.');
    btnSettingsClick(nil);
  end;
  
  // Load addon list
  LoadAddonList;
  
  // Auto-scan on first run if list is empty
  if (FAddonManager.AddonCount = 0) and (FAddonManager.WoWAddonPath <> '') then
  begin
    if MessageDlg(
      'No addons configured yet.' + sLineBreak + sLineBreak +
      'Would you like to scan your AddOns folder and import everything?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      ScanAndImportAddons;
    end;
  end;
  
  // Check for first run / periodic backup
  CheckFirstRun;
  CheckPeriodicBackupReminder;
  
  // Start FileWatcher if enabled
  if FAddonManager.IniFile.ReadBool('FileWatcher', 'Enabled', False) then
  begin
    try
      FFileWatcher.Start;
    except
      on E: Exception do
        ShowMessage('Could not start FileWatcher: ' + E.Message);
    end;
  end;
  
  UpdateWatcherStatus;
  UpdateStatusBar('Ready');
  ProgressBar1.Visible := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FFileWatcher) then
    FFileWatcher.Stop;
    
  FBackupEngine.Free;
  FAddonManager.Free;
end;

procedure TForm1.ScanAndImportAddons;
var
  ScanResult: TScanResult;
  Msg: string;
begin
  if FAddonManager.WoWAddonPath = '' then
  begin
    ShowMessage('Please configure your WoW AddOns path in Settings first.');
    btnSettingsClick(nil);
    Exit;
  end;
  
  UpdateStatusBar('Scanning AddOns folder...');
  Application.ProcessMessages;
  
  try
    ScanResult := FAddonManager.ScanAndImportAddons(True);
    
    // Build result message
    Msg := Format(
      'Scan Complete!' + sLineBreak + sLineBreak +
      'Total addons found: %d' + sLineBreak +
      'New addons imported: %d' + sLineBreak +
      'Already tracked (skipped): %d',
      [ScanResult.TotalFound, ScanResult.NewImported, ScanResult.Skipped]
    );
    
    if ScanResult.NoURLCount > 0 then
    begin
      Msg := Msg + sLineBreak + sLineBreak +
             Format('⚠ %d addons have no download URL.' + sLineBreak +
                    'You can add URLs manually by right-clicking them.',
                    [ScanResult.NoURLCount]);
    end;
    
    ShowMessage(Msg);
    
    // Reload the list
    LoadAddonList;
    UpdateStatusBar(Format('Imported %d addons', [ScanResult.NewImported]));
    
  except
    on E: Exception do
    begin
      ShowMessage('Error scanning addons: ' + E.Message);
      UpdateStatusBar('Scan failed');
    end;
  end;
end;

procedure TForm1.btnScanAddonsClick(Sender: TObject);
begin
  ScanAndImportAddons;
end;

procedure TForm1.mnuSetURLClick(Sender: TObject);
var
  Index: Integer;
  Addon: TAddonInfo;
  NewURL: string;
begin
  if ListView1.Selected = nil then
  begin
    ShowMessage('Please select an addon first.');
    Exit;
  end;
  
  Index := Integer(ListView1.Selected.Data);
  Addon := FAddonManager.GetAddon(Index);
  
  NewURL := InputBox(
    'Set Download URL',
    Format('Enter download URL for %s:', [Addon.Name]),
    Addon.URL
  );
  
  if NewURL <> '' then
  begin
    Addon.URL := NewURL;
    FAddonManager.UpdateAddon(Index, Addon);
    LoadAddonList;
    UpdateStatusBar('URL updated for ' + Addon.Name);
  end;
end;

procedure TForm1.UpdateWatcherStatus;
begin
  if FFileWatcher.Enabled then
  begin
    lblWatcherStatus.Caption := '👁️ Watching Downloads';
    lblWatcherStatus.Font.Color := clGreen;
    lblWatcherStatus.Font.Style := [fsBold];
    lblWatcherStatus.Hint := 'FileWatcher is monitoring for new addons. Click to disable.';
  end
  else
  begin
    lblWatcherStatus.Caption := '○ Not Watching';
    lblWatcherStatus.Font.Color := clGray;
    lblWatcherStatus.Font.Style := [];
    lblWatcherStatus.Hint := 'FileWatcher is disabled. Click to enable.';
  end;
  
  lblWatcherStatus.ShowHint := True;
end;

procedure TForm1.lblWatcherStatusClick(Sender: TObject);
begin
  // Toggle FileWatcher on click
  if FFileWatcher.Enabled then
  begin
    FFileWatcher.Stop;
    FAddonManager.IniFile.WriteBool('FileWatcher', 'Enabled', False);
    UpdateStatusBar('FileWatcher disabled');
  end
  else
  begin
    try
      FFileWatcher.Start;
      FAddonManager.IniFile.WriteBool('FileWatcher', 'Enabled', True);
      UpdateStatusBar('FileWatcher enabled');
    except
      on E: Exception do
      begin
        ShowMessage('Could not start FileWatcher: ' + E.Message);
        Exit;
      end;
    end;
  end;
  
  FAddonManager.IniFile.UpdateFile;
  UpdateWatcherStatus;
end;

procedure TForm1.OnAddonDetected(Sender: TObject; const ZipInfo: TAddonZipInfo);
begin
  // FileWatcher detected an addon
  Application.BringToFront;
  UpdateStatusBar(Format('Addon detected: %s', [ExtractFileName(ZipInfo.ZipPath)]));
  
  // Flash the form if minimized
  if Application.MainForm.WindowState = wsMinimized then
    FlashWindow(Application.MainForm.Handle, True);
end;

// ... rest of existing MainForm procedures remain the same ...

procedure TForm1.CheckFirstRun;
begin
  // Check if this is first run
  if FAddonManager.IniFile.ReadString('System', 'FirstRunComplete', '') = '' then
  begin
    if PromptForFullBackup then
    begin
      CreateFullSystemBackup;
      FAddonManager.IniFile.WriteString('System', 'FirstRunComplete', 'true');
      FAddonManager.IniFile.UpdateFile;
    end
    else
    begin
      // User declined, still mark first run complete
      FAddonManager.IniFile.WriteString('System', 'FirstRunComplete', 'true');
      FAddonManager.IniFile.UpdateFile;
    end;
  end;
end;

procedure TForm1.CheckPeriodicBackupReminder;
var
  LastFullBackup: TDateTime;
  DaysSince: Integer;
begin
  // Only check if not first run and we have addons
  if (FAddonManager.IniFile.ReadString('System', 'FirstRunComplete', '') <> '') and
     (FAddonManager.AddonCount > 0) then
  begin
    LastFullBackup := FAddonManager.IniFile.ReadDateTime('System', 'LastFullBackup', 0);
    
    if LastFullBackup > 0 then
    begin
      DaysSince := DaysBetween(Now, LastFullBackup);
      
      // Suggest backup every 30 days
      if DaysSince > 30 then
      begin
        if MessageDlg(
          Format('It has been %d days since your last full backup.' + sLineBreak + sLineBreak +
                 'Create a fresh backup of all addons for safety?', [DaysSince]),
          mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          CreateFullSystemBackup;
        end;
      end;
    end;
  end;
end;

function TForm1.PromptForFullBackup: Boolean;
var
  DlgResult: Integer;
begin
  DlgResult := MessageDlg(
    'Welcome to Greg''s Manager!' + sLineBreak + sLineBreak +
    'Before we begin managing your addons, would you like to create' + sLineBreak +
    'a full backup of ALL current addons?' + sLineBreak + sLineBreak +
    'This protects you in case anything goes wrong.' + sLineBreak +
    '(Recommended for first use)' + sLineBreak + sLineBreak +
    'Estimated time: 2-5 minutes',
    mtConfirmation, [mbYes, mbNo], 0
  );
  
  Result := (DlgResult = mrYes);
end;

procedure TForm1.CreateFullSystemBackup;
var
  i: Integer;
  Addon: TAddonInfo;
  Thread: TBackupThread;
begin
  if FAddonManager.AddonCount = 0 then
  begin
    ShowMessage('No addons configured yet. Add some addons first.');
    Exit;
  end;
  
  if MessageDlg(
    Format('Create a full backup of all %d addons?' + sLineBreak + sLineBreak +
           'This may take several minutes.', [FAddonManager.AddonCount]),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  
  // Setup for full backup
  FFullBackupInProgress := True;
  FFullBackupCount := 0;
  FFullBackupTotal := FAddonManager.AddonCount;
  
  btnBackup.Enabled := False;
  btnBackupAll.Enabled := False;
  ProgressBar1.Visible := True;
  ProgressBar1.Position := 0;
  
  UpdateStatusBar(Format('Full backup: Starting %d addons...', [FFullBackupTotal]));
  
  // Backup first addon (will chain through OnFullBackupComplete)
  if FFullBackupTotal > 0 then
  begin
    Addon := FAddonManager.GetAddon(0);
    Thread := TBackupThread.Create(FBackupEngine, Addon.FolderName);
    Thread.OnProgress := OnFullBackupProgress;
    Thread.OnComplete := OnFullBackupComplete;
    Thread.OnError := OnFullBackupError;
    Thread.Start;
  end;
end;

procedure TForm1.OnFullBackupProgress(const Msg: string; Percent: Integer);
begin
  UpdateStatusBar(Format('Full backup: %d/%d - %s', 
    [FFullBackupCount + 1, FFullBackupTotal, Msg]));
end;

procedure TForm1.OnFullBackupComplete(const BackupPath: string);
var
  Addon: TAddonInfo;
  Thread: TBackupThread;
begin
  Inc(FFullBackupCount);
  ProgressBar1.Position := (FFullBackupCount * 100) div FFullBackupTotal;
  
  // Check if more addons to backup
  if FFullBackupCount < FFullBackupTotal then
  begin
    // Backup next addon
    Addon := FAddonManager.GetAddon(FFullBackupCount);
    Thread := TBackupThread.Create(FBackupEngine, Addon.FolderName);
    Thread.OnProgress := OnFullBackupProgress;
    Thread.OnComplete := OnFullBackupComplete;
    Thread.OnError := OnFullBackupError;
    Thread.Start;
  end
  else
  begin
    // All done!
    FFullBackupInProgress := False;
    btnBackup.Enabled := True;
    btnBackupAll.Enabled := True;
    ProgressBar1.Visible := False;
    
    // Save timestamp
    FAddonManager.IniFile.WriteDateTime('System', 'LastFullBackup', Now);
    FAddonManager.IniFile.UpdateFile;
    
    UpdateStatusBar(Format('Full backup complete: %d addons backed up', [FFullBackupCount]));
    ShowMessage(Format('Full system backup created successfully!' + sLineBreak + 
                       '%d addons backed up.', [FFullBackupCount]));
  end;
end;

procedure TForm1.OnFullBackupError(const ErrorMsg: string);
var
  Addon: TAddonInfo;
  Thread: TBackupThread;
begin
  // Log error but continue with next addon
  UpdateStatusBar(Format('Error backing up addon %d: %s', [FFullBackupCount + 1, ErrorMsg]));
  
  Inc(FFullBackupCount);
  
  // Continue with next addon
  if FFullBackupCount < FFullBackupTotal then
  begin
    Addon := FAddonManager.GetAddon(FFullBackupCount);
    Thread := TBackupThread.Create(FBackupEngine, Addon.FolderName);
    Thread.OnProgress := OnFullBackupProgress;
    Thread.OnComplete := OnFullBackupComplete;
    Thread.OnError := OnFullBackupError;
    Thread.Start;
  end
  else
  begin
    // Finished (with errors)
    FFullBackupInProgress := False;
    btnBackup.Enabled := True;
    btnBackupAll.Enabled := True;
    ProgressBar1.Visible := False;
    
    ShowMessage('Full backup completed with some errors.' + sLineBreak +
                       'Check status messages for details.');
  end;
end;

procedure TForm1.LoadAddonList;
var
  i: Integer;
  Addon: TAddonInfo;
begin
  ListView1.Items.Clear;
  
  for i := 0 to FAddonManager.AddonCount - 1 do
  begin
    Addon := FAddonManager.GetAddon(i);
    AddAddonToList(Addon, i);
  end;
  
  UpdateStatusBar(Format('Loaded %d addons', [FAddonManager.AddonCount]));
end;

procedure TForm1.AddAddonToList(const AddonInfo: TAddonInfo; Index: Integer);
var
  Item: TListItem;
  URLStatus: string;
begin
  Item := ListView1.Items.Add;
  Item.Caption := AddonInfo.Name;
  Item.SubItems.Add(AddonInfo.FolderName);
  
  if AddonInfo.LastUpdated > 0 then
    Item.SubItems.Add(DateTimeToStr(AddonInfo.LastUpdated))
  else
    Item.SubItems.Add('Never');
    
  Item.SubItems.Add(AddonInfo.CurrentVersion);
  
  // URL Status column
  if AddonInfo.URL <> '' then
  begin
    URLStatus := '✓ Has URL';
    Item.SubItemImages[3] := 0;  // Could use an image index if we had icons
  end
  else
  begin
    URLStatus := '⚠ No URL';
    // Could color this red if desired
  end;
  Item.SubItems.Add(URLStatus);
  
  Item.Data := Pointer(Index);
end;

procedure TForm1.UpdateStatusBar(const Msg: string);
begin
  StatusBar1.SimpleText := Msg;
  Application.ProcessMessages;
end;

procedure TForm1.btnAddAddonClick(Sender: TObject);
var
  AddonName, FolderName, URL: string;
  Addon: TAddonInfo;
begin
  // Simple input dialogs for now
  AddonName := InputBox('Add Addon', 'Addon Name:', '');
  if AddonName = '' then Exit;
  
  FolderName := InputBox('Add Addon', 'Folder Name (in AddOns directory):', AddonName);
  if FolderName = '' then Exit;
  
  URL := InputBox('Add Addon', 'URL (CurseForge/WoWInterface page):', '');
  
  // Create addon info
  Addon.Name := AddonName;
  Addon.FolderName := FolderName;
  Addon.URL := URL;
  Addon.CurrentVersion := '';
  Addon.LastUpdated := 0;
  Addon.Notes := '';
  
  FAddonManager.AddAddon(Addon);
  LoadAddonList;
  
  UpdateStatusBar('Added: ' + AddonName);
end;

procedure TForm1.btnEditAddonClick(Sender: TObject);
begin
  if ListView1.Selected = nil then
  begin
    ShowMessage('Please select an addon to edit.');
    Exit;
  end;
  
  ShowMessage('Edit dialog - TODO');
  // Will create proper edit form later
end;

procedure TForm1.btnRemoveAddonClick(Sender: TObject);
var
  Index: Integer;
  AddonName: string;
begin
  if ListView1.Selected = nil then
  begin
    ShowMessage('Please select an addon to remove.');
    Exit;
  end;
  
  Index := Integer(ListView1.Selected.Data);
  AddonName := FAddonManager.GetAddon(Index).Name;
  
  if MessageDlg('Remove ' + AddonName + ' from the list?' + sLineBreak + 
                '(This will not delete the addon or backups)',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FAddonManager.RemoveAddon(Index);
    LoadAddonList;
    UpdateStatusBar('Removed: ' + AddonName);
  end;
end;

procedure TForm1.btnBackupClick(Sender: TObject);
var
  Index: Integer;
  Addon: TAddonInfo;
  Thread: TBackupThread;
begin
  if ListView1.Selected = nil then
  begin
    ShowMessage('Please select an addon to backup.');
    Exit;
  end;
  
  Index := Integer(ListView1.Selected.Data);
  Addon := FAddonManager.GetAddon(Index);
  
  // Disable buttons during backup
  btnBackup.Enabled := False;
  btnBackupAll.Enabled := False;
  ProgressBar1.Visible := True;
  ProgressBar1.Position := 0;
  
  // Create threaded backup
  Thread := TBackupThread.Create(FBackupEngine, Addon.FolderName);
  Thread.OnProgress := OnBackupProgress;
  Thread.OnComplete := OnBackupComplete;
  Thread.OnError := OnBackupError;
  Thread.Start;
end;

procedure TForm1.btnBackupAllClick(Sender: TObject);
begin
  CreateFullSystemBackup;
end;

procedure TForm1.OnBackupProgress(const Msg: string; Percent: Integer);
begin
  UpdateStatusBar(Msg);
  ProgressBar1.Position := Percent;
end;

procedure TForm1.OnBackupComplete(const BackupPath: string);
begin
  btnBackup.Enabled := True;
  btnBackupAll.Enabled := True;
  ProgressBar1.Visible := False;
  UpdateStatusBar('Backup complete!');
  ShowMessage('Backup created successfully!');
end;

procedure TForm1.OnBackupError(const ErrorMsg: string);
begin
  btnBackup.Enabled := True;
  btnBackupAll.Enabled := True;
  ProgressBar1.Visible := False;
  UpdateStatusBar('Backup failed');
  ShowMessage('Backup error: ' + ErrorMsg);
end;

procedure TForm1.btnRestoreClick(Sender: TObject);
var
  Index: Integer;
  Addon: TAddonInfo;
  Restore: TForm3;
begin
  if ListView1.Selected = nil then
  begin
    ShowMessage('Please select an addon to restore.');
    Exit;
  end;
  
  Index := Integer(ListView1.Selected.Data);
  Addon := FAddonManager.GetAddon(Index);
  
  // Open restore/version switcher dialog
  Restore := TForm3.Create(Self, FAddonManager, FBackupEngine,
    Addon.Name, Addon.FolderName);
  try
    Restore.ShowModal;
  finally
    Restore.Free;
  end;
end;

procedure TForm1.btnSettingsClick(Sender: TObject);
var
  Settings: TForm2;
  WatcherWasEnabled: Boolean;
begin
  WatcherWasEnabled := FFileWatcher.Enabled;
  
  Settings := TForm2.Create(Self, FAddonManager);
  try
    if Settings.ShowModal = mrOk then
    begin
      // Settings saved, update FileWatcher
      FFileWatcher.DownloadFolder := FAddonManager.DownloadFolder;
      FFileWatcher.PollingInterval := 
        FAddonManager.IniFile.ReadInteger('FileWatcher', 'PollingInterval', 5) * 1000;
      
      // Restart FileWatcher if settings changed
      if FAddonManager.IniFile.ReadBool('FileWatcher', 'Enabled', False) then
      begin
        if not WatcherWasEnabled then
        begin
          try
            FFileWatcher.Start;
          except
            on E: Exception do
              ShowMessage('Could not start FileWatcher: ' + E.Message);
          end;
        end;
      end
      else
      begin
        if WatcherWasEnabled then
          FFileWatcher.Stop;
      end;
      
      UpdateWatcherStatus;
      UpdateStatusBar('Settings updated');
    end;
  finally
    Settings.Free;
  end;
end;

procedure TForm1.btnCheckUpdatesClick(Sender: TObject);
begin
  ShowMessage('Version checking - TODO');
  // Will implement embedded browser checking later
end;

procedure TForm1.ListView1DblClick(Sender: TObject);
var
  Index: Integer;
  Addon: TAddonInfo;
begin
  if ListView1.Selected = nil then Exit;
  
  Index := Integer(ListView1.Selected.Data);
  Addon := FAddonManager.GetAddon(Index);
  
  if Addon.URL <> '' then
    ShellExecute(0, 'open', PChar(Addon.URL), nil, nil, SW_SHOWNORMAL);
end;

// PopupMenu handlers
procedure TForm1.mnuBackupClick(Sender: TObject);
begin
  btnBackupClick(Sender);
end;

procedure TForm1.mnuRestoreClick(Sender: TObject);
begin
  btnRestoreClick(Sender);
end;

procedure TForm1.mnuEditClick(Sender: TObject);
begin
  btnEditAddonClick(Sender);
end;

procedure TForm1.mnuRemoveClick(Sender: TObject);
begin
  btnRemoveAddonClick(Sender);
end;

procedure TForm1.mnuViewBackupsClick(Sender: TObject);
var
  Index: Integer;
  Addon: TAddonInfo;
  BackupPath: string;
begin
  if ListView1.Selected = nil then Exit;
  
  Index := Integer(ListView1.Selected.Data);
  Addon := FAddonManager.GetAddon(Index);
  BackupPath := TPath.Combine(FAddonManager.DataFolder, 'Backups\' + Addon.FolderName);
  
  if TDirectory.Exists(BackupPath) then
    ShellExecute(0, 'open', PChar(BackupPath), nil, nil, SW_SHOWNORMAL)
  else
    ShowMessage('No backups found for this addon.');
end;

end.
