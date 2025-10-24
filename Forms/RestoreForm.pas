unit RestoreForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, System.IOUtils,
  System.DateUtils,  // For HoursBetween
  System.StrUtils,   // For IfThen
  AddonManager, BackupEngine, BackupEngineThreaded;

type
  TForm3 = class(TForm)
    pnlTop: TPanel;
    lblTitle: TLabel;
    lblAddonName: TLabel;
    ListView1: TListView;
    pnlOptions: TPanel;
    grpRestoreType: TGroupBox;
    rbRestoreEverything: TRadioButton;
    rbRestoreAddonOnly: TRadioButton;
    rbRestoreSettingsOnly: TRadioButton;
    chkBackupFirst: TCheckBox;
    pnlButtons: TPanel;
    btnSwitch: TButton;
    btnDelete: TButton;
    btnClose: TButton;
    StatusBar1: TStatusBar;
    ProgressBar1: TProgressBar;
    lblActiveVersion: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSwitchClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ListView1DblClick(Sender: TObject);
  private
    FAddonManager: TAddonManager;
    FBackupEngine: TBackupEngine;
    FAddonName: string;
    FAddonFolderName: string;
    FActiveBackupPath: string;
    FSwitching: Boolean;
    
    procedure LoadBackupHistory;
    procedure UpdateActiveVersionDisplay;
    function GetSelectedBackup: TBackupInfo;
    function GetActiveBackupPath: string;
    procedure SetActiveBackupPath(const Path: string);
    procedure UpdateButtonStates;
    procedure OnSwitchProgress(const Msg: string; Percent: Integer);
    procedure OnSwitchComplete(Sender: TObject);
    procedure OnSwitchError(const ErrorMsg: string);
    procedure ScheduleDeletion(const BackupPath: string);
    procedure ImmediateDeletion(const BackupPath: string);
  public
    constructor Create(AOwner: TComponent; AAddonManager: TAddonManager; 
      ABackupEngine: TBackupEngine; const AddonName, AddonFolderName: string); reintroduce;
  end;

var
  Restore: TForm3;

implementation

{$R *.dfm}

uses
  Winapi.ShellAPI;

// TODO: FUTURE FEATURE - Advanced Restoration Options
// =====================================================
// Potential enhancements for v2.0+:
// 
// 1. Comparison Summary:
//    - Show file-level diff (added/removed/modified files)
//    - Size deltas, file counts
//    - Highlight significant changes
//
// 2. External Diff Tool Integration:
//    - Button: "Compare in [WinMerge/Beyond Compare/etc]"
//    - Extract both versions to temp folders
//    - ShellExecute the diff tool with both paths
//    - Let power users analyze manually before committing
//
// 3. Selective File Restoration:
//    - Checkbox tree of individual files
//    - Restore only selected files/folders
//    - Granular control for advanced users
//
// Design decision: Keep v1.0 simple - version switching only
// Users needing detailed comparison can manually explore backup folders

constructor TForm3.Create(AOwner: TComponent; AAddonManager: TAddonManager;
  ABackupEngine: TBackupEngine; const AddonName, AddonFolderName: string);
begin
  inherited Create(AOwner);
  FAddonManager := AAddonManager;
  FBackupEngine := ABackupEngine;
  FAddonName := AddonName;
  FAddonFolderName := AddonFolderName;
  FSwitching := False;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  Caption := 'Restore / Switch Versions - ' + FAddonName;
  lblAddonName.Caption := 'Addon: ' + FAddonName;
  
  // Setup ListView
  ListView1.ViewStyle := vsReport;
  ListView1.Columns.Add.Caption := '';
  ListView1.Columns.Add.Caption := 'Backup Date/Time';
  ListView1.Columns.Add.Caption := 'Size';
  ListView1.Columns.Add.Caption := 'Status';
  ListView1.Columns[0].Width := 30;  // Radio indicator
  ListView1.Columns[1].Width := 200;
  ListView1.Columns[2].Width := 80;
  ListView1.Columns[3].Width := 150;
  
  // Default restore options
  rbRestoreEverything.Checked := True;
  chkBackupFirst.Checked := True;
  
  // Load backup history
  LoadBackupHistory;
  UpdateActiveVersionDisplay;
  UpdateButtonStates;
  
  ProgressBar1.Visible := False;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  // Nothing to free - managers owned by MainForm
end;

function TForm3.GetActiveBackupPath: string;
begin
  // Read from INI which backup is currently active
  Result := FAddonManager.IniFile.ReadString('ActiveVersions', FAddonFolderName, '');
end;

procedure TForm3.SetActiveBackupPath(const Path: string);
begin
  // Store which backup is active
  FAddonManager.IniFile.WriteString('ActiveVersions', FAddonFolderName, Path);
  FAddonManager.IniFile.UpdateFile;
end;

procedure TForm3.LoadBackupHistory;
var
  Backups: TArray<TBackupInfo>;
  Backup: TBackupInfo;
  Item: TListItem;
  StatusText: string;
  IsActive: Boolean;
  i: Integer;
begin
  ListView1.Items.Clear;
  FActiveBackupPath := GetActiveBackupPath;
  
  Backups := FBackupEngine.GetBackupHistory(FAddonFolderName);
  
  if Length(Backups) = 0 then
  begin
    StatusBar1.SimpleText := 'No backups found for this addon.';
    Exit;
  end;
  
  // Add backups to list (already sorted newest first)
  for i := 0 to High(Backups) do
  begin
    Backup := Backups[i];
    Item := ListView1.Items.Add;
    
    // Check if this is the active version
    IsActive := (FActiveBackupPath <> '') and 
                (Pos(ExtractFileName(Backup.BackupPath), FActiveBackupPath) > 0);
    
    // Radio indicator
    if IsActive then
      Item.Caption := '●'  // Active indicator
    else
      Item.Caption := '○';  // Inactive
      
    // DateTime
    Item.SubItems.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss', Backup.Timestamp));
    
    // Size
    Item.SubItems.Add(Format('%d MB', [Backup.SizeMB]));
    
    // Status
    case Backup.Status of
      bsNormal: StatusText := 'Available';
      bsPendingDeletion: 
        StatusText := Format('Deleting in %d hours', 
          [HoursBetween(Backup.DeletionScheduledFor, Now)]);
    end;
    
    Item.SubItems.Add(StatusText);
    Item.Data := Pointer(i);  // Store index
    
    // Highlight active version
    if IsActive then
    begin
      Item.MakeVisible(False);
      ListView1.Selected := Item;
    end;
  end;
  
  StatusBar1.SimpleText := Format('%d backups available', [Length(Backups)]);
end;

procedure TForm3.UpdateActiveVersionDisplay;
var
  ActivePath: string;
  ActiveTimestamp: string;
begin
  ActivePath := GetActiveBackupPath;
  
  if ActivePath <> '' then
  begin
    // Extract timestamp from path
    ActiveTimestamp := ExtractFileName(ActivePath);
    lblActiveVersion.Caption := 'Active Version: ' + ActiveTimestamp;
    lblActiveVersion.Font.Style := [fsBold];
  end
  else
  begin
    lblActiveVersion.Caption := 'Active Version: Current (no switching yet)';
    lblActiveVersion.Font.Style := [];
  end;
end;

function TForm3.GetSelectedBackup: TBackupInfo;
var
  Backups: TArray<TBackupInfo>;
  Index: Integer;
begin
  if ListView1.Selected = nil then
    raise Exception.Create('No backup selected');
    
  Index := Integer(ListView1.Selected.Data);
  Backups := FBackupEngine.GetBackupHistory(FAddonFolderName);
  
  if (Index >= 0) and (Index < Length(Backups)) then
    Result := Backups[Index]
  else
    raise Exception.Create('Invalid backup selection');
end;

procedure TForm3.UpdateButtonStates;
begin
  btnSwitch.Enabled := (ListView1.Selected <> nil) and not FSwitching;
  btnDelete.Enabled := (ListView1.Selected <> nil) and not FSwitching;
end;

procedure TForm3.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  UpdateButtonStates;
end;

procedure TForm3.ListView1DblClick(Sender: TObject);
begin
  if btnSwitch.Enabled then
    btnSwitchClick(nil);
end;

procedure TForm3.btnSwitchClick(Sender: TObject);
var
  SelectedBackup: TBackupInfo;
  RestoreOption: TRestoreOption;
  RestoreThread: TRestoreThread;
begin
  if ListView1.Selected = nil then
  begin
    ShowMessage('Please select a backup to switch to.');
    Exit;
  end;

  try
    SelectedBackup := GetSelectedBackup;
  except
    on E: Exception do
    begin
      ShowMessage('Error: ' + E.Message);
      Exit;
    end;
  end;

  // Determine restore type
  if rbRestoreEverything.Checked then
    RestoreOption := roEverything
  else if rbRestoreAddonOnly.Checked then
    RestoreOption := roAddonOnly
  else
    RestoreOption := roSettingsOnly;

  // Confirm switch
  if MessageDlg(
    Format('Switch to backup from %s?' + sLineBreak + sLineBreak +
           'Restore type: %s',
           [FormatDateTime('yyyy-mm-dd hh:nn', SelectedBackup.Timestamp),
            rbRestoreEverything.Caption]),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  FSwitching := True;
  btnSwitch.Enabled := False;
  btnDelete.Enabled := False;
  btnClose.Enabled := False;
  ProgressBar1.Visible := True;

  // TODO: Safety backup feature - implement in v1.1
  // For now, just do the restore directly

  StatusBar1.SimpleText := 'Restoring selected version...';

  RestoreThread := TRestoreThread.Create(FBackupEngine,
    SelectedBackup.BackupPath, RestoreOption);
  RestoreThread.OnProgress := OnSwitchProgress;
  RestoreThread.OnComplete := OnSwitchComplete;
  RestoreThread.OnError := OnSwitchError;
  RestoreThread.Start;

  // Mark this version as active
  SetActiveBackupPath(SelectedBackup.BackupPath);
end;

procedure TForm3.OnSwitchProgress(const Msg: string; Percent: Integer);
begin
  StatusBar1.SimpleText := Msg;
  ProgressBar1.Position := Percent;
  Application.ProcessMessages;
end;

procedure TForm3.OnSwitchComplete(Sender: TObject);  // Changed parameter
begin
  FSwitching := False;
  btnSwitch.Enabled := True;
  btnDelete.Enabled := True;
  btnClose.Enabled := True;
  ProgressBar1.Visible := False;

  StatusBar1.SimpleText := 'Version switched successfully!';
  ShowMessage('Version switched successfully!' + sLineBreak + sLineBreak +
              'The selected backup is now active.');

  // Reload to update active indicator
  LoadBackupHistory;
  UpdateActiveVersionDisplay;
end;

procedure TForm3.OnSwitchError(const ErrorMsg: string);
begin
  FSwitching := False;
  btnSwitch.Enabled := True;
  btnDelete.Enabled := True;
  btnClose.Enabled := True;
  ProgressBar1.Visible := False;
  
  StatusBar1.SimpleText := 'Switch failed';
  ShowMessage('Error switching versions: ' + ErrorMsg);
end;

procedure TForm3.btnDeleteClick(Sender: TObject);
var
  SelectedBackup: TBackupInfo;
  ConfirmDeletions: Boolean;
begin
  if ListView1.Selected = nil then
  begin
    ShowMessage('Please select a backup to delete.');
    Exit;
  end;
  
  try
    SelectedBackup := GetSelectedBackup;
  except
    on E: Exception do
    begin
      ShowMessage('Error: ' + E.Message);
      Exit;
    end;
  end;
  
  ConfirmDeletions := FAddonManager.IniFile.ReadBool('Settings', 'ConfirmDeletions', True);
  
  if ConfirmDeletions then
  begin
    // Show deletion options
    case MessageDlg(
      Format('Delete backup from %s?' + sLineBreak +
             'Size: %d MB' + sLineBreak + sLineBreak +
             'This memory will be erased.' + sLineBreak +
             'This cannot be undone.' + sLineBreak + sLineBreak +
             'Yes = Schedule deletion (24 hours)' + sLineBreak +
             'No = Delete immediately' + sLineBreak +
             'Cancel = Don''t delete',
             [FormatDateTime('yyyy-mm-dd hh:nn', SelectedBackup.Timestamp),
              SelectedBackup.SizeMB]),
      mtWarning, [mbYes, mbNo, mbCancel], 0) of
      
      mrYes: ScheduleDeletion(SelectedBackup.BackupPath);
      mrNo: ImmediateDeletion(SelectedBackup.BackupPath);
      mrCancel: Exit;
    end;
  end
  else
  begin
    // Instant deletion (setting disabled confirmation)
    if MessageDlg(
      Format('Delete backup from %s?' + sLineBreak +
             'Size: %d MB' + sLineBreak + sLineBreak +
             'Deletion is permanent.',
             [FormatDateTime('yyyy-mm-dd hh:nn', SelectedBackup.Timestamp),
              SelectedBackup.SizeMB]),
      mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      ImmediateDeletion(SelectedBackup.BackupPath);
    end;
  end;
end;

procedure TForm3.ScheduleDeletion(const BackupPath: string);
begin
  FBackupEngine.DeleteBackup(BackupPath, False); // Schedule
  StatusBar1.SimpleText := 'Backup scheduled for deletion in 24 hours';
  ShowMessage('Backup scheduled for deletion in 24 hours.' + sLineBreak +
              'You can cancel this by reopening this dialog.');
  LoadBackupHistory;
end;

procedure TForm3.ImmediateDeletion(const BackupPath: string);
begin
  // Final warning
  if MessageDlg(
    'Final Warning' + sLineBreak + sLineBreak +
    'Deletion is permanent.' + sLineBreak +
    'No recovery possible.' + sLineBreak + sLineBreak +
    'Delete this backup now?',
    mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    FBackupEngine.DeleteBackup(BackupPath, True); // Immediate
    StatusBar1.SimpleText := 'Backup deleted';
    ShowMessage('Backup deleted permanently.');
    LoadBackupHistory;
  end;
end;

procedure TForm3.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
