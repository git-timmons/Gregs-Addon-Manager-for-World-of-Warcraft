unit FileWatcher;

// ============================================================================
// FUTURE DEVELOPMENT ROADMAP
// ============================================================================
//
// VERSION 1.0 (Current): Integrated FileWatcher
// -----------------------------------------------
// - Monitors Downloads folder while Greg's Manager is running
// - TTimer-based polling (every 5 seconds)
// - Prompts user when WoW addon ZIP detected
// - Automated backup + install + cleanup
//
// VERSION 1.5: In-Game Update Tracking
// -------------------------------------
// TODO: Create GregsVersionTracker WoW Addon
// 
// Purpose: Hook into existing addon update notification systems
// 
// Implementation Strategy:
//   1. Create lightweight WoW addon: GregsVersionTracker
//   2. Hook into popular addon update broadcasters:
//      - DBM (Deadly Boss Mods) notification system
//      - BigWigs update announcer
//      - ElvUI version checker
//      - WeakAuras companion updater
//      - Any addon that broadcasts "update available" events
//   3. Capture update notifications and write to SavedVariables:
//      GregsVersionTrackerDB = {
//        updates = {
//          ["WeakAuras"] = { version = "5.3.2", timestamp = 1729180822, source = "DBM" },
//          ["DBM-Core"] = { version = "10.2.51", timestamp = 1729180845, source = "self" }
//        }
//      }
//   4. Greg's Manager reads SavedVariables file
//   5. Aggregates all update notifications in one dashboard
//   6. User sees: "3 addons have updates available"
//
// Advantages:
//   - Leverage existing infrastructure (don't reinvent the wheel)
//   - No web scraping needed
//   - No CurseForge API conflicts
//   - Community-powered version tracking
//   - Works with any addon that announces updates
//
// VERSION 2.0: Standalone FileWatcher Process
// --------------------------------------------
// TODO: Branch FileWatcher into separate executable
//
// Purpose: Always-on monitoring even when Greg's Manager closed
//
// Architecture:
//   - FileWatcher.exe - Lightweight background process
//   - Runs in system tray
//   - Minimal memory footprint (~5-10 MB)
//   - Starts with Windows (optional)
//   - Monitors Downloads folder 24/7
//   - Queues installations or launches Greg's Manager when needed
//   - Auto-updates itself via GitHub releases
//
// Inter-Process Communication:
//   - Named pipes or WM_COPYDATA messages
//   - FileWatcher detects addon → sends message to Greg's Manager
//   - Or: Creates queue file that Greg's Manager reads on startup
//
// Benefits:
//   - Set and forget - always watching
//   - Install addons even if main app wasn't running
//   - Better user experience for heavy addon users
//
// Challenges:
//   - Process lifecycle management
//   - Two executables to distribute
//   - More complex teaching/documentation
//
// Decision: Keep integrated for v1.0, branch later based on user demand
//
// ============================================================================

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Zip, System.Types,
  System.UITypes,
  Vcl.ExtCtrls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Controls,
  System.Generics.Collections,
  AddonManager, BackupEngine, BackupEngineThreaded;

type
  TAddonZipInfo = record
    ZipPath: string;
    AddonNames: TArray<string>;
    FileCount: Integer;
    TotalSizeMB: Integer;
    IsValid: Boolean;
  end;

  TFileWatcherEvent = procedure(Sender: TObject; const ZipInfo: TAddonZipInfo) of object;

  TFileWatcher = class(TComponent)
  private
    FTimer: TTimer;
    FDownloadFolder: string;
    FEnabled: Boolean;
    FPollingInterval: Integer;
    FProcessedFiles: TStringList;
    FAddonManager: TAddonManager;
    FBackupEngine: TBackupEngine;
    
    FOnAddonDetected: TFileWatcherEvent;
    
    procedure TimerTick(Sender: TObject);
    function AnalyzeAddonZip(const ZipPath: string): TAddonZipInfo;
    function IsWoWAddonZip(const ZipPath: string): Boolean;
    procedure ScanDownloadsFolder;
    function PromptForInstall(const ZipInfo: TAddonZipInfo): Boolean;
    procedure InstallAddon(const ZipInfo: TAddonZipInfo; 
      BackupFirst, DeleteZip, CleanVariables: Boolean);
    procedure DeleteSavedVariablesForAddon(const AddonName: string);
  public
    constructor Create(AOwner: TComponent; AAddonManager: TAddonManager;
      ABackupEngine: TBackupEngine); reintroduce;
    destructor Destroy; override;
    
    procedure Start;
    procedure Stop;
    procedure ClearProcessedFiles;
    
    property DownloadFolder: string read FDownloadFolder write FDownloadFolder;
    property Enabled: Boolean read FEnabled write FEnabled;
    property PollingInterval: Integer read FPollingInterval write FPollingInterval;
    property OnAddonDetected: TFileWatcherEvent read FOnAddonDetected write FOnAddonDetected;
  end;

implementation

uses
  Winapi.Windows;

constructor TFileWatcher.Create(AOwner: TComponent; AAddonManager: TAddonManager;
  ABackupEngine: TBackupEngine);
begin
  inherited Create(AOwner);
  
  FAddonManager := AAddonManager;
  FBackupEngine := ABackupEngine;
  FProcessedFiles := TStringList.Create;
  FProcessedFiles.Sorted := True;
  FProcessedFiles.Duplicates := dupIgnore;
  
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := TimerTick;
  FTimer.Enabled := False;
  
  FPollingInterval := 5000; // 5 seconds default
  FEnabled := False;
end;

destructor TFileWatcher.Destroy;
begin
  Stop;
  FProcessedFiles.Free;
  inherited;
end;

procedure TFileWatcher.Start;
begin
  if FDownloadFolder = '' then
    raise Exception.Create('Download folder not set');
    
  if not TDirectory.Exists(FDownloadFolder) then
    raise Exception.Create('Download folder does not exist: ' + FDownloadFolder);
  
  FEnabled := True;
  FTimer.Interval := FPollingInterval;
  FTimer.Enabled := True;
end;

procedure TFileWatcher.Stop;
begin
  FEnabled := False;
  FTimer.Enabled := False;
end;

procedure TFileWatcher.ClearProcessedFiles;
begin
  FProcessedFiles.Clear;
end;

procedure TFileWatcher.TimerTick(Sender: TObject);
begin
  if FEnabled then
    ScanDownloadsFolder;
end;

procedure TFileWatcher.ScanDownloadsFolder;
var
  Files: TStringDynArray;
  ZipFile: string;
  ZipInfo: TAddonZipInfo;
begin
  try
    Files := TDirectory.GetFiles(FDownloadFolder, '*.zip');
    
    for ZipFile in Files do
    begin
      // Skip if already processed
      if FProcessedFiles.IndexOf(ZipFile) >= 0 then
        Continue;
      
      // Check if it's a WoW addon
      if IsWoWAddonZip(ZipFile) then
      begin
        ZipInfo := AnalyzeAddonZip(ZipFile);
        
        if ZipInfo.IsValid then
        begin
          // Mark as processed immediately (don't prompt multiple times)
          FProcessedFiles.Add(ZipFile);
          
          // Trigger event or prompt
          if Assigned(FOnAddonDetected) then
            FOnAddonDetected(Self, ZipInfo)
          else
            PromptForInstall(ZipInfo);
        end;
      end;
    end;
  except
    // Silently continue - don't crash on file access errors
  end;
end;

function TFileWatcher.IsWoWAddonZip(const ZipPath: string): Boolean;
var
  Zip: TZipFile;
  i: Integer;
  FileName: string;
begin
  Result := False;
  
  Zip := TZipFile.Create;
  try
    try
      Zip.Open(ZipPath, zmRead);
      
      // Just look for .toc file - that's the definitive addon marker
      for i := 0 to Zip.FileCount - 1 do
      begin
        FileName := Zip.FileNames[i].ToLower;
        
        if FileName.EndsWith('.toc') then
        begin
          Result := True;
          Break;
        end;
      end;
      
      Zip.Close;
    except
      Result := False;
    end;
  finally
    Zip.Free;
  end;
end;

function TFileWatcher.AnalyzeAddonZip(const ZipPath: string): TAddonZipInfo;
var
  Zip: TZipFile;
  i: Integer;
  FileName, AddonName: string;
  Names: TList<string>;
  TotalSize: Int64;
begin
  Result.ZipPath := ZipPath;
  Result.FileCount := 0;
  Result.TotalSizeMB := 0;
  Result.IsValid := False;
  SetLength(Result.AddonNames, 0);
  
  Names := TList<string>.Create;
  try
    Zip := TZipFile.Create;
    try
      Zip.Open(ZipPath, zmRead);
      Result.FileCount := Zip.FileCount;
      TotalSize := 0;
      
      // Scan all files
      for i := 0 to Zip.FileCount - 1 do
      begin
        FileName := Zip.FileNames[i];
        TotalSize := TotalSize + Zip.FileInfo[i].UncompressedSize;
        
        // Found a .toc file?
        if FileName.ToLower.EndsWith('.toc') then
        begin
          // Extract addon name from path
          // "WeakAuras/WeakAuras.toc" -> "WeakAuras"
          AddonName := TPath.GetFileNameWithoutExtension(FileName);
          
          // Remove folder prefix if present
          if AddonName.Contains('/') or AddonName.Contains('\') then
            AddonName := TPath.GetFileName(AddonName);
          
          if (AddonName <> '') and (not Names.Contains(AddonName)) then
            Names.Add(AddonName);
        end;
      end;
      
      Zip.Close;
      
      Result.TotalSizeMB := (TotalSize div 1048576) + 1;
      Result.AddonNames := Names.ToArray;
      Result.IsValid := Length(Result.AddonNames) > 0;
    finally
      Zip.Free;
    end;
  finally
    Names.Free;
  end;
end;

function TFileWatcher.PromptForInstall(const ZipInfo: TAddonZipInfo): Boolean;
var
  Msg: string;
  AddonList: string;
  AddonName: string;
  BackupFirst, DeleteZip, CleanVariables: Boolean;
  DlgResult: Integer;
begin
  Result := False;
  
  // Build addon list
  AddonList := '';
  for AddonName in ZipInfo.AddonNames do
    AddonList := AddonList + '  • ' + AddonName + sLineBreak;
  
  Msg := Format(
    'WoW Addon Detected!' + sLineBreak + sLineBreak +
    'File: %s' + sLineBreak +
    'Size: %d MB (%d files)' + sLineBreak + sLineBreak +
    'Contains addons:' + sLineBreak +
    '%s' + sLineBreak +
    'Install to WoW AddOns folder?',
    [ExtractFileName(ZipInfo.ZipPath), ZipInfo.TotalSizeMB, 
     ZipInfo.FileCount, AddonList]
  );
  
  DlgResult := MessageDlg(Msg, mtConfirmation, [mbYes, mbNo], 0);
  
  if DlgResult = mrYes then
  begin
    // TODO: Create proper dialog with checkboxes
    // For now, use safe defaults
    BackupFirst := True;
    DeleteZip := True;
    CleanVariables := False;
    
    InstallAddon(ZipInfo, BackupFirst, DeleteZip, CleanVariables);
    Result := True;
  end;
end;

procedure TFileWatcher.InstallAddon(const ZipInfo: TAddonZipInfo;
  BackupFirst, DeleteZip, CleanVariables: Boolean);
var
  Zip: TZipFile;
  AddonName: string;
  WoWAddOnsPath: string;
begin
  WoWAddOnsPath := FAddonManager.WoWAddonPath;
  
  if not TDirectory.Exists(WoWAddOnsPath) then
    raise Exception.Create('WoW AddOns path not configured or does not exist');
  
  try
    // Step 1: Backup existing addons if requested
    if BackupFirst then
    begin
      for AddonName in ZipInfo.AddonNames do
      begin
        try
          FBackupEngine.CreateBackup(AddonName);
        except
          // Continue even if backup fails
        end;
      end;
    end;
    
    // Step 2: Clean SavedVariables if requested
    if CleanVariables then
    begin
      for AddonName in ZipInfo.AddonNames do
        DeleteSavedVariablesForAddon(AddonName);
    end;
    
    // Step 3: Extract ZIP to AddOns folder
    Zip := TZipFile.Create;
    try
      Zip.Open(ZipInfo.ZipPath, zmRead);
      Zip.ExtractAll(WoWAddOnsPath);
      Zip.Close;
    finally
      Zip.Free;
    end;
    
    // Step 4: Delete ZIP if requested
    if DeleteZip then
      TFile.Delete(ZipInfo.ZipPath);
    
    ShowMessage('Addon(s) installed successfully!');
  except
    on E: Exception do
      ShowMessage('Error installing addon: ' + E.Message);
  end;
end;

procedure TFileWatcher.DeleteSavedVariablesForAddon(const AddonName: string);
var
  WTFPath: string;
  AccountFolders: TStringDynArray;
  Account, VarFile: string;
  Files: TStringDynArray;
  F: string;
begin
  // Get WTF path from WoW addon path
  WTFPath := TPath.Combine(
    TPath.GetDirectoryName(TPath.GetDirectoryName(FAddonManager.WoWAddonPath)),
    'WTF\Account'
  );
  
  if not TDirectory.Exists(WTFPath) then Exit;
  
  AccountFolders := TDirectory.GetDirectories(WTFPath);
  
  // For each account
  for Account in AccountFolders do
  begin
    // Delete global SavedVariables
    VarFile := TPath.Combine(Account, 'SavedVariables\' + AddonName + '.lua');
    if TFile.Exists(VarFile) then
      TFile.Delete(VarFile);
    
    // TODO: Also delete character-specific SavedVariables
    // Would need to recursively search Server\Character\SavedVariables folders
  end;
end;

end.