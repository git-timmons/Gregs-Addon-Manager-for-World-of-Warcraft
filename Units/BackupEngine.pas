unit BackupEngine;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Zip, System.Types,
  System.DateUtils, Generics.Collections,
  System.Generics.Defaults;

type
  TRestoreOption = (roEverything, roAddonOnly, roSettingsOnly);
  
  TBackupStatus = (bsNormal, bsPendingDeletion);
  
  TBackupInfo = record
    AddonName: string;
    BackupPath: string;
    Timestamp: TDateTime;
    SizeMB: Integer;
    Status: TBackupStatus;
    DeletionScheduledFor: TDateTime;
  end;

  TBackupEngine = class
  private
    FWoWPath: string;
    FBackupRootPath: string;
    FMaxBackupsPerAddon: Integer;
    FConfirmDeletions: Boolean;
    
    function GetAddOnsPath: string;
    function GetWTFPath: string;
    function GetAddonBackupPath(const AddonName: string): string;
    function CalculateFolderSize(const Path: string): Int64;
    function GetFolderSizeMB(const Path: string): Integer;
    procedure PruneOldBackups(const AddonName: string);
    procedure CopyDirectory(const Source, Dest: string);
  public
    constructor Create(const AWoWPath, ABackupRootPath: string; 
      AMaxBackups: Integer; AConfirmDeletions: Boolean);
    
    // Backup operations
    function CreateBackup(const AddonBaseName: string): string;
    procedure DeleteBackup(const BackupPath: string; Immediate: Boolean = False);
    procedure CancelScheduledDeletion(const BackupPath: string);
    
    // Restore operations
    procedure RestoreBackup(const BackupPath: string; 
      Option: TRestoreOption = roEverything);
    
    // Query operations
    function GetBackupHistory(const AddonName: string): TArray<TBackupInfo>;
    function GetAllBackups: TArray<TBackupInfo>;
    procedure ProcessScheduledDeletions;
    
    property WoWPath: string read FWoWPath write FWoWPath;
    property BackupRootPath: string read FBackupRootPath write FBackupRootPath;
    property MaxBackupsPerAddon: Integer read FMaxBackupsPerAddon write FMaxBackupsPerAddon;
  end;

implementation

const
  DELETION_MARKER = '.scheduled_deletion';

constructor TBackupEngine.Create(const AWoWPath, ABackupRootPath: string;
  AMaxBackups: Integer; AConfirmDeletions: Boolean);
begin
  FWoWPath := AWoWPath;
  FBackupRootPath := ABackupRootPath;
  FMaxBackupsPerAddon := AMaxBackups;
  FConfirmDeletions := AConfirmDeletions;
  
  // Ensure backup root exists
  TDirectory.CreateDirectory(FBackupRootPath);
end;

function TBackupEngine.GetAddOnsPath: string;
begin
  Result := TPath.Combine(FWoWPath, 'Interface\AddOns');
end;

function TBackupEngine.GetWTFPath: string;
begin
  Result := TPath.Combine(
    TPath.GetDirectoryName(TPath.GetDirectoryName(FWoWPath)),
    'WTF'
  );
end;

function TBackupEngine.GetAddonBackupPath(const AddonName: string): string;
begin
  Result := TPath.Combine(FBackupRootPath, 'Backups\' + AddonName);
  TDirectory.CreateDirectory(Result);
end;

function TBackupEngine.CalculateFolderSize(const Path: string): Int64;
var
  Files: TStringDynArray;
  F: string;
begin
  Result := 0;
  
  if not TDirectory.Exists(Path) then
    Exit;
    
  Files := TDirectory.GetFiles(Path, '*.*', TSearchOption.soAllDirectories);
  
  for F in Files do
  begin
    if TFile.Exists(F) then
      Result := Result + TFile.GetSize(F);
  end;
end;

function TBackupEngine.GetFolderSizeMB(const Path: string): Integer;
var
  Size: Int64;
begin
  Size := CalculateFolderSize(Path);
  // Round UP to nearest MB
  Result := (Size div 1048576) + 1;
  if Size = 0 then Result := 0;
end;

procedure TBackupEngine.CopyDirectory(const Source, Dest: string);
var
  Files: TStringDynArray;
  Dirs: TStringDynArray;
  F, D, RelPath, DestPath: string;
begin
  // Create destination
  TDirectory.CreateDirectory(Dest);
  
  // Copy all files
  Files := TDirectory.GetFiles(Source, '*.*', TSearchOption.soAllDirectories);
  for F in Files do
  begin
    RelPath := F.Replace(Source, '');
    DestPath := TPath.Combine(Dest, RelPath);
    TDirectory.CreateDirectory(TPath.GetDirectoryName(DestPath));
    TFile.Copy(F, DestPath, True);
  end;
end;

function TBackupEngine.CreateBackup(const AddonBaseName: string): string;
var
  TimeStamp: string;
  BackupFolder: string;
  AddonBackupPath: string;
  TempPath: string;
  AddOnsFolders: TStringDynArray;
  Folder, FolderName: string;
  SavedVarsPath: string;
  AccountFolders: TStringDynArray;
  Account: string;
  ZipFile: TZipFile;
begin
  // Create timestamp: YYYY-MM-DD_HHMMSS
  TimeStamp := FormatDateTime('yyyy-mm-dd_hhnnss', Now);
  
  // Get addon's backup folder
  AddonBackupPath := GetAddonBackupPath(AddonBaseName);
  BackupFolder := TPath.Combine(AddonBackupPath, TimeStamp);
  TempPath := TPath.Combine(BackupFolder, 'temp');
  
  try
    TDirectory.CreateDirectory(TempPath);
    
    // === PHASE 1: Backup addon files ===
    // Find all folders matching pattern
    AddOnsFolders := TDirectory.GetDirectories(GetAddOnsPath);
    
    for Folder in AddOnsFolders do
    begin
      FolderName := TPath.GetFileName(Folder);
      if FolderName.StartsWith(AddonBaseName, True) then
      begin
        // Copy this addon folder
        CopyDirectory(
          Folder,
          TPath.Combine(TempPath, 'addon_files\' + FolderName)
        );
      end;
    end;
    
    // === PHASE 2: Backup ALL SavedVariables (greedy approach) ===
    SavedVarsPath := TPath.Combine(GetWTFPath, 'Account');
    
    if TDirectory.Exists(SavedVarsPath) then
    begin
      AccountFolders := TDirectory.GetDirectories(SavedVarsPath);
      
      for Account in AccountFolders do
      begin
        // Copy entire SavedVariables folder for this account
        CopyDirectory(
          Account,
          TPath.Combine(TempPath, 'saved_variables\' + TPath.GetFileName(Account))
        );
      end;
    end;
    
    // === PHASE 3: Create ZIP ===
    ZipFile := TZipFile.Create;
    try
      ZipFile.Open(TPath.Combine(BackupFolder, 'backup.zip'), zmWrite);
      ZipFile.Add(TempPath, '', zcDeflate);
      ZipFile.Close;
    finally
      ZipFile.Free;
    end;
    
    // Clean up temp folder
    TDirectory.Delete(TempPath, True);
    
    Result := BackupFolder;
    
    // Prune old backups if needed
    PruneOldBackups(AddonBaseName);
    
  except
    on E: Exception do
    begin
      // Cleanup on failure
      if TDirectory.Exists(BackupFolder) then
        TDirectory.Delete(BackupFolder, True);
      raise Exception.Create('Backup failed: ' + E.Message);
    end;
  end;
end;

procedure TBackupEngine.PruneOldBackups(const AddonName: string);
var
  Backups: TArray<TBackupInfo>;
  i: Integer;
begin
  Backups := GetBackupHistory(AddonName);
  
  // If we have more than max, delete oldest
  if Length(Backups) > FMaxBackupsPerAddon then
  begin
    // Backups are already sorted by timestamp (oldest first in our implementation)
    for i := 0 to Length(Backups) - FMaxBackupsPerAddon - 1 do
    begin
      if Backups[i].Status = bsNormal then
        DeleteBackup(Backups[i].BackupPath, True); // Force immediate on auto-prune
    end;
  end;
end;

procedure TBackupEngine.DeleteBackup(const BackupPath: string; Immediate: Boolean);
var
  MarkerFile: string;
  MarkerContent: TStringList;
begin
  if Immediate or not FConfirmDeletions then
  begin
    // Delete immediately
    if TDirectory.Exists(BackupPath) then
      TDirectory.Delete(BackupPath, True);
  end
  else
  begin
    // Schedule for deletion (24 hours)
    MarkerFile := TPath.Combine(BackupPath, DELETION_MARKER);
    MarkerContent := TStringList.Create;
    try
      MarkerContent.Add(DateTimeToStr(IncHour(Now, 24)));
      MarkerContent.SaveToFile(MarkerFile);
    finally
      MarkerContent.Free;
    end;
  end;
end;

procedure TBackupEngine.CancelScheduledDeletion(const BackupPath: string);
var
  MarkerFile: string;
begin
  MarkerFile := TPath.Combine(BackupPath, DELETION_MARKER);
  if TFile.Exists(MarkerFile) then
    TFile.Delete(MarkerFile);
end;

procedure TBackupEngine.RestoreBackup(const BackupPath: string; 
  Option: TRestoreOption);
var
  ZipPath: string;
  TempPath: string;
  ZipFile: TZipFile;
  AddonFilesPath, SavedVarsPath: string;
begin
  ZipPath := TPath.Combine(BackupPath, 'backup.zip');
  
  if not TFile.Exists(ZipPath) then
    raise Exception.Create('Backup file not found');
    
  TempPath := TPath.Combine(BackupPath, 'restore_temp');
  
  try
    // Extract ZIP
    TDirectory.CreateDirectory(TempPath);
    
    ZipFile := TZipFile.Create;
    try
      ZipFile.Open(ZipPath, zmRead);
      ZipFile.ExtractAll(TempPath);
      ZipFile.Close;
    finally
      ZipFile.Free;
    end;
    
    AddonFilesPath := TPath.Combine(TempPath, 'addon_files');
    SavedVarsPath := TPath.Combine(TempPath, 'saved_variables');
    
    // Restore based on option
    case Option of
      roEverything, roAddonOnly:
      begin
        if TDirectory.Exists(AddonFilesPath) then
          CopyDirectory(AddonFilesPath, GetAddOnsPath);
          
        if (Option = roEverything) and TDirectory.Exists(SavedVarsPath) then
          CopyDirectory(SavedVarsPath, TPath.Combine(GetWTFPath, 'Account'));
      end;
      
      roSettingsOnly:
      begin
        if TDirectory.Exists(SavedVarsPath) then
          CopyDirectory(SavedVarsPath, TPath.Combine(GetWTFPath, 'Account'));
      end;
    end;
    
  finally
    // Cleanup temp
    if TDirectory.Exists(TempPath) then
      TDirectory.Delete(TempPath, True);
  end;
end;

function TBackupEngine.GetBackupHistory(const AddonName: string): TArray<TBackupInfo>;
var
  AddonBackupPath: string;
  Folders: TStringDynArray;
  Folder: string;
  List: TList<TBackupInfo>;
  Info: TBackupInfo;
  MarkerFile: string;
  MarkerContent: TStringList;
begin
  List := TList<TBackupInfo>.Create;
  try
    AddonBackupPath := GetAddonBackupPath(AddonName);
    
    if TDirectory.Exists(AddonBackupPath) then
    begin
      Folders := TDirectory.GetDirectories(AddonBackupPath);
      
      for Folder in Folders do
      begin
        Info.AddonName := AddonName;
        Info.BackupPath := Folder;
        Info.SizeMB := GetFolderSizeMB(Folder);
        
        // Parse timestamp from folder name
        try
          Info.Timestamp := EncodeDateTime(
            StrToInt(Copy(TPath.GetFileName(Folder), 1, 4)),  // year
            StrToInt(Copy(TPath.GetFileName(Folder), 6, 2)),  // month
            StrToInt(Copy(TPath.GetFileName(Folder), 9, 2)),  // day
            StrToInt(Copy(TPath.GetFileName(Folder), 12, 2)), // hour
            StrToInt(Copy(TPath.GetFileName(Folder), 14, 2)), // min
            StrToInt(Copy(TPath.GetFileName(Folder), 16, 2)), // sec
            0
          );
        except
          Info.Timestamp := 0;
        end;
        
        // Check deletion status
        MarkerFile := TPath.Combine(Folder, DELETION_MARKER);
        if TFile.Exists(MarkerFile) then
        begin
          Info.Status := bsPendingDeletion;
          MarkerContent := TStringList.Create;
          try
            MarkerContent.LoadFromFile(MarkerFile);
            if MarkerContent.Count > 0 then
              Info.DeletionScheduledFor := StrToDateTime(MarkerContent[0]);
          finally
            MarkerContent.Free;
          end;
        end
        else
        begin
          Info.Status := bsNormal;
          Info.DeletionScheduledFor := 0;
        end;
        
        List.Add(Info);
      end;
    end;
    
    // Sort by timestamp (newest first)
    List.Sort(TComparer<TBackupInfo>.Construct(
      function(const A, B: TBackupInfo): Integer
      begin
        Result := CompareDateTime(B.Timestamp, A.Timestamp);
      end
    ));
    
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function TBackupEngine.GetAllBackups: TArray<TBackupInfo>;
var
  AddonFolders: TStringDynArray;
  Folder: string;
  AllList: TList<TBackupInfo>;
  AddonBackups: TArray<TBackupInfo>;
  Backup: TBackupInfo;
begin
  AllList := TList<TBackupInfo>.Create;
  try
    if TDirectory.Exists(TPath.Combine(FBackupRootPath, 'Backups')) then
    begin
      AddonFolders := TDirectory.GetDirectories(
        TPath.Combine(FBackupRootPath, 'Backups')
      );
      
      for Folder in AddonFolders do
      begin
        AddonBackups := GetBackupHistory(TPath.GetFileName(Folder));
        for Backup in AddonBackups do
          AllList.Add(Backup);
      end;
    end;
    
    Result := AllList.ToArray;
  finally
    AllList.Free;
  end;
end;

procedure TBackupEngine.ProcessScheduledDeletions;
var
  Backups: TArray<TBackupInfo>;
  Backup: TBackupInfo;
begin
  Backups := GetAllBackups;
  
  for Backup in Backups do
  begin
    if (Backup.Status = bsPendingDeletion) and 
       (Now >= Backup.DeletionScheduledFor) then
    begin
      // Time's up - delete it
      if TDirectory.Exists(Backup.BackupPath) then
        TDirectory.Delete(Backup.BackupPath, True);
    end;
  end;
end;

end.