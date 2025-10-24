unit AddonManager;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.IOUtils;

type
  TAddonInfo = record
    Name: string;
    FolderName: string;
    URL: string;
    CurrentVersion: string;
    LastUpdated: TDateTime;
    Notes: string;
  end;

  TAddonManager = class
  private
    FIniFile: TIniFile;
    FAddons: TArray<TAddonInfo>;
    FDataFolder: string;
    
    function GetDataFolder: string;
    procedure LoadSettings;
    procedure LoadAddons;
  public
    WoWAddonPath: string;
    DownloadFolder: string;
    BackupEnabled: Boolean;
    KeepBackupHistory: Boolean;
    FreshInstallMode: Boolean;
    MaxBackupsPerAddon: Integer;

    constructor Create;
    destructor Destroy; override;

    procedure SaveSettings;
    procedure AddAddon(const AAddon: TAddonInfo);
    procedure UpdateAddon(Index: Integer; const AAddon: TAddonInfo);
    procedure RemoveAddon(Index: Integer);
    function GetAddon(Index: Integer): TAddonInfo;
    function AddonCount: Integer;

    property DataFolder: string read FDataFolder;
    property IniFile: TIniFile read FIniFile; // Expose for settings access
  end;

implementation

constructor TAddonManager.Create;
begin
  inherited;
  FDataFolder := GetDataFolder;
  FIniFile := TIniFile.Create(TPath.Combine(FDataFolder, 'Settings.ini'));
  LoadSettings;
  LoadAddons;
end;

destructor TAddonManager.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

function TAddonManager.GetDataFolder: string;
var
  ExePath: string;
begin
  ExePath := ExtractFilePath(ParamStr(0));
  
  // Check if running from Desktop
  if Pos('\Desktop\', ExePath) > 0 then
  begin
    // Use Documents folder
    Result := TPath.Combine(
      TPath.GetDocumentsPath, 
      'WoWAddonUpdater'
    );
  end
  else
  begin
    // Use app's own folder
    Result := ExePath;
  end;
  
  // Create folder if doesn't exist
  TDirectory.CreateDirectory(Result);
  TDirectory.CreateDirectory(TPath.Combine(Result, 'Backups'));
end;

procedure TAddonManager.LoadSettings;
begin
  WoWAddonPath := FIniFile.ReadString('Settings', 'WoWAddonPath', '');
  DownloadFolder := FIniFile.ReadString('Settings', 'DownloadFolder', 
    TPath.Combine(TPath.GetHomePath, 'Downloads'));
  BackupEnabled := FIniFile.ReadBool('Settings', 'BackupEnabled', True);
  KeepBackupHistory := FIniFile.ReadBool('Settings', 'KeepBackupHistory', True);
  FreshInstallMode := FIniFile.ReadBool('Settings', 'FreshInstallMode', False);
  MaxBackupsPerAddon := FIniFile.ReadInteger('Settings', 'MaxBackupsPerAddon', 10);
end;

procedure TAddonManager.SaveSettings;
begin
  FIniFile.WriteString('Settings', 'WoWAddonPath', WoWAddonPath);
  FIniFile.WriteString('Settings', 'DownloadFolder', DownloadFolder);
  FIniFile.WriteBool('Settings', 'BackupEnabled', BackupEnabled);
  FIniFile.WriteBool('Settings', 'KeepBackupHistory', KeepBackupHistory);
  FIniFile.WriteBool('Settings', 'FreshInstallMode', FreshInstallMode);
  FIniFile.WriteInteger('Settings', 'MaxBackupsPerAddon', MaxBackupsPerAddon);
  FIniFile.UpdateFile;
end;

procedure TAddonManager.LoadAddons;
var
  Sections: TStringList;
  i: Integer;
  Addon: TAddonInfo;
begin
  Sections := TStringList.Create;
  try
    FIniFile.ReadSections(Sections);
    SetLength(FAddons, 0);
    
    for i := 0 to Sections.Count - 1 do
    begin
      if Sections[i].StartsWith('Addon_') then
      begin
        Addon.Name := FIniFile.ReadString(Sections[i], 'Name', '');
        Addon.FolderName := FIniFile.ReadString(Sections[i], 'FolderName', '');
        Addon.URL := FIniFile.ReadString(Sections[i], 'URL', '');
        Addon.CurrentVersion := FIniFile.ReadString(Sections[i], 'CurrentVersion', '');
        Addon.LastUpdated := FIniFile.ReadDateTime(Sections[i], 'LastUpdated', 0);
        Addon.Notes := FIniFile.ReadString(Sections[i], 'Notes', '');
        
        SetLength(FAddons, Length(FAddons) + 1);
        FAddons[High(FAddons)] := Addon;
      end;
    end;
  finally
    Sections.Free;
  end;
end;

procedure TAddonManager.AddAddon(const AAddon: TAddonInfo);
var
  Section: string;
begin
  SetLength(FAddons, Length(FAddons) + 1);
  FAddons[High(FAddons)] := AAddon;
  
  Section := 'Addon_' + IntToStr(Length(FAddons));
  FIniFile.WriteString(Section, 'Name', AAddon.Name);
  FIniFile.WriteString(Section, 'FolderName', AAddon.FolderName);
  FIniFile.WriteString(Section, 'URL', AAddon.URL);
  FIniFile.WriteString(Section, 'CurrentVersion', AAddon.CurrentVersion);
  FIniFile.WriteDateTime(Section, 'LastUpdated', AAddon.LastUpdated);
  FIniFile.WriteString(Section, 'Notes', AAddon.Notes);
  FIniFile.UpdateFile;
end;

procedure TAddonManager.UpdateAddon(Index: Integer; const AAddon: TAddonInfo);
var
  Section: string;
begin
  if (Index >= 0) and (Index < Length(FAddons)) then
  begin
    FAddons[Index] := AAddon;
    
    Section := 'Addon_' + IntToStr(Index + 1);
    FIniFile.WriteString(Section, 'Name', AAddon.Name);
    FIniFile.WriteString(Section, 'FolderName', AAddon.FolderName);
    FIniFile.WriteString(Section, 'URL', AAddon.URL);
    FIniFile.WriteString(Section, 'CurrentVersion', AAddon.CurrentVersion);
    FIniFile.WriteDateTime(Section, 'LastUpdated', AAddon.LastUpdated);
    FIniFile.WriteString(Section, 'Notes', AAddon.Notes);
    FIniFile.UpdateFile;
  end;
end;

procedure TAddonManager.RemoveAddon(Index: Integer);
var
  Section: string;
  i: Integer;
begin
  if (Index >= 0) and (Index < Length(FAddons)) then
  begin
    Section := 'Addon_' + IntToStr(Index + 1);
    FIniFile.EraseSection(Section);
    
    // Shift array
    for i := Index to High(FAddons) - 1 do
      FAddons[i] := FAddons[i + 1];
    SetLength(FAddons, Length(FAddons) - 1);
    
    FIniFile.UpdateFile;
  end;
end;

function TAddonManager.GetAddon(Index: Integer): TAddonInfo;
begin
  if (Index >= 0) and (Index < Length(FAddons)) then
    Result := FAddons[Index]
  else
    raise Exception.Create('Invalid addon index');
end;

function TAddonManager.AddonCount: Integer;
begin
  Result := Length(FAddons);
end;

end.