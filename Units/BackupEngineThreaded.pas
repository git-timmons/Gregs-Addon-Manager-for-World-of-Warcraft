unit BackupEngineThreaded;

interface

uses
  System.Classes, System.SysUtils, BackupEngine;

type
  TBackupProgressEvent = procedure(const Message: string; Percent: Integer) of object;
  TBackupCompleteEvent = procedure(const BackupPath: string) of object;
  TBackupErrorEvent = procedure(const ErrorMsg: string) of object;

  TBackupThread = class(TThread)
  private
    FEngine: TBackupEngine;
    FAddonName: string;
    FResultPath: string;
    FErrorMsg: string;
    
    FOnProgress: TBackupProgressEvent;
    FOnComplete: TBackupCompleteEvent;
    FOnError: TBackupErrorEvent;
    
    // Sync methods
    FProgressMsg: string;
    FProgressPct: Integer;
    procedure SyncProgress;
    procedure SyncComplete;
    procedure SyncError;
  protected
    procedure Execute; override;
  public
    constructor Create(AEngine: TBackupEngine; const AddonName: string);
    
    property OnProgress: TBackupProgressEvent read FOnProgress write FOnProgress;
    property OnComplete: TBackupCompleteEvent read FOnComplete write FOnComplete;
    property OnError: TBackupErrorEvent read FOnError write FOnError;
  end;

  TRestoreThread = class(TThread)
  private
    FEngine: TBackupEngine;
    FBackupPath: string;
    FOption: TRestoreOption;
    FErrorMsg: string;
    
    FOnProgress: TBackupProgressEvent;
    FOnComplete: TNotifyEvent;
    FOnError: TBackupErrorEvent;
    
    FProgressMsg: string;
    FProgressPct: Integer;
    procedure SyncProgress;
    procedure SyncComplete;
    procedure SyncError;
  protected
    procedure Execute; override;
  public
    constructor Create(AEngine: TBackupEngine; const BackupPath: string; 
      Option: TRestoreOption);
      
    property OnProgress: TBackupProgressEvent read FOnProgress write FOnProgress;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property OnError: TBackupErrorEvent read FOnError write FOnError;
  end;

implementation

{ TBackupThread }

constructor TBackupThread.Create(AEngine: TBackupEngine; const AddonName: string);
begin
  inherited Create(True); // Suspended
  FEngine := AEngine;
  FAddonName := AddonName;
  FreeOnTerminate := True;
end;

procedure TBackupThread.SyncProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(FProgressMsg, FProgressPct);
end;

procedure TBackupThread.SyncComplete;
begin
  if Assigned(FOnComplete) then
    FOnComplete(FResultPath);
end;

procedure TBackupThread.SyncError;
begin
  if Assigned(FOnError) then
    FOnError(FErrorMsg);
end;

procedure TBackupThread.Execute;
begin
  try
    // Progress updates (would need to modify BackupEngine slightly)
    FProgressMsg := 'Starting backup...';
    FProgressPct := 0;
    Synchronize(SyncProgress);
    
    FProgressMsg := 'Copying addon files...';
    FProgressPct := 25;
    Synchronize(SyncProgress);
    
    // THE ACTUAL WORK - engine doesn't know it's threaded
    FResultPath := FEngine.CreateBackup(FAddonName);
    
    FProgressMsg := 'Backup complete!';
    FProgressPct := 100;
    Synchronize(SyncProgress);
    
    // Success!
    Synchronize(SyncComplete);
    
  except
    on E: Exception do
    begin
      FErrorMsg := E.Message;
      Synchronize(SyncError);
    end;
  end;
end;

{ TRestoreThread }

constructor TRestoreThread.Create(AEngine: TBackupEngine; 
  const BackupPath: string; Option: TRestoreOption);
begin
  inherited Create(True); // Suspended
  FEngine := AEngine;
  FBackupPath := BackupPath;
  FOption := Option;
  FreeOnTerminate := True;
end;

procedure TRestoreThread.SyncProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(FProgressMsg, FProgressPct);
end;

procedure TRestoreThread.SyncComplete;
begin
  if Assigned(FOnComplete) then
    FOnComplete(Self);
end;

procedure TRestoreThread.SyncError;
begin
  if Assigned(FOnError) then
    FOnError(FErrorMsg);
end;

procedure TRestoreThread.Execute;
begin
  try
    FProgressMsg := 'Extracting backup...';
    FProgressPct := 25;
    Synchronize(SyncProgress);
    
    FProgressMsg := 'Restoring files...';
    FProgressPct := 50;
    Synchronize(SyncProgress);
    
    // THE WORK
    FEngine.RestoreBackup(FBackupPath, FOption);
    
    FProgressMsg := 'Restore complete!';
    FProgressPct := 100;
    Synchronize(SyncProgress);
    
    Synchronize(SyncComplete);
    
  except
    on E: Exception do
    begin
      FErrorMsg := E.Message;
      Synchronize(SyncError);
    end;
  end;
end;

end.
