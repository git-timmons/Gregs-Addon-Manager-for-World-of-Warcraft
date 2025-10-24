program GregsAddonManager;

uses
  Vcl.Forms,
  MainForm in 'Forms\MainForm.pas' {TMainForm},
  SettingsForm in 'Forms\SettingsForm.pas' {TSettingsForm},
  RestoreForm in 'Forms\RestoreForm.pas' {TRestoreForm},
  AddonManager in 'Units\AddonManager.pas',
  BackupEngine in 'Units\BackupEngine.pas',
  BackupEngineThreaded in 'Units\BackupEngineThreaded.pas',
  FileWatcher in 'Units\FileWatcher.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.