; Inno Setup configuration for the eturnal TURN server.

; https://github.com/DomGries/InnoDependencyInstaller

#define public Dependency_NoExampleSetup
#include "..\..\InnoDependencyInstaller\CodeDependencies.iss"

[Setup]
AppName=eturnal
AppPublisher=ProcessOne, SARL
AppPublisherURL=https://eturnal.net
AppSupportURL=https://github.com/processone/eturnal/issues
AppVersion=@VERSION@
WizardStyle=modern
DefaultDirName={autopf}\eturnal
DefaultGroupName=eturnal
SolidCompression=yes
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
AllowNoIcons=yes
LicenseFile=doc\LICENSE.txt
SourceDir=..\_build\prod\rel\eturnal
OutputDir=..\..\..\..\windows\installer
SetupIconFile=..\..\..\..\windows\logo.ico

[Files]
Source: *; DestDir: {app}; Flags: createallsubdirs recursesubdirs

[INI]
Filename: {app}\doc\eturnal.url; Section: InternetShortcut; Key: URL; String: https://eturnal.net

[Icons]
Name: {group}\License; Filename: {app}\doc\LICENSE.txt
Name: {group}\Web Site; Filename: {app}\doc\eturnal.url

[Run]
Filename: {app}\bin\eturnal.cmd; Parameters: install; WorkingDir: {app}; Flags: runhidden
Filename: {app}\bin\delay-start.cmd; Flags: runhidden

[UninstallRun]
Filename: {app}\bin\eturnal.cmd; Parameters: stop; WorkingDir: {app}; Flags: runhidden; RunOnceId: StopService
Filename: {app}\bin\eturnal.cmd; Parameters: uninstall; WorkingDir: {app}; Flags: runhidden; RunOnceId: DelService

[UninstallDelete]
Type: files; Name: {app}\doc\eturnal.url

[Code]
function InitializeSetup: Boolean;
begin
  Dependency_AddVC2015To2022;

  Result := True
end;

function EditCfgFiles: Boolean;
var
  SysCfgFile: TStrings;
  YmlCfgFile: TStrings;
  SysCfgPath: String;
  YmlCfgPath: String;
  SysContent: String;
  YmlContent: String;
  RelRootDir: String;
begin
  SysCfgFile := TStringList.Create;
  YmlCfgFile := TStringList.Create;
  SysCfgPath := ExpandConstant('{app}\releases\{#SetupSetting("AppVersion")}\sys.config');
  YmlCfgPath := ExpandConstant('{app}\etc\eturnal.yml');
  RelRootDir := ExpandConstant('{app}');

  StringChangeEx(RelRootDir, '\', '/', True);

  try
    Result := True;

    try
      SysCfgFile.LoadFromFile(SysCfgPath);
      YmlCfgFile.LoadFromFile(YmlCfgPath);
      SysContent := SysCfgFile.Text;
      YmlContent := YmlCfgFile.Text;

      { Only save if content has been modified. }
      if StringChangeEx(SysContent, '@INSTALL_DIR@', RelRootDir, True) > 0 then
      begin
        SysCfgFile.Text := SysContent;
        SysCfgFile.SaveToFile(SysCfgPath)
      end;
      if StringChangeEx(YmlContent, '@INSTALL_DIR@', RelRootDir, True) > 0 then
      begin
        YmlCfgFile.Text := YmlContent;
        YmlCfgFile.SaveToFile(YmlCfgPath)
      end;

    except
      Result := False
    end;

  finally
    SysCfgFile.Free;
    YmlCfgFile.Free
  end
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  ResultCode: Integer;
begin
  if CurStep = ssPostInstall then
  begin
    if not EditCfgFiles() then
      MsgBox('Cannot adjust installation path within eturnal configuration',
             mbError, MB_OK);
    if not Exec(ExpandConstant('{app}\bin\eturnal.cmd'), 'start', '', SW_HIDE,
                ewWaitUntilTerminated, ResultCode) then
      MsgBox('Cannot start eturnal service: ' + SysErrorMessage(ResultCode),
              mbError, MB_OK)
  end
end;
