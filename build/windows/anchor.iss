; Inno Setup script for ANCHOR Studio (Windows). Built by build.ps1.
#ifndef AppVersion
  #define AppVersion "1.5.1"
#endif
#ifndef RVersion
  #define RVersion "R"
#endif

[Setup]
AppId={{3F2A9C1E-7B4D-4E8A-9C21-6D5E4F3A2B10}
AppName=ANCHOR Studio
AppVersion={#AppVersion}
AppVerName=ANCHOR Studio {#AppVersion} by A. Mamdouh Kastour
AppPublisher=Amira Mamdouh Kastour
AppComments=Article-anchored literature reviews with bibliometrix (includes R {#RVersion})
DefaultDirName={localappdata}\Programs\ANCHOR Studio
DisableDirPage=auto
DisableProgramGroupPage=yes
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
OutputDir=..\..\dist
OutputBaseFilename=ANCHOR-Studio-{#AppVersion}-Windows-Setup
SetupIconFile=..\common\anchor.ico
UninstallDisplayIcon={app}\anchor.ico
Compression=lzma2/max
SolidCompression=yes
WizardStyle=modern
LicenseFile=..\..\LICENSE
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible

[Tasks]
Name: "desktopicon"; Description: "Create a desktop shortcut"; GroupDescription: "Shortcuts:"

[Files]
Source: "..\..\dist\ANCHOR Studio\*"; DestDir: "{app}"; Flags: recursesubdirs createallsubdirs ignoreversion

[Icons]
Name: "{autoprograms}\ANCHOR Studio"; Filename: "{app}\R\bin\Rscript.exe"; Parameters: "--no-save --no-restore ""{app}\app\launch.R"""; WorkingDir: "{app}"; IconFilename: "{app}\anchor.ico"; Comment: "Start ANCHOR Studio"; Flags: runminimized
Name: "{autodesktop}\ANCHOR Studio"; Filename: "{app}\R\bin\Rscript.exe"; Parameters: "--no-save --no-restore ""{app}\app\launch.R"""; WorkingDir: "{app}"; IconFilename: "{app}\anchor.ico"; Comment: "Start ANCHOR Studio"; Flags: runminimized; Tasks: desktopicon

[Run]
Filename: "{app}\R\bin\Rscript.exe"; Parameters: "--no-save --no-restore ""{app}\app\launch.R"""; WorkingDir: "{app}"; Description: "Start ANCHOR Studio now"; Flags: nowait postinstall skipifsilent runminimized
