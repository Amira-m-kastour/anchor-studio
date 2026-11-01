; ANCHOR Studio 1.7.1 - Windows installer (NSIS). Installs for the current user, no administrator rights.
Unicode true
Target amd64-unicode
!include "MUI2.nsh"

!define APPNAME "ANCHOR Studio"
!define VERSION "1.7.1"
!define PUBLISHER "Amira Mamdouh Kastour"
!define SRC "..\..\dist-win\ANCHOR Studio"
!define UNINSTKEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\ANCHORStudio"

Name "${APPNAME} ${VERSION}"
OutFile "..\..\dist-win\ANCHOR-Studio-${VERSION}-Windows-Setup.exe"
InstallDir "$LOCALAPPDATA\Programs\${APPNAME}"
InstallDirRegKey HKCU "Software\ANCHORStudio" "InstallDir"
RequestExecutionLevel user
SetCompressor /SOLID lzma
SetCompressorDictSize 64
BrandingText "${APPNAME} ${VERSION} by A. Mamdouh Kastour"
VIProductVersion "1.7.1.0"
VIAddVersionKey "ProductName" "${APPNAME}"
VIAddVersionKey "FileDescription" "${APPNAME} installer - by A. Mamdouh Kastour"
VIAddVersionKey "FileVersion" "${VERSION}"
VIAddVersionKey "ProductVersion" "${VERSION}"
VIAddVersionKey "CompanyName" "${PUBLISHER}"
VIAddVersionKey "LegalCopyright" "(c) 2026 ${PUBLISHER}, GPL-3"

!define MUI_ICON "${SRC}\anchor.ico"
!define MUI_UNICON "${SRC}\anchor.ico"
!define MUI_ABORTWARNING
!define MUI_WELCOMEFINISHPAGE_BITMAP "wizard.bmp"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "wizard.bmp"
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_RIGHT
!define MUI_HEADERIMAGE_BITMAP "header.bmp"
!define MUI_WELCOMEPAGE_TITLE "Install ${APPNAME} ${VERSION}"
!define MUI_WELCOMEPAGE_TEXT "ANCHOR Studio by A. Mamdouh Kastour$\r$\nA. Mamdouh Kastour's Scholarly Anchor$\r$\nIn tribute to Mamdouh Kastour (1950-2020)$\r$\n$\r$\nIt guides an article-anchored literature review through six gated phases and includes 39 bibliometric analyses from bibliometrix, lessons and live-checked references at every stage.$\r$\n$\r$\nEverything it needs (R 4.3 and all packages) is included. It installs for your user account only and needs no administrator rights.$\r$\n$\r$\nAbout 600 MB of disk space is needed."
!define MUI_FINISHPAGE_RUN "$INSTDIR\ANCHOR Studio.exe"
!define MUI_FINISHPAGE_RUN_TEXT "Start ANCHOR Studio now"
!define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\README.txt"
!define MUI_FINISHPAGE_SHOWREADME_TEXT "Show the read-me file"
!define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "${SRC}\LICENSE"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "ANCHOR Studio (required)" SecMain
  SectionIn RO
  ; a running copy (R or its helpers) keeps files locked: stop processes started from this folder first
  nsExec::Exec 'powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "Get-Process | Where-Object { $$_.Path -like \"$INSTDIR\*\" } | Stop-Process -Force -ErrorAction SilentlyContinue"'
  Pop $0
  Sleep 1500
  ; replace an older version cleanly, but never touch the user's saved work (kept in AppData\Local\R)
  retry_clean:
  RMDir /r "$INSTDIR\runtime"
  RMDir /r "$INSTDIR\app"
  IfFileExists "$INSTDIR\runtime\*.*" 0 cleaned
    MessageBox MB_RETRYCANCEL|MB_ICONEXCLAMATION "Some files of the previous ANCHOR Studio are still in use.$\r$\nClose ANCHOR Studio (and any R windows it opened), then click Retry." /SD IDCANCEL IDRETRY retry_clean
    Abort "Installation stopped: files of the previous version are still in use."
  cleaned:
  SetOutPath "$INSTDIR"
  File "${SRC}\ANCHOR Studio.exe"
  File "${SRC}\anchor.ico"
  File "${SRC}\LICENSE"
  File "${SRC}\NOTICE.md"
  File "${SRC}\README.txt"
  File "${SRC}\THIRD-PARTY-LICENSES.txt"
  File "${SRC}\ANCHOR Studio - troubleshoot.cmd"
  File /r "${SRC}\app"
  File /r "${SRC}\runtime"
  WriteUninstaller "$INSTDIR\Uninstall ANCHOR Studio.exe"
  WriteRegStr HKCU "Software\ANCHORStudio" "InstallDir" "$INSTDIR"
  ; open .anchor project files with ANCHOR Studio (double-click in Explorer)
  WriteRegStr HKCU "Software\Classes\.anchor" "" "ANCHORStudio.Project"
  WriteRegStr HKCU "Software\Classes\ANCHORStudio.Project" "" "ANCHOR Studio project"
  WriteRegStr HKCU "Software\Classes\ANCHORStudio.Project\DefaultIcon" "" "$INSTDIR\anchor.ico"
  WriteRegStr HKCU "Software\Classes\ANCHORStudio.Project\shell\open\command" "" '"$INSTDIR\ANCHOR Studio.exe" "%1"'
  System::Call 'shell32::SHChangeNotify(i 0x08000000, i 0, p 0, p 0)'
  WriteRegStr HKCU "${UNINSTKEY}" "DisplayName" "${APPNAME}"
  WriteRegStr HKCU "${UNINSTKEY}" "DisplayVersion" "${VERSION}"
  WriteRegStr HKCU "${UNINSTKEY}" "Publisher" "${PUBLISHER}"
  WriteRegStr HKCU "${UNINSTKEY}" "DisplayIcon" "$INSTDIR\anchor.ico"
  WriteRegStr HKCU "${UNINSTKEY}" "InstallLocation" "$INSTDIR"
  WriteRegStr HKCU "${UNINSTKEY}" "UninstallString" '"$INSTDIR\Uninstall ANCHOR Studio.exe"'
  WriteRegDWORD HKCU "${UNINSTKEY}" "NoModify" 1
  WriteRegDWORD HKCU "${UNINSTKEY}" "NoRepair" 1
  WriteRegDWORD HKCU "${UNINSTKEY}" "EstimatedSize" 600000
  CreateShortcut "$SMPROGRAMS\${APPNAME}.lnk" "$INSTDIR\ANCHOR Studio.exe" "" "$INSTDIR\anchor.ico" 0 SW_SHOWNORMAL "" "Start ANCHOR Studio"
  CreateShortcut "$SMPROGRAMS\${APPNAME} - troubleshoot.lnk" "$INSTDIR\ANCHOR Studio - troubleshoot.cmd" "" "$INSTDIR\anchor.ico" 0 SW_SHOWNORMAL "" "Start ANCHOR Studio and show its messages"
SectionEnd

Section "Desktop shortcut" SecDesktop
  CreateShortcut "$DESKTOP\${APPNAME}.lnk" "$INSTDIR\ANCHOR Studio.exe" "" "$INSTDIR\anchor.ico" 0 SW_SHOWNORMAL "" "Start ANCHOR Studio"
SectionEnd

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecMain} "The app, with R 4.3 and all packages it needs."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDesktop} "Put an ANCHOR Studio shortcut on the desktop."
!insertmacro MUI_FUNCTION_DESCRIPTION_END

Section "Uninstall"
  nsExec::Exec 'powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "Get-Process | Where-Object { $$_.Path -like \"$INSTDIR\*\" } | Stop-Process -Force -ErrorAction SilentlyContinue"'
  Pop $0
  Delete "$SMPROGRAMS\${APPNAME}.lnk"
  Delete "$SMPROGRAMS\${APPNAME} - troubleshoot.lnk"
  Delete "$INSTDIR\ANCHOR Studio - troubleshoot.cmd"
  Delete "$DESKTOP\${APPNAME}.lnk"
  RMDir /r "$INSTDIR\runtime"
  RMDir /r "$INSTDIR\app"
  Delete "$INSTDIR\ANCHOR Studio.exe"
  Delete "$INSTDIR\anchor.ico"
  Delete "$INSTDIR\LICENSE"
  Delete "$INSTDIR\NOTICE.md"
  Delete "$INSTDIR\README.txt"
  Delete "$INSTDIR\THIRD-PARTY-LICENSES.txt"
  Delete "$INSTDIR\Uninstall ANCHOR Studio.exe"
  RMDir "$INSTDIR"
  DeleteRegKey HKCU "${UNINSTKEY}"
  DeleteRegKey HKCU "Software\ANCHORStudio"
  ReadRegStr $0 HKCU "Software\Classes\.anchor" ""
  StrCmp $0 "ANCHORStudio.Project" 0 +2
  DeleteRegKey HKCU "Software\Classes\.anchor"
  DeleteRegKey HKCU "Software\Classes\ANCHORStudio.Project"
  System::Call 'shell32::SHChangeNotify(i 0x08000000, i 0, p 0, p 0)'
SectionEnd
