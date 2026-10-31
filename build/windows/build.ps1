# Builds the Windows version of ANCHOR Studio:
#   dist\ANCHOR-Studio-<version>-Windows-Setup.exe   (installer, no admin rights needed)
#   dist\ANCHOR-Studio-<version>-Windows-Portable.zip (unzip and double-click)
# Runs on a GitHub Actions windows-latest machine (or any Windows PC with Inno Setup 6).
# Optional: set $env:R_VERSION (e.g. "4.5.1") to pin R; otherwise the current CRAN release is used.
$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'
function Check($what) { if ($LASTEXITCODE -ne 0) { throw "$what failed (exit $LASTEXITCODE)" } }

$root = (Resolve-Path "$PSScriptRoot\..\..").Path
$distRoot = Join-Path $root 'dist'
$dist = Join-Path $distRoot 'ANCHOR Studio'
if (Test-Path $distRoot) { Remove-Item -Recurse -Force $distRoot }
New-Item -ItemType Directory -Force $dist | Out-Null
$appVersion = ([regex]::Match((Get-Content "$root\app\R\report.R" -Raw), 'APP_VERSION <- "([^"]+)"')).Groups[1].Value
Write-Host "ANCHOR Studio $appVersion"

# 1. R for Windows from CRAN, installed straight into the app folder (R on Windows is relocatable)
$cran = 'https://cloud.r-project.org/bin/windows/base'
if ($env:R_VERSION) { $rver = $env:R_VERSION; $url = "$cran/old/$rver/R-$rver-win.exe" }
else {
  $idx = (Invoke-WebRequest "$cran/" -UseBasicParsing).Content
  $rver = ([regex]::Match($idx, 'R-(\d+\.\d+\.\d+)-win\.exe')).Groups[1].Value
  if (-not $rver) { throw 'Could not find the current R version on CRAN' }
  $url = "$cran/R-$rver-win.exe"
}
Write-Host "R $rver from $url"
$setup = Join-Path $env:TEMP 'R-setup.exe'
Invoke-WebRequest $url -OutFile $setup -UseBasicParsing
$rdir = Join-Path $dist 'R'
$p = Start-Process -Wait -PassThru -FilePath $setup -ArgumentList @('/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART', '/NOICONS', '/MERGETASKS=!recordversion,!associate', "/DIR=`"$rdir`"")
if ($p.ExitCode -ne 0) { throw "R installer exit code $($p.ExitCode)" }
$rscript = Join-Path $rdir 'bin\Rscript.exe'
if (-not (Test-Path $rscript)) { throw "Rscript not found at $rscript" }
Set-Content -Path (Join-Path $dist 'r-version.txt') -Value $rver

# 2. All R packages, as Windows binaries, into R's own library
& $rscript --no-save "$root\build\common\install_packages.R" (Join-Path $rdir 'library'); Check 'Package installation'

# 3. App files, icon, portable start file
Copy-Item -Recurse "$root\app" (Join-Path $dist 'app')
Copy-Item "$root\build\common\anchor.ico" $dist
Copy-Item "$root\build\windows\Start ANCHOR Studio.bat" $dist
Copy-Item "$root\build\windows\README.txt" $dist
Copy-Item "$root\LICENSE", "$root\NOTICE.md" $dist
& $rscript --no-save "$root\build\common\write_notices.R" (Join-Path $rdir 'library') (Join-Path $dist 'THIRD-PARTY-LICENSES.txt'); Check 'Licence notices'

# 3b. Typst, the offline PDF engine for the PDF report (Apache-2.0), into app\bin
$typstVer = if ($env:TYPST_VERSION) { $env:TYPST_VERSION } else { '0.15.1' }
$tz = Join-Path $env:TEMP 'typst.zip'; $tx = Join-Path $env:TEMP 'typst-x'
Invoke-WebRequest "https://github.com/typst/typst/releases/download/v$typstVer/typst-x86_64-pc-windows-msvc.zip" -OutFile $tz -UseBasicParsing
Expand-Archive $tz -DestinationPath $tx -Force
New-Item -ItemType Directory -Force (Join-Path $dist 'app\bin') | Out-Null
Copy-Item (Get-ChildItem $tx -Recurse -Filter 'typst.exe' | Select-Object -First 1).FullName (Join-Path $dist 'app\bin\typst.exe')
Copy-Item (Get-ChildItem $tx -Recurse -Filter 'LICENSE' | Select-Object -First 1).FullName (Join-Path $dist 'app\bin\TYPST-LICENSE.txt')

# 4. Test before packaging
& $rscript --no-save "$root\build\common\smoke_test.R" (Join-Path $dist 'app'); Check 'Smoke test'

# 5. Portable zip and installer
Compress-Archive -Path $dist -DestinationPath (Join-Path $distRoot "ANCHOR-Studio-$appVersion-Windows-Portable.zip") -CompressionLevel Optimal
$iscc = @("${env:ProgramFiles(x86)}\Inno Setup 6\ISCC.exe", "$env:ProgramFiles\Inno Setup 6\ISCC.exe") | Where-Object { Test-Path $_ } | Select-Object -First 1
if (-not $iscc) { choco install innosetup -y --no-progress | Out-Host; $iscc = "${env:ProgramFiles(x86)}\Inno Setup 6\ISCC.exe" }
& $iscc "/DAppVersion=$appVersion" "/DRVersion=$rver" "$root\build\windows\anchor.iss"; Check 'Inno Setup'
Get-ChildItem $distRoot -File | Format-Table Name, @{n = 'MB'; e = { [math]::Round($_.Length / 1MB, 1) } }
