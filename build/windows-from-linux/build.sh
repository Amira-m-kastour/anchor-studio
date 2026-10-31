#!/bin/bash
# Builds ANCHOR Studio for Windows on a Linux machine (no Windows, no GitHub needed):
#   dist-win/ANCHOR-Studio-<version>-Windows-Setup.exe and ...-Windows-Portable.zip
# Needs: curl, zip, python3 with pefile, x86_64-w64-mingw32-gcc + windres (mingw-w64), makensis (NSIS 3 with amd64 stubs).
# Optional test: wine64 + xvfb (set TEST=1).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"; HERE="$ROOT/build/windows-from-linux"
OUT="$ROOT/dist-win"; D="$OUT/ANCHOR Studio"; rm -rf "$OUT"; mkdir -p "$D"
export MAMBA_ROOT_PREFIX="${MAMBA_ROOT_PREFIX:-$OUT/.mamba}"
curl -sSL -o "$OUT/micromamba" https://github.com/mamba-org/micromamba-releases/releases/download/2.0.5-0/micromamba-linux-64; chmod +x "$OUT/micromamba"

# 1. Windows R runtime from conda-forge (OpenBLAS build), then remove what the app never uses
"$OUT/micromamba" create -y -p "$D/runtime" --platform win-64 -c conda-forge "libblas=*=*openblas" r-base=4.3 r-bibliometrix=4.3.1 \
  r-bibliometrixdata r-shiny r-bslib r-dt r-plotly r-ggplot2 r-ggrepel r-tidyr r-scales r-igraph r-pdftools r-httr2 r-jsonlite \
  r-officer r-processx r-callr r-wordcloud r-maps r-readxl r-openxlsx r-htmltools r-later r-httpuv r-commonmark r-png \
  r-xml2 r-zip typst \
  r-fixest r-sandwich r-lmtest r-metafor r-lavaan r-openalexr \
  r-shinycssloaders r-wordcloud2 r-ggmap r-visnetwork r-shinydashboardplus r-shinydashboard r-shinyjs r-rcurl r-shinywidgets \
  r-webshot2 r-chromote r-pagedown r-tidygraph r-ggraph
cd "$D/runtime"
rm -rf Library/libexec Library/x86_64-w64-mingw32 Library/include include Library/share/doc Library/share/man Library/share/info \
       Library/share/gtk-doc Library/lib/gcc Library/lib/pkgconfig Library/lib/cmake lib/python lib/python3.* Lib/site-packages DLLs
find . -name "*.pdb" -delete; find Library/lib lib -maxdepth 1 \( -name "*.a" -o -name "*.lib" \) -delete
rm -f python*.exe python*.dll pythonw*.exe
(cd Library/bin && for f in *.exe; do case "$f" in fc-*|gspawn-*|curl.exe|openssl.exe|typst.exe) ;; *) rm -f "$f";; esac; done)
rm -rf lib/R/doc/manual lib/R/tests; for d in lib/R/library/*/; do rm -rf "$d/doc" "$d/tests" "$d/unitTests" "$d/examples"; done
python3 -c "import pefile" 2>/dev/null || pip install --user pefile
python3 "$HERE/fix_libstdcxx.py" "$D/runtime"   # every DLL import must resolve on real Windows (Wine stubs missing ones)
cat "$ROOT/build/common/preload_dlls.R" >> lib/R/etc/Rprofile.site   # bundled GCC runtime wins over copies in System32
mkdir -p Library/share/licenses/typst && cp "$MAMBA_ROOT_PREFIX"/pkgs/typst-*/info/licenses/* Library/share/licenses/typst/ 2>/dev/null || true
cd "$ROOT"

# 2. App, launcher, notices
cp -r "$ROOT/app" "$D/app"; find "$D/app" -name "Rplots.pdf" -delete
cp "$ROOT/build/common/anchor.ico" "$HERE/anchor.rc" "$HERE/anchor_launcher.c" "$OUT/"
(cd "$OUT" && x86_64-w64-mingw32-windres anchor.rc -O coff -o anchor_res.o &&
 x86_64-w64-mingw32-gcc -O2 -municode -mwindows -o "ANCHOR Studio/ANCHOR Studio.exe" anchor_launcher.c anchor_res.o -lshell32 -lgdi32 -luser32 -static)
cp "$ROOT/LICENSE" "$ROOT/NOTICE.md" "$ROOT/build/common/anchor.ico" "$D/"
cp "$HERE/README.txt" "$D/README.txt"; cp "$HERE/troubleshoot.cmd" "$D/ANCHOR Studio - troubleshoot.cmd"
python3 - "$D" <<'PY'
import json, glob, sys, os
D=sys.argv[1]; rows=sorted(((j['name'], j['version'], (j.get('license') or 'see package')) for j in
  (json.load(open(f)) for f in glob.glob(os.path.join(D,'runtime','conda-meta','*.json')))), key=lambda r: r[0].lower())
L=["ANCHOR Studio for Windows - third-party software included in this package","",
   "R and its packages and libraries as built by conda-forge (https://conda-forge.org); source at https://anaconda.org/conda-forge/<name>.",""]
L+=[f"{n:34} {v:18} {l[:80]}" for n,v,l in rows]+["","App font: Liberation Sans (SIL OFL 1.1, app/www/fonts/OFL.txt)."]
open(os.path.join(D,'THIRD-PARTY-LICENSES.txt'),'w',newline='\r\n').write('\n'.join(L)+'\n')
PY

# 3. Optional test under Wine
if [ "${TEST:-0}" = "1" ]; then
  export WINEDEBUG=-all WINEPREFIX="$OUT/.wine"; wineboot -i >/dev/null 2>&1 || true
  export WINEPATH="$(winepath -w "$D/runtime/Library/bin");$(winepath -w "$D/runtime/Library/mingw-w64/bin");$(winepath -w "$D/runtime")"
  xvfb-run -a wine "$D/runtime/lib/R/bin/x64/Rscript.exe" --vanilla "$ROOT/build/common/smoke_test.R" "$(winepath -w "$D/app")"
fi

# 4. Installer and portable zip
cp "$HERE/anchor.nsi" "$HERE/wizard.bmp" "$HERE/header.bmp" "$OUT/"
mkdir -p "$OUT/nsis"; mv "$OUT/anchor.nsi" "$OUT/wizard.bmp" "$OUT/header.bmp" "$OUT/nsis/"
sed -i 's|..\\..\\dist-win\\|..\\|g' "$OUT/nsis/anchor.nsi"
(cd "$OUT/nsis" && makensis -V2 anchor.nsi)
VERSION="$(sed -n 's/^APP_VERSION <- "\(.*\)"/\1/p' "$ROOT/app/R/report.R")"
(cd "$OUT" && zip -r -9 -q "ANCHOR-Studio-$VERSION-Windows-Portable.zip" "ANCHOR Studio")
ls -lh "$OUT"/*.exe "$OUT"/*.zip
