#!/bin/bash
# Builds the macOS version of ANCHOR Studio:
#   dist/ANCHOR-Studio-<version>-macOS-<arm64|x86_64>.dmg
# Runs on a GitHub Actions macOS machine (or any Mac with admin rights).
#   bash build/macos/build.sh [arm64|x86_64]
# Optional: R_PKG_URL=<CRAN .pkg url> to pin the R version.
set -euo pipefail
ARCH="${1:-$(uname -m)}"
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DIST="$ROOT/dist"; rm -rf "$DIST"; mkdir -p "$DIST"
VERSION="$(sed -n 's/^APP_VERSION <- "\(.*\)"/\1/p' "$ROOT/app/R/report.R")"
echo "ANCHOR Studio $VERSION for macOS $ARCH"

# 1. The official CRAN R installer for this architecture (embedded in the app; installed on the build machine)
CRAN=https://cloud.r-project.org/bin/macosx
if [ -z "${R_PKG_URL:-}" ]; then
  REL="$(curl -fsSL "$CRAN/" | grep -oE 'href="[^"]*R-[0-9]+\.[0-9]+\.[0-9]+-'"$ARCH"'\.pkg"' | head -1 | sed -e 's/^href="//' -e 's/"$//')"
  [ -n "$REL" ] || { echo "Could not find an R $ARCH installer on $CRAN"; exit 1; }
  case "$REL" in http*) R_PKG_URL="$REL" ;; *) R_PKG_URL="$CRAN/$REL" ;; esac
fi
RVER="$(basename "$R_PKG_URL" | sed -E 's/^R-([0-9.]+)-.*/\1/')"
echo "R $RVER from $R_PKG_URL"
curl -fL --retry 3 -o "$DIST/R.pkg" "$R_PKG_URL"
sudo installer -pkg "$DIST/R.pkg" -target /
RS=/Library/Frameworks/R.framework/Resources/bin/Rscript
"$RS" -e 'cat("Build R:", R.version.string, R.version$arch, "\n")'

# 2. App bundle
APP="$DIST/ANCHOR Studio.app"; C="$APP/Contents"
mkdir -p "$C/MacOS" "$C/Resources"
cp -R "$ROOT/app" "$C/Resources/app"
cp "$ROOT/build/common/anchor.icns" "$C/Resources/anchor.icns"
mv "$DIST/R.pkg" "$C/Resources/R.pkg"
echo "$RVER" > "$C/Resources/r-version.txt"
cp "$ROOT/build/macos/launcher.sh" "$C/MacOS/ANCHOR Studio"; chmod +x "$C/MacOS/ANCHOR Studio"
sed -e "s/@VERSION@/$VERSION/g" -e "s/@RVERSION@/$RVER/g" "$ROOT/build/macos/Info.plist" > "$C/Info.plist"

# 3. All R packages, as macOS binaries, into the app's own library
"$RS" --no-save "$ROOT/build/common/install_packages.R" "$C/Resources/library"

cp "$ROOT/LICENSE" "$ROOT/NOTICE.md" "$C/Resources/"
"$RS" --no-save "$ROOT/build/common/write_notices.R" "$C/Resources/library" "$C/Resources/THIRD-PARTY-LICENSES.txt"

# 3b. Typst, the offline PDF engine for the PDF report (Apache-2.0), into app/bin
TYPST_VERSION="${TYPST_VERSION:-0.15.1}"; TARCH="$([ "$ARCH" = arm64 ] && echo aarch64 || echo x86_64)"
curl -fL --retry 3 -o "$DIST/typst.tar.xz" "https://github.com/typst/typst/releases/download/v$TYPST_VERSION/typst-$TARCH-apple-darwin.tar.xz"
mkdir -p "$DIST/typst-x" "$C/Resources/app/bin"; tar -xJf "$DIST/typst.tar.xz" -C "$DIST/typst-x"
cp "$(find "$DIST/typst-x" -type f -name typst | head -1)" "$C/Resources/app/bin/typst"; chmod +x "$C/Resources/app/bin/typst"
cp "$(find "$DIST/typst-x" -type f -name LICENSE | head -1)" "$C/Resources/app/bin/TYPST-LICENSE.txt" || true
rm -rf "$DIST/typst.tar.xz" "$DIST/typst-x"

# 4. Test before packaging
"$RS" --no-save "$ROOT/build/common/smoke_test.R" "$C/Resources/app" "$C/Resources/library"

# 5. Ad-hoc signature (not notarised) and disk image
codesign --force --deep --sign - "$APP" || echo "codesign skipped"
STAGE="$DIST/dmg"; mkdir -p "$STAGE"; mv "$APP" "$STAGE/"
ln -s /Applications "$STAGE/Applications"
cp "$ROOT/build/macos/READ ME.txt" "$STAGE/READ ME.txt"
cp "$ROOT/LICENSE" "$ROOT/NOTICE.md" "$STAGE/ANCHOR Studio.app/Contents/Resources/THIRD-PARTY-LICENSES.txt" "$STAGE/"
hdiutil create -volname "ANCHOR Studio" -srcfolder "$STAGE" -ov -format UDZO "$DIST/ANCHOR-Studio-$VERSION-macOS-$ARCH.dmg"
rm -rf "$STAGE"
ls -lh "$DIST"
