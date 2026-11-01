#!/bin/bash
# ANCHOR Studio.app/Contents/MacOS/ANCHOR Studio
# Runs the R version the app was built with, directly from its own folder in R.framework, so the
# Mac's default R version is never changed. If that R version is missing, it is installed once from the
# copy inside the app, and the previous default R version is put back afterwards.
CONTENTS="$(cd "$(dirname "$0")/.." && pwd)"
RES="$CONTENTS/Resources"
APPDIR="$(cd "$CONTENTS/.." && pwd)"
LOG="$HOME/Library/Logs/ANCHOR Studio.log"; mkdir -p "$HOME/Library/Logs"
NEED="$(cat "$RES/r-version.txt")"; NEED_MINOR="${NEED%.*}"
FW=/Library/Frameworks/R.framework

alert() { /usr/bin/osascript -e "display alert \"ANCHOR Studio\" message \"$1\" as critical" >/dev/null 2>&1; }

# The R start script (bin/R) inside every R version folder points at the Mac's *default* R
# (R.framework/Resources = Versions/Current), so running it directly would start the default version.
# To run the wanted version without changing the default, a small private R home is made whose bin/R and
# bin/Rscript point at itself and whose other folders link to the real version folder. Child R processes
# (background analyses) start from R.home() and so also get the right version.
PRIV="$HOME/Library/Caches/ANCHOR-Studio"
make_home() {  # $1 = real R home (.../Versions/<v>/Resources); sets RH and RS
  local real="$1" ver home f n
  ver="$(basename "$(dirname "$real")")"; home="$PRIV/R-$ver"
  rm -rf "$home"; mkdir -p "$home/bin" || return 1
  for f in "$real"/*; do n="$(basename "$f")"; [ "$n" = bin ] || ln -s "$f" "$home/$n"; done
  for f in "$real"/bin/*; do n="$(basename "$f")"; case "$n" in R|Rscript) ;; *) ln -s "$f" "$home/bin/$n" ;; esac; done
  sed -e "s|/Library/Frameworks/R.framework/Resources|$home|g" "$real/bin/R" > "$home/bin/R" || return 1
  printf '#!/bin/sh\nRHOME="%s" exec "%s" "$@"\n' "$home" "$real/bin/Rscript" > "$home/bin/Rscript"
  chmod +x "$home/bin/R" "$home/bin/Rscript"
  RH="$home"; RS="$home/bin/R"
}

# minor version (e.g. 4.6) of the R that a start script really runs (no spaces or backslashes in the
# expression: bin/R passes its arguments on unquoted)
minor_of() { "$1" --no-echo --no-save --no-restore -e 'cat(as.character(getRversion()))' 2>>"$LOG" | cut -d. -f1,2; }

find_r() {
  RS=""; RH=""
  local d
  for d in "$FW"/Versions/"$NEED_MINOR"*/Resources; do
    [ -x "$d/bin/exec/R" ] || continue
    make_home "$d" || continue
    if [ "$(minor_of "$RS")" = "$NEED_MINOR" ]; then return 0; fi
    echo "$(date) · $d did not start as R $NEED_MINOR" >>"$LOG"
  done
  RS=""; RH=""; return 1
}

# Files downloaded from the internet carry a quarantine flag that can block R from loading
# the packages inside the app; remove it from the app's own files (first start only).
if [ ! -f "$RES/.unquarantined" ]; then
  /usr/bin/xattr -dr com.apple.quarantine "$APPDIR" 2>/dev/null && touch "$RES/.unquarantined" 2>/dev/null
fi

if ! find_r; then
  PREV="$(readlink "$FW/Versions/Current" 2>/dev/null)"
  if [ -n "$PREV" ]; then MSG="ANCHOR Studio needs R $NEED_MINOR, which is not on this Mac yet (you have R $PREV). Install R $NEED alongside it now? Your current default R stays the default. This is needed once and asks for your Mac password."
  else MSG="ANCHOR Studio needs R, the free statistics software it runs on. Install R $NEED now? This is needed once and asks for your Mac password."; fi
  ANSWER="$(/usr/bin/osascript -e "button returned of (display dialog \"$MSG\" buttons {\"Cancel\", \"Install R\"} default button \"Install R\" with title \"ANCHOR Studio\" with icon note)" 2>/dev/null)"
  [ "$ANSWER" = "Install R" ] || exit 0
  # install, then point "Current" back at the previous default version (if there was one)
  /usr/bin/osascript - "$RES/R.pkg" "$PREV" <<'APPLESCRIPT' >>"$LOG" 2>&1
on run argv
  set cmd to "/usr/sbin/installer -pkg " & quoted form of (item 1 of argv) & " -target /"
  if (item 2 of argv) is not "" then
    set cmd to cmd & " && cd /Library/Frameworks/R.framework/Versions && [ -d " & quoted form of (item 2 of argv) & " ] && ln -sfn " & quoted form of (item 2 of argv) & " Current"
  end if
  do shell script cmd with administrator privileges
end run
APPLESCRIPT
  if ! find_r; then alert "R $NEED could not be installed or started. Details are in $LOG"; exit 1; fi
fi

echo "=== $(date) · R at $RH ===" >>"$LOG"
unset R_HOME RHOME
# from inside the app folder, so the file name has no spaces (bin/R splits its arguments at spaces)
cd "$RES/app" || exit 1
"$RS" --no-echo --no-save --no-restore --file=launch.R --args --lib "$RES/library" >>"$LOG" 2>&1
STATUS=$?
if [ $STATUS -ne 0 ]; then alert "ANCHOR Studio stopped with an error. Details are in $LOG"; fi
exit $STATUS
