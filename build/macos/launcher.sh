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

# minor version (e.g. 4.5) reported by an Rscript, with RHOME pointing at its own folder
minor_of() { RHOME="$2" "$1" -e 'cat(R.version$major, ".", sub("\\..*", "", R.version$minor), sep = "")' 2>/dev/null; }

find_r() {
  RS=""; RH=""
  for d in "$FW"/Versions/"$NEED_MINOR"*/Resources; do
    [ -x "$d/bin/Rscript" ] || continue
    if [ "$(minor_of "$d/bin/Rscript" "$d")" = "$NEED_MINOR" ]; then RS="$d/bin/Rscript"; RH="$d"; return 0; fi
  done
  # fallback: the default R, if it happens to be the right version
  if [ -x "$FW/Resources/bin/Rscript" ] && [ "$(minor_of "$FW/Resources/bin/Rscript" "$FW/Resources")" = "$NEED_MINOR" ]; then
    RS="$FW/Resources/bin/Rscript"; RH="$FW/Resources"; return 0
  fi
  return 1
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
RHOME="$RH" R_HOME="$RH" "$RS" --no-save --no-restore "$RES/app/launch.R" --lib "$RES/library" >>"$LOG" 2>&1
STATUS=$?
if [ $STATUS -ne 0 ]; then alert "ANCHOR Studio stopped with an error. Details are in $LOG"; fi
exit $STATUS
