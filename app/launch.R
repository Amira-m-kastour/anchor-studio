# ANCHOR Studio - standalone launcher (used by the Windows and macOS apps)
#   Rscript launch.R [--lib <package library>]
# Opens the app in its own browser window and stops R about 20 seconds after
# the last ANCHOR Studio window is closed.

# Windows: load the bundled GCC runtime DLLs by full path before any package, so that a copy of libstdc++-6.dll or
# libgcc_s_seh-1.dll installed by another program in C:\Windows\System32 (searched before PATH) cannot be picked instead.
if (.Platform$OS.type == "windows") local({
  for (b in unique(c(file.path(R.home(), "..", "..", "Library", "bin"), file.path(R.home(), "..", "..", "Library", "mingw-w64", "bin")))) {
    if (!dir.exists(b)) next
    for (f in c("libwinpthread-1.dll", "libgcc_s_seh-1.dll", "libstdc++-6.dll", "libgomp-1.dll", "libquadmath-0.dll", "libgfortran-5.dll")) {
      p <- normalizePath(file.path(b, f), winslash = "\\", mustWork = FALSE)
      if (file.exists(p) && !any(vapply(getLoadedDLLs(), function(d) identical(tolower(d[["path"]]), tolower(p)), TRUE))) try(dyn.load(p, local = FALSE, now = TRUE), silent = TRUE)
    }
  }
})

args <- commandArgs(trailingOnly = TRUE)
i <- match("--lib", args)
if (!is.na(i) && length(args) > i && dir.exists(args[i + 1])) .libPaths(c(normalizePath(args[i + 1]), .libPaths()))
Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))   # also used by "Open full Biblioshiny"

f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
app_dir <- if (length(f)) dirname(normalizePath(f[1])) else getwd()
options(anchor.standalone = TRUE, shiny.launch.browser = FALSE)
# a project file to open at start: double-clicked in Windows Explorer (the launcher passes it as ANCHOR_OPEN) or --open <file>
local({ j <- match("--open", args); of <- if (!is.na(j) && length(args) > j) args[j + 1] else Sys.getenv("ANCHOR_OPEN")
  if (nzchar(of) && file.exists(of)) options(anchor.open_file = normalizePath(of)) })

# Prefer an app-style window (Edge on Windows, Chrome on macOS); fall back to the default browser.
open_window <- function(url) {
  if (.Platform$OS.type == "windows") {
    edge <- c(file.path(Sys.getenv("ProgramFiles(x86)"), "Microsoft/Edge/Application/msedge.exe"),
              file.path(Sys.getenv("ProgramFiles"), "Microsoft/Edge/Application/msedge.exe"),
              file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft/Edge/Application/msedge.exe"))
    edge <- edge[file.exists(edge)]
    # processx passes the path and arguments without a shell, so spaces and brackets in
    # "C:\Program Files (x86)\..." are safe (system2() quotes the command itself; quoting it again broke the path)
    if (length(edge)) {
      ok <- tryCatch({ processx::process$new(edge[1], c(paste0("--app=", url), "--new-window"), cleanup = FALSE, cleanup_tree = FALSE); TRUE },
                     error = function(e) { message("Could not open Edge (", conditionMessage(e), "); using the default browser."); FALSE })
      if (ok) return(invisible(TRUE))
    }
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    for (b in c("Google Chrome", "Microsoft Edge", "Brave Browser"))
      if (dir.exists(file.path("/Applications", paste0(b, ".app"))))
        return(invisible(system2("open", c("-na", shQuote(b), "--args", paste0("--app=", url)), wait = FALSE)))
  }
  utils::browseURL(url)
}

port <- httpuv::randomPort()
url <- sprintf("http://127.0.0.1:%d", port)
message("ANCHOR Studio ", url, " (R ", getRversion(), ", bibliometrix ", utils::packageVersion("bibliometrix"), ")")
ready <- function() { f <- Sys.getenv("ANCHOR_READY_FILE"); if (nzchar(f)) try(writeLines(url, f), silent = TRUE) }
if (!nzchar(Sys.getenv("ANCHOR_NO_BROWSER"))) later::later(function() { open_window(url); later::later(ready, 2) }, 1.5) else later::later(ready, 1)
shiny::runApp(app_dir, port = port, host = "127.0.0.1", launch.browser = FALSE)
