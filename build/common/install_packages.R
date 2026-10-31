# Installs every R package ANCHOR Studio needs into the app's own library.
#   Rscript install_packages.R <library folder>
args <- commandArgs(trailingOnly = TRUE)
lib <- if (length(args)) args[1] else .libPaths()[1]
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 900, Ncpus = 4)

pkgs <- readLines(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))), "packages.txt"))
pkgs <- trimws(sub("#.*$", "", pkgs)); pkgs <- pkgs[nzchar(pkgs)]
type <- if (.Platform$OS.type == "windows" || Sys.info()[["sysname"]] == "Darwin") "binary" else getOption("pkgType")
cat("Installing", length(pkgs), "packages (and their dependencies) into", lib, "as", type, "\n")
install.packages(pkgs, lib = lib, type = type)

ok <- vapply(pkgs, function(p) requireNamespace(p, lib.loc = .libPaths(), quietly = TRUE), logical(1))
if (!all(ok)) { cat("MISSING:", paste(pkgs[!ok], collapse = ", "), "\n"); quit(status = 1) }
ip <- utils::installed.packages(lib.loc = lib)
cat("OK:", nrow(ip), "packages in the app library. bibliometrix", as.character(utils::packageVersion("bibliometrix", lib.loc = lib)), "\n")
