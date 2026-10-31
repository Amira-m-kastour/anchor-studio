# Writes THIRD-PARTY-LICENSES.txt for an installer: R itself plus every package in the bundled library.
#   Rscript write_notices.R <package library> <output file>
args <- commandArgs(trailingOnly = TRUE)
lib <- args[1]; out <- args[2]
ip <- as.data.frame(utils::installed.packages(lib.loc = unique(c(lib, .libPaths())), fields = c("License", "URL")), stringsAsFactors = FALSE)
ip <- ip[!duplicated(ip$Package), ]
ip <- ip[order(tolower(ip$Package)), ]
lines <- c("ANCHOR Studio - third-party software included in this installer",
           sprintf("Generated %s", format(Sys.time(), "%Y-%m-%d %H:%M %Z")), "",
           sprintf("R %s - The R Foundation for Statistical Computing - GPL-2 | GPL-3 - https://cran.r-project.org/src/base/",
                   as.character(getRversion())), "",
           "R packages (name, version, licence, source):",
           sprintf("%-22s %-12s %-40s %s", ip$Package, ip$Version, substr(gsub("\\s+", " ", ip$License), 1, 40),
                   ifelse(ip$Priority %in% c("base", "recommended"), "included with R",
                          sprintf("https://cran.r-project.org/package=%s", ip$Package))), "",
           "Typst (PDF report engine, app/bin) - Apache-2.0 - https://github.com/typst/typst (licence: app/bin/TYPST-LICENSE.txt)", "",
           "Full licence texts: R/share/licenses (Windows) or the R framework's share/licenses (macOS),",
           "and the LICENSE or COPYING file inside each package folder where provided.",
           "Source code for every item above is available from CRAN, including its Archive for older versions.")
writeLines(lines, out)
cat("Wrote", out, "with", nrow(ip), "packages\n")
