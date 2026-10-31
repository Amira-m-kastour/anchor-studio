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
