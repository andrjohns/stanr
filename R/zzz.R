# Suppresses R CMD check NOTES for R6 `self`/`private`.
private <- self <- NULL

# Session memo: compiled model envs, stanc context, stan version.
.stanr_memo <- new.env(parent = emptyenv())

.stanr_dll_path <- function(libname, pkgname) {
  ext <- .Platform$dynlib.ext
  arch <- .Platform$r_arch
  root <- file.path(libname, pkgname)

  libs_dir <- if (nzchar(arch)) {
    file.path(root, "libs", arch)
  } else {
    file.path(root, "libs")
  }
  installed <- file.path(libs_dir, paste0(pkgname, ext))
  if (file.exists(installed)) {
    return(installed)
  }

  dev <- file.path(root, "src", paste0(pkgname, ext))
  if (file.exists(dev)) dev else installed
}

.stanr_dll <- NULL

.onLoad <- function(libname, pkgname) {
  # TBB is a regular dependency with a package-private library identity and is
  # resolved through stanr.so's rpath. Keep the DLL and its dependencies local
  # so their symbols are not added to the process-wide lookup scope.
  dll <- dyn.load(.stanr_dll_path(libname, pkgname), local = TRUE, now = TRUE)
  .stanr_dll <<- dll

  # No useDynLib -- bind routines by hand.
  ns <- asNamespace(pkgname)
  assign(
    "stanr_xptr_is_null",
    getNativeSymbolInfo("stanr_xptr_is_null", dll),
    envir = ns
  )
  assign(
    "stanr_hash_strings",
    getNativeSymbolInfo("stanr_hash_strings", dll),
    envir = ns
  )
  assign(
    "stanr_max_concurrency",
    getNativeSymbolInfo("stanr_max_concurrency", dll),
    envir = ns
  )
  invisible(NULL)
}

.onUnload <- function(libpath) {
  if (!is.null(.stanr_dll)) {
    try(dyn.unload(.stanr_dll[["path"]]), silent = TRUE)
  }
  invisible(NULL)
}
