# Suppresses R CMD check NOTES for R6 `self`/`private`.
private <- self <- NULL

# Session memo: compiled model envs, stanc context, stan version.
.stanr_memo <- new.env(parent = emptyenv())

# No useDynLib (see stanr-package.R): .onLoad loads TBB before stanr.so
# itself, so its rpath resolves under devtools::load_all(). Mirrors RcppParallel.
.stanr_tbb_lib_names <- function() {
  if (R.version$os == "emscripten") {
    return(character())
  }
  sysname <- Sys.info()[["sysname"]]
  if (sysname == "Darwin") {
    c("libtbb.dylib", "libtbbmalloc.dylib")
  } else if (sysname == "Windows") {
    c("tbb.dll", "tbbmalloc.dll")
  } else {
    c("libtbb.so.2", "libtbbmalloc.so.2")
  }
}

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
.stanr_tbb_dlls <- list()

.onLoad <- function(libname, pkgname) {
  lib_dir <- system.file("lib", .Platform$r_arch, package = pkgname)
  if (nzchar(lib_dir) && dir.exists(lib_dir)) {
    for (name in .stanr_tbb_lib_names()) {
      path <- file.path(lib_dir, name)
      if (file.exists(path)) {
        .stanr_tbb_dlls[[name]] <<- dyn.load(path, local = FALSE, now = TRUE)
      }
    }
  }

  # Per-model libraries link their runner support directly. Only TBB must be
  # global; keeping the package DLL local avoids interposing its Stan/stanli
  # implementation symbols on other Stan packages in the same R process.
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
  for (dll in .stanr_tbb_dlls) {
    try(dyn.unload(dll[["path"]]), silent = TRUE)
  }
  invisible(NULL)
}
