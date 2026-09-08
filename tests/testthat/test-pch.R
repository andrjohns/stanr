local_test_context()

init_test_cache("pch")

# The compile-failure retry gate in .stanr_compile_with_pch_retry()
# (R/pch.R) should only rebuild the PCH when the compiler reports a
# PCH-related diagnostic, not on every compile failure. These tests call it
# directly with a scripted `compile_fn`, faking out the actual subprocess
# compiles (via .stanr_system2, as above), so no real PCH or model compile
# ever runs.

# A minimal .stanr_rcmd mock shared by both tests below: it answers the
# `R CMD config CXX20` probe (used by `.stanr_compiler_identity()` to find
# the compiler to run `--version` on) with a fixed compiler name, and every
# other `R CMD config <var>` probe (used while fingerprinting the PCH) with
# "".
mock_pch_rcmd <- function(args, ...) {
  if (identical(args, c("config", "CXX20"))) {
    return("clang++")
  }
  ""
}

# A minimal .stanr_system2 mock shared by both tests below: it answers the
# `clang++ --version` probe (so .stanr_pch_flags()'s compiler_type
# resolution is deterministic across dev machines and CI) with a fixed clang
# identity, and the `... pch` build invocation by creating an empty file at
# the path the real `make ... pch` recipe would have built -- faking a
# successful PCH build without invoking a real compiler. Each `pch`
# invocation is counted via `pch_build_calls`.
mock_pch_system2 <- function(pch_build_calls_env) {
  function(command, args = character(), stdout = TRUE, stderr = TRUE, ...) {
    if (identical(command, "clang++") && identical(args, "--version")) {
      return("clang version 15.0.0 (clang-1500.3.9.4)")
    }
    if (length(args) && identical(args[[length(args)]], "pch")) {
      pch_build_calls_env$n <- pch_build_calls_env$n + 1L
      pch_arg <- grep("PCH=", args, value = TRUE)[[1]]
      # `shQuote()` wraps in `'` under a POSIX shell but `"` under Windows'
      # cmd default, so strip whichever trailing quote is there.
      pch_path <- sub("^['\"]", "", sub("['\"]$", "", sub(".*PCH=", "", pch_arg)))
      dir.create(dirname(pch_path), recursive = TRUE, showWarnings = FALSE)
      file.create(pch_path)
      return(character())
    }
    character()
  }
}

test_that("compile failure with a fresh PCH does not trigger a PCH rebuild", {
  skip_on_webr()
  cache_home <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_home)
  withr::local_options(stanr_pch_dir = file.path(cache_home, "pch"))

  pch_build_calls <- new.env()
  pch_build_calls$n <- 0L
  testthat::local_mocked_bindings(
    .stanr_rcmd = mock_pch_rcmd,
    .stanr_system2 = mock_pch_system2(pch_build_calls)
  )

  base_cppflags <- stanr:::.stanr_base_cppflags()
  cppflags <- paste(stanr:::.stanr_pch_flags(base_cppflags), base_cppflags)

  compile_calls <- 0L
  expect_error(
    stanr:::.stanr_compile_with_pch_retry(
      function(compilation_cppflags) {
        compile_calls <<- compile_calls + 1L
        stop("simulated genuine model compile error")
      },
      cppflags,
      base_cppflags,
      pch_enabled = TRUE
    ),
    "simulated genuine model compile error"
  )
  # One initial PCH build (there was none cached yet) and one compile
  # attempt -- but no rebuild-and-retry, because the error message carries
  # no PCH-related diagnostic.
  expect_equal(pch_build_calls$n, 1L)
  expect_equal(compile_calls, 1L)
})

test_that("compile failure with a PCH-related diagnostic triggers exactly one rebuild-and-retry", {
  skip_on_webr()
  cache_home <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache_home)
  withr::local_options(stanr_pch_dir = file.path(cache_home, "pch"))

  pch_build_calls <- new.env()
  pch_build_calls$n <- 0L
  testthat::local_mocked_bindings(
    .stanr_rcmd = mock_pch_rcmd,
    .stanr_system2 = mock_pch_system2(pch_build_calls)
  )

  base_cppflags <- stanr:::.stanr_base_cppflags()
  cppflags <- paste(stanr:::.stanr_pch_flags(base_cppflags), base_cppflags)
  compile_calls <- 0L

  # Call A: establishes a fresh, on-disk PCH via a successful compile.
  stanr:::.stanr_compile_with_pch_retry(
    function(compilation_cppflags) compile_calls <<- compile_calls + 1L,
    cppflags,
    base_cppflags,
    pch_enabled = TRUE
  )
  expect_equal(pch_build_calls$n, 1L)
  expect_equal(compile_calls, 1L)

  # Call B: fails on its primary attempt (the 2nd compile_fn call overall)
  # with a stale-PCH diagnostic, succeeds on the rebuild-and-retry (3rd).
  stanr:::.stanr_compile_with_pch_retry(
    function(compilation_cppflags) {
      compile_calls <<- compile_calls + 1L
      if (compile_calls == 2L) {
        stop("error: PCH file built from a different file than the one being compiled")
      }
    },
    cppflags,
    base_cppflags,
    pch_enabled = TRUE
  )
  expect_equal(compile_calls, 3L)
  expect_equal(pch_build_calls$n, 2L)
})

test_that("PCH builds announce themselves and retain only two cache entries", {
  skip_on_webr()
  cache_home <- withr::local_tempdir()
  cache_dir <- file.path(cache_home, "pch")
  withr::local_options(stanr_pch_dir = cache_dir)

  pch_build_calls <- new.env()
  pch_build_calls$n <- 0L
  testthat::local_mocked_bindings(
    .stanr_rcmd = mock_pch_rcmd,
    .stanr_system2 = mock_pch_system2(pch_build_calls)
  )

  # Two older, valid entries make the PCH built below the third one.
  old_entries <- file.path(cache_dir, c("oldest", "newer"))
  for (entry in old_entries) {
    dir.create(entry, recursive = TRUE)
    file.create(file.path(entry, "model_pch.hpp.gch"))
  }
  Sys.setFileTime(
    file.path(old_entries, "model_pch.hpp.gch"),
    as.POSIXct(c("2001-01-01", "2002-01-01"), tz = "UTC")
  )

  expect_message(
    stanr:::.stanr_pch_flags(stanr:::.stanr_base_cppflags(), verbose = FALSE),
    "Compiling precompiled model header"
  )
  entries <- list.dirs(cache_dir, full.names = FALSE, recursive = FALSE)
  expect_length(entries, 2L)
  expect_false("oldest" %in% entries)
  expect_equal(pch_build_calls$n, 1L)
})

test_that("a PCH failure after rebuilding falls back without exposing it", {
  testthat::local_mocked_bindings(
    .stanr_pch_flags = function(...) "-include-pch rebuilt.gch"
  )
  compile_calls <- 0L
  result <- expect_no_error(
    stanr:::.stanr_compile_with_pch_retry(
      function(compilation_cppflags) {
        compile_calls <<- compile_calls + 1L
        if (compile_calls == 1L || compile_calls == 2L) {
          stop("error: unable to read PCH file: cached.gch")
        }
        "compiled without PCH"
      },
      cppflags = "-include-pch cached.gch",
      base_cppflags = "-Iinclude",
      pch_enabled = TRUE
    )
  )
  expect_equal(result, "compiled without PCH")
  expect_equal(compile_calls, 3L)
})

withr::deferred_run()
