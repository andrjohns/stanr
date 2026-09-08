# Compiled models now cache to a single file next to their `stan_file` (see
# `.stanr_build_cache_file()`, R/stan_model.R), so tests must never compile
# directly from `test_path("test-models", ...)`  -- that would write a
# `.so`/`.dll` sibling into the checked-in fixtures tree. `test_stan_file()`
# copies the whole fixtures tree into a session-scratch tempdir once (so
# `#include`/`include_paths`-relative lookups inside a copied `.stan` file
# still resolve against copied siblings) and every `stan_file =` test call
# site is routed through it instead of `test_path()` directly.
test_stan_file <- local({
  scratch_dir <- NULL
  function(relpath) {
    if (is.null(scratch_dir)) {
      scratch_dir <<- file.path(tempdir(), "test-models-scratch")
      dir.create(scratch_dir, recursive = TRUE, showWarnings = FALSE)
      file.copy(
        list.files(test_path("test-models"), full.names = TRUE),
        scratch_dir,
        recursive = TRUE
      )
    }
    file.path(scratch_dir, relpath)
  }
})

# ---------------------------------------------------------------------------
# Backend selection.
#
# `stan_model()` has two backends -- "compiled" (Stan -> C++ -> shared
# library) and "stanli" (interpreted) -- and this suite runs in full under
# either one. The package takes the default `backend` from the
# `stanr_backend` option, then the STANR_BACKEND environment variable (see
# `.stanr_default_backend()`, R/stan_model.R). The suite drives that through
# STANR_BACKEND: one backend per testthat process, "compiled" when unset,
# with tests/testthat.R running the suite once per backend under R CMD
# check. To run the stanli pass interactively:
#
#   withr::with_envvar(c(STANR_BACKEND = "stanli"), devtools::test())
#
# Plain `stan_model()` and `test_model()` calls therefore build models with
# the backend under test, while a call site that passes `backend =` itself
# (as test-stanli-backend.R does) behaves the same under either setting. A
# test of a feature only one backend has starts with `skip_if_backend()`, so
# the other pass reports it as skipped rather than silently narrowing its
# assertions.
# ---------------------------------------------------------------------------
# The wasm build of the suite (tools/webr-test/run-stanli-tests.mjs, run by
# the "wasm" R-CMD-check job) runs the whole suite under the stanli backend in
# webR. Some tests need what webR/Emscripten cannot provide regardless of
# backend -- a C++ toolchain for `R CMD SHLIB` (`system()` is unsupported), or
# std::thread (webR is built without pthreads). Those skip via this helper so
# the webR pass reports them as skipped rather than erroring; `...` is pasted
# into the reason.
skip_on_webr <- function(...) {
  if (identical(R.version$os, "emscripten")) {
    testthat::skip(paste0(paste0(...), " [webR/Emscripten]"))
  }
}

test_backend <- function() {
  backend <- stanr:::.stanr_default_backend()
  if (!backend %in% c("compiled", "stanli")) {
    stop(
      "STANR_BACKEND must be \"compiled\" or \"stanli\", not \"",
      backend,
      "\" (tests/testthat.R is what runs both passes).",
      call. = FALSE
    )
  }
  backend
}

# `...` is pasted into the reason, so long reasons can be split over lines.
skip_if_backend <- function(backend, ...) {
  if (identical(test_backend(), backend)) {
    testthat::skip(paste0(paste0(...), " [", backend, " backend]"))
  }
}

# Model cache shared across test files (helpers load once per run), keyed by
# backend so a cached entry never leaks across passes.
test_model <- local({
  models <- new.env(parent = emptyenv())
  function(name) {
    key <- paste(test_backend(), name, sep = "/")
    if (is.null(models[[key]])) {
      models[[key]] <- stan_model(
        stan_file = test_stan_file(paste0(name, ".stan"))
      )
    }
    models[[key]]
  }
})

bernoulli_data <- list(N = 10, y = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0))

# Data for the tuple_complex_battery test model: exercises every tuple/complex
# shape it declares (scalar/vector/matrix/array complex, plain and nested and
# array-of tuples).
battery_data <- function() {
  list(
    zd = 1 + 2i,
    zv = c(1 + 1i, 2 - 2i),
    zm = matrix(c(1 + 1i, 2 + 2i, 3 + 3i, 4 + 4i), 2, 2),
    za = c(5 + 5i, 6 - 6i),
    td = list(1.5, c(2.5, 3.5)),
    tad = list(list(10L, 1 + 1i), list(20L, 2 + 2i)),
    acv = list(
      list(complex(real = 1:3, imaginary = 11:13), 100),
      list(complex(real = 4:6, imaginary = 14:16), 200)
    ),
    t2d = list(
      list(list(11L, 1.1), list(12L, 1.2)),
      list(list(21L, 2.1), list(22L, 2.2))
    ),
    nt = list(999, list(list(1, 1 + 2i), list(2, 3 + 4i)))
  )
}

# Initialise a unique PCH cache per test file, per run. Compiled models no
# longer go through an options-driven cache dir (see `test_stan_file()`
# above), but the PCH cache is still a shared, options-driven, on-disk cache
# and test files still get their own isolated copy of it.
init_test_cache <- function(test_name) {
  cache_path <- file.path(tempdir(), "test-cache", test_name)
  if (dir.exists(cache_path)) {
    unlink(cache_path, recursive = TRUE, force = TRUE)
  }

  options(stanr_pch_dir = file.path(cache_path, "pch"))
}

# `model_hash` (R/stan_model.R) is keyed on Stan code content alone, and the
# on-disk cache file persists for the whole test run -- so any two `code = `
# calls anywhere in the suite using textually identical Stan source would
# otherwise share a cache entry (including with a mocked/fake compile from a
# PCH test). Wrap any throwaway `code` string in this to keep it unique to
# its call site.
unique_stan_code <- function(
  body = "parameters { real theta; } model { theta ~ normal(0, 1); }"
) {
  paste0("// ", basename(tempfile()), "\n", body)
}

test_threads <- function() {
  if (utils::getFromNamespace("on_cran", "testthat")()) {
    1
  } else {
    parallel::detectCores()
  }
}
