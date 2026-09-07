local_test_context()

init_test_cache("stan_model")

test_that("stan_model compiles from file", {
  path <- test_stan_file("bernoulli.stan")
  mod <- stan_model(stan_file = path)
  expect_s3_class(mod, "StanModel")
  expect_s3_class(mod, "R6")
  expect_true(mod$is_compiled())
})

test_that("stan_model compiles from code", {
  code <- '
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
  '
  mod <- stan_model(code = code)
  expect_s3_class(mod, "StanModel")
  expect_true(mod$is_compiled())
})

test_that("stan_model errors when neither file nor code given", {
  expect_snapshot(stan_model(), error = TRUE)
})

test_that("stan_model errors when both file and code given", {
  path <- test_stan_file("bernoulli.stan")
  expect_snapshot(
    stan_model(stan_file = path, code = "parameters { real x; }"),
    error = TRUE
  )
})

test_that("stan_model validates the precompiled_headers argument", {
  expect_error(
    stan_model(code = "parameters { real x; }", precompiled_headers = NA),
    "`precompiled_headers` must be TRUE or FALSE"
  )
})

test_that("stan_model errors on missing file", {
  expect_snapshot(stan_model(stan_file = "nonexistent.stan"), error = TRUE)
})

test_that("stan_model validates the cpp_options argument", {
  expect_error(
    stan_model(code = "parameters { real x; }", cpp_options = "not-a-list"),
    "`cpp_options` must be a list"
  )
  expect_error(
    stan_model(code = "parameters { real x; }", cpp_options = list(1)),
    "Unnamed `cpp_options` entries must each be a single string"
  )
  expect_error(
    stan_model(
      code = "parameters { real x; }",
      cpp_options = list("not an assignment")
    ),
    "form '<NAME> = <value>' or '<NAME> \\+= <value>'"
  )
  expect_error(
    stan_model(
      code = "parameters { real x; }",
      cpp_options = list(CXXFLAGS = list("-O3"))
    ),
    "single non-missing string, number, or logical"
  )
})

test_that("stan_model accepts a name repeated across cpp_options entries", {
  skip_if_backend(
    "stanli",
    "non-empty cpp_options are a compiled-backend feature"
  )
  # The same name may legitimately appear more than once (e.g. an
  # overriding assignment followed by an appending one), so this must not
  # error.
  expect_no_error(
    stan_model(
      code = "parameters { real x; }",
      cpp_options = list(CXXFLAGS = "-O3", "CXXFLAGS += -Wall"),
      compile = FALSE
    )
  )
})

test_that("stan_model stores and reports cpp_options via $cpp_options()", {
  skip_if_backend(
    "stanli",
    "non-empty cpp_options are a compiled-backend feature"
  )
  mod <- stan_model(
    code = "parameters { real x; }",
    cpp_options = list(CXXFLAGS = "-O3", STAN_THREADS = TRUE),
    compile = FALSE
  )
  expect_equal(
    mod$cpp_options(),
    list(CXXFLAGS = "-O3", STAN_THREADS = TRUE)
  )
})

test_that("a named cpp_options entry compiles successfully (overrides, doesn't break the build)", {
  skip_if_backend(
    "stanli",
    "non-empty cpp_options are a compiled-backend feature"
  )
  # Overriding CXXFLAGS drops stanr's own -O3 -g0 optimization flags, but
  # must not break the build: Makeconf's own CXXFLAGS still apply via the
  # outer `withr::with_makevars(assignment = "+=")` layer, and PKG_CPPFLAGS
  # (carrying the `-I` include path stanr's headers need) is untouched.
  mod <- stan_model(
    code = '
      parameters { real theta; }
      model { theta ~ normal(0, 1); }
    ',
    precompiled_headers = FALSE,
    cpp_options = list(CXXFLAGS = "-O0")
  )
  expect_true(mod$is_compiled())
})

test_that(".stanr_parse_cpp_options parses named, '=', and '+=' entries", {
  parsed <- stanr:::.stanr_parse_cpp_options(
    list(
      CXX = "g++",
      "CXXFLAGS = -O3",
      "CXXFLAGS += -Wno-psabi",
      THREADS = TRUE
    )
  )
  expect_equal(
    parsed,
    list(
      list(name = "CXX", op = "=", value = "g++"),
      list(name = "CXXFLAGS", op = "=", value = "-O3"),
      list(name = "CXXFLAGS", op = "+=", value = "-Wno-psabi"),
      list(name = "THREADS", op = "=", value = "TRUE")
    )
  )
})

test_that(".stanr_apply_makevars overrides on '=' and appends on '+='", {
  base <- c(CXXFLAGS = "-g -O2", PKG_LIBS = "-lfoo")

  # A named argument (i.e. `op = "="`) overrides rather than appends.
  overridden <- stanr:::.stanr_apply_makevars(
    base,
    list(list(name = "CXXFLAGS", op = "=", value = "-O3"))
  )
  expect_equal(unname(overridden[["CXXFLAGS"]]), "-O3")

  # `+=` appends to the existing value instead of replacing it.
  appended <- stanr:::.stanr_apply_makevars(
    base,
    list(list(name = "CXXFLAGS", op = "+=", value = "-Wno-psabi"))
  )
  expect_equal(unname(appended[["CXXFLAGS"]]), "-g -O2 -Wno-psabi")

  # `+=` on a name absent from `base` degrades to a plain set.
  new_var <- stanr:::.stanr_apply_makevars(
    base,
    list(list(name = "LDFLAGS", op = "+=", value = "-lbar"))
  )
  expect_equal(unname(new_var[["LDFLAGS"]]), "-lbar")

  # Assignments are applied in order: override then append composes.
  composed <- stanr:::.stanr_apply_makevars(
    base,
    list(
      list(name = "CXXFLAGS", op = "=", value = "-O3"),
      list(name = "CXXFLAGS", op = "+=", value = "-Wall")
    )
  )
  expect_equal(unname(composed[["CXXFLAGS"]]), "-O3 -Wall")
})

# Persistent, cross-session model cache ---------------------------------------
#
# The compiled model is now cached as a single file next to `stan_file` (or
# under `tempdir()` for a `code`-string model), keyed by a hash embedded
# inside the file rather than a shared central cache_dir -- see
# `.stanr_build_cache_file()` / `.stanr_write_build_cache()` /
# `.stanr_restore_build_cache()` (R/stan_model.R). A forced recompile always
# builds into a brand-new scratch directory (never a shared/reused one), so
# there is nothing to redirect/alias the way the old central-cache registry
# had to: a live fit's mapped library is never touched by a later recompile.
#
# `model_hash` is keyed on Stan code content alone (not on `stan_file`'s
# path), and the on-disk cache persists for the whole test run -- so any two
# tests using textually identical Stan code would otherwise share a cache
# entry. Each test below therefore uses `unique_stan_code()` (helpers.R) to
# keep its code, and hence its hash, unique to that test.

test_that("stan_model writes exactly one cache file next to stan_file", {
  skip_if_backend(
    "stanli",
    "the on-disk compiled-library cache is a compiled-backend feature"
  )
  src_dir <- withr::local_tempdir()
  stan_file <- file.path(src_dir, "model.stan")
  writeLines(unique_stan_code(), stan_file)

  mod <- stan_model(stan_file = stan_file, precompiled_headers = FALSE)
  expect_true(mod$is_compiled())

  cache_file <- file.path(src_dir, paste0(".model", .Platform$dynlib.ext))
  expect_true(file.exists(cache_file))
  contents <- setdiff(list.files(src_dir, all.files = TRUE), c(".", ".."))
  expect_setequal(
    contents,
    c("model.stan", paste0(".model", .Platform$dynlib.ext))
  )
})

test_that("stan_model caches to tempdir, not the working directory, for a code string", {
  skip_if_backend(
    "stanli",
    "the on-disk compiled-library cache is a compiled-backend feature"
  )
  code <- unique_stan_code()
  before <- list.files(tempdir(), pattern = paste0("\\", .Platform$dynlib.ext, "$"))

  mod <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod$is_compiled())

  after <- list.files(tempdir(), pattern = paste0("\\", .Platform$dynlib.ext, "$"))
  expect_gt(length(setdiff(after, before)), 0L)
})

test_that("a second compile of identical code in the same session skips stanc() via the in-memory memo", {
  skip_if_backend(
    "stanli",
    "counts stanc() calls made by the compiled backend; stanli builds from a ",
    "MIR without calling stanc()"
  )
  call_count <- 0
  real_stanc <- stanc
  testthat::local_mocked_bindings(
    stanc = function(...) {
      call_count <<- call_count + 1
      real_stanc(...)
    },
    .package = "stanr"
  )

  code <- unique_stan_code()
  mod <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod$is_compiled())
  expect_equal(call_count, 1L)

  mod2 <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod2$is_compiled())
  expect_equal(call_count, 1L)
})

test_that("restoring a warm on-disk cache skips stanc() even without an in-memory hit", {
  skip_if_backend(
    "stanli",
    "the on-disk compiled-library cache is a compiled-backend feature"
  )
  src_dir <- withr::local_tempdir()
  stan_file <- file.path(src_dir, "model.stan")
  writeLines(unique_stan_code(), stan_file)
  mod <- stan_model(stan_file = stan_file, precompiled_headers = FALSE)
  expect_true(mod$is_compiled())

  # Forget the in-session memo, so the next compile must go through the
  # on-disk cache file rather than the in-memory shortcut.
  memo <- stanr:::.stanr_memo$compiled_envs
  rm(list = ls(memo, all.names = TRUE), envir = memo)

  call_count <- 0
  real_stanc <- stanc
  testthat::local_mocked_bindings(
    stanc = function(...) {
      call_count <<- call_count + 1
      real_stanc(...)
    },
    .package = "stanr"
  )
  mod2 <- stan_model(stan_file = stan_file, precompiled_headers = FALSE)
  expect_true(mod2$is_compiled())
  expect_equal(call_count, 0L)
})

test_that("force_recompile forces a fresh compile and overwrites the single cache file in place", {
  skip_if_backend(
    "stanli",
    "the on-disk compiled-library cache is a compiled-backend feature"
  )
  src_dir <- withr::local_tempdir()
  stan_file <- file.path(src_dir, "model.stan")
  writeLines(unique_stan_code(), stan_file)
  mod <- stan_model(stan_file = stan_file, precompiled_headers = FALSE)
  cache_file <- file.path(src_dir, paste0(".model", .Platform$dynlib.ext))
  expect_true(file.exists(cache_file))
  mtime_before <- file.mtime(cache_file)
  Sys.sleep(1)

  call_count <- 0
  real_stanc <- stanc
  testthat::local_mocked_bindings(
    stanc = function(...) {
      call_count <<- call_count + 1
      real_stanc(...)
    },
    .package = "stanr"
  )
  mod$compile(force_recompile = TRUE, quiet = TRUE)
  expect_equal(call_count, 1L)
  expect_gt(file.mtime(cache_file), mtime_before)
  # Overwritten in place, not accumulated.
  expect_length(
    list.files(
      src_dir,
      pattern = paste0("\\", .Platform$dynlib.ext, "$"),
      all.files = TRUE
    ),
    1L
  )
})

test_that("the stanr_force_recompile option forces a fresh compile on a warm cache", {
  skip_if_backend(
    "stanli",
    "counts stanc() calls made by the compiled backend; stanli builds from a ",
    "MIR without calling stanc()"
  )
  call_count <- 0
  real_stanc <- stanc
  testthat::local_mocked_bindings(
    stanc = function(...) {
      call_count <<- call_count + 1
      real_stanc(...)
    },
    .package = "stanr"
  )

  code <- unique_stan_code()
  mod <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod$is_compiled())
  expect_equal(call_count, 1L)

  withr::local_options(stanr_force_recompile = TRUE)
  mod2 <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod2$is_compiled())
  expect_equal(call_count, 2L)
})

test_that("force_recompile does not unload the library a live fit still points into", {
  src_dir <- withr::local_tempdir()
  stan_file <- file.path(src_dir, "model.stan")
  writeLines(unique_stan_code(), stan_file)
  mod <- stan_model(stan_file = stan_file, precompiled_headers = FALSE)
  fit <- mod$sample(
    data = list(),
    chains = 1,
    iter_warmup = 20,
    iter_sampling = 20,
    seed = 1,
    show_messages = FALSE
  )

  mod$compile(force_recompile = TRUE, quiet = TRUE)

  # The old fit's pointers came from the superseded build, which stays
  # mapped (a forced recompile always builds into a fresh scratch dir, never
  # overwriting one in place), so it must still be usable.
  expect_true(is.finite(fit$summary()$mean[[1L]]))
})

test_that("changing external_cpp file contents (same path) changes model_hash and triggers a fresh compile", {
  skip_if_backend("stanli", "external_cpp is a compiled-backend feature")
  call_count <- 0
  real_stanc <- stanc
  testthat::local_mocked_bindings(
    stanc = function(...) {
      call_count <<- call_count + 1
      real_stanc(...)
    },
    .package = "stanr"
  )

  external_cpp_dir <- withr::local_tempdir()
  external_cpp_path <- file.path(external_cpp_dir, "external_mean.hpp")
  writeLines(
    c(
      "#include <ostream>",
      "",
      "template <typename T__>",
      "T__ external_mean(const T__& x, std::ostream* pstream__) {",
      "  return x;",
      "}"
    ),
    external_cpp_path
  )

  code <- paste(
    "functions { real external_mean(real x); }",
    "data { real x; }",
    "parameters { real mu; }",
    "model { mu ~ normal(external_mean(x), 1); }",
    sep = "\n"
  )

  mod <- stan_model(
    code = code,
    external_cpp = external_cpp_path,
    precompiled_headers = FALSE
  )
  expect_true(mod$is_compiled())
  expect_equal(call_count, 1L)

  # Same path, different contents: the returned value doubles rather than
  # passing `x` straight through, so the hash (keyed on file contents) must
  # differ even though `external_cpp_path` is unchanged.
  writeLines(
    c(
      "#include <ostream>",
      "",
      "template <typename T__>",
      "T__ external_mean(const T__& x, std::ostream* pstream__) {",
      "  return x + x;",
      "}"
    ),
    external_cpp_path
  )

  mod2 <- stan_model(
    code = code,
    external_cpp = external_cpp_path,
    precompiled_headers = FALSE
  )
  expect_true(mod2$is_compiled())
  expect_equal(call_count, 2L)
})

test_that("changing cpp_options changes model_hash and triggers a fresh compile", {
  skip_if_backend(
    "stanli",
    "non-empty cpp_options are a compiled-backend feature"
  )
  call_count <- 0
  real_stanc <- stanc
  testthat::local_mocked_bindings(
    stanc = function(...) {
      call_count <<- call_count + 1
      real_stanc(...)
    },
    .package = "stanr"
  )

  code <- unique_stan_code()

  # `+=` (not a named override) so the internally computed `PKG_CPPFLAGS`
  # (which carries the `-I` include path stanr's own headers need) is
  # appended to rather than replaced.
  mod <- stan_model(
    code = code,
    precompiled_headers = FALSE,
    cpp_options = list("PKG_CPPFLAGS += -DSTANR_TEST_FLAG=1")
  )
  expect_true(mod$is_compiled())
  expect_equal(call_count, 1L)

  # Same Stan code, different `cpp_options`: must be treated as a distinct
  # compiled artifact, not reused from the first model's cache entry.
  mod2 <- stan_model(
    code = code,
    precompiled_headers = FALSE,
    cpp_options = list("PKG_CPPFLAGS += -DSTANR_TEST_FLAG=2")
  )
  expect_true(mod2$is_compiled())
  expect_equal(call_count, 2L)
})

test_that("stan_model falls back to tempdir when stan_file's directory is unwritable", {
  skip_if_backend(
    "stanli",
    "the on-disk compiled-library cache is a compiled-backend feature"
  )
  src_dir <- withr::local_tempdir()
  stan_file <- file.path(src_dir, "model.stan")
  writeLines(unique_stan_code(), stan_file)
  old_mode <- file.mode(src_dir)
  Sys.chmod(src_dir, mode = "0500")
  on.exit(Sys.chmod(src_dir, mode = old_mode), add = TRUE)
  skip_if_not(
    file.access(src_dir, 2) != 0,
    "cannot make a directory unwritable in this environment (e.g. running as root)"
  )

  before <- list.files(tempdir(), pattern = paste0("\\", .Platform$dynlib.ext, "$"))
  expect_no_warning(
    mod <- stan_model(stan_file = stan_file, precompiled_headers = FALSE)
  )
  expect_true(mod$is_compiled())
  # Nothing written into the unwritable source dir itself.
  expect_equal(
    setdiff(list.files(src_dir, all.files = TRUE), c(".", "..", "model.stan")),
    character()
  )
  after <- list.files(tempdir(), pattern = paste0("\\", .Platform$dynlib.ext, "$"))
  expect_gt(length(setdiff(after, before)), 0L)
})

withr::deferred_run()
