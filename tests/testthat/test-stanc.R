local_test_context()

init_test_cache("stanc")

test_that("stanc resolves nested includes from include_directories", {
  code <- paste(
    readLines(test_path("test-models/include_model.stan")),
    collapse = "\n"
  )

  cpp_code <- stanc(
    code,
    include_directories = test_path("test-models/includes")
  )

  expect_type(cpp_code, "character")
  expect_match(cpp_code, "shifted_normal_lpdf")
  expect_match(cpp_code, "centered")
})

test_that("stan_model forwards include_paths to stanc", {
  mod <- stan_model(
    stan_file = test_stan_file("include_model.stan"),
    include_paths = test_path("test-models/includes")
  )

  expect_s3_class(mod, "StanModel")
  expect_true(mod$is_compiled())
})

test_that("stanc reports an unresolved include and its search path", {
  expect_error(
    stanc(
      'functions {\n#include "does-not-exist.stan"\n}',
      include_directories = test_path("test-models/includes")
    ),
    "Could not find Stan include file 'does-not-exist.stan'"
  )
})

test_that("stanc detects circular includes", {
  include_dir <- tempfile("stanr-includes-")
  dir.create(include_dir)
  writeLines('#include "second.stan"', file.path(include_dir, "first.stan"))
  writeLines('#include "first.stan"', file.path(include_dir, "second.stan"))

  expect_error(
    stanc(
      'functions {\n#include "first.stan"\n}',
      include_directories = include_dir
    ),
    "Circular Stan include detected"
  )
})

test_that("stanc prepends external C++ and permits declared C++ functions", {
  external_cpp <- c(
    test_path("test-models/external_mean.hpp"),
    test_path("test-models/external_marker.hpp")
  )
  code <- paste(
    "functions { real external_mean(real x); }",
    "data { real x; }",
    "parameters { real mu; }",
    "model { mu ~ normal(external_mean(x), 1); }",
    sep = "\n"
  )

  cpp_code <- stanc(code, external_cpp = external_cpp)

  expect_match(cpp_code, "^#include <ostream>")
  expect_match(cpp_code, "external_mean\\(x, pstream__\\)")
  expect_match(cpp_code, "external_cpp_marker")
  expect_lt(
    regexpr("external_mean", cpp_code)[1],
    regexpr("external_cpp_marker", cpp_code)[1]
  )
})

test_that("stan_model compiles a model using external C++", {
  skip_if_backend("stanli", "external_cpp is a compiled-backend feature")
  code <- paste(
    "functions { real external_mean(real x); }",
    "data { real x; }",
    "parameters { real mu; }",
    "model { mu ~ normal(external_mean(x), 1); }",
    sep = "\n"
  )

  mod <- stan_model(
    code = code,
    external_cpp = test_path("test-models/external_mean.hpp")
  )

  expect_s3_class(mod, "StanModel")
  expect_true(mod$is_compiled())
})

test_that("stanc validates external C++ paths", {
  expect_error(
    stanc(
      "parameters { real x; } model { x ~ normal(0, 1); }",
      external_cpp = "does-not-exist.hpp"
    ),
    "External C\\+\\+ files do not exist"
  )
})

test_that("model_variables returns correct structure from stanc info", {
  code <- paste(
    "data { int N; }",
    "parameters { real alpha; vector[N] beta; }",
    "transformed parameters { real gamma; }",
    "generated quantities { real delta; }",
    sep = "\n"
  )

  vars <- stanr:::model_variables(code)

  expect_named(
    vars,
    c("data", "parameters", "transformed_parameters", "generated_quantities")
  )
  expect_named(vars$data, "N")
  expect_equal(vars$data$N$type, "int")
  expect_equal(vars$data$N$dimensions, 0L)

  expect_named(vars$parameters, c("alpha", "beta"))
  expect_equal(vars$parameters$alpha$dimensions, 0L)
  expect_equal(vars$parameters$beta$dimensions, 1L)

  expect_named(vars$transformed_parameters, "gamma")
  expect_named(vars$generated_quantities, "delta")
})

test_that("model_variables handles arrays and matrices", {
  code <- paste(
    "data { array[2, 3] int y; }",
    "parameters { array[2] matrix[3, 4] theta; }",
    sep = "\n"
  )

  vars <- stanr:::model_variables(code)

  expect_equal(vars$data$y$dimensions, 2L)
  expect_equal(vars$parameters$theta$dimensions, 3L)
})

test_that("model_variables allows undefined functions with flag", {
  code <- paste(
    "functions { real external_fn(real x); }",
    "parameters { real x; }",
    "model { x ~ normal(external_fn(x), 1); }",
    sep = "\n"
  )

  expect_error(stanr:::model_variables(code), "declared without")
  vars <- stanr:::model_variables(code, allow_undefined = TRUE)
  expect_named(vars$parameters, "x")
})

test_that("StanModel$variables() returns correct structure", {
  mod <- stan_model(
    code = paste(
      "data { int N; }",
      "parameters { real alpha; vector[N] beta; }",
      sep = "\n"
    ),
    compile = FALSE
  )

  vars <- mod$variables()
  expect_named(
    vars,
    c("data", "parameters", "transformed_parameters", "generated_quantities")
  )
  expect_named(vars$data, "N")
  expect_named(vars$parameters, c("alpha", "beta"))
  expect_equal(vars$parameters$alpha$dimensions, 0L)
  expect_equal(vars$parameters$beta$dimensions, 1L)
})

test_that("StanModel$variables() caches result", {
  mod <- stan_model(
    code = "parameters { real x; }",
    compile = FALSE
  )

  vars1 <- mod$variables()
  vars2 <- mod$variables()
  expect_identical(vars1, vars2)
})

test_that("StanModel$variables() works with stan_file", {
  mod <- stan_model(
    stan_file = test_stan_file("bernoulli.stan"),
    compile = FALSE
  )

  vars <- mod$variables()
  expect_named(vars$data, c("N", "y"))
  expect_named(vars$parameters, "theta")
})

test_that("StanModel resolves includes identically regardless of compile()/variables() order", {
  # $compile() first, then $variables()
  mod_compile_first <- stan_model(
    stan_file = test_stan_file("include_model.stan"),
    include_paths = test_path("test-models/includes"),
    compile = FALSE
  )
  mod_compile_first$compile()
  vars_compile_first <- mod_compile_first$variables()

  # $variables() first, then $compile()
  mod_variables_first <- stan_model(
    stan_file = test_stan_file("include_model.stan"),
    include_paths = test_path("test-models/includes"),
    compile = FALSE
  )
  vars_variables_first <- mod_variables_first$variables()
  mod_variables_first$compile()

  expect_true(mod_compile_first$is_compiled())
  expect_true(mod_variables_first$is_compiled())
  expect_identical(vars_compile_first, vars_variables_first)
  expect_named(vars_compile_first$data, "y")
  expect_named(vars_compile_first$parameters, "mu")
})

test_that("StanModel caches resolved #include code between $compile() and $variables()", {
  # `resolve_stan_includes()` is called both by our own caching wrapper
  # (`resolved_code()`) and, redundantly but harmlessly, by `stanc()` /
  # `model_variables()` themselves (they always call it, but hit the
  # fast-path no-op return when there's nothing left to resolve -- see
  # R/stanc.R). So the *total* call count isn't a reliable signal by itself;
  # what matters is that no call after the first actually does resolution
  # work, i.e. is ever invoked with code that still contains "#include".
  real_resolve <- stanr:::resolve_stan_includes
  work_call_count <- 0
  total_call_count <- 0
  testthat::local_mocked_bindings(
    resolve_stan_includes = function(model_code, ...) {
      total_call_count <<- total_call_count + 1
      if (grepl("#include", model_code, fixed = TRUE)) {
        work_call_count <<- work_call_count + 1
      }
      real_resolve(model_code, ...)
    },
    .package = "stanr"
  )

  mod <- stan_model(
    stan_file = test_stan_file("include_model.stan"),
    include_paths = test_path("test-models/includes"),
    compile = FALSE
  )
  expect_equal(total_call_count, 0)

  mod$compile()
  # The fixture's #include chain (include_model -> nested_helpers ->
  # normal_helpers) requires resolving 2 levels of `#include` lines -- this
  # is the only real resolution work performed across the model's whole
  # lifetime.
  expect_equal(work_call_count, 2)
  expect_gt(total_call_count, 0)

  vars <- mod$variables()
  # $variables() reuses the cached resolved code from $compile(): no
  # additional #include resolution work happens, whatever fast-path
  # passthrough calls `model_variables()` makes internally.
  expect_equal(work_call_count, 2)
  expect_named(vars$parameters, "mu")

  # Repeat calls to $variables() are already cached on their own (via
  # `variables_`), independent of this change -- no more calls at all.
  total_after_variables <- total_call_count
  mod$variables()
  expect_equal(total_call_count, total_after_variables)
  expect_equal(work_call_count, 2)
})

test_that("stanc(warn_pedantic = TRUE) surfaces a pedantic warning", {
  code <- paste(
    "parameters { real theta; }",
    "model { theta ~ uniform(0, 1); }",
    sep = "\n"
  )

  expect_warning(stanc(code, warn_pedantic = TRUE))
})

test_that("stanc(optim_level = 1) compiles and names the model", {
  code <- "parameters { real x; } model { x ~ normal(0, 1); }"

  cpp_code <- stanc(code, optim_level = 1)

  expect_type(cpp_code, "character")
  expect_match(cpp_code, "model_namespace")
})

test_that("stanc(warn_uninitialized = TRUE) compiles without error", {
  code <- paste(
    "parameters { real x; }",
    "transformed parameters {",
    "  real y;",
    "  if (x > 0) {",
    "    y = x;",
    "  }",
    "}",
    "model { x ~ normal(0, 1); }",
    sep = "\n"
  )

  expect_no_error(stanc(code, warn_uninitialized = TRUE))
})

test_that("StanModel$variables() works with external_cpp", {
  skip_if_backend("stanli", "external_cpp is a compiled-backend feature")
  code <- paste(
    "functions { real external_mean(real x); }",
    "parameters { real mu; }",
    "model { mu ~ normal(external_mean(0), 1); }",
    sep = "\n"
  )

  mod <- stan_model(
    code = code,
    external_cpp = test_path("test-models/external_mean.hpp"),
    compile = FALSE
  )

  vars <- mod$variables()
  expect_named(vars$parameters, "mu")
})

withr::deferred_run()
