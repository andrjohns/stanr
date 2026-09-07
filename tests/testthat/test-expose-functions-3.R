local_test_context()

init_test_cache("expose-functions-3")

# Below: StanModel-level integration tests for `$expose_stan_functions()` /
# `$expose_functions()`.

test_that("StanModel$expose_stan_functions() populates $functions on a compiled model", {
  mod <- stan_model(
    code = "
functions {
  real my_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  )

  result <- withVisible(mod$expose_stan_functions())
  expect_false(result$visible)
  expect_identical(result$value, mod$functions)

  expect_true(is.function(mod$functions$my_add))
  expect_equal(mod$functions$my_add(2, 3), 5)
})

test_that("StanModel$expose_stan_functions(global = TRUE) works and can be verified safely", {
  mod <- stan_model(
    code = "
functions {
  real model_global_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  )

  on.exit(
    if (exists("model_global_add", envir = globalenv(), inherits = FALSE)) {
      rm("model_global_add", envir = globalenv())
    },
    add = TRUE
  )

  expect_no_error(mod$expose_stan_functions(global = TRUE))
  expect_true(is.function(mod$functions$model_global_add))
  expect_equal(mod$functions$model_global_add(2, 3), 5)
  expect_true(exists("model_global_add", envir = globalenv(), inherits = FALSE))
  expect_equal(get("model_global_add", envir = globalenv())(2, 3), 5)
})

test_that("a second StanModel$expose_stan_functions() call does not error and still works", {
  mod <- stan_model(
    code = "
functions {
  real model_twice_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  )

  mod$expose_stan_functions()
  first_env <- mod$functions
  expect_equal(mod$functions$model_twice_add(2, 3), 5)

  expect_no_error(mod$expose_stan_functions())
  expect_identical(mod$functions, first_env)
  expect_equal(mod$functions$model_twice_add(4, 5), 9)
})

test_that("StanModel$expose_functions() alias works identically to $expose_stan_functions()", {
  mod <- stan_model(
    code = "
functions {
  real model_alias_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  )

  result <- mod$expose_functions()
  expect_identical(result, mod$functions)
  expect_true(is.function(mod$functions$model_alias_add))
  expect_equal(mod$functions$model_alias_add(2, 3), 5)
})

test_that("expose_stan_functions() works on a compile = FALSE model without compiling it", {
  mod <- stan_model(
    code = "
functions {
  real never_compiled_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
",
    compile = FALSE
  )

  expect_false(mod$is_compiled())

  mod$expose_stan_functions()

  expect_true(is.function(mod$functions$never_compiled_add))
  expect_equal(mod$functions$never_compiled_add(2, 3), 5)
  # The most important assertion in this batch: exposing functions must not
  # trigger a full model compile as a side effect.
  expect_false(mod$is_compiled())
})

test_that("expose_stan_functions() errors for a Stan program with no functions block", {
  skip_if_backend(
    "stanli",
    "stanli populates an empty $functions env for a program with no functions ",
    "block instead of erroring"
  )
  mod <- stan_model(
    code = "
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
",
    compile = FALSE
  )

  expect_error(mod$expose_stan_functions())
})

# Below: `compile_standalone = TRUE` integration tests -- the model's own
# compile also exposes functions via the same sourceCpp-based functions path
# (see .compile_standalone_functions_environment()).

test_that("compile_standalone = TRUE exposes functions with zero extra compilation, and the model still works normally", {
  mod <- stan_model(
    code = "
functions {
  real my_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
",
    compile_standalone = TRUE
  )

  # No call to $expose_stan_functions() anywhere above -- functions must
  # already be populated as a side effect of $compile() / stan_model().
  expect_true(is.function(mod$functions$my_add))
  expect_equal(mod$functions$my_add(2, 3), 5)

  # Most important correctness check: the combined TU must not break normal
  # model compilation/ODR-safety.
  expect_true(mod$is_compiled())
  result <- mod$sample(
    data = list(),
    chains = 1,
    iter_warmup = 10,
    iter_sampling = 10,
    refresh = 0,
    show_messages = FALSE,
    num_threads = test_threads()
  )
  expect_equal(result$return_codes(), 0L)
  expect_true("theta" %in% posterior::variables(result$draws()))
  expect_equal(posterior::ndraws(result$draws()), 10L)
})

test_that("a second compile_standalone model with identical code still populates $functions", {
  code <- "
functions {
  real cache_hit_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  mod1 <- stan_model(
    code = code,
    compile_standalone = TRUE,
    precompiled_headers = FALSE
  )
  expect_true(mod1$is_compiled())
  expect_equal(mod1$functions$cache_hit_add(2, 3), 5)

  # Fresh R6 object, identical code: the post-compile hook that populates
  # $functions must still run.
  mod2 <- stan_model(
    code = code,
    compile_standalone = TRUE,
    precompiled_headers = FALSE
  )
  expect_true(mod2$is_compiled())
  expect_true(is.function(mod2$functions$cache_hit_add))
  expect_equal(mod2$functions$cache_hit_add(4, 5), 9)
})

test_that("compile_standalone compiles the functions block via a separate stanc call", {
  skip_if_backend(
    "stanli",
    "counts stanc() calls made by the compiled backend; stanli builds from a ",
    "MIR without calling stanc()"
  )
  code <- "
functions {
  real hash_sep_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  call_count <- 0
  real_stanc <- stanc
  testthat::local_mocked_bindings(
    stanc = function(...) {
      call_count <<- call_count + 1
      real_stanc(...)
    },
    .package = "stanr"
  )

  mod_plain <- stan_model(code = code, precompiled_headers = FALSE)
  expect_true(mod_plain$is_compiled())
  calls_after_plain <- call_count
  expect_gt(calls_after_plain, 0L)

  # Same Stan code, `compile_standalone = TRUE`: the model TU is identical
  # (a cache hit), but generating the wrappers runs its own stanc(debug-ast)
  # call.
  mod_standalone <- stan_model(
    code = code,
    compile_standalone = TRUE,
    precompiled_headers = FALSE
  )
  expect_true(mod_standalone$is_compiled())
  expect_gt(call_count, calls_after_plain)
})

test_that("compile_standalone errors on a Stan function named stanr_exposed_functions (reserved-name collision)", {
  code <- "
functions {
  real stanr_exposed_functions(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
"
  expect_error(
    stan_model(code = code, compile_standalone = TRUE),
    "stanr_exposed_functions.*reserved"
  )
})

test_that("compile_standalone works together with external_cpp: model, external fn, and plain fn all callable", {
  skip_if_backend("stanli", "external_cpp is a compiled-backend feature")
  code <- paste(
    "functions {",
    "  real external_mean(real x);",
    "  real double_it(real x) { return 2 * x; }",
    "}",
    "data { real x; }",
    "parameters { real mu; }",
    "model { mu ~ normal(external_mean(x), 1); }",
    sep = "\n"
  )

  mod <- stan_model(
    code = code,
    external_cpp = test_path("test-models/external_mean.hpp"),
    compile_standalone = TRUE
  )

  expect_true(mod$is_compiled())
  expect_true(is.function(mod$functions$double_it))
  expect_equal(mod$functions$double_it(3), 6)
  # external_mean's C++ implementation (test-models/external_mean.hpp) just
  # returns its argument unchanged.
  expect_true(is.function(mod$functions$external_mean))
  expect_equal(mod$functions$external_mean(7), 7)
})

test_that("expose_stan_functions(global = TRUE) on a compile_standalone model uses the fast path and sets globals", {
  mod <- stan_model(
    code = "
functions {
  real standalone_global_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
",
    compile_standalone = TRUE
  )

  on.exit(
    if (
      exists("standalone_global_add", envir = globalenv(), inherits = FALSE)
    ) {
      rm("standalone_global_add", envir = globalenv())
    },
    add = TRUE
  )

  # A compile_standalone model already compiled its functions env, so
  # $expose_stan_functions() must not recompile via the separate-TU helper.
  testthat::local_mocked_bindings(
    .compile_standalone_functions_environment = function(...) {
      stop(
        "must not recompile: compile_standalone model already has a functions env"
      )
    },
    .package = "stanr"
  )

  expect_no_error(mod$expose_stan_functions(global = TRUE))
  expect_true(is.function(mod$functions$standalone_global_add))
  expect_equal(mod$functions$standalone_global_add(2, 3), 5)
  expect_true(exists(
    "standalone_global_add",
    envir = globalenv(),
    inherits = FALSE
  ))
  expect_equal(get("standalone_global_add", envir = globalenv())(2, 3), 5)
})

test_that("$compile(force_recompile = TRUE) on a compile_standalone model keeps $functions live and correct", {
  mod <- stan_model(
    code = "
functions {
  real force_recompile_add(real a, real b) { return a + b; }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
",
    compile_standalone = TRUE
  )

  expect_equal(mod$functions$force_recompile_add(2, 3), 5)

  mod$compile(force_recompile = TRUE)

  expect_true(is.function(mod$functions$force_recompile_add))
  expect_equal(mod$functions$force_recompile_add(4, 5), 9)
})

test_that("compile_standalone = TRUE combined-TU model exposes a tuple function correctly", {
  skip_if_backend(
    "stanli",
    "the stanli Function API cannot expose tuple functions"
  )
  mod <- stan_model(
    code = "
functions {
  tuple(real, real) combined_tuple_fn(real a) { return (a, a * 2); }
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
}
",
    compile_standalone = TRUE
  )

  expect_true(mod$is_compiled())
  expect_true(is.function(mod$functions$combined_tuple_fn))
  expect_equal(mod$functions$combined_tuple_fn(3), list(3, 6))
})

withr::deferred_run()
