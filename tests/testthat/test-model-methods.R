local_test_context()

init_test_cache("model-methods")

.stanr_model_method_state <- new.env(parent = emptyenv())

loaded_dll_paths <- function() {
  vapply(
    getLoadedDLLs(),
    function(dll) normalizePath(dll[["path"]], winslash = "/", mustWork = FALSE),
    character(1),
    USE.NAMES = FALSE
  )
}

.stanr_model_method_data <- function(double = FALSE) {
  y <- c(1L, 1L, 1L, 0L)
  if (double) {
    y <- rep(y, 2L)
  }
  list(N = length(y), y = y, mu = 0)
}

.stanr_model_method_model <- function() {
  if (!exists("model", envir = .stanr_model_method_state, inherits = FALSE)) {
    model <- stan_model(
      stan_file = test_stan_file("model_methods.stan"),
      quiet = TRUE
    )
    assign("model", model, envir = .stanr_model_method_state)
  }
  get("model", envir = .stanr_model_method_state, inherits = FALSE)
}

.stanr_model_method_fit <- function(double = FALSE) {
  key <- if (double) "fit_double" else "fit"
  if (!exists(key, envir = .stanr_model_method_state, inherits = FALSE)) {
    fit <- .stanr_model_method_model()$optimize(
      data = .stanr_model_method_data(double),
      seed = 2468,
      init = list(theta = 0.5, beta = c(0.5, -0.5)),
      iter = 50,
      refresh = 0,
      show_messages = FALSE,
      show_exceptions = FALSE,
      num_threads = test_threads()
    )
    assign(key, fit, envir = .stanr_model_method_state)
  }
  get(key, envir = .stanr_model_method_state, inherits = FALSE)
}

.stanr_model_method_mcmc <- function() {
  key <- "mcmc"
  if (!exists(key, envir = .stanr_model_method_state, inherits = FALSE)) {
    fit <- .stanr_model_method_model()$sample(
      data = .stanr_model_method_data(),
      seed = 1357,
      init = list(theta = 0.5, beta = c(0.5, -0.5)),
      chains = 2,
      num_threads = 2,
      iter_warmup = 2,
      iter_sampling = 3,
      save_warmup = TRUE,
      adapt_engaged = FALSE,
      step_size = 0.1,
      refresh = 0,
      show_messages = FALSE,
      show_exceptions = FALSE
    )
    assign(key, fit, envir = .stanr_model_method_state)
  }
  get(key, envir = .stanr_model_method_state, inherits = FALSE)
}

.stanr_expected_target <- function(upars, double = FALSE, jacobian = TRUE) {
  theta <- stats::plogis(upars[[1L]])
  beta <- upars[2:3]
  y <- .stanr_model_method_data(double)$y
  value <- sum(y) * log(theta) + (length(y) - sum(y)) * log1p(-theta)
  value <- value - 0.5 * sum(beta^2)
  if (jacobian) {
    value <- value + log(theta) + log1p(-theta)
  }
  value
}

test_that("log probability, gradient, and Hessian match analytical values", {
  fit <- .stanr_model_method_fit()
  upars <- c(0, 0.5, -0.5)

  expected_lp <- -6 * log(2) - 0.25
  expected_gradient <- c(1, -0.5, 0.5)
  expected_hessian <- diag(c(-1.5, -1, -1))

  # Calling a model method must initialize the native pointer lazily.
  expect_equal(fit$log_prob(upars), expected_lp, tolerance = 1e-10)

  gradient <- fit$grad_log_prob(upars)
  expect_equal(as.numeric(gradient), expected_gradient, tolerance = 1e-9)
  expect_equal(attr(gradient, "log_prob"), expected_lp, tolerance = 1e-10)

  hessian <- fit$hessian(upars)
  expect_named(hessian, c("log_prob", "grad_log_prob", "hessian"))
  expect_equal(hessian$log_prob, expected_lp, tolerance = 1e-10)
  expect_equal(
    as.numeric(hessian$grad_log_prob),
    expected_gradient,
    tolerance = 1e-8
  )
  expect_equal(hessian$hessian, expected_hessian, tolerance = 1e-6)
  expect_equal(hessian$hessian, t(hessian$hessian), tolerance = 1e-12)
})

test_that("log_prob() and hessian() honour jacobian = FALSE", {
  skip_if_backend("stanli", "stanli model methods support only jacobian = TRUE")
  fit <- .stanr_model_method_fit()
  upars <- c(0, 0.5, -0.5)

  expected_lp_no_jacobian <- -4 * log(2) - 0.25
  expected_gradient <- c(1, -0.5, 0.5)
  expected_hessian_no_jacobian <- diag(c(-1, -1, -1))

  expect_equal(
    fit$log_prob(upars, jacobian = FALSE),
    expected_lp_no_jacobian,
    tolerance = 1e-10
  )

  hessian_no_jacobian <- fit$hessian(upars, jacobian = FALSE)
  expect_equal(
    hessian_no_jacobian$log_prob,
    expected_lp_no_jacobian,
    tolerance = 1e-10
  )
  expect_equal(
    as.numeric(hessian_no_jacobian$grad_log_prob),
    expected_gradient,
    tolerance = 1e-8
  )
  expect_equal(
    hessian_no_jacobian$hessian,
    expected_hessian_no_jacobian,
    tolerance = 1e-6
  )
})

test_that("constraining and unconstraining preserve structure and values", {
  fit <- .stanr_model_method_fit()
  upars <- c(0, 0.5, -0.5)

  parameters <- fit$constrain_variables(
    upars,
    transformed_parameters = FALSE,
    generated_quantities = FALSE
  )
  expect_named(parameters, c("theta", "beta"))
  expect_equal(parameters$theta, 0.5, tolerance = 1e-12)
  expect_equal(as.numeric(parameters$beta), c(0.5, -0.5), tolerance = 1e-12)
  expect_equal(fit$unconstrain_variables(parameters), upars, tolerance = 1e-10)

  with_tparams <- fit$constrain_variables(
    upars,
    transformed_parameters = TRUE,
    generated_quantities = FALSE
  )
  expect_named(with_tparams, c("theta", "beta", "beta_sum"))
  expect_equal(with_tparams$beta_sum, 0, tolerance = 1e-12)

  with_gqs <- fit$constrain_variables(upars)
  expect_named(
    with_gqs,
    c(
      "theta",
      "beta",
      "beta_sum",
      "deterministic_gq",
      "stochastic_gq"
    )
  )
  expect_equal(with_gqs$deterministic_gq, 0.5, tolerance = 1e-12)

  skeleton <- fit$variable_skeleton()
  expect_named(
    skeleton,
    c(
      "theta",
      "beta",
      "beta_sum",
      "deterministic_gq",
      "stochastic_gq"
    )
  )
  expect_identical(
    vapply(skeleton, length, integer(1)),
    c(
      theta = 1L,
      beta = 2L,
      beta_sum = 1L,
      deterministic_gq = 1L,
      stochastic_gq = 1L
    )
  )
  expect_named(
    fit$variable_skeleton(FALSE, FALSE),
    c("theta", "beta")
  )
})

test_that("model_constrain_matrix matches per-draw constrain_variables()", {
  fit <- .stanr_model_method_fit()
  upars <- rbind(c(0, 0.5, -0.5), c(1, -1, 0.25))

  # include_gqs = FALSE keeps both paths deterministic (no RNG involved).
  fit_private <- fit$.__enclos_env__$private
  constrained <- fit_private$native_call(
    "model_constrain_matrix",
    fit_private$rng_ptr_,
    upars,
    TRUE,
    FALSE
  )
  expect_equal(dim(constrained), c(2L, 4L))
  expect_equal(
    colnames(constrained),
    c("theta", "beta.1", "beta.2", "beta_sum")
  )

  row2 <- fit$constrain_variables(
    upars[2, ],
    transformed_parameters = TRUE,
    generated_quantities = FALSE
  )
  expect_equal(
    unname(constrained[2, ]),
    c(row2$theta, as.numeric(row2$beta), row2$beta_sum),
    tolerance = 1e-12
  )
})

test_that("model-method RNG is fit-local and advances across calls", {
  fit <- .stanr_model_method_fit()
  upars <- c(0, 0.5, -0.5)

  first <- fit$constrain_variables(upars)$stochastic_gq
  second <- fit$constrain_variables(upars)$stochastic_gq
  expect_false(isTRUE(all.equal(first, second)))

  # `upars <- c(0, 0.5, -0.5)` gives beta_sum = 0 regardless of data, so a
  # fit built with the same seed (2468) as `fit` reproduces the same
  # normal_rng(beta_sum, 1) stream from its own first call -- unless RNG
  # state were shared across StanFit instances, in which case a
  # freshly-constructed fit's first draw would instead continue from
  # wherever the shared state was left. Built fresh (not via the memoized
  # `.stanr_model_method_fit()`) because `fit`'s RNG was already advanced
  # by an earlier test in this file, so its own first draw is no longer
  # observable here.
  fresh_fit <- .stanr_model_method_model()$optimize(
    data = .stanr_model_method_data(),
    seed = 2468,
    init = list(theta = 0.5, beta = c(0.5, -0.5)),
    iter = 50,
    refresh = 0,
    show_messages = FALSE,
    show_exceptions = FALSE,
    num_threads = test_threads()
  )
  fresh_first <- fresh_fit$constrain_variables(upars)$stochastic_gq

  fit_double <- .stanr_model_method_fit(double = TRUE)
  fit_double_first <- fit_double$constrain_variables(upars)$stochastic_gq

  expect_equal(fit_double_first, fresh_first)
})

test_that("model methods reject malformed or non-finite inputs", {
  fit <- .stanr_model_method_fit()

  for (method in c("log_prob", "grad_log_prob", "hessian")) {
    expect_error(fit[[method]](c(0, 1)), "3 unconstrained")
    expect_error(fit[[method]](c(0, 1, 2, 3)), "3 unconstrained")
    expect_error(fit[[method]](c(0, NA_real_, 1)), "finite|NA")
    expect_error(fit[[method]](c(0, Inf, 1)), "finite|Inf")
    expect_error(fit[[method]](c(0, 1, 2), jacobian = NA), "jacobian")
  }

  expect_error(fit$constrain_variables(c(0, 1)), "3 unconstrained")
  expect_error(
    fit$constrain_variables(c(0, 1, Inf)),
    "finite|Inf"
  )
  expect_error(
    fit$unconstrain_variables(list(theta = 0.5)),
    "beta|not provided|missing"
  )
  expect_error(
    fit$unconstrain_variables(list(theta = 0.5, beta = 1)),
    "beta|dimension|dims|size"
  )
  expect_error(
    fit$unconstrain_variables(list(theta = 1.5, beta = c(0, 0))),
    "theta|bounds|range"
  )

  expected <- fit$unconstrain_variables(list(theta = 0.5, beta = c(1, -1)))
  with_extra <- fit$unconstrain_variables(list(
    theta = 0.5,
    beta = c(1, -1),
    not_a_parameter = 42
  ))
  expect_equal(with_extra, expected)
})

test_that("fits from one model retain independent data-bound targets", {
  fit <- .stanr_model_method_fit()
  fit_double <- .stanr_model_method_fit(double = TRUE)
  upars <- c(0.4, 0.2, -0.1)

  target <- fit$log_prob(upars)
  target_double <- fit_double$log_prob(upars)
  expect_equal(
    target,
    .stanr_expected_target(upars),
    tolerance = 1e-10
  )
  expect_equal(
    target_double,
    .stanr_expected_target(upars, double = TRUE),
    tolerance = 1e-10
  )
  expect_false(isTRUE(all.equal(target, target_double)))

  # Re-evaluating the first fit detects accidental shared mutable model state.
  expect_equal(fit$log_prob(upars), target, tolerance = 0)
})

test_that("unconstrain_draws preserves posterior dimensions and formats", {
  fit <- .stanr_model_method_mcmc()
  constrained <- posterior::as_draws_matrix(fit$draws(format = "draws_matrix"))
  unconstrained <- fit$unconstrain_draws(format = "draws_matrix")

  expect_s3_class(unconstrained, "draws_matrix")
  expect_identical(nrow(unconstrained), nrow(constrained))
  expect_identical(ncol(unconstrained), 3L)
  expect_identical(
    posterior::variables(unconstrained),
    c("theta", "beta[1]", "beta[2]")
  )
  expect_equal(
    unconstrained[, "theta"],
    stats::qlogis(constrained[, "theta"]),
    tolerance = 1e-8
  )
  expect_equal(
    unconstrained[, c("beta[1]", "beta[2]")],
    constrained[, c("beta[1]", "beta[2]")],
    tolerance = 1e-10
  )

  unconstrained_array <- fit$unconstrain_draws()
  expect_s3_class(unconstrained_array, "draws_array")
  expect_identical(posterior::nchains(unconstrained_array), 2L)
  expect_identical(posterior::niterations(unconstrained_array), 3L)

  with_warmup <- fit$unconstrain_draws(inc_warmup = TRUE)
  expect_identical(posterior::nchains(with_warmup), 2L)
  expect_identical(posterior::niterations(with_warmup), 5L)

  supplied <- fit$unconstrain_draws(
    draws = fit$draws(format = "draws_array"),
    format = "draws_df"
  )
  expect_s3_class(supplied, "draws_df")
  expect_identical(posterior::nchains(supplied), 2L)
  expect_identical(posterior::niterations(supplied), 3L)
})

test_that("serialized fits lazily rebuild data-bound model methods", {
  fit <- .stanr_model_method_fit()
  upars <- c(0.2, -0.3, 0.4)
  expected_lp <- fit$log_prob(upars)
  expected_gradient <- fit$grad_log_prob(upars)
  expected_hessian <- fit$hessian(upars)
  expected_parameters <- fit$constrain_variables(
    upars,
    transformed_parameters = FALSE,
    generated_quantities = FALSE
  )

  path <- tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  saveRDS(fit, path)
  restored <- readRDS(path)

  expect_s3_class(restored, "StanMLE")
  expect_s3_class(restored$draws(), "draws")
  expect_equal(restored$log_prob(upars), expected_lp, tolerance = 1e-10)
  expect_equal(
    restored$grad_log_prob(upars),
    expected_gradient,
    tolerance = 1e-8
  )
  restored_hessian <- restored$hessian(upars)
  expect_equal(restored_hessian$log_prob, expected_hessian$log_prob)
  expect_equal(
    restored_hessian$grad_log_prob,
    expected_hessian$grad_log_prob,
    tolerance = 1e-8
  )
  expect_equal(
    restored_hessian$hessian,
    expected_hessian$hessian,
    tolerance = 1e-6
  )
  expect_equal(
    restored$constrain_variables(
      upars,
      transformed_parameters = FALSE,
      generated_quantities = FALSE
    ),
    expected_parameters,
    tolerance = 1e-10
  )
  expect_equal(
    restored$unconstrain_variables(expected_parameters),
    upars,
    tolerance = 1e-10
  )
})

test_that("model methods on a tuple-data fit still work after readRDS()", {
  skip_if_backend(
    "stanli",
    "stanli does not support tuple or complex data and parameters"
  )
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  # iter_sampling = 1 is below the 3-iteration minimum for E-BFMI, which
  # $sample() would otherwise warn about unprompted; irrelevant here.
  fit <- suppressWarnings(mod$sample(
    data = data,
    iter_warmup = 1,
    iter_sampling = 1,
    chains = 1,
    seed = 18,
    show_messages = FALSE,
    num_threads = test_threads()
  ))
  expect_equal(fit$return_codes(), 0L)

  expected <- fit$constrain_variables(fit$unconstrain_variables(list(x = 0)))

  path <- tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  saveRDS(fit, path)
  restored <- readRDS(path)

  expect_equal(
    restored$constrain_variables(restored$unconstrain_variables(list(x = 0))),
    expected
  )
})

test_that("log_prob still succeeds after mod$compile(force_recompile = TRUE) on a live fit", {
  # Not an isolated pair: the generated C++ is cached by content hash, so this
  # `mod` shares its shared library with the memoized fits used throughout
  # this file, and forcing a recompile here reaches every live fit.
  mod <- stan_model(
    stan_file = test_stan_file("model_methods.stan"),
    quiet = TRUE
  )
  fit <- mod$optimize(
    data = .stanr_model_method_data(),
    seed = 2468,
    init = list(theta = 0.5, beta = c(0.5, -0.5)),
    iter = 50,
    refresh = 0,
    show_messages = FALSE,
    show_exceptions = FALSE,
    num_threads = test_threads()
  )
  upars <- c(0.2, -0.3, 0.4)
  expected_lp <- fit$log_prob(upars)

  fit_private <- fit$.__enclos_env__$private
  superseded_ptr <- fit_private$model_ptr_
  loaded_before <- loaded_dll_paths()

  # Directly recompile the fit's underlying model while the fit is still
  # alive and holding a `model_ptr_` built against the *old* compiled
  # artifact -- this is the "generation changed mid-session" path from
  # `ensure_native()`, distinct from the readRDS/stale-XPtr path exercised
  # above.
  mod$compile(force_recompile = TRUE, quiet = TRUE)

  # The invariant (a forced recompile always builds into a fresh scratch
  # dir -- see `.stanr_build_scratch_dir()`, R/stan_model.R): it may load an
  # additional shared library, but never unloads one.
  expect_identical(
    setdiff(loaded_before, loaded_dll_paths()),
    character()
  )

  expect_equal(fit$log_prob(upars), expected_lp, tolerance = 1e-10)

  # ensure_native() must rebuild the pointer against the new artifact, not
  # keep probing the superseded one.
  expect_false(identical(fit_private$model_ptr_, superseded_ptr))
  expect_identical(fit_private$native_generation_, mod$compile_generation())

  # The original segfault fired only when a later GC ran the superseded
  # pointers' finalizers; reaching the next expectation means both ran clean.
  rm(fit, fit_private, superseded_ptr)
  gc()
  gc()
  expect_true(mod$is_compiled())
})

test_that("ensure_native()'s probe is invoked exactly once across N consecutive native calls", {
  mod <- stan_model(
    stan_file = test_stan_file("model_methods.stan"),
    quiet = TRUE
  )
  fit <- mod$optimize(
    data = .stanr_model_method_data(),
    seed = 2468,
    init = list(theta = 0.5, beta = c(0.5, -0.5)),
    iter = 50,
    refresh = 0,
    show_messages = FALSE,
    show_exceptions = FALSE,
    num_threads = test_threads()
  )
  upars <- c(0.2, -0.3, 0.4)

  # `model_num_upars` is the native probe `ensure_native()` calls to check
  # pointer validity. There's no package-level function to intercept with
  # `local_mocked_bindings()` here (the probe is a closure pulled out of the
  # model's private `compiled_env_`), so instrument the exact call site
  # directly: wrap the real probe in a counting proxy and splice it into the
  # model's private compiled environment, mirroring how `test-pch.R`'s
  # ".stanr_system2 call count" test intercepts a seam to count
  # invocations without relying on timing. This is on a fresh fit/model
  # pair (never native-called before), so the first of the N calls below
  # must take the one-time slow path.
  model_private <- mod$.__enclos_env__$private
  real_probe <- model_private$compiled_env_$model_num_upars
  call_count <- 0
  model_private$compiled_env_$model_num_upars <- function(...) {
    call_count <<- call_count + 1
    real_probe(...)
  }
  withr::defer(model_private$compiled_env_$model_num_upars <- real_probe)

  for (i in 1:5) {
    fit$log_prob(upars)
  }

  expect_equal(call_count, 1)
})

# --- variable_skeleton()/constrain_variables() over tuple- and complex-typed --
# variables, plus regression cover for the bracket-name paths                --
# (unconstrain_draws/optimize/laplace/generate_quantities) that consume the  --
# same fit but don't otherwise touch tuple/complex shapes. Reuses the two    --
# battery models already compiled for test-tuple-data.R                     --
# (test-models/tuple_complex_battery.stan and                               --
# tuple_complex_unbounded.stan) via the shared `test_model()` cache.         --

test_that("constrain_variables(unconstrain_variables(x)) recovers canonical tuple/complex shapes exactly", {
  skip_if_backend(
    "stanli",
    "stanli does not support tuple or complex data and parameters"
  )
  # Unbounded parameters (`tuple(real, vector[2]) t; complex z;`) make
  # unconstrain -> constrain the identity transform, so this is a property
  # test, not just a shape check.
  mod <- test_model("tuple_complex_unbounded")
  # iter_sampling = 1 is below the 3-iteration minimum for E-BFMI, which
  # $sample() would otherwise warn about unprompted; irrelevant here.
  fit <- suppressWarnings(mod$sample(
    data = list(),
    iter_warmup = 1,
    iter_sampling = 1,
    chains = 1,
    seed = 11,
    show_messages = FALSE,
    init = list(t = list(0, c(0, 0)), z = 0 + 0i),
    num_threads = test_threads()
  ))
  expect_equal(fit$return_codes(), 0L)

  x <- list(t = list(1.25, c(-0.5, 3)), z = 2 - 1.5i)
  upars <- fit$unconstrain_variables(x)
  back <- fit$constrain_variables(upars)

  expect_named(back, c("t", "z"))
  expect_equal(back$t[[1]], x$t[[1]], tolerance = 1e-10)
  expect_equal(as.numeric(back$t[[2]]), x$t[[2]], tolerance = 1e-10)
  expect_equal(back$z, x$z, tolerance = 1e-10)
  # Canonical shape: a single-dim leaf is a bare vector, not a 1-d array.
  expect_null(dim(back$t[[2]]))
})

test_that("variable_skeleton() has the exact golden nested-list/array shape for the tuple/complex battery model", {
  skip_if_backend(
    "stanli",
    "stanli does not support tuple or complex data and parameters"
  )
  mod <- test_model("tuple_complex_battery")
  # iter_sampling = 1 is below the 3-iteration minimum for E-BFMI, which
  # $sample() would otherwise warn about unprompted; irrelevant here.
  fit <- suppressWarnings(mod$sample(
    data = battery_data(),
    iter_warmup = 1,
    iter_sampling = 1,
    chains = 1,
    seed = 12,
    show_messages = FALSE,
    num_threads = test_threads()
  ))
  expect_equal(fit$return_codes(), 0L)

  skeleton <- fit$variable_skeleton()

  expected <- list(
    x = NA_real_,
    zd_out = NA_complex_,
    zv_out = rep(NA_complex_, 2),
    zm_out = array(NA_complex_, dim = c(2, 2)),
    za_out = rep(NA_complex_, 2),
    td_out = list(NA_real_, rep(NA_real_, 2)),
    tad_out = list(
      list(NA_real_, NA_complex_),
      list(NA_real_, NA_complex_)
    ),
    acv_out = list(
      list(rep(NA_complex_, 3), NA_real_),
      list(rep(NA_complex_, 3), NA_real_)
    ),
    t2d_out = list(
      list(list(NA_real_, NA_real_), list(NA_real_, NA_real_)),
      list(list(NA_real_, NA_real_), list(NA_real_, NA_real_))
    ),
    nt_out = list(
      NA_real_,
      list(
        list(NA_real_, NA_complex_),
        list(NA_real_, NA_complex_)
      )
    )
  )
  expect_equal(skeleton, expected)

  # `x` is the only `parameter`-stage variable; everything else lives in
  # `generated quantities` (the battery model has no `transformed
  # parameters` block).
  expect_named(fit$variable_skeleton(FALSE, FALSE), "x")
})

test_that("unconstrain_draws() still works on a tuple/complex-model fit (regression: bracket-name path unaffected by tuple/complex support)", {
  skip_if_backend(
    "stanli",
    "stanli does not support tuple or complex data and parameters"
  )
  mod <- test_model("tuple_complex_unbounded")
  # iter_sampling = 2 is below the 3-iteration minimum for E-BFMI, which
  # $sample() would otherwise warn about unprompted; irrelevant here.
  fit <- suppressWarnings(mod$sample(
    data = list(),
    iter_warmup = 2,
    iter_sampling = 2,
    chains = 1,
    seed = 13,
    show_messages = FALSE,
    init = list(t = list(0, c(0, 0)), z = 0 + 0i),
    num_threads = test_threads()
  ))
  expect_equal(fit$return_codes(), 0L)

  unconstrained <- fit$unconstrain_draws(format = "draws_matrix")
  expect_s3_class(unconstrained, "draws_matrix")
  expect_identical(nrow(unconstrained), 2L)

  expected_names <- .stanr_bracket_names(
    fit$.__enclos_env__$private$native_call("model_unconstrained_names")
  )
  expect_identical(posterior::variables(unconstrained), expected_names)
  expect_false(anyNA(unconstrained))
})

test_that("$optimize() and $laplace() succeed on a tuple/complex model (regression: bracket-name paths unaffected by tuple/complex support)", {
  skip_if_backend(
    "stanli",
    "stanli does not support tuple or complex data and parameters"
  )
  mod <- test_model("tuple_complex_unbounded")
  init <- list(t = list(0, c(0, 0)), z = 0 + 0i)

  fit_opt <- mod$optimize(
    data = list(),
    seed = 14,
    init = init,
    refresh = 0,
    show_messages = FALSE,
    show_exceptions = FALSE,
    num_threads = test_threads()
  )
  expect_s3_class(fit_opt, "StanMLE")
  # Every parameter has a std_normal() prior and no data -- the MLE is 0 for
  # every unconstrained (and, since these parameters are unbounded, every
  # constrained) dimension.
  expect_equal(unname(fit_opt$mle()), rep(0, 5), tolerance = 1e-4)
  # Bracket names derived from the native call, never hand-typed (house
  # rule: `.stanr_bracket_names` is load-bearing and unaffected by
  # tuple/complex support).
  expected_names <- .stanr_bracket_names(
    fit_opt$.__enclos_env__$private$native_call(
      "model_constrained_names",
      FALSE,
      FALSE
    )
  )
  expect_named(fit_opt$mle(), expected_names)

  fit_lap <- mod$laplace(
    data = list(),
    seed = 15,
    mode = fit_opt,
    draws = 50,
    refresh = 0,
    show_messages = FALSE,
    show_exceptions = FALSE,
    num_threads = test_threads()
  )
  expect_s3_class(fit_lap, "StanLaplace")
  draws <- fit_lap$draws(format = "draws_matrix")
  expect_identical(nrow(draws), 50L)
  expect_true(all(expected_names %in% posterior::variables(draws)))
})

test_that("generate_quantities() runs on draws from a tuple/complex-model $sample() fit", {
  skip_if_backend(
    "stanli",
    "stanli does not support tuple or complex data and parameters"
  )
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  # iter_sampling = 2 is below the 3-iteration minimum for E-BFMI, which
  # $sample() would otherwise warn about unprompted; irrelevant here.
  fit <- suppressWarnings(mod$sample(
    data = data,
    iter_warmup = 1,
    iter_sampling = 2,
    chains = 1,
    seed = 16,
    show_messages = FALSE,
    num_threads = test_threads()
  ))
  expect_equal(fit$return_codes(), 0L)

  gq <- mod$generate_quantities(
    fitted_params = fit,
    data = data,
    seed = 17,
    show_messages = FALSE,
    num_threads = test_threads()
  )
  expect_s3_class(gq, "StanGQ")

  gq_draws <- as.data.frame(gq$draws(format = "draws_df"))
  fit_draws <- as.data.frame(fit$draws(format = "draws_df"))
  # Derive the "*_out" column names from the actual draws (never hand-typed
  # bracket strings, per house rule -- `.stanr_bracket_names` is
  # load-bearing and unaffected by tuple/complex support). The echoed
  # "*_out" quantities
  # are pure functions of `data` (not of the sampled parameter `x`), so
  # re-running generated quantities from the fit's own draws must reproduce
  # the fit's own values exactly, for every echoed column.
  out_names <- posterior::variables(gq$draws(format = "draws_df"))
  out_names <- out_names[
    grepl("^(zd|zv|zm|za|td|tad|acv|t2d|nt)_out", out_names)
  ]
  expect_true(length(out_names) > 0)
  expect_setequal(
    out_names,
    colnames(fit_draws)[
      grepl("^(zd|zv|zm|za|td|tad|acv|t2d|nt)_out", colnames(fit_draws))
    ]
  )
  expect_equal(
    unname(vapply(
      out_names,
      function(nm) as.numeric(gq_draws[1, nm]),
      numeric(1)
    )),
    unname(vapply(
      out_names,
      function(nm) as.numeric(fit_draws[1, nm]),
      numeric(1)
    ))
  )
})

test_that("constrain_variables() reconstructs array-of-tuple/2D-tuple-array/complex-in-tuple-array values exactly (not just shape)", {
  skip_if_backend(
    "stanli",
    "stanli does not support tuple or complex data and parameters"
  )
  # The golden-shape test above exercises the native skeleton builder (no
  # values); this one exercises the element-major native reconstruction on
  # every array-of-tuple shape the battery model declares (`acv_out`:
  # complex inside a 1-D tuple array; `t2d_out`: a 2-D tuple array;
  # `tad_out`: a simple 1-D tuple array; `nt_out`: a tuple nesting a 1-D
  # tuple array), checking reconstructed *values* land at the exact right
  # position, not just that the shape matches.
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  # iter_sampling = 1 is below the 3-iteration minimum for E-BFMI, which
  # $sample() would otherwise warn about unprompted; irrelevant here.
  fit <- suppressWarnings(mod$sample(
    data = data,
    iter_warmup = 1,
    iter_sampling = 1,
    chains = 1,
    seed = 18,
    show_messages = FALSE,
    num_threads = test_threads()
  ))
  expect_equal(fit$return_codes(), 0L)

  # The generated quantities are pure echoes of `data`, independent of `x`;
  # any valid unconstrained `x` reproduces them exactly via constrain_variables().
  upars <- fit$unconstrain_variables(list(x = 0))
  back <- fit$constrain_variables(upars)

  expect_equal(back$acv_out, data$acv)
  expect_equal(back$t2d_out, data$t2d)
  expect_equal(back$tad_out, data$tad)
  expect_equal(back$nt_out, data$nt)
  expect_equal(back$td_out, data$td)
  expect_equal(back$zd_out, data$zd)
  expect_equal(back$zv_out, data$zv)
  expect_equal(back$zm_out, data$zm)
  expect_equal(back$za_out, data$za)
})

withr::deferred_run()
