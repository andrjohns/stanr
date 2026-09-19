local_test_context()

init_test_cache("laplace")

test_that("laplace evaluates model-side gradients in the generated model library", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$laplace(
    data = data,
    mode = c(theta = 0.5),
    draws = 2,
    calculate_lp = TRUE,
    refresh = 0,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_s3_class(result, "StanLaplace")
  expect_s3_class(result$summary(), "draws_summary")
  expect_equal(posterior::ndraws(result$draws()), 2L)
  expect_equal(result$metadata()$arguments$num_draws, 2)
})

test_that("laplace lp() and lp_approx() return numeric vectors", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$laplace(
    data = data,
    mode = c(theta = 0.5),
    draws = 10,
    calculate_lp = TRUE,
    refresh = 0,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_true(is.numeric(result$lp()))
  expect_length(result$lp(), 10L)
  expect_true(is.numeric(result$lp_approx()))
  expect_length(result$lp_approx(), 10L)
})

test_that("laplace with mode = NULL works for models with vector/array parameters", {
  path <- test_path("test-models/model_methods.stan")
  mod <- stan_model(stan_file = path, quiet = TRUE)
  data <- list(N = 4, y = c(1L, 1L, 1L, 0L), mu = 0)

  result <- mod$laplace(
    data = data,
    draws = 5,
    refresh = 0,
    seed = 42,
    jacobian = FALSE,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_s3_class(result, "StanLaplace")
  draws <- result$draws()
  expect_true(all(
    c("theta", "beta[1]", "beta[2]") %in% posterior::variables(draws)
  ))
  expect_equal(posterior::ndraws(draws), 5L)
})

test_that("laplace with mode = StanMLE works for models with vector parameters", {
  path <- test_path("test-models/model_methods.stan")
  mod <- stan_model(stan_file = path, quiet = TRUE)
  data <- list(N = 4, y = c(1L, 1L, 1L, 0L), mu = 0)

  opt_fit <- mod$optimize(
    data = data,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  result <- mod$laplace(
    data = data,
    mode = opt_fit,
    draws = 5,
    refresh = 0,
    seed = 42,
    jacobian = FALSE,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_s3_class(result, "StanLaplace")
  expect_true(all(
    c("theta", "beta[1]", "beta[2]") %in% posterior::variables(result$draws())
  ))
})

test_that("laplace with a raw numeric mode vector uses bracket-format names for vector parameters", {
  path <- test_path("test-models/model_methods.stan")
  mod <- stan_model(stan_file = path, quiet = TRUE)
  data <- list(N = 4, y = c(1L, 1L, 1L, 0L), mu = 0)

  opt_fit <- mod$optimize(
    data = data,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )
  mode_vec <- opt_fit$mle()
  expect_true(all(c("theta", "beta[1]", "beta[2]") %in% names(mode_vec)))

  result <- mod$laplace(
    data = data,
    mode = mode_vec,
    draws = 5,
    refresh = 0,
    seed = 42,
    jacobian = FALSE,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_s3_class(result, "StanLaplace")
  expect_true(all(
    c("theta", "beta[1]", "beta[2]") %in% posterior::variables(result$draws())
  ))
})

test_that("laplace with no mode and default jacobian succeeds", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$laplace(
    data = data,
    draws = 5,
    refresh = 0,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_s3_class(result, "StanLaplace")
  expect_equal(posterior::ndraws(result$draws()), 5L)
})

test_that("laplace with jacobian = TRUE and mode = StanMLE fitted with jacobian = TRUE works", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  opt_fit <- mod$optimize(
    data = data,
    seed = 42,
    jacobian = TRUE,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  result <- mod$laplace(
    data = data,
    mode = opt_fit,
    draws = 5,
    refresh = 0,
    seed = 42,
    jacobian = TRUE,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  expect_s3_class(result, "StanLaplace")
  expect_equal(posterior::ndraws(result$draws()), 5L)
})

test_that("laplace rejects mode and opt_args together", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_error(
    mod$laplace(
      data = data,
      mode = c(theta = 0.5),
      opt_args = list(iter = 100L),
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "`mode` and `opt_args` cannot both be supplied"
  )
})

test_that("laplace rejects opt_args that override reserved arguments", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_error(
    mod$laplace(
      data = data,
      opt_args = list(jacobian = TRUE),
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "`opt_args` cannot override"
  )
})

withr::deferred_run()
