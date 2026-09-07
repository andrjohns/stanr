local_test_context()

init_test_cache("optimizing")

test_that("optimizing returns expected structure", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$optimize(
    data = data,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_s3_class(result, "StanMLE")
  expect_s3_class(result, "StanFit")
  expect_type(result$mle(), "double")
  expect_named(result$summary(), c("variable", "estimate"))
  expect_equal(result$return_codes(), 0L)
  expect_false("converged__" %in% names(result$mle()))
  expect_false("converged__" %in% result$summary()$variable)
  expect_false("converged__" %in% posterior::variables(result$draws()))
})

test_that("optimizing with lbfgs algorithm works", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$optimize(
    data = data,
    algorithm = "lbfgs",
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
})

test_that("optimizing with an invalid algorithm errors before reaching C++", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  expect_error(
    mod$optimize(
      data = data,
      algorithm = "typo",
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "`algorithm` must be one of"
  )
})

test_that("optimizing finds reasonable theta for bernoulli", {
  mod <- test_model("bernoulli")
  # 5 successes out of 10 -> MLE theta = 0.5
  data <- bernoulli_data

  result <- mod$optimize(
    data = data,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_true(result$mle("theta") > 0.3)
  expect_true(result$mle("theta") < 0.7)
})

test_that("optimizing output() returns non-empty Stan log messages", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$optimize(
    data = data,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_type(result$output(), "character")
  expect_true(length(result$output()) > 0)
})

test_that("optimizing with jacobian = TRUE vs FALSE gives different mle() for constrained parameters", {
  skip_if_backend(
    "stanli",
    "stanli's density always includes the Jacobian, so the services ignore ",
    "jacobian = FALSE"
  )
  mod <- test_model("sigma_normal")
  data <- list(N = 5, y = c(0.8, -1.2, 0.5, 1.7, -0.3))

  result_no_jacobian <- mod$optimize(
    data = data,
    seed = 42,
    jacobian = FALSE,
    show_messages = FALSE,
    num_threads = test_threads()
  )
  result_jacobian <- mod$optimize(
    data = data,
    seed = 42,
    jacobian = TRUE,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result_no_jacobian$return_codes(), 0L)
  expect_equal(result_jacobian$return_codes(), 0L)
  expect_false(isTRUE(all.equal(
    result_no_jacobian$mle(),
    result_jacobian$mle()
  )))
})

test_that("optimizing with save_iterations = TRUE exposes the full optimization path via draws()", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$optimize(
    data = data,
    seed = 42,
    save_iterations = TRUE,
    show_messages = FALSE,
    num_threads = test_threads()
  )
  result_default <- mod$optimize(
    data = data,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)
  draws <- unclass(as.matrix(result$draws()))
  expect_true(nrow(draws) > 1)
  expect_false("converged__" %in% posterior::variables(result$draws()))

  last_row <- draws[nrow(draws), ]
  expect_equal(
    unname(last_row[names(result$mle())]),
    unname(result$mle())
  )
  # Saving iterations should not change the converged estimate: the last row
  # of the full path should match the single-row output of a default run
  # with the same seed/data.
  default_row <- unclass(as.matrix(result_default$draws()))[1L, ]
  expect_equal(unname(last_row[names(default_row)]), unname(default_row))
})

test_that("optimizing default save_iterations yields a single row in draws()", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$optimize(
    data = data,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  draws <- unclass(as.matrix(result$draws()))
  expect_equal(nrow(draws), 1L)
})

test_that("draws(variables = ...) works for both default and save_iterations = TRUE optimize() fits", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  default_fit <- mod$optimize(
    data = data,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )
  expect_equal(
    posterior::variables(default_fit$draws(variables = "lp__")),
    "lp__"
  )

  full_path_fit <- mod$optimize(
    data = data,
    seed = 42,
    save_iterations = TRUE,
    show_messages = FALSE,
    num_threads = test_threads()
  )
  expect_equal(
    posterior::variables(full_path_fit$draws(variables = "lp__")),
    "lp__"
  )
})

test_that("mle() errors on an unknown variable name", {
  mod <- test_model("bernoulli")
  data <- bernoulli_data

  result <- mod$optimize(
    data = data,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_error(result$mle(variables = "theta_typo"), "Unknown variable")
})

withr::deferred_run()
