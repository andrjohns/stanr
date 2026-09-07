local_test_context()

init_test_cache("data-validation")

test_that("numeric values are accepted for Stan integer data only when integral", {
  mod <- test_model("bernoulli")

  result <- mod$sample(
    data = list(N = 4, y = c(1, 0, 1, 0)),
    iter_warmup = 5,
    iter_sampling = 5,
    chains = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result$return_codes(), 0L)

  expect_error(
    mod$sample(
      data = list(N = 4, y = c(1, 0.5, 1, 0)),
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "int variable contained non-int values"
  )
})

test_that("NA integer data is rejected before Stan services run", {
  mod <- test_model("bernoulli")

  expect_error(
    mod$sample(
      data = list(N = 4L, y = c(1L, NA_integer_, 1L, 0L)),
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    # The compiled backend's R-side check and stanli's data conversion each
    # word this differently, but both reject it before any service runs.
    "Integer variable 'y' contains NA|stanli data cannot contain NA: y"
  )
})

test_that("NA/NaN real data is rejected before Stan services run", {
  mod <- test_model("model_methods")
  data <- list(N = 4L, y = c(1L, 0L, 1L, 0L), mu = NA_real_)

  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "Real variable 'mu' contains NA or NaN|stanli data cannot contain NA, NaN, or Inf: mu"
  )

  data$mu <- NaN
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "Real variable 'mu' contains NA or NaN|stanli data cannot contain NA, NaN, or Inf: mu"
  )
})

test_that("invalid sampling counts throw a configuration error", {
  mod <- test_model("bernoulli")
  data <- list(N = 4L, y = c(1L, 0L, 1L, 0L))

  expect_error(
    mod$sample(
      data = data,
      chains = 0,
      iter_warmup = 5,
      iter_sampling = 5,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "`chains` must be a single integer"
  )
  expect_error(
    mod$sample(
      data = data,
      thin = 0,
      iter_warmup = 5,
      iter_sampling = 5,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "`thin` must be a single integer >= 1"
  )
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = .Machine$integer.max,
      iter_sampling = .Machine$integer.max,
      save_warmup = TRUE,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "Requested number of saved draws is too large"
  )
})

test_that("numeric initialization radius is accepted", {
  mod <- test_model("bernoulli")
  expect_silent(
    mod$sample(
      data = list(N = 4L, y = c(1L, 0L, 1L, 0L)),
      init = c(0.1),
      iter_warmup = 2,
      iter_sampling = 3,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    )
  )
})

test_that("a numeric data.frame is accepted in place of matrix data", {
  mod <- test_model("hierarchical_logistic")
  x_values <- rbind(
    c(1, -1),
    c(1, -0.5),
    c(1, 0),
    c(1, 0.5),
    c(1, 1),
    c(1, -0.8),
    c(1, 0.3),
    c(1, 0.8)
  )
  base_data <- list(
    N = 8,
    K = 2,
    G = 2,
    group = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    y = c(0L, 0L, 1L, 1L, 1L, 0L, 1L, 1L)
  )

  result_matrix <- mod$sample(
    data = c(base_data, list(X = x_values)),
    iter_warmup = 5,
    iter_sampling = 5,
    chains = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )
  result_frame <- mod$sample(
    data = c(base_data, list(X = as.data.frame(x_values))),
    iter_warmup = 5,
    iter_sampling = 5,
    chains = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  )

  expect_equal(result_frame$return_codes(), 0L)
  expect_equal(
    as.numeric(result_frame$draws()),
    as.numeric(result_matrix$draws())
  )
})

test_that("a data.frame with a non-numeric column is rejected", {
  mod <- test_model("hierarchical_logistic")
  data <- list(
    N = 2L,
    K = 2L,
    G = 1L,
    X = data.frame(V1 = c(1, 1), V2 = c("a", "b")),
    group = c(1L, 1L),
    y = c(0L, 1L)
  )

  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 1,
      seed = 42,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "numeric"
  )
})

withr::deferred_run()
