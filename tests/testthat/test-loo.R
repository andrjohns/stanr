local_test_context()

init_test_cache("loo")

# Setup ------------------------------------------------------------------

skip_if_not_installed("loo")

bernoulli_log_lik_file <- testthat::test_path(
  "test-models",
  "bernoulli_log_lik.stan"
)
bernoulli_log_lik_code <- paste(
  readLines(bernoulli_log_lik_file),
  collapse = "\n"
)
bernoulli_log_lik_mod <- stan_model(
  code = bernoulli_log_lik_code,
  compile = TRUE
)

loo_bernoulli_data <- list(N = 10, y = c(0, 1, 1, 0, 1, 0, 1, 1, 1, 0))

bernoulli_fit <- bernoulli_log_lik_mod$sample(
  data = loo_bernoulli_data,
  chains = 1,
  num_threads = 1,
  iter_sampling = 100,
  iter_warmup = 50,
  seed = 1234
)

# Basic LOO tests --------------------------------------------------------

test_that("$loo() returns a loo object", {
  loo_result <- suppressWarnings(bernoulli_fit$loo())
  expect_s3_class(loo_result, "loo")
})

test_that("$loo() accepts r_eff = FALSE (default)", {
  loo_result <- suppressWarnings(bernoulli_fit$loo(r_eff = FALSE))
  expect_s3_class(loo_result, "loo")
})

test_that("$loo() accepts r_eff = TRUE", {
  loo_result <- suppressWarnings(bernoulli_fit$loo(r_eff = TRUE))
  expect_s3_class(loo_result, "loo")
})

test_that("$loo() accepts moment_match = TRUE", {
  loo_result <- suppressWarnings(bernoulli_fit$loo(moment_match = TRUE))
  expect_s3_class(loo_result, "loo")
})

test_that("$loo() rejects a non-logical moment_match", {
  expect_error(
    bernoulli_fit$loo(moment_match = "yes"),
    "`moment_match` must be TRUE or FALSE.",
    fixed = TRUE
  )
})

test_that("$loo() passes extra arguments to loo.array()", {
  loo_result <- suppressWarnings(bernoulli_fit$loo(cores = 1, save_psis = TRUE))
  expect_s3_class(loo_result, "loo")
  expect_true("psis_object" %in% names(loo_result))
})

# Variable name tests ----------------------------------------------------

test_that("$loo() uses custom variable name via 'variables'", {
  loo_result <- suppressWarnings(bernoulli_fit$loo(variables = "log_lik"))
  expect_s3_class(loo_result, "loo")
})

test_that("$loo() errors on multiple variable names", {
  expect_error(
    bernoulli_fit$loo(variables = c("log_lik", "theta")),
    "Only a single variable name is allowed"
  )
})

# Missing log_lik tests --------------------------------------------------

test_that("$loo() errors when log_lik is not in draws", {
  bernoulli_no_ll_mod <- stan_model(
    code = paste(
      readLines(testthat::test_path("test-models", "bernoulli.stan")),
      collapse = "\n"
    ),
    compile = TRUE
  )
  bernoulli_no_ll_fit <- bernoulli_no_ll_mod$sample(
    data = loo_bernoulli_data,
    chains = 1,
    num_threads = 1,
    iter_sampling = 50,
    iter_warmup = 25,
    seed = 1234
  )
  expect_error(bernoulli_no_ll_fit$loo(), "log_lik")
})

# Moment-matching tests --------------------------------------------------

# A Poisson regression whose posterior has heavy-tailed importance ratios for a
# number of observations, so plain PSIS-LOO reports too-high Pareto k values
# that moment-matching is able to correct. Built lazily and reused by the
# moment-matching tests below.
moment_match_fit <- local({
  fit <- NULL
  function() {
    if (is.null(fit)) {
      data_json <- readLines(
        testthat::test_path("test-models", "loo_moment_match.data.json")
      )
      fit <<- test_model("loo_moment_match")$sample(
        data = QuickJSR::from_json(paste(data_json, collapse = "\n")),
        chains = 1,
        num_threads = 1,
        seed = 1000,
        refresh = 0,
        show_messages = FALSE
      )
    }
    fit
  }
})

test_that("$loo() reports too-high Pareto k values without moment-matching", {
  expect_warning(
    loo_result <- moment_match_fit()$loo(),
    "Some Pareto k diagnostic values are too high.",
    fixed = TRUE
  )
  expect_gt(max(loo_result$diagnostics$pareto_k), 0.7)
})

test_that("$loo(moment_match = TRUE) fixes the too-high Pareto k values", {
  fit <- moment_match_fit()
  uncorrected <- suppressWarnings(fit$loo())

  # In loo < 2.7.0 the warning is downgraded to "slightly high" rather than
  # dropped, since that version still had the intermediate 0.5 threshold.
  if (utils::packageVersion("loo") < "2.7.0") {
    expect_warning(
      loo_result <- fit$loo(moment_match = TRUE),
      "Some Pareto k diagnostic values are slightly high.",
      fixed = TRUE
    )
  } else {
    expect_no_warning(loo_result <- fit$loo(moment_match = TRUE))
  }

  expect_s3_class(loo_result, "loo")
  expect_lt(max(loo_result$diagnostics$pareto_k), 0.7)
  expect_lt(
    max(loo_result$diagnostics$pareto_k),
    max(uncorrected$diagnostics$pareto_k)
  )
})

test_that("$loo(moment_match = TRUE) preserves the pointwise structure", {
  fit <- moment_match_fit()
  uncorrected <- suppressWarnings(fit$loo())
  loo_result <- suppressWarnings(fit$loo(moment_match = TRUE))

  expect_equal(dim(loo_result$pointwise), dim(uncorrected$pointwise))
  expect_equal(
    nrow(loo_result$pointwise),
    posterior::nvariables(fit$draws("log_lik", format = "draws_array"))
  )
  expect_true(all(is.finite(loo_result$pointwise[, "elpd_loo"])))
  expect_true(is.finite(loo_result$estimates["elpd_loo", "Estimate"]))
})

test_that("$loo(moment_match = TRUE) passes extra arguments through", {
  fit <- moment_match_fit()

  # A lower `k_threshold` corrects more observations, so all of the remaining
  # Pareto k values end up below it.
  expect_no_warning(
    loo_result <- fit$loo(moment_match = TRUE, k_threshold = 0.4)
  )
  expect_lt(max(loo_result$diagnostics$pareto_k), 0.4)

  # `split = FALSE` uses the (less accurate) unsplit transformation, which loo
  # warns about, but it must still reach the moment-matching code path.
  unsplit <- suppressWarnings(fit$loo(moment_match = TRUE, split = FALSE))
  expect_s3_class(unsplit, "loo")
  expect_lt(max(unsplit$diagnostics$pareto_k), 0.7)
})

test_that("$loo() combines moment_match = TRUE with r_eff = TRUE", {
  loo_result <- suppressWarnings(
    moment_match_fit()$loo(r_eff = TRUE, moment_match = TRUE)
  )
  expect_s3_class(loo_result, "loo")
  expect_lt(max(loo_result$diagnostics$pareto_k), 0.7)
})

withr::deferred_run()
