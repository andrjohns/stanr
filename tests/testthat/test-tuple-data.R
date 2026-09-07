local_test_context()

init_test_cache("tuple-data")

# Every test here feeds tuple/complex data or parameters through a model.
skip_if_backend(
  "stanli",
  "stanli does not support tuple or complex data and parameters"
)

# Coverage for the tuple/complex data & init interop: the native flattening
# and windowed-complex storage in `src/r_data_context.cpp`, and its wiring
# into `.stanr_run_service()` / `fit$unconstrain_variables()`.

# --- Codegen-contract battery: compiled model, exact-equality draw echo -----
#
# Pins the stanc var_context reading contracts against upgrades. If this
# section fails after a stanc upgrade, suspect blocked-AoS tuple array
# flattening or windowed vals_c for tuple-slot complex leaves codegen drift
# first, not a Stan-library API change.

# Builds the expected (native dotted/colon name -> real-valued scalar) table
# for every leaf the battery model echoes. Names are the *native* flat
# constrained-parameter form -- never the bracketed form -- so the actual
# bracket strings used to index the draws always come from calling
# `.stanr_bracket_names()` itself (below), not from a hand-typed guess.
.stanr_battery_expected <- function(data) {
  complex_entry <- function(prefix, value) {
    stats::setNames(
      c(Re(value), Im(value)),
      c(paste0(prefix, ".real"), paste0(prefix, ".imag"))
    )
  }

  out <- c(
    complex_entry("zd_out", data$zd),
    complex_entry("zv_out.1", data$zv[1]),
    complex_entry("zv_out.2", data$zv[2]),
    complex_entry("za_out.1", data$za[1]),
    complex_entry("za_out.2", data$za[2])
  )
  for (j in 1:2) {
    for (i in 1:2) {
      out <- c(out, complex_entry(paste0("zm_out.", i, ".", j), data$zm[i, j]))
    }
  }
  out <- c(
    out,
    stats::setNames(data$td[[1]], "td_out:1"),
    stats::setNames(data$td[[2]][1], "td_out:2.1"),
    stats::setNames(data$td[[2]][2], "td_out:2.2")
  )
  for (e in 1:2) {
    out <- c(
      out,
      stats::setNames(data$tad[[e]][[1]], paste0("tad_out.", e, ":1")),
      complex_entry(paste0("tad_out.", e, ":2"), data$tad[[e]][[2]])
    )
  }
  for (e in 1:2) {
    cv <- data$acv[[e]][[1]]
    for (k in 1:3) {
      out <- c(out, complex_entry(paste0("acv_out.", e, ":1.", k), cv[k]))
    }
    out <- c(
      out,
      stats::setNames(data$acv[[e]][[2]], paste0("acv_out.", e, ":2"))
    )
  }
  for (i in 1:2) {
    for (j in 1:2) {
      out <- c(
        out,
        stats::setNames(
          data$t2d[[i]][[j]][[1]],
          paste0("t2d_out.", i, ".", j, ":1")
        ),
        stats::setNames(
          data$t2d[[i]][[j]][[2]],
          paste0("t2d_out.", i, ".", j, ":2")
        )
      )
    }
  }
  out <- c(out, stats::setNames(data$nt[[1]], "nt_out:1"))
  for (e in 1:2) {
    out <- c(
      out,
      stats::setNames(data$nt[[2]][[e]][[1]], paste0("nt_out:2.", e, ":1")),
      complex_entry(paste0("nt_out:2.", e, ":2"), data$nt[[2]][[e]][[2]])
    )
  }
  out
}

test_that("the battery model echoes every input exactly via generated quantities", {
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()

  # iter_sampling = 2 is below the 3-iteration minimum for E-BFMI, which
  # $sample() would otherwise warn about unprompted; irrelevant here.
  fit <- suppressWarnings(mod$sample(
    data = data,
    iter_warmup = 2,
    iter_sampling = 2,
    chains = 1,
    seed = 42,
    show_messages = FALSE,
    num_threads = test_threads()
  ))
  expect_equal(fit$return_codes(), 0L)

  expected <- .stanr_battery_expected(data)
  # The expected keys are native dotted/colon names; the actual lookup keys
  # come from calling the real `.stanr_bracket_names()` on them, not from
  # a hand-typed bracket string.
  bracket_keys <- .stanr_bracket_names(names(expected))

  draws <- fit$draws(format = "draws_df")
  actual_names <- posterior::variables(draws)
  missing <- setdiff(bracket_keys, actual_names)
  expect_equal(missing, character())

  # Plain data.frame subsetting avoids `draws_df`'s metadata-preserving `[`
  # (which drops/warns when required columns like `.chain` aren't selected).
  draws_plain <- as.data.frame(draws)
  first_draw <- as.list(draws_plain[1, bracket_keys])
  expect_equal(
    unname(vapply(first_draw, as.numeric, numeric(1))),
    unname(expected)
  )
  # Every draw is identical: the echoed generated quantities don't depend on
  # the sampled parameter `x`.
  last_draw <- as.list(draws_plain[nrow(draws_plain), bracket_keys])
  expect_equal(
    unname(vapply(last_draw, as.numeric, numeric(1))),
    unname(expected)
  )

  # No stray/uncovered "*_out" columns: the expected table accounts for
  # every leaf the battery model declares.
  out_names <- actual_names[
    grepl("^(zd|zv|zm|za|td|tad|acv|t2d|nt)_out", actual_names)
  ]
  expect_equal(length(out_names), length(expected))
})

test_that("a list for a variable not declared as a tuple errors (compiled model)", {
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  data$zd <- list(1, 2)
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 1,
      iter_sampling = 1,
      chains = 1,
      seed = 1,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "not declared as a tuple"
  )
})

test_that("wrong tuple arity errors (compiled model)", {
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  data$td <- list(1.5, c(2.5, 3.5), 99)
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 1,
      iter_sampling = 1,
      chains = 1,
      seed = 1,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "length 2"
  )
})

test_that("a named tuple list errors (compiled model)", {
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  data$td <- list(a = 1.5, b = c(2.5, 3.5))
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 1,
      iter_sampling = 1,
      chains = 1,
      seed = 1,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "unnamed list"
  )
})

test_that("a dotted-name collision errors (compiled model)", {
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  data[["td.1"]] <- 42
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 1,
      iter_sampling = 1,
      chains = 1,
      seed = 1,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "collide|already exist"
  )
})

test_that("a non-rectangular tuple array errors (compiled model)", {
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  data$t2d <- list(
    list(list(11L, 1.1), list(12L, 1.2)),
    list(list(21L, 2.1))
  )
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 1,
      iter_sampling = 1,
      chains = 1,
      seed = 1,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "not a rectangular tuple array"
  )
})

test_that("a shape mismatch across enclosing array elements errors (compiled model)", {
  mod <- test_model("tuple_complex_battery")
  data <- battery_data()
  data$acv <- list(
    list(complex(real = 1:3, imaginary = 11:13), 100),
    list(complex(real = 4:5, imaginary = 14:15), 200)
  )
  expect_error(
    mod$sample(
      data = data,
      iter_warmup = 1,
      iter_sampling = 1,
      chains = 1,
      seed = 1,
      show_messages = FALSE,
      num_threads = test_threads()
    ),
    "inconsistent value shapes"
  )
})

# --- Init / unconstrain round trips for tuple and complex parameters --------

test_that("unconstrain_variables() is the identity for unbounded tuple/complex parameters", {
  mod <- test_model("tuple_complex_unbounded")
  # iter_sampling = 2 is below the 3-iteration minimum for E-BFMI, which
  # $sample() would otherwise warn about unprompted; irrelevant here.
  fit <- suppressWarnings(mod$sample(
    data = list(),
    iter_warmup = 2,
    iter_sampling = 2,
    chains = 1,
    seed = 1,
    show_messages = FALSE,
    init = list(t = list(0.5, c(1, 2)), z = 0.1 + 0.2i),
    num_threads = test_threads()
  ))
  expect_equal(fit$return_codes(), 0L)

  upars <- fit$unconstrain_variables(list(
    t = list(0.5, c(1, 2)),
    z = 0.1 + 0.2i
  ))
  expect_equal(as.numeric(upars), c(0.5, 1, 2, 0.1, 0.2))
})

test_that("sample() accepts a tuple/complex init list end to end", {
  mod <- test_model("tuple_complex_unbounded")
  # iter_sampling = 1 is below the 3-iteration minimum for E-BFMI, which
  # $sample() would otherwise warn about unprompted; irrelevant here.
  fit <- suppressWarnings(mod$sample(
    data = list(),
    iter_warmup = 1,
    iter_sampling = 1,
    chains = 1,
    seed = 7,
    show_messages = FALSE,
    init = list(t = list(0, c(0, 0)), z = 0 + 0i),
    num_threads = test_threads()
  ))
  expect_equal(fit$return_codes(), 0L)
})

withr::deferred_run()
