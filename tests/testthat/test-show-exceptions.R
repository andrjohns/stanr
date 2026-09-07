local_test_context()

init_test_cache("show-exceptions")

# Helper: cached model instance to avoid recompilation across test_that()
# blocks. The model rejects any draw with |x| > 0.5, which reliably triggers
# both known "exception chatter" sources during a short run with a fixed
# seed: Metropolis proposal rejections (error-level, stderr) during
# sampling, and initial-value rejections (warn-level, stdout) during
# initialization.
.stanr_show_exceptions_cache <- new.env(parent = emptyenv())

get_rejecting_model <- function() {
  if (!exists("mod", envir = .stanr_show_exceptions_cache)) {
    .stanr_show_exceptions_cache$mod <- stan_model(
      code = "
      parameters { real x; }
      model {
        x ~ normal(0, 1);
        if (abs(x) > 0.5) reject(\"test rejection\");
      }
    "
    )
  }
  .stanr_show_exceptions_cache$mod
}

# Runs `expr`, capturing Rprintf (stdout) and REprintf (stderr) output
# separately while preserving the return value. Both r_logger streams route
# through R's normal output/message connections, so a dual sink captures
# them without altering behavior.
run_captured <- function(expr) {
  stdout_lines <- character()
  stderr_lines <- character()
  con_out <- textConnection("stdout_lines", open = "w", local = TRUE)
  con_err <- textConnection("stderr_lines", open = "w", local = TRUE)
  sink(con_out, type = "output")
  sink(con_err, type = "message")
  on.exit(
    {
      sink(type = "message")
      sink(type = "output")
      close(con_out)
      close(con_err)
    },
    add = TRUE
  )
  result <- expr
  list(result = result, stdout = stdout_lines, stderr = stderr_lines)
}

test_that("show_exceptions = TRUE (default) prints Metropolis rejection chatter to stderr", {
  mod <- get_rejecting_model()

  cap <- run_captured(
    mod$sample(
      data = list(),
      iter_warmup = 30,
      iter_sampling = 20,
      chains = 1,
      seed = 42,
      refresh = 1,
      show_messages = TRUE,
      show_exceptions = TRUE,
      num_threads = test_threads()
    )
  )

  expect_match(paste(cap$stderr, collapse = "\n"), "Informational Message")
})

test_that("show_exceptions = FALSE silences exception chatter on both streams but keeps it in output()", {
  mod <- get_rejecting_model()

  cap <- run_captured(
    mod$sample(
      data = list(),
      iter_warmup = 30,
      iter_sampling = 20,
      chains = 1,
      seed = 42,
      refresh = 1,
      show_messages = TRUE,
      show_exceptions = FALSE,
      num_threads = test_threads()
    )
  )

  both_streams <- paste(c(cap$stdout, cap$stderr), collapse = "\n")
  expect_no_match(both_streams, "Informational Message")
  expect_no_match(both_streams, "test rejection")
  expect_no_match(both_streams, "Rejecting initial value")

  output <- cap$result$output()
  expect_match(paste(output, collapse = "\n"), "Informational Message")
  expect_match(paste(output, collapse = "\n"), "test rejection")
  expect_match(paste(output, collapse = "\n"), "Rejecting initial value")

  # Progress/informational output is a separate gate (show_messages) and
  # must still print.
  expect_match(paste(cap$stdout, collapse = "\n"), "Iteration:")
})

test_that("show_messages = FALSE silences progress output but show_exceptions = TRUE still prints chatter", {
  mod <- get_rejecting_model()

  cap <- run_captured(
    mod$sample(
      data = list(),
      iter_warmup = 30,
      iter_sampling = 20,
      chains = 1,
      seed = 42,
      refresh = 1,
      show_messages = FALSE,
      show_exceptions = TRUE,
      num_threads = test_threads()
    )
  )

  expect_no_match(paste(cap$stdout, collapse = "\n"), "Iteration:")
  expect_match(paste(cap$stderr, collapse = "\n"), "Informational Message")
})

test_that("multi-chain run with show_exceptions = FALSE completes and retains chatter in output()", {
  mod <- get_rejecting_model()

  result <- mod$sample(
    data = list(),
    iter_warmup = 30,
    iter_sampling = 20,
    chains = 2,
    seed = 42,
    refresh = 1,
    show_exceptions = FALSE,
    num_threads = test_threads()
  )

  expect_true(all(result$return_codes() == 0L))
  # Best-effort suppression under multi-chain interleaving; console silence
  # is not asserted here (see SHOW_EXCEPTIONS_PLAN.md section 9), only that
  # $output() remains complete.
  expect_match(paste(result$output(), collapse = "\n"), "test rejection")
})

# print() travels the same route as the chatter above: generated code (and
# the stanli adapter) write it to the stream Stan's services connect to the
# logger, so it reaches stdout and stays in $output().
test_that("print() in the model block reaches stdout and output()", {
  mod <- stan_model(
    code = "
      parameters { real x; }
      model {
        print(\"model print: x = \", x);
        x ~ normal(0, 1);
      }
    "
  )

  cap <- run_captured(
    mod$sample(
      data = list(),
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 1,
      seed = 42,
      refresh = 0,
      show_messages = TRUE,
      num_threads = test_threads()
    )
  )

  expect_match(paste(cap$stdout, collapse = "\n"), "model print: x = ")
  expect_match(paste(cap$result$output(), collapse = "\n"), "model print: x = ")
})

test_that("print() in generated quantities reaches stdout and output()", {
  mod <- stan_model(
    code = "
      parameters { real x; }
      model { x ~ normal(0, 1); }
      generated quantities { print(\"gq print: x = \", x); }
    "
  )

  cap <- run_captured(
    mod$sample(
      data = list(),
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 1,
      seed = 42,
      refresh = 0,
      show_messages = TRUE,
      num_threads = test_threads()
    )
  )

  expect_match(paste(cap$stdout, collapse = "\n"), "gq print: x = ")
  expect_match(paste(cap$result$output(), collapse = "\n"), "gq print: x = ")
})

test_that("show_exceptions wiring also works for a non-sampling service ($optimize)", {
  mod <- get_rejecting_model()

  expect_no_error(
    mod$optimize(
      data = list(),
      seed = 42,
      show_exceptions = FALSE,
      num_threads = test_threads()
    )
  )
})

withr::deferred_run()
