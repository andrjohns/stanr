library(testthat)
library(stanr)

# The suite runs once per StanModel backend (see the "Backend selection"
# section of tests/testthat/helpers.R). STANR_BACKEND narrows this to a
# comma-separated subset, e.g. `STANR_BACKEND=stanli R CMD check`. The
# `stanr_backend` option would override the per-pass variable, so clear it.
options(stanr_backend = NULL)
backends <- Sys.getenv("STANR_BACKEND", "compiled,stanli")
backends <- trimws(strsplit(backends, ",", fixed = TRUE)[[1]])
if (identical(backends, "both")) {
  backends <- c("compiled", "stanli")
}

failed <- vapply(
  backends,
  function(backend) {
    Sys.setenv(STANR_BACKEND = backend)
    message("\n== stanr tests: backend = \"", backend, "\" ==")
    results <- as.data.frame(test_check("stanr", stop_on_failure = FALSE))
    any(results$failed > 0L | results$error)
  },
  logical(1)
)

if (any(failed)) {
  stop(
    "Test failures under backend(s): ",
    paste(backends[failed], collapse = ", "),
    call. = FALSE
  )
}
