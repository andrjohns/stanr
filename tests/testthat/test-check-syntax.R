local_test_context()

init_test_cache("check-syntax")

test_that("check_syntax() returns TRUE invisibly and does not compile the model", {
  mod <- stan_model(
    code = "parameters { real x; } model { x ~ normal(0, 1); }",
    compile = FALSE
  )

  expect_true(suppressMessages(mod$check_syntax()))
  expect_false(mod$is_compiled())
})

test_that("check_syntax() prints a success message by default", {
  mod <- stan_model(
    code = "parameters { real x; } model { x ~ normal(0, 1); }",
    compile = FALSE
  )

  expect_message(mod$check_syntax(), "syntactically correct")
})

test_that("check_syntax(quiet = TRUE) suppresses the success message", {
  mod <- stan_model(
    code = "parameters { real x; } model { x ~ normal(0, 1); }",
    compile = FALSE
  )

  expect_no_message(mod$check_syntax(quiet = TRUE))
})

test_that("check_syntax() errors on invalid Stan code", {
  mod <- stan_model(code = "parameters { real x }", compile = FALSE)

  expect_error(mod$check_syntax(), "Syntax error")
})

test_that("check_syntax(pedantic = TRUE) surfaces a pedantic warning", {
  mod <- stan_model(
    code = "parameters { real theta; } model { theta ~ uniform(0, 1); }",
    compile = FALSE
  )

  expect_warning(mod$check_syntax(pedantic = TRUE, quiet = TRUE))
})

test_that("check_syntax() resolves #include directives", {
  mod <- stan_model(
    stan_file = test_stan_file("include_model.stan"),
    include_paths = test_path("test-models/includes"),
    compile = FALSE
  )

  expect_true(suppressMessages(mod$check_syntax()))
})

test_that("check_syntax() allows undefined functions declared via external_cpp", {
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
    external_cpp = test_path("test-models/external_mean.hpp"),
    compile = FALSE
  )

  expect_true(suppressMessages(mod$check_syntax()))
})

withr::deferred_run()
