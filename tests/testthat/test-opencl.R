local_test_context()

init_test_cache("opencl")

test_that("use_opencl = TRUE stores and reports the flag without compiling", {
  skip_if_backend("stanli", "use_opencl is a compiled-backend feature")
  mod <- stan_model(
    code = "
      data { int<lower=0> N; }
      parameters { real theta; }
      model { theta ~ normal(0, 1); }
    ",
    use_opencl = TRUE,
    compile = FALSE
  )

  expect_true(mod$use_opencl())
  expect_false(mod$is_compiled())
})

withr::deferred_run()
