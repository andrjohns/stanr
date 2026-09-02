local_test_context()

init_test_cache("stanli-backend")

# ---------------------------------------------------------------------------
# Construction and backend selection
# ---------------------------------------------------------------------------

test_that("stan_model(backend = \"stanli\") compiles and reports its backend", {
  mod <- stan_model(
    code = "parameters { real theta; } model { theta ~ normal(0, 1); }",
    backend = "stanli"
  )
  expect_true(mod$is_compiled())
  expect_equal(mod$backend(), "stanli")
})

test_that("the default backend is \"compiled\"", {
  mod <- stan_model(
    code = "parameters { real theta; } model { theta ~ normal(0, 1); }",
    compile = FALSE
  )
  expect_equal(mod$backend(), "compiled")
})

test_that("an unknown backend value errors", {
  expect_error(
    stan_model(code = "parameters { real x; }", backend = "nope"),
    "should be one of"
  )
})

test_that("use_opencl = TRUE with backend = \"stanli\" errors, even without compiling", {
  expect_error(
    stan_model(
      code = "parameters { real x; }",
      backend = "stanli",
      use_opencl = TRUE,
      compile = FALSE
    ),
    "`use_opencl = TRUE` is not supported by the stanli backend"
  )
})

test_that("compiled-backend C++ inputs fail early for stanli", {
  code <- "parameters { real x; }"
  expect_error(
    stan_model(
      code = code,
      backend = "stanli",
      external_cpp = "unused.hpp",
      compile = FALSE
    ),
    "external_cpp.*not supported"
  )
  expect_error(
    stan_model(
      code = code,
      backend = "stanli",
      cpp_options = list(CXXFLAGS = "-O3"),
      compile = FALSE
    ),
    "cpp_options.*not supported"
  )
})

test_that("stanli requests the optimized stock-stanc MIR fallback", {
  flags <- NULL
  testthat::local_mocked_bindings(
    stanc_ctx = function() {
      list(call = function(entrypoint, model_name, code, options) {
        flags <<- as.character(options)
        list(result = "mir", warnings = character())
      })
    },
    .package = "stanr"
  )
  expect_identical(stanr:::.stanr_stanli_mir("parameters { real x; }"), "mir")
  expect_identical(flags, c("O1", "debug-optimized-mir"))
})

# ---------------------------------------------------------------------------
# Sampling parity with the compiled backend
# ---------------------------------------------------------------------------

test_that("sampling a stanli-backend model produces a usable fit", {
  mod <- stan_model(
    stan_file = test_stan_file("bernoulli.stan"),
    backend = "stanli"
  )
  fit <- mod$sample(
    data = bernoulli_data,
    chains = 2,
    iter_warmup = 200,
    iter_sampling = 200,
    seed = 42,
    show_messages = FALSE
  )

  expect_equal(fit$return_codes(), c(0L, 0L))
  theta_mean <- fit$summary()$mean[fit$summary()$variable == "theta"]
  # 5/10 successes (bernoulli_data$y) with a beta(1,1) prior: posterior is
  # beta(6, 6), mean 0.5.
  expect_equal(theta_mean, 0.5, tolerance = 0.05)
  expect_error(
    fit$log_prob(0, jacobian = FALSE),
    "stanli supports only jacobian = TRUE"
  )
})

# ---------------------------------------------------------------------------
# Data marshaling: every shape the stanli backend accepts.
#
# SEXP -> stanli::DataMap is a direct, one-pass conversion with no JSON or
# intermediate var_context (see sexp_to_data_map(), src/stanli_model.cpp).
#
# Values are read back through indexed transformed-data expressions rather
# than by echoing whole arrays through generated quantities: stanli's own
# write_array output naming for a directly-assigned 3-D `array[,,]`
# generated quantity does not line up with its values (a preexisting stanli
# issue, unrelated to data marshaling -- confirmed by cross-checking against
# the compiled backend, which names/values these consistently). Indexed
# reads sidestep that entirely and test exactly what this backend's R/C++
# integration is responsible for: did the right value reach the right cell.
# ---------------------------------------------------------------------------

test_that("all stanli data shapes round-trip to the correct values", {
  code <- "
    data {
      int n;
      real x;
      int<lower=0, upper=1> flag;
      vector[3] v;
      matrix[2, 3] m;
      array[2, 3] int im;
      array[2, 3, 4] real ar3;
      array[2, 3, 4] int iar3;
    }
    transformed data {
      real chk_v2 = v[2];
      real chk_m = m[2, 3];
      int chk_im = im[2, 3];
      real chk_ar3 = ar3[2, 1, 3];
      int chk_iar3 = iar3[1, 3, 2];
    }
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
    generated quantities {
      int n_out = n;
      real x_out = x;
      int flag_out = flag;
      real v2_out = chk_v2;
      real m_out = chk_m;
      int im_out = chk_im;
      real ar3_out = chk_ar3;
      int iar3_out = chk_iar3;
    }
  "
  mod <- stan_model(code = code, backend = "stanli")

  v <- c(10.1, 20.2, 30.3)
  m <- matrix(c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), nrow = 2, ncol = 3)
  im <- matrix(1:6L, nrow = 2, ncol = 3)
  ar3 <- array(seq(100, by = 1, length.out = 24), dim = c(2, 3, 4))
  iar3 <- array(1:24L, dim = c(2, 3, 4))

  data <- list(
    n = 5L,
    x = 3.5,
    flag = TRUE,
    v = v,
    m = m,
    im = im,
    ar3 = ar3,
    iar3 = iar3
  )

  fit <- mod$sample(
    data = data,
    chains = 1,
    iter_warmup = 5,
    iter_sampling = 1,
    seed = 1,
    show_messages = FALSE,
    fixed_param = TRUE
  )
  s <- fit$summary()
  get_val <- function(name) s$mean[s$variable == name]

  expect_equal(get_val("n_out"), 5)
  expect_equal(get_val("x_out"), 3.5)
  expect_equal(get_val("flag_out"), 1)
  expect_equal(get_val("v2_out"), v[2])
  expect_equal(get_val("m_out"), m[2, 3])
  expect_equal(get_val("im_out"), im[2, 3])
  expect_equal(get_val("ar3_out"), ar3[2, 1, 3])
  expect_equal(get_val("iar3_out"), iar3[1, 3, 2])
})

test_that("a bare logical scalar round-trips through set_int()", {
  code <- "
    data { int<lower=0, upper=1> flag; }
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
    generated quantities { int flag_out = flag; }
  "
  mod <- stan_model(code = code, backend = "stanli")

  fit <- mod$sample(
    data = list(flag = FALSE),
    chains = 1,
    iter_warmup = 5,
    iter_sampling = 1,
    seed = 1,
    show_messages = FALSE,
    fixed_param = TRUE
  )
  expect_equal(fit$summary()$mean[fit$summary()$variable == "flag_out"], 0)
})

test_that("a logical matrix round-trips through direct DataMap conversion", {
  code <- "
    data { array[2, 2] int lm; }
    transformed data { int chk = lm[2, 1]; }
    parameters { real theta; }
    model { theta ~ normal(0, 1); }
    generated quantities { int out = chk; }
  "
  mod <- stan_model(code = code, backend = "stanli")
  lm <- matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2)

  fit <- mod$sample(
    data = list(lm = lm),
    chains = 1,
    iter_warmup = 5,
    iter_sampling = 1,
    seed = 1,
    show_messages = FALSE,
    fixed_param = TRUE
  )
  expect_equal(
    fit$summary()$mean[fit$summary()$variable == "out"],
    as.numeric(lm[2, 1])
  )
})

# ---------------------------------------------------------------------------
# Data validation errors.
# ---------------------------------------------------------------------------

test_that("NA/NaN/Inf real data errors", {
  mod <- stan_model(
    code = "data { real x; } parameters { real theta; } model { theta ~ normal(0, x); }",
    backend = "stanli"
  )
  for (bad in list(NA_real_, NaN, Inf, -Inf)) {
    expect_error(
      mod$sample(
        data = list(x = bad),
        chains = 1,
        iter_warmup = 5,
        iter_sampling = 5,
        seed = 1,
        show_messages = FALSE
      ),
      "contains NA or NaN|stanli data cannot contain NA, NaN, or Inf"
    )
  }
})

test_that("NA integer data errors for a multi-dim int array", {
  mod <- stan_model(
    code = "data { array[2, 2] int im; } parameters { real theta; } model { theta ~ normal(0, 1); }",
    backend = "stanli"
  )
  expect_error(
    mod$sample(
      data = list(im = matrix(c(1L, NA, 3L, 4L), 2, 2)),
      chains = 1,
      iter_warmup = 5,
      iter_sampling = 5,
      seed = 1,
      show_messages = FALSE
    ),
    "stanli data cannot contain NA"
  )
})

test_that("complex data is rejected", {
  mod <- stan_model(
    code = "data { real x; } parameters { real theta; } model { theta ~ normal(0, 1); }",
    backend = "stanli"
  )
  expect_error(
    mod$sample(
      data = list(x = 1 + 2i),
      chains = 1,
      iter_warmup = 5,
      iter_sampling = 5,
      seed = 1,
      show_messages = FALSE
    ),
    "stanli data does not support complex values yet"
  )
})

test_that("numeric data.frame data is accepted as a matrix", {
  mod <- stan_model(
    code = "
      data { matrix[2, 2] m; }
      transformed data { real check = m[2, 1]; }
      parameters { real theta; }
      model { theta ~ normal(0, 1); }
      generated quantities {
        real out = check;
        real total = sum(m);
      }
    ",
    backend = "stanli"
  )
  frame <- data.frame(
    first = c(1.5, 2.5),
    second = c(3.5, 4.5)
  )
  fit <- mod$sample(
    data = list(m = frame),
    chains = 1,
    iter_warmup = 5,
    iter_sampling = 1,
    seed = 1,
    show_messages = FALSE,
    fixed_param = TRUE
  )
  s <- fit$summary()
  expect_equal(s$mean[s$variable == "out"], frame[2, 1])
  expect_equal(s$mean[s$variable == "total"], sum(as.matrix(frame)))
})

test_that("non-numeric data.frame columns are rejected", {
  mod <- stan_model(
    code = "data { real x; } parameters { real theta; } model { theta ~ normal(0, 1); }",
    backend = "stanli"
  )
  expect_error(
    mod$sample(
      data = list(x = data.frame(value = c(1, 2), label = c("a", "b"))),
      chains = 1,
      iter_warmup = 5,
      iter_sampling = 5,
      seed = 1,
      show_messages = FALSE
    ),
    "stanli data frames must contain only integer or numeric columns"
  )
})

test_that("tuple-typed (list) data is rejected", {
  mod <- stan_model(
    code = "data { real x; } parameters { real theta; } model { theta ~ normal(0, 1); }",
    backend = "stanli"
  )
  expect_error(
    mod$sample(
      data = list(x = list(1, 2)),
      chains = 1,
      iter_warmup = 5,
      iter_sampling = 5,
      seed = 1,
      show_messages = FALSE
    ),
    "tuple-typed"
  )
})

test_that("four-dimensional real and integer arrays round-trip", {
  mod <- stan_model(
    code = "
      data {
        array[2, 2, 2, 2] real a;
        array[2, 2, 2, 2] int b;
      }
      transformed data {
        real a_value = a[2, 1, 2, 1];
        int b_value = b[1, 2, 1, 2];
      }
      parameters { real theta; }
      model { theta ~ normal(0, 1); }
      generated quantities {
        real a_out = a_value;
        int b_out = b_value;
      }
    ",
    backend = "stanli"
  )
  a <- array(seq(100, by = 1, length.out = 16), dim = c(2, 2, 2, 2))
  b <- array(seq_len(16), dim = c(2, 2, 2, 2))
  fit <- mod$sample(
    data = list(a = a, b = b),
    chains = 1,
    iter_warmup = 5,
    iter_sampling = 1,
    seed = 1,
    show_messages = FALSE,
    fixed_param = TRUE
  )
  summary <- fit$summary()
  expect_equal(
    summary$mean[summary$variable == "a_out"],
    a[2, 1, 2, 1]
  )
  expect_equal(
    summary$mean[summary$variable == "b_out"],
    b[1, 2, 1, 2]
  )
})

# ---------------------------------------------------------------------------
# Constrained-scale initialization and inverse parameter transforms.
# ---------------------------------------------------------------------------

test_that("a constrained-scale (named) init samples and unconstrains", {
  mod <- stan_model(
    code = "parameters { real theta; } model { theta ~ normal(0, 1); }",
    backend = "stanli"
  )
  fit <- mod$sample(
    data = list(),
    chains = 1,
    iter_warmup = 5,
    iter_sampling = 5,
    seed = 1,
    show_messages = FALSE,
    init = list(theta = 0.5)
  )
  expect_equal(fit$return_codes(), 0L)
  expect_equal(fit$unconstrain_variables(list(theta = 0.5)), 0.5)
})

test_that("radius-only (unconstrained-compatible) init samples successfully", {
  mod <- stan_model(
    code = "parameters { real theta; } model { theta ~ normal(0, 1); }",
    backend = "stanli"
  )
  fit <- mod$sample(
    data = list(),
    chains = 1,
    iter_warmup = 20,
    iter_sampling = 20,
    seed = 1,
    show_messages = FALSE,
    init = 1
  )
  expect_equal(fit$return_codes(), 0L)
})

test_that("stanli inverse transforms handle constrained containers", {
  mod <- stan_model(
    code = "
      parameters {
        real<lower=0> sigma;
        simplex[3] p;
      }
      model {
        sigma ~ exponential(1);
        p ~ dirichlet(rep_vector(1, 3));
      }
    ",
    backend = "stanli"
  )
  fit <- mod$sample(
    data = list(),
    chains = 1,
    iter_warmup = 5,
    iter_sampling = 2,
    seed = 2,
    show_messages = FALSE,
    init = list(sigma = 2, p = c(0.2, 0.3, 0.5))
  )
  expect_equal(fit$return_codes(), 0L)
  constrained <- list(sigma = 2, p = c(0.2, 0.3, 0.5))
  free <- fit$unconstrain_variables(constrained)
  expect_length(free, 3L)
  expect_equal(
    fit$constrain_variables(
      free,
      transformed_parameters = FALSE,
      generated_quantities = FALSE
    ),
    constrained,
    tolerance = 1e-10
  )
})

# ---------------------------------------------------------------------------
# Pure value functions through stanli::Function.
# ---------------------------------------------------------------------------

test_that("compile_standalone exposes stanli scalar and container functions", {
  mod <- stan_model(
    code = "
      functions {
        real stanr_add_values(real a, real b) { return a + b; }
        int stanr_increment(int x) { return x + 1; }
        matrix stanr_scale_matrix(matrix x, real a) { return a * x; }
      }
      parameters { real theta; }
      model { theta ~ normal(0, 1); }
    ",
    backend = "stanli",
    compile_standalone = TRUE
  )
  expect_equal(names(formals(mod$functions$stanr_add_values)), c("a", "b"))
  expect_equal(mod$functions$stanr_add_values(2, 3), 5)
  expect_identical(mod$functions$stanr_increment(2L), 3L)
  x <- matrix(1:4, 2, 2)
  expect_equal(mod$functions$stanr_scale_matrix(x, 2), 2 * x)
})

test_that("stanli exposes functions without constructing the model graph", {
  mod <- stan_model(
    code = "
      functions { real stanr_square_value(real x) { return x * x; } }
      parameters { real theta; }
      model { theta ~ normal(0, 1); }
    ",
    backend = "stanli",
    compile = FALSE
  )
  mod$expose_stan_functions()
  expect_false(mod$is_compiled())
  expect_equal(mod$functions$stanr_square_value(4), 16)
})

test_that("stanli function exposure rejects unsupported value surfaces", {
  mod <- stan_model(
    code = "
      functions { real draw_rng() { return normal_rng(0, 1); } }
      parameters { real theta; }
      model { theta ~ normal(0, 1); }
    ",
    backend = "stanli",
    compile = FALSE
  )
  expect_error(mod$expose_stan_functions(), "cannot expose RNG")
})

test_that("fit-level OpenCL selection fails instead of being ignored", {
  mod <- stan_model(
    code = "parameters { real theta; } model { theta ~ normal(0, 1); }",
    backend = "stanli"
  )
  expect_error(
    mod$sample(
      data = list(),
      chains = 1,
      iter_warmup = 1,
      iter_sampling = 1,
      opencl_ids = c(0, 0),
      show_messages = FALSE
    ),
    "opencl_ids.*not supported"
  )
})

# ---------------------------------------------------------------------------
# In-session model cache: force_recompile must touch only the model being
# recompiled, not evict other cached stanli models (regression test).
# ---------------------------------------------------------------------------

test_that("force_recompile of one stanli model does not evict another's cache entry", {
  call_count <- 0
  real_mir <- stanr:::.stanr_stanli_mir
  testthat::local_mocked_bindings(
    .stanr_stanli_mir = function(code) {
      call_count <<- call_count + 1
      real_mir(code)
    },
    .package = "stanr"
  )

  code_a <- unique_stan_code()
  code_b <- unique_stan_code()
  mod_a <- stan_model(code = code_a, backend = "stanli")
  stan_model(code = code_b, backend = "stanli")
  expect_equal(call_count, 2L)

  mod_a$compile(force_recompile = TRUE, quiet = TRUE)
  expect_equal(call_count, 3L)

  # B must still be served from its untouched cache entry.
  stan_model(code = code_b, backend = "stanli")
  expect_equal(call_count, 3L)
})

withr::deferred_run()
