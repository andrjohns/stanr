local_test_context()

init_test_cache("expose-functions-2")

# Every test here drives the compiled functions-block toolchain directly
# (.compile_standalone_functions_environment / .stanr_build_functions_env),
# which shells out to `R CMD SHLIB` whatever the model backend under test.
skip_on_webr("compiling a functions block needs R CMD SHLIB")

# Below: integration tests that actually invoke stanc.js + a real C++
# compile (via .compile_standalone_functions_environment()). Slow but
# expected for this package; the cache dir is redirected by setup.R.

test_that(".compile_standalone_functions_environment compiles a functions block into a callable env", {
  code <- "
functions {
  real my_add(real a, real b) { return a + b; }
  real my_add_rng(real a, real b) { return a + b + normal_rng(0, 1); }
  vector vec_add(vector a, vector b) { return a + b; }
}
"
  env <- stanr:::.compile_standalone_functions_environment(code)

  expect_true(is.function(env$stanr_exposed_functions))
  expect_true(is.function(env$my_add))
  expect_true(is.function(env$my_add_rng))
  expect_true(is.function(env$vec_add))

  registry <- env$stanr_exposed_functions()
  expect_equal(sort(registry$name), sort(c("my_add", "my_add_rng", "vec_add")))

  expect_equal(env$my_add(2, 3), 5)
  expect_equal(env$vec_add(c(1, 2), c(3, 4)), c(4, 6))
})

test_that("numeric-array/tuple/complex functions round-trip through cpp11 marshalling", {
  code <- "
functions {
  array[] real real_array_id(array[] real x) { return x; }
  array[] int int_array_id(array[] int x) { return x; }
  tuple(real, vector) split_stat(vector x) { return (mean(x), head(x, 2)); }
  complex_vector rotate(complex_vector z, complex phase) { return z * phase; }
  complex_matrix cmat_id(complex_matrix m) { return m; }
  complex_row_vector crv_id(complex_row_vector v) { return v; }
  array[] tuple(complex, real) mixed_out(tuple(complex_matrix, int) tm) {
    return {(tm.1[1,1], 1.0), (tm.1[2,2], 2.0)};
  }
  real tup_arr_in(data array[] tuple(real, array[] int) v) {
    real total = 0;
    for (e in v) {
      total += e.1;
      for (j in e.2) {
        total += j;
      }
    }
    return total;
  }
  array[,] tuple(int, real) deep_id(array[,] tuple(int, real) v) { return v; }
  complex czmul_rng(complex a) { return a * normal_rng(0, 1); }
}
"
  env <- stanr:::.compile_standalone_functions_environment(code)

  # Long compact sequences exercise the region-copy input path without
  # materializing cpp11's large buffered iterators.
  real_sequence <- as.double(seq_len(5000))
  integer_sequence <- seq_len(5000)
  expect_equal(env$real_array_id(real_sequence), real_sequence)
  expect_equal(env$int_array_id(integer_sequence), integer_sequence)

  # tuple(real, vector) return: unnamed list, element 2 shaped as the
  # declared vector.
  expect_equal(env$split_stat(c(1, 2, 3)), list(2, c(1, 2)))

  # complex_vector * complex scalar (elementwise rotation).
  expect_equal(
    env$rotate(c(1 + 2i, 3 - 1i), 1i),
    c(-2 + 1i, 1 + 3i)
  )

  cm <- matrix(c(1 + 1i, 2 + 2i, 3 + 3i, 4 + 4i), nrow = 2)
  expect_equal(env$cmat_id(cm), cm)

  # complex_row_vector round-trips as a 1xn matrix -- the same shape
  # the Eigen interop already uses for a plain (real) row_vector.
  crv <- c(1 + 1i, 2 + 2i)
  expect_equal(env$crv_id(crv), matrix(crv, nrow = 1))

  # tuple(complex_matrix, int) argument; array[] tuple(complex, real)
  # return -- an unnamed list of unnamed lists (AoS), picking diagonal
  # entries of the matrix slot and ignoring the int slot.
  expect_equal(
    env$mixed_out(list(cm, 7L)),
    list(list(cm[1, 1], 1), list(cm[2, 2], 2))
  )

  # data-qualified array[] tuple(real, array[] int) argument: a list of
  # tuple-shaped lists in, a plain scalar out.
  expect_equal(
    env$tup_arr_in(list(list(1.5, c(1L, 2L)), list(2.5, c(3L)))),
    1.5 + 1 + 2 + 2.5 + 3
  )

  # array[,] tuple(int, real): list (over first index) of lists (over
  # second index) of tuples -- exact round trip through an identity
  # function.
  deep_in <- list(
    list(list(1L, 1.5), list(2L, 2.5)),
    list(list(3L, 3.5), list(4L, 4.5))
  )
  expect_equal(env$deep_id(deep_in), deep_in)

  # `_rng` function taking complex: reproducible with an explicit seed.
  v1 <- env$czmul_rng(2 + 0i, seed = 1L)
  v2 <- env$czmul_rng(2 + 0i, seed = 1L)
  expect_equal(v1, v2)
})

test_that(".compile_standalone_functions_environment compiles a functions block into a callable env", {
  code <- "
functions {
  real cache_reuse_add(real a, real b) { return a + b; }
}
"
  env <- stanr:::.compile_standalone_functions_environment(code)
  expect_true(is.function(env$cache_reuse_add))
  expect_equal(env$cache_reuse_add(2, 3), 5)
})

test_that(".stanr_build_functions_env populates a target env with callable functions, wrapping _rng exports", {
  code <- "
functions {
  real build_env_add(real a, real b) { return a + b; }
  real build_env_add_rng(real a, real b) { return a + b + normal_rng(0, 1); }
}
"
  compiled_env <- stanr:::.compile_standalone_functions_environment(code)
  target_env <- new.env()

  stanr:::.stanr_build_functions_env(
    compiled_env,
    target_env,
    global = FALSE
  )

  expect_true(is.function(target_env$build_env_add))
  expect_equal(target_env$build_env_add(2, 3), 5)

  rng_fn <- target_env$build_env_add_rng
  expect_true(is.function(rng_fn))
  # explicit formals copied from the compiled export: real argument names,
  # not `...`, plus a trailing seed argument defaulting to the current R seed.
  expect_equal(names(formals(rng_fn)), c("a", "b", "seed"))
  expect_false(identical(formals(rng_fn)$seed, quote(expr = )))

  # same explicit seed on two separate calls -> identical draws
  expect_equal(rng_fn(1, 2, seed = 42), rng_fn(1, 2, seed = 42))

  # required-argument propagation still works through the wrapper
  expect_error(rng_fn(1))
})

test_that(".stanr_build_functions_env(global = TRUE) assigns into a designated env, never the real .GlobalEnv", {
  code <- "
functions {
  real global_test_add(real a, real b) { return a + b; }
}
"
  compiled_env <- stanr:::.compile_standalone_functions_environment(code)
  target_env <- new.env()
  test_global <- new.env()

  stanr:::.stanr_build_functions_env(
    compiled_env,
    target_env,
    global = TRUE,
    global_env = test_global
  )

  expect_true(is.function(test_global$global_test_add))
  expect_equal(test_global$global_test_add(2, 3), 5)
  expect_false(exists("global_test_add", envir = globalenv(), inherits = FALSE))
})

test_that("rebuilding via .stanr_build_functions_env clears stale bindings from a previous build", {
  code <- "
functions {
  real rebuild_add(real a, real b) { return a + b; }
}
"
  compiled_env <- stanr:::.compile_standalone_functions_environment(code)
  target_env <- new.env()

  stanr:::.stanr_build_functions_env(
    compiled_env,
    target_env,
    global = FALSE
  )
  assign("stale_binding", 123, envir = target_env)
  expect_true(exists("stale_binding", envir = target_env, inherits = FALSE))

  stanr:::.stanr_build_functions_env(
    compiled_env,
    target_env,
    global = FALSE
  )

  expect_false(exists("stale_binding", envir = target_env, inherits = FALSE))
  expect_true(is.function(target_env$rebuild_add))
  expect_equal(target_env$rebuild_add(4, 5), 9)
})

withr::deferred_run()
