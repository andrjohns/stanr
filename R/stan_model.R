.stanr_apply_makevars <- function(base, assignments) {
  for (a in assignments) {
    if (identical(a$op, "+=") && a$name %in% names(base)) {
      base[[a$name]] <- paste(base[[a$name]], a$value)
    } else {
      base[a$name] <- a$value
    }
  }
  base
}

.stanr_append_build_key <- function(cpp_file, key_hash) {
  cat(
    "\nextern \"C\" SEXP stanr_build_key(void) {\n",
    "  return Rf_ScalarString(Rf_mkChar(\"",
    key_hash,
    "\"));\n",
    "}\n",
    file = cpp_file,
    append = TRUE,
    sep = ""
  )
}

.stanr_compile_shlib <- function(cpp_file, verbose) {
  lib_name <- paste0(
    tools::file_path_sans_ext(basename(cpp_file)),
    .Platform$dynlib.ext
  )
  output <- withr::with_dir(
    dirname(cpp_file),
    tryCatch(
      .stanr_rcmd(
        c("SHLIB", "-o", shQuote(lib_name), shQuote(basename(cpp_file))),
        stdout = TRUE,
        stderr = TRUE
      ),
      error = function(e) conditionMessage(e)
    )
  )
  lib_file <- file.path(dirname(cpp_file), lib_name)
  if (!file.exists(lib_file)) {
    stop(paste(output, collapse = "\n"), call. = FALSE)
  }
  if (verbose) {
    cat(output, sep = "\n")
  }
  lib_file
}

# USE_CXX20=1 -> SHLIB uses CXX20FLAGS, not CXXFLAGS.
.stanr_compile <- function(
  cpp_file,
  cppflags,
  libs,
  extra_assignments,
  verbose
) {
  withr::with_envvar(
    c(USE_CXX20 = "1"),
    withr::with_makevars(
      .stanr_apply_makevars(
        c(
          PKG_CPPFLAGS = paste(cppflags, collapse = " "),
          PKG_LIBS = libs,
          CXX20FLAGS = .stanr_opt_flags()
        ),
        extra_assignments
      ),
      assignment = "+=",
      .stanr_compile_shlib(cpp_file, verbose)
    )
  )
}

.stanr_tbb_libs <- function() {
  tbb_lib_dir <- system.file(
    "lib",
    .Platform$r_arch,
    package = "stanr",
    mustWork = TRUE
  )
  if (R.version$os == "emscripten") {
    return(shQuote(file.path(tbb_lib_dir, "libtbb.a")))
  }
  paste0(
    "-L",
    shQuote(tbb_lib_dir),
    " -Wl,-rpath,",
    shQuote(tbb_lib_dir),
    " -ltbb -ltbbmalloc"
  )
}

.stanr_base_cppflags <- function() {
  paste(
    paste0(
      "-I",
      shQuote(system.file("include", package = "stanr", mustWork = TRUE))
    ),
    # TBB_INTERFACE_NEW: see src/Makevars.
    "-D_REENTRANT -DSTAN_THREADS -D_HAS_AUTO_PTR_ETC=0 -DEIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS -DTBB_INTERFACE_NEW"
  )
}

# PCH skipped when cpp_options overrides compiler flags.
.stanr_cpp_options_block_pch <- function(assignments) {
  compiler_vars <- c(
    "CPPFLAGS",
    "PKG_CPPFLAGS",
    "CXX",
    "CXXFLAGS",
    "CXXPICFLAGS",
    "CXX20",
    "CXX20STD",
    "CXX20FLAGS",
    "CXX20PICFLAGS"
  )
  any(vapply(
    assignments,
    function(a) a$name %in% compiler_vars,
    logical(1)
  ))
}

.stanr_build_scratch_dir <- function() {
  dir <- tempfile("stanr_build_")
  dir.create(dir)
  dir
}

.stanr_build_cache_file <- function(stan_file, key_hash, suffix = "") {
  if (length(stan_file)) {
    dir <- dirname(stan_file)
    if (file.access(dir, 2) == 0) {
      stem <- tools::file_path_sans_ext(basename(stan_file))
      return(file.path(dir, paste0(".", stem, suffix, .Platform$dynlib.ext)))
    }
  }
  file.path(tempdir(), paste0("stanr_", key_hash, suffix, .Platform$dynlib.ext))
}

.stanr_write_build_cache <- function(cache_file, lib_file) {
  tryCatch(
    {
      dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
      tmp_out <- paste0(cache_file, ".tmp", Sys.getpid())
      file.copy(lib_file, tmp_out, overwrite = TRUE)
      # file.rename() won't replace an existing file on Windows.
      unlink(cache_file)
      file.rename(tmp_out, cache_file)
      invisible(NULL)
    },
    error = function(e) invisible(NULL)
  )
}

# Bind exposed standalone functions (`_sexp` wrappers + registry) from a dll.
.stanr_bind_exposed_functions <- function(dll, env) {
  if (
    is.null(tryCatch(
      getNativeSymbolInfo("stanr_exposed_functions", dll),
      error = function(e) NULL
    ))
  ) {
    return(invisible(FALSE))
  }
  registry_fun <- local({
    addr <- getNativeSymbolInfo("stanr_exposed_functions", dll)$address
    function(...) do.call(".Call", list(addr, ...))
  })
  registry <- registry_fun()
  env[["stanr_exposed_functions"]] <- registry_fun
  for (i in seq_along(registry[["name"]])) {
    name <- registry[["name"]][[i]]
    arg_names <- strsplit(registry[["args"]][[i]], ",", fixed = TRUE)[[1]]
    is_rng <- isTRUE(registry[["is_rng"]][[i]])
    if (is_rng) {
      arg_names <- c(arg_names, "seed")
    }
    sym <- getNativeSymbolInfo(paste0(name, "_sexp"), dll)
    addr <- sym$address
    # Explicit formals forwarding to .Call; RNG `seed` defaults to session seed.
    formals_list <- stats::setNames(
      rep(list(quote(expr = )), length(arg_names)),
      arg_names
    )
    if (is_rng) {
      formals_list[["seed"]] <- quote(sample.int(.Machine$integer.max, 1))
    }
    fn <- eval(call(
      "function",
      as.pairlist(formals_list),
      as.call(c(
        list(quote(.Call), addr),
        lapply(arg_names, as.name)
      ))
    ))
    env[[name]] <- fn
  }
  invisible(TRUE)
}

.stanr_load_build <- function(so_file, env) {
  dll <- dyn.load(so_file)
  for (name in .stanr_model_support_exports) {
    sym <- getNativeSymbolInfo(name, dll)
    env[[name]] <- local({
      addr <- sym$address
      function(...) do.call(".Call", list(addr, ...))
    })
  }
  # Bind exposed standalone functions if present.
  .stanr_bind_exposed_functions(dll, env)
  do.call(".Call", list(getNativeSymbolInfo("stanr_build_key", dll)$address))
}

# Loads a standalone-functions-only .so (no model exports): bind exposed fns.
.stanr_load_functions_build <- function(so_file, env) {
  dll <- dyn.load(so_file)
  .stanr_bind_exposed_functions(dll, env)
  invisible(env)
}

# dyn.load a private copy so recompiles never touch a live fit.
.stanr_restore_build_cache <- function(cache_file, key_hash, env) {
  if (!file.exists(cache_file)) {
    return(FALSE)
  }
  so_copy <- file.path(
    .stanr_build_scratch_dir(),
    paste0("lib", .Platform$dynlib.ext)
  )
  if (!isTRUE(file.copy(cache_file, so_copy))) {
    return(FALSE)
  }
  hash <- tryCatch(.stanr_load_build(so_copy, env), error = function(e) NULL)
  if (identical(hash, key_hash)) {
    return(TRUE)
  }
  try(dyn.unload(so_copy), silent = TRUE)
  FALSE
}

.stanr_model_support_exports <- c(
  "new_model",
  "run_model",
  "constrained_param_names",
  "new_base_rng",
  "model_num_upars",
  "model_param_metadata",
  "model_constrained_names",
  "model_unconstrained_names",
  "model_log_prob",
  "model_grad_log_prob",
  "model_hessian",
  "model_unconstrain",
  "model_unconstrain_matrix",
  "model_constrain",
  "model_constrain_matrix",
  "model_constrain_variables",
  "model_variable_skeleton",
  "select_opencl_device"
)

.stanr_stanli_mir <- function(code) {
  res <- stanc_ctx()$call(
    "stanc",
    "model",
    code,
    as.array(c("O1", "debug-optimized-mir"))
  )
  if (!is.null(res$errors)) {
    stop(paste(res$errors, collapse = "\n"), call. = FALSE)
  }
  if (length(res$warnings)) {
    warning(paste(res$warnings, collapse = "\n"), call. = FALSE)
  }
  res$result
}

.stanr_stanli_native_function <- function(name) {
  # stanr deliberately has no useDynLib directive: zzz.R loads the package
  # DLL explicitly and stores its DLLInfo object in `.stanr_dll`. Resolve
  # registered routines against that object rather than by package name.
  dll <- .stanr_dll
  if (is.null(dll)) {
    stop("the stanr native library is not loaded", call. = FALSE)
  }
  address <- getNativeSymbolInfo(name, dll)$address
  function(...) do.call(".Call", c(list(address), list(...)))
}

.stanr_stanli_native_functions <- function() {
  cached <- .stanr_memo$stanli_native_functions
  if (!is.null(cached)) {
    return(cached)
  }
  # OpenCL is a compiled-backend capability. Keeping it out of this table
  # prevents stanli from satisfying that API with a misleading no-op.
  names <- c(
    setdiff(.stanr_model_support_exports, "select_opencl_device"),
    "new_function",
    "call_function"
  )
  cached <- stats::setNames(
    lapply(
      names,
      function(name) {
        .stanr_stanli_native_function(paste0("stanr_stanli_", name))
      }
    ),
    names
  )
  .stanr_memo$stanli_native_functions <- cached
  cached
}

.stanr_stanli_functions_environment <- function(mir, code, native) {
  declarations <- .stanr_functions_to_cpp_wrappers(code)$functions
  env <- new.env(parent = emptyenv())
  if (is.null(declarations) || !nrow(declarations)) {
    registry <- list(name = character(), is_rng = logical(), args = character())
    env$stanr_exposed_functions <- function() registry
    return(env)
  }

  unsupported <- declarations$is_rng |
    grepl("_(rng|lp)$", declarations$name) |
    declarations$returntype == "void" |
    grepl("complex|tuple", declarations$returntype, ignore.case = TRUE) |
    grepl("complex|tuple", declarations$arg_types, ignore.case = TRUE)
  if (any(unsupported)) {
    stop(
      "The stanli Function API cannot expose RNG, _lp, void, complex, or tuple ",
      "functions: ",
      paste(unique(declarations$name[unsupported]), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  duplicated_names <- duplicated(declarations$name)
  if (any(duplicated_names)) {
    warning(
      "Overloaded Stan functions use the first declaration's R argument ",
      "names; stanli selects the matching overload from their values: ",
      paste(unique(declarations$name[duplicated_names]), collapse = ", "),
      ".",
      call. = FALSE
    )
    declarations <- declarations[!duplicated_names, , drop = FALSE]
  }

  for (i in seq_len(nrow(declarations))) {
    name <- declarations$name[[i]]
    args <- if (nzchar(declarations$args[[i]])) {
      strsplit(declarations$args[[i]], ",", fixed = TRUE)[[1]]
    } else {
      character()
    }
    pointer <- native$new_function(mir, name)
    wrapper_env <- list2env(
      list(
        .stanr_call = native$call_function,
        .stanr_function = pointer
      ),
      parent = baseenv()
    )
    arguments <- stats::setNames(lapply(args, as.name), args)
    argument_list <- as.call(c(list(quote(list)), arguments))
    body <- as.call(list(
      as.name(".stanr_call"),
      as.name(".stanr_function"),
      argument_list
    ))
    formals <- stats::setNames(rep(list(quote(expr = )), length(args)), args)
    env[[name]] <- eval(
      call("function", as.pairlist(formals), body),
      envir = wrapper_env
    )
  }

  registry <- list(
    name = declarations$name,
    is_rng = rep(FALSE, nrow(declarations)),
    args = declarations$args
  )
  env$stanr_exposed_functions <- function() registry
  env
}

.compile_stan_model_environment <- function(
  code,
  model_name,
  stan_file = NULL,
  external_cpp = NULL,
  verbose = FALSE,
  precompiled_headers = TRUE,
  force_recompile = FALSE,
  use_opencl = FALSE,
  cpp_options = list(),
  compile_standalone = FALSE
) {
  model_support <- readLines(
    system.file("stan_model.cpp", package = "stanr", mustWork = TRUE)
  )
  # external_cpp hashed by content, not path.
  external_cpp_contents <- .stanr_external_cpp_contents(external_cpp)
  extra_assignments <- .stanr_parse_cpp_options(cpp_options)
  # Stable sort so reordering cpp_options doesn't change the hash.
  if (length(extra_assignments)) {
    assignment_names <- vapply(extra_assignments, `[[`, character(1), "name")
    ord <- order(assignment_names)
    hash_component <- vapply(
      extra_assignments[ord],
      function(a) paste(a$name, a$op, a$value),
      character(1)
    )
  } else {
    hash_component <- character()
  }
  model_hash <- .stanr_hash(
    c(
      code,
      external_cpp_contents,
      model_support,
      as.character(utils::packageVersion("stanr")),
      .stanr_stan_version(),
      R.version$platform,
      .stanr_compiler_identity(),
      as.character(use_opencl),
      as.character(compile_standalone),
      hash_component
    )
  )

  memo <- if (is.null(.stanr_memo$compiled_envs)) {
    .stanr_memo$compiled_envs <- new.env(parent = emptyenv())
  } else {
    .stanr_memo$compiled_envs
  }
  if (!force_recompile && !is.null(memo[[model_hash]])) {
    return(memo[[model_hash]])
  }

  cache_file <- .stanr_build_cache_file(stan_file, model_hash)
  if (!force_recompile) {
    env <- new.env()
    if (.stanr_restore_build_cache(cache_file, model_hash, env)) {
      memo[[model_hash]] <- env
      return(env)
    }
  }

  build_dir <- .stanr_build_scratch_dir()
  cpp_file <- file.path(build_dir, paste0("stan_", model_hash, ".cpp"))
  if (verbose) {
    message("[stanr] Compiling '", model_name, "'...")
  }
  cpp_code <- stanc(
    code,
    external_cpp = external_cpp,
    use_opencl = use_opencl
  )
  if (compile_standalone) {
    # Generate R->C++ SEXP wrappers from the AST.
    gen <- .stanr_functions_to_cpp_wrappers(code)
    wrapper_section <- gen$code
    # external_cpp is at file scope before `model_namespace`; unqualify calls.
    if (length(external_cpp) > 0) {
      namespace_pos <- regexpr(
        "namespace model_namespace",
        cpp_code,
        fixed = TRUE
      )[[1]]
      for (fn_name in gen$functions$name) {
        first_pos <- regexpr(paste0("\\b", fn_name, "\\b"), cpp_code)[[1]]
        if (first_pos > 0 && first_pos < namespace_pos) {
          wrapper_section <- gsub(
            paste0("model_namespace::", fn_name, "("),
            paste0(fn_name, "("),
            wrapper_section,
            fixed = TRUE
          )
        }
      }
    }
    cpp_code <- c(cpp_code, wrapper_section)
  }
  writeLines(c(cpp_code, model_support), cpp_file)

  .stanr_append_build_key(cpp_file, model_hash)

  cppflags <- .stanr_base_cppflags()
  if (use_opencl) {
    # Pinned to 0/0; real device selection at runtime.
    cppflags <- paste(
      cppflags,
      "-DSTAN_OPENCL -DOPENCL_PLATFORM_ID=0 -DOPENCL_DEVICE_ID=0",
      "-DCL_HPP_TARGET_OPENCL_VERSION=120 -DCL_HPP_MINIMUM_OPENCL_VERSION=120",
      "-DCL_HPP_ENABLE_EXCEPTIONS -DINTEGRATED_OPENCL=0 -Wno-ignored-attributes"
    )
  }
  base_cppflags <- cppflags
  pch_enabled <- FALSE
  if (
    precompiled_headers &&
      length(external_cpp) == 0 &&
      !.stanr_cpp_options_block_pch(extra_assignments)
  ) {
    pch_flags <- .stanr_pch_flags(base_cppflags, verbose)
    pch_enabled <- nzchar(pch_flags)
    cppflags <- paste(pch_flags, base_cppflags)
  }

  env <- new.env()
  runtime_archive <- system.file(
    "lib",
    Sys.getenv("R_ARCH"),
    "libstanr_runner.a",
    package = "stanr",
    mustWork = TRUE
  )

  tbb_libs <- .stanr_tbb_libs()

  # runner.a built without STAN_OPENCL; services touch model via virtual iface.
  libs <- paste(shQuote(runtime_archive), tbb_libs)
  if (use_opencl) {
    opencl_default <- if (Sys.info()[["sysname"]] == "Darwin") {
      "-framework OpenCL"
    } else {
      "-lOpenCL"
    }
    libs <- paste(libs, opencl_default)
  }

  compile_model <- function(compilation_cppflags) {
    .stanr_compile(
      cpp_file = cpp_file,
      cppflags = compilation_cppflags,
      libs = libs,
      extra_assignments = extra_assignments,
      verbose = verbose
    )
  }

  lib_file <- .stanr_compile_with_pch_retry(
    compile_model,
    cppflags,
    base_cppflags,
    pch_enabled,
    verbose
  )
  .stanr_load_build(lib_file, env)
  .stanr_write_build_cache(cache_file, lib_file)

  memo[[model_hash]] <- env
  env
}

.compile_stanli_model_environment <- function(
  code,
  model_name,
  verbose = FALSE,
  force_recompile = FALSE,
  compile_standalone = FALSE
) {
  memo <- if (is.null(.stanr_memo$stanli_mirs)) {
    .stanr_memo$stanli_mirs <- new.env(parent = emptyenv())
  } else {
    .stanr_memo$stanli_mirs
  }
  key <- .stanr_hash(c("stanli-o1-mir", code))
  mir <- memo[[key]]
  if (force_recompile || is.null(mir)) {
    if (verbose) {
      message("[stanr] Constructing '", model_name, "' with stanli...")
    }
    mir <- .stanr_stanli_mir(code)
    memo[[key]] <- mir
  }

  native <- .stanr_stanli_native_functions()
  model_native <- native[setdiff(
    .stanr_model_support_exports,
    "select_opencl_device"
  )]
  env <- list2env(model_native, parent = emptyenv())
  new_model <- native$new_model
  env$new_model <- function(data, seed, declarations = NULL) {
    new_model(mir, data, model_name)
  }
  if (compile_standalone) {
    functions <- .stanr_stanli_functions_environment(mir, code, native)
    for (name in ls(functions, all.names = TRUE)) {
      env[[name]] <- functions[[name]]
    }
  }
  env
}

#' Create a Stan model object
#'
#' @description Create a new [`StanModel`] object from a Stan program file or
#'   from Stan code as a string. The [`StanModel`] object stores the Stan
#'   program source and compiled model, and provides methods for fitting the
#'   model using Stan's inference algorithms.
#'
#'   See the `compile` argument for control over whether and how compilation
#'   happens.
#'
#' @param stan_file (string) The path to a `.stan` file containing a Stan
#'   program. If `stan_file` is not specified then `code` must be specified.
#' @param code (string) A Stan program as a single string. If `code` is not
#'   specified then `stan_file` must be specified.
#' @param compile (logical) Should the model be compiled? The default is
#'   `TRUE`. If `FALSE` compilation can be done later via the
#'   [`$compile()`][model-method-compile] method.
#' @param compile_standalone (logical) Should the Stan program's `functions`
#'   block be exposed as part of this compilation? The default is `FALSE`.
#'   When `TRUE`, `$functions` is already populated when `stan_model()`
#'   returns -- equivalent to `FALSE` here plus calling
#'   [`$expose_stan_functions()`][model-method-expose-stan-functions]
#'   immediately after, but without a second compile. See
#'   [`$expose_stan_functions()`][model-method-expose-stan-functions] for
#'   what gets exposed and how.
#' @param model_name (string) The name to use for the model. If `NULL` (the
#'   default), the model name is derived from the Stan file name (if provided)
#'   or set to `"model"`.
#' @param include_paths (character vector) Paths to directories where Stan
#'   should look for files specified in `#include` directives.
#' @param user_header (string) A path to a single C++ header file to prepend to
#'   the generated model code. A legacy alias for `external_cpp`; supplied
#'   headers are appended to `external_cpp` and treated identically.
#' @param cpp_options (list) C++ compilation options, merged into the
#'   Makevars flags used to compile the model. Each element is either:
#'   * A named element, e.g. `list(CXX = "g++")`. This *overrides* any value
#'     computed internally for that name (e.g. `CXXFLAGS`), replacing it
#'     rather than adding to it.
#'   * An unnamed string of the form `"<NAME> = <value>"`, e.g.
#'     `"CXX = g++"`. Equivalent to the named form above -- also overrides.
#'   * An unnamed string of the form `"<NAME> += <value>"`, e.g.
#'     `"CXXFLAGS += -Wno-psabi"`. *Appends* `<value>` to any existing value
#'     for `<NAME>` instead of replacing it (space-separated), matching how
#'     `+=` behaves in a Makevars file.
#'
#'   Entries are applied in list order, so the same name may appear more
#'   than once, e.g. `list(CXXFLAGS = "-O3", "CXXFLAGS += -Wall")` first
#'   overrides `CXXFLAGS`, then appends to it.
#' @param stanc_options (list) Stan-to-C++ transpiler options. Not yet supported.
#' @param force_recompile (logical) Should the model be recompiled even if it
#'   has not been modified? The default is `FALSE`, but can be set via the
#'   `stanr_force_recompile` option.
#' @param precompiled_headers (logical) Should precompiled headers be used to
#'   speed up compilation? The default is `TRUE`. Automatically disabled when
#'   `cpp_options` overrides compiler flags (e.g. `CXXFLAGS`), since the
#'   precompiled header is not built with those flags.
#' @param quiet (logical) Should verbose output from compilation be suppressed?
#'   The default is `TRUE`.
#' @param external_cpp (character vector) Paths to C++ files to prepend to the
#'   generated model code. Useful for defining custom functions.
#' @param use_opencl (logical) Should the model be compiled with OpenCL
#'   support? The default is `FALSE`. When `TRUE`, `stanc` generates
#'   OpenCL-accelerated code for the functions that support it (most notably
#'   `bernoulli_logit_glm` and other GLM likelihoods), and the compiled model
#'   can run its computation on an OpenCL device. Which platform/device is
#'   used at runtime is controlled by the `opencl_ids` argument of the fit
#'   methods (e.g. [`$sample()`][model-method-sample]), not by this argument;
#'   `opencl_ids = NULL` (the default there) means `select_opencl_device()`
#'   is simply never called, so the platform/device baked in at compile time
#'   (0/0) is used. The default OpenCL link flags are `"-framework OpenCL"`
#'   on macOS and `"-lOpenCL"` elsewhere.
#' @param backend (string) Either `"compiled"` (the default), which compiles
#'   the model to a native shared library, or `"stanli"`, which interprets
#'   the model instead of compiling it. The `"stanli"` backend supports
#'   constrained-scale initialization, but not `use_opencl`, `external_cpp`,
#'   or non-empty `cpp_options`. Its exposed-function backend supports
#'   deterministic integer/real scalar and container functions, but not RNG,
#'   `_lp`, void, complex, or tuple functions.
#'
#' @return A [`StanModel`] object.
#'
#'   The compiled model is cached persistently on disk as the compiled shared
#'   library itself, next to `stan_file` (named after it, e.g. `mymodel.stan`
#'   gets a sibling `.mymodel.so`/`.mymodel.dll`), or under [tempdir()] when
#'   the model was created from a `code` string, or when `stan_file`'s
#'   directory isn't writable. The library embeds a hash of the generated
#'   C++, so it's reused across R sessions as long as the Stan program,
#'   `include_paths`, `external_cpp`, and installed stanr/Stan versions are
#'   unchanged, and is silently recompiled and overwritten otherwise --
#'   there is no separate cache-clearing step. On a cache hit, the
#'   Stan-to-C++ transpiler is skipped entirely, so any transpiler warnings
#'   (e.g. from pedantic mode or deprecated syntax) are only surfaced the
#'   first time a given model is compiled, not on subsequent cache hits.
#'
#' @seealso [`StanModel`], [`$compile()`][model-method-compile],
#'   [`$sample()`][model-method-sample]
#'
#' @examples
#' \dontrun{
#' # Create a StanModel from Stan code
#' mod <- stan_model(
#'   code = "
#'     parameters {
#'       real theta;
#'     }
#'     model {
#'       theta ~ normal(0, 1);
#'     }
#'   "
#' )
#' mod$model_name()
#' mod$variables()
#'
#' # Run MCMC sampling
#' fit <- mod$sample(data = list(), chains = 2)
#' fit$summary()
#' }
#'
#' @export
stan_model <- function(
  stan_file = NULL,
  code = NULL,
  compile = TRUE,
  model_name = NULL,
  include_paths = NULL,
  user_header = NULL,
  cpp_options = list(),
  stanc_options = list(),
  force_recompile = getOption("stanr_force_recompile", FALSE),
  precompiled_headers = TRUE,
  quiet = TRUE,
  external_cpp = NULL,
  use_opencl = FALSE,
  compile_standalone = FALSE,
  backend = "compiled"
) {
  StanModel$new(
    stan_file = stan_file,
    code = code,
    compile = compile,
    model_name = model_name,
    include_paths = include_paths,
    user_header = user_header,
    cpp_options = cpp_options,
    stanc_options = stanc_options,
    force_recompile = force_recompile,
    precompiled_headers = precompiled_headers,
    quiet = quiet,
    external_cpp = external_cpp,
    use_opencl = use_opencl,
    compile_standalone = compile_standalone,
    backend = backend
  )
}
