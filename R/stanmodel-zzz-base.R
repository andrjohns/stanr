# StanModel class definition

#' StanModel objects
#'
#' @name StanModel
#' @description A `StanModel` object is an [R6][R6::R6Class] object created by
#'   [stan_model()]. The object stores the Stan program source code and compiled
#'   model environment, and provides methods for fitting the model using Stan's
#'   inference algorithms.
#'
#' @section Methods: `StanModel` objects have the following associated methods,
#'   many of which have their own (linked) documentation pages:
#'
#'  ## Stan code
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$code()`][model-method-model-info] | Return Stan program as a string. |
#'  [`$print()`][model-method-model-info] | Print the Stan program. |
#'
#'  ## Model information
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$model_name()`][model-method-model-info] | Return the model name. |
#'  [`$stan_file()`][model-method-model-info] | Return the path to the Stan file. |
#'  [`$has_stan_file()`][model-method-model-info] | Check whether the model was created with a Stan file. |
#'  [`$include_paths()`][model-method-model-info] | Return the Stan include paths. |
#'  [`$stan_version()`][model-method-model-info] | Return the Stan version used by the package. |
#'  [`$variables()`][model-method-variables] | Return input and output variable information. |
#'  [`$cpp_options()`][model-method-model-info] | Return the C++ options associated with the model. |
#'  [`$stanc_options()`][model-method-model-info] | Return the stanc options associated with the model. |
#'
#'  ## Compilation
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$check_syntax()`][model-method-check-syntax] | Check the Stan program's syntax without compiling. |
#'  [`$format()`][model-method-format] | Reformat the Stan program using `stanc`'s auto-formatter. |
#'  [`$compile()`][model-method-compile] | Compile the Stan program. |
#'  [`$is_compiled()`][model-method-model-info] | Check whether the model has been compiled. |
#'
#'  ## Diagnostics
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$diagnose()`][model-method-diagnose] | Run Stan's `"diagnose"` method to test gradients, return [`StanDiagnose`] object. |
#'
#'  ## Function exposure
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$expose_stan_functions()`][model-method-expose-stan-functions] | Expose the program's `functions` block as R functions. |
#'  [`$functions`][model-method-expose-stan-functions] | Environment holding the exposed functions. |
#'
#'  ## Model fitting
#'
#'  |**Method**|**Description**|
#'  |:----------|:---------------|
#'  [`$sample()`][model-method-sample] | Run Stan's `"sample"` method (HMC/NUTS MCMC), return [`StanMCMC`] object. |
#'  [`$optimize()`][model-method-optimize] | Run Stan's `"optimize"` method, return [`StanMLE`] object. |
#'  [`$laplace()`][model-method-laplace] | Run Stan's `"laplace"` method, return [`StanLaplace`] object. |
#'  [`$variational()`][model-method-variational] | Run Stan's `"variational"` method (ADVI), return [`StanVB`] object. |
#'  [`$pathfinder()`][model-method-pathfinder] | Run Stan's `"pathfinder"` method, return [`StanPathfinder`] object. |
#'  [`$generate_quantities()`][model-method-generate-quantities] | Run Stan's `"generate quantities"` method, return [`StanGQ`] object. |
#'
NULL

StanModel <- R6Class(
  "StanModel",
  public = list(
    functions = NULL,

    initialize = function(
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
      compile <- .stanr_flag(compile, "compile")
      force_recompile <- .stanr_flag(force_recompile, "force_recompile")
      precompiled_headers <- .stanr_flag(
        precompiled_headers,
        "precompiled_headers"
      )
      quiet <- .stanr_flag(quiet, "quiet")
      use_opencl <- .stanr_flag(use_opencl, "use_opencl")
      compile_standalone <- .stanr_flag(
        compile_standalone,
        "compile_standalone"
      )
      backend <- match.arg(backend, c("compiled", "stanli"))
      if (backend == "stanli" && use_opencl) {
        stop("`use_opencl = TRUE` is not supported by the stanli backend.",
             call. = FALSE)
      }
      if (is.null(stan_file) == is.null(code)) {
        stop("Supply exactly one of `stan_file` and `code`.", call. = FALSE)
      }
      if (!is.null(stan_file)) {
        if (
          !is.character(stan_file) ||
            length(stan_file) != 1 ||
            is.na(stan_file) ||
            !file.exists(stan_file)
        ) {
          stop("`stan_file` must name an existing Stan file.", call. = FALSE)
        }
        stan_file <- normalizePath(stan_file, mustWork = TRUE)
        code <- paste(readLines(stan_file, warn = FALSE), collapse = "\n")
      }
      if (!is.character(code) || length(code) != 1L || is.na(code)) {
        stop("`code` must be a single non-missing string.", call. = FALSE)
      }
      if (is.null(model_name)) {
        model_name <- if (is.null(stan_file)) {
          "model"
        } else {
          sub("\\.stan$", "", basename(stan_file))
        }
      }
      if (
        !is.character(model_name) ||
          length(model_name) != 1L ||
          is.na(model_name) ||
          !nzchar(model_name)
      ) {
        stop("`model_name` must be a single non-empty string.", call. = FALSE)
      }
      include_paths <- include_paths %||% character()
      if (!is.character(include_paths) || anyNA(include_paths)) {
        stop("`include_paths` must be a character vector.", call. = FALSE)
      }
      if (length(include_paths)) {
        include_paths <- normalizePath(include_paths, mustWork = TRUE)
      }
      # Validate up front so a malformed entry fails fast at construction.
      .stanr_parse_cpp_options(cpp_options)
      if (!is.list(stanc_options)) {
        stop("`stanc_options` must be a list.", call. = FALSE)
      }
      if (length(stanc_options)) {
        stop(
          "Non-empty `stanc_options` are not yet supported by `stan_model()`.",
          call. = FALSE
        )
      }
      # `user_header` is a legacy alias for `external_cpp`.
      if (!is.null(user_header)) {
        external_cpp <- c(external_cpp, user_header)
      }
      if (backend == "stanli") {
        if (length(external_cpp)) {
          stop(
            "`external_cpp` and `user_header` are not supported by the stanli backend.",
            call. = FALSE
          )
        }
        if (length(cpp_options)) {
          stop(
            "Non-empty `cpp_options` are not supported by the stanli backend.",
            call. = FALSE
          )
        }
      }

      private$stan_file_ <- stan_file
      private$code_ <- code
      private$model_name_ <- model_name
      private$include_paths_ <- include_paths
      private$cpp_options_ <- cpp_options
      private$stanc_options_ <- stanc_options
      private$force_recompile_ <- force_recompile
      private$precompiled_headers_ <- precompiled_headers
      private$quiet_ <- quiet
      private$external_cpp_ <- external_cpp
      private$use_opencl_ <- use_opencl
      private$compile_standalone_ <- compile_standalone
      private$backend_ <- backend
      self$functions <- new.env(parent = emptyenv())
      if (compile) {
        self$compile()
      }
      invisible(self)
    },

    # ---- Information methods
    stan_file = function() private$stan_file_ %||% character(),
    has_stan_file = function() !is.null(private$stan_file_),
    code = function() private$code_,
    print = function() {
      cat(private$code_, "\n", sep = "")
      invisible(self)
    },
    model_name = function() private$model_name_,
    include_paths = function() private$include_paths_,
    stan_version = function() .stanr_stan_version(),
    is_compiled = function() !is.null(private$compiled_env_),
    compile_generation = function() private$compile_generation_,
    cpp_options = function() private$cpp_options_,
    stanc_options = function() private$stanc_options_,
    use_opencl = function() private$use_opencl_,
    backend = function() private$backend_,

    # ---- Variables method
    variables = function() {
      if (is.null(private$variables_)) {
        private$variables_ <- model_variables(
          model_code = private$resolved_code(),
          include_directories = character(),
          allow_undefined = length(private$external_cpp_) > 0
        )
      }
      private$variables_
    },

    # ---- Compilation methods
    compile = function(
      force_recompile = private$force_recompile_,
      quiet = private$quiet_,
      compile_standalone = private$compile_standalone_
    ) {
      force_recompile <- .stanr_flag(force_recompile, "force_recompile")
      quiet <- .stanr_flag(quiet, "quiet")
      compile_standalone <- .stanr_flag(
        compile_standalone,
        "compile_standalone"
      )
      # Increment first so it reflects "a compile was attempted" even on throw.
      private$compile_generation_ <- private$compile_generation_ + 1L
      private$compiled_env_ <- if (private$backend_ == "stanli") {
        .compile_stanli_model_environment(
          code = private$resolved_code(),
          model_name = private$model_name_,
          verbose = !quiet,
          force_recompile = force_recompile,
          compile_standalone = compile_standalone
        )
      } else .compile_stan_model_environment(
        code = private$resolved_code(),
        model_name = private$model_name_,
        stan_file = private$stan_file_,
        external_cpp = private$external_cpp_,
        verbose = !quiet,
        precompiled_headers = private$precompiled_headers_,
        force_recompile = force_recompile,
        use_opencl = private$use_opencl_,
        cpp_options = private$cpp_options_,
        compile_standalone = compile_standalone
      )
      if (compile_standalone) {
        # cmdstanr parity: expose functions without a separate call.
        private$functions_compiled_env_ <- private$compiled_env_
        .stanr_build_functions_env(
          private$functions_compiled_env_,
          self$functions,
          global = FALSE
        )
      }
      invisible(self)
    },

    check_syntax = function(pedantic = FALSE, quiet = FALSE) {
      pedantic <- .stanr_flag(pedantic, "pedantic")
      quiet <- .stanr_flag(quiet, "quiet")
      stanc(
        private$resolved_code(),
        external_cpp = private$external_cpp_,
        use_opencl = private$use_opencl_,
        warn_pedantic = pedantic
      )
      if (!quiet) {
        message("[stanr] Stan program is syntactically correct.")
      }
      invisible(TRUE)
    },

    format = function(
      overwrite_file = FALSE,
      canonicalize = FALSE,
      backup = TRUE,
      max_line_length = NULL,
      quiet = FALSE
    ) {
      overwrite_file <- .stanr_flag(overwrite_file, "overwrite_file")
      backup <- .stanr_flag(backup, "backup")
      quiet <- .stanr_flag(quiet, "quiet")
      if (
        !isFALSE(canonicalize) &&
          !isTRUE(canonicalize) &&
          !is.character(canonicalize)
      ) {
        stop(
          "`canonicalize` must be FALSE, TRUE, or a character vector.",
          call. = FALSE
        )
      }
      if (!is.null(max_line_length)) {
        max_line_length <- .stanr_int(
          max_line_length,
          "max_line_length",
          min = 1L
        )
      }
      if (overwrite_file) {
        if (!self$has_stan_file()) {
          stop(
            "`overwrite_file = TRUE` requires a model created with `stan_file`.",
            call. = FALSE
          )
        }
        if (grepl("#include", private$code_, fixed = TRUE)) {
          stop(
            "`overwrite_file = TRUE` is not supported for programs with ",
            "`#include` directives, since formatting inlines them.",
            call. = FALSE
          )
        }
      }

      formatted <- stanc_format(
        private$resolved_code(),
        canonicalize = canonicalize,
        max_line_length = max_line_length
      )

      if (overwrite_file) {
        if (backup) {
          backup_file <- paste0(
            private$stan_file_,
            ".bak-",
            format(Sys.time(), "%Y%m%d%H%M%S")
          )
          file.copy(private$stan_file_, backup_file)
          if (!quiet) {
            message("[stanr] Old version of the model stored to ", backup_file)
          }
        }
        # `formatted` already ends in "\n"; writeLines() would add another.
        writeLines(formatted, private$stan_file_, sep = "")
      }

      formatted
    },

    # ---- Function exposure methods
    expose_stan_functions = function(global = FALSE, verbose = FALSE) {
      global <- .stanr_flag(global, "global")
      verbose <- .stanr_flag(verbose, "verbose")

      if (is.null(private$functions_compiled_env_)) {
        # Must work on a compile = FALSE model without compiling it.
        private$functions_compiled_env_ <- if (private$backend_ == "stanli") {
          .compile_stanli_model_environment(
            code = private$resolved_code(),
            model_name = private$model_name_,
            verbose = verbose,
            force_recompile = FALSE,
            compile_standalone = TRUE
          )
        } else {
          .compile_standalone_functions_environment(
            code = private$resolved_code(),
            stan_file = private$stan_file_,
            external_cpp = private$external_cpp_,
            cpp_options = private$cpp_options_,
            verbose = verbose,
            precompiled_headers = private$precompiled_headers_
          )
        }
      }

      .stanr_build_functions_env(
        private$functions_compiled_env_,
        self$functions,
        global
      )
      invisible(self$functions)
    },
    expose_functions = function(global = FALSE, verbose = FALSE) {
      self$expose_stan_functions(global = global, verbose = verbose)
    },

    # ---- Fitting methods (defined in stanmodel-*.R files)
    sample = stan_model_sample_impl,
    optimize = stan_model_optimize_impl,
    laplace = stan_model_laplace_impl,
    variational = stan_model_variational_impl,
    pathfinder = stan_model_pathfinder_impl,
    generate_quantities = stan_model_generate_quantities_impl,
    diagnose = stan_model_diagnose_impl,

    # ---- Internal native entry points
    # Public because sourceCpp functions live in a model-specific env.

    new_model = function(data, seed, declarations = NULL) {
      private$ensure_compiled()
      private$compiled_env_$new_model(data, seed, declarations)
    },
    run_model = function(model, args) {
      private$ensure_compiled()
      private$compiled_env_$run_model(model, args)
    },
    constrained_param_names = function(model) {
      private$ensure_compiled()
      private$compiled_env_$constrained_param_names(model)
    },
    native_function = function(name, required = TRUE) {
      private$ensure_compiled()
      fun <- private$compiled_env_[[name]]
      if (is.null(fun) && required) {
        stop(
          "The compiled model does not provide native function `",
          name,
          "`; recompile the model with the current stanr version.",
          call. = FALSE
        )
      }
      fun
    }
  ),
  private = list(
    stan_file_ = NULL,
    code_ = NULL,
    model_name_ = NULL,
    include_paths_ = NULL,
    cpp_options_ = NULL,
    stanc_options_ = NULL,
    force_recompile_ = FALSE,
    precompiled_headers_ = TRUE,
    quiet_ = TRUE,
    external_cpp_ = NULL,
    use_opencl_ = FALSE,
    compile_standalone_ = FALSE,
    backend_ = "compiled",
    compiled_env_ = NULL,
    functions_compiled_env_ = NULL,
    compile_generation_ = 0L,
    variables_ = NULL,
    resolved_code_ = NULL,
    ensure_compiled = function() {
      if (is.null(private$compiled_env_)) {
        self$compile()
      }
      invisible(NULL)
    },
    # `code_` is immutable after `initialize()`, so no invalidation needed.
    # Shared by `$compile()` and `$variables()` so `#include` resolution
    # happens at most once per model.
    resolved_code = function() {
      if (is.null(private$resolved_code_)) {
        private$resolved_code_ <- resolve_stan_includes(
          private$code_,
          private$include_paths_
        )
      }
      private$resolved_code_
    },
    # Selects the OpenCL platform/device; triggers lazy compile if needed.
    select_opencl = function(opencl_ids) {
      ids <- as.integer(opencl_ids)
      if (length(ids) != 2L || anyNA(ids) || any(ids < 0L)) {
        stop("`opencl_ids` must be c(platform_id, device_id).", call. = FALSE)
      }
      if (private$backend_ == "stanli") {
        stop("`opencl_ids` are not supported by the stanli backend.",
             call. = FALSE)
      }
      self$native_function("select_opencl_device")(ids[[1]], ids[[2]])
      invisible(NULL)
    }
  ),
  cloneable = FALSE
)

# StanModel information method documentation
#' Access information from a `StanModel` object
#'
#' @name model-method-model-info
#' @family StanModel methods
#'
#' @description These methods access information stored in a [`StanModel`]
#'   object and print its Stan program.
#'
#'   ```
#'   stan_file()
#'   has_stan_file()
#'   code()
#'   print()
#'   model_name()
#'   include_paths()
#'   stan_version()
#'   is_compiled()
#'   cpp_options()
#'   stanc_options()
#'   use_opencl()
#'   ```
#'
#' @return
#' * `$stan_file()` returns a path as a string, or `character()` if the model
#'   was created from code (not a file).
#' * `$has_stan_file()` returns `TRUE` if the model was created with a Stan file
#'   and `FALSE` otherwise.
#' * `$code()` returns the Stan program as a single string.
#' * `$print()` prints the Stan program and returns the [`StanModel`] object
#'   invisibly.
#' * `$model_name()` returns the model name as a string.
#' * `$include_paths()` returns a character vector of absolute paths.
#' * `$stan_version()` returns the Stan version bundled with the package as a
#'   string.
#' * `$is_compiled()` returns `TRUE` if the model has been compiled.
#' * `$cpp_options()` returns the `cpp_options` list the model was created
#'   with (see [stan_model()]).
#' * `$stanc_options()` returns a named list of stanc options.
#' * `$use_opencl()` returns `TRUE` if the model was created with
#'   `use_opencl = TRUE` and `FALSE` otherwise.
#'
#' @seealso [`$compile()`][model-method-compile] and [stan_model()]
#'
NULL

# StanModel variables method documentation
#' Input and output variables of a Stan program
#'
#' @name model-method-variables
#' @aliases variables
#' @family StanModel methods
#'
#' @description The `$variables()` method of a [`StanModel`] object returns
#'   a list, each element representing a Stan model block: `data`, `parameters`,
#'   `transformed_parameters` and `generated_quantities`.
#'
#'   Each element contains a list of variables, with each variable represented
#'   as a list with information on its scalar type (`real` or `int`) and
#'   number of dimensions.
#'
#'   The number of dimensions reported is the number of indexing dimensions in
#'   the declared Stan variable, equivalently the number of indices needed to
#'   access one scalar element. This means a scalar has 0 dimensions, a vector
#'   or one-dimensional array has 1, and a matrix or two-dimensional array has
#'   2. Array dimensions are added to any vector or matrix dimensions, so
#'   `array[J] matrix[N, K]` has 3 dimensions.
#'
#'   `transformed data` is not included, as variables in that block are not
#'   part of the model's input or output.
#'
#' @return A list with information on input and output variables for each of
#'   the Stan model blocks.
#'
#' @examples
#' \dontrun{
#' mod <- stan_model(
#'   code = "
#'   data {
#'     int N;
#'     array[2, 3] int y;
#'   }
#'   parameters {
#'     real alpha;
#'     vector[N] beta;
#'     array[2] matrix[3, 4] theta;
#'   }
#'   ",
#'   compile = FALSE
#' )
#'
#' vars <- mod$variables()
#' str(vars)
#' }
#'
NULL

# StanModel compilation method documentation
#' Compile a Stan program
#'
#' @name model-method-compile
#' @aliases compile
#' @family StanModel methods
#'
#' @description The `$compile()` method of a [`StanModel`] object compiles the
#'   Stan program using the in-process backend. In most cases the user does not
#'   need to explicitly call `$compile()` as compilation occurs automatically
#'   when calling [stan_model()]. However it is possible to set `compile = FALSE`
#'   in the call to `stan_model()` and subsequently call `$compile()` directly.
#'
#' @param force_recompile (logical) Should the model be recompiled even if it
#'   has not been modified since it was last compiled? The default is `FALSE`.
#' @param quiet (logical) Should verbose output from compilation be suppressed?
#'   The default is `TRUE`.
#' @param compile_standalone (logical) Should the Stan program's `functions`
#'   block be exposed as part of this compilation? Defaults to whatever was
#'   set at [stan_model()] construction time. See
#'   [`$expose_stan_functions()`][model-method-expose-stan-functions].
#'
#' @return The [`StanModel`] object, invisibly.
#'
#' @seealso [`$is_compiled()`][model-method-model-info] and [stan_model()]
#'
NULL

#' Check Stan program syntax
#'
#' @name model-method-check-syntax
#' @aliases check_syntax
#' @family StanModel methods
#'
#' @description The `$check_syntax()` method of a [`StanModel`] object runs
#'   the Stan program through `stanc` without compiling the generated C++. It
#'   is a cheap way to validate a program before calling
#'   [`$compile()`][model-method-compile].
#'
#' @param pedantic (logical) Should `stanc`'s pedantic-mode warnings be
#'   requested? The default is `FALSE`.
#' @param quiet (logical) Should the success message be suppressed? The
#'   default is `FALSE`.
#'
#' @return `TRUE`, invisibly. Errors if the program has a syntax error.
#'
#' @seealso [`$compile()`][model-method-compile]
#'
NULL

#' Format a Stan program
#'
#' @name model-method-format
#' @aliases format
#' @family StanModel methods
#'
#' @description The `$format()` method of a [`StanModel`] object reformats
#'   the Stan program using `stanc`'s auto-formatter and returns the result as
#'   a string. Unlike [`$check_syntax()`][model-method-check-syntax], it does
#'   not work on programs with unresolved `#include` directives, since
#'   formatting would inline their contents.
#'
#' @param overwrite_file (logical) Should the formatted code be written back
#'   to [`$stan_file()`][model-method-model-info]? The default is `FALSE`.
#'   Requires a model created with `stan_file`, and does not update this
#'   object's in-memory code -- construct a new [`StanModel`] to pick up the
#'   change.
#' @param canonicalize (logical or character) `FALSE` (the default) formats
#'   without canonicalizing, `TRUE` also canonicalizes deprecated syntax, and
#'   a character vector requests specific canonicalizations (e.g.
#'   `c("braces", "parentheses")`).
#' @param backup (logical) When `overwrite_file = TRUE`, should the original
#'   file be backed up first? The default is `TRUE`.
#' @param max_line_length (integer) Maximum output line width. The default,
#'   `NULL`, uses `stanc`'s default.
#' @param quiet (logical) Should the backup message be suppressed? The
#'   default is `FALSE`.
#'
#' @return The formatted Stan code as a string.
#'
#' @seealso [`$check_syntax()`][model-method-check-syntax], [`$code()`][model-method-model-info]
#'
NULL

# StanModel function-exposure method documentation
#' Expose Stan functions as R functions
#'
#' @name model-method-expose-stan-functions
#' @aliases expose_stan_functions expose_functions
#' @family StanModel methods
#'
#' @description The `$expose_stan_functions()` method of a [`StanModel`]
#'   object compiles the functions declared in the Stan program's `functions`
#'   block and makes them callable from R. `$expose_functions()` is an alias
#'   (cmdstanr's name for the same method).
#'
#'   ```
#'   expose_stan_functions(global = FALSE, verbose = FALSE)
#'   expose_functions(global = FALSE, verbose = FALSE)
#'   ```
#'
#'   Exposed functions are always assigned into the `$functions` member
#'   environment (e.g. `mod$functions$my_fun(...)`); `global = TRUE`
#'   additionally assigns each one into the global environment, so it can
#'   also be called directly (`my_fun(...)`). Repeat calls are cheap: once a
#'   model's functions have been compiled -- whether by an earlier
#'   `$expose_stan_functions()` call, or automatically via
#'   `compile_standalone = TRUE` at [stan_model()] time -- a later call just
#'   performs the requested `$functions`/global assignments, without
#'   recompiling.
#'
#'   A Stan function whose name ends in `_rng` gets a trailing `seed = NULL`
#'   argument on its R wrapper. All exposed `_rng` functions share one
#'   underlying RNG, seeded once per `$expose_stan_functions()` call from
#'   R's own RNG stream (so calling `set.seed()` before exposing makes draws
#'   reproducible); passing `seed` explicitly to a call reseeds the
#'   generator immediately before that call.
#'
#'   Stan `tuple(...)` arguments/returns map to/from **unnamed** R lists (one
#'   element per slot; arrays of tuples become lists of such lists), and
#'   `complex` / `complex_vector` / `complex_row_vector` / `complex_matrix`
#'   map to/from R's native complex type. An overloaded Stan function (same
#'   name, different signature) exposes only the first-defined overload, with
#'   a warning.
#'
#' @param global (logical) Should the exposed functions also be assigned
#'   into the global environment? The default, `FALSE`, only populates
#'   `$functions`.
#' @param verbose (logical) Should compiler progress messages be printed?
#'   No compilation happens (so this has no effect) if the functions were
#'   already compiled, e.g. via `compile_standalone = TRUE`.
#'
#' @return The [`StanModel`] object's `$functions` environment, invisibly.
#'
#' @section Caching: Compiled functions are cached on disk the same way
#'   compiled models are (see [stan_model()]) -- a warm cache skips
#'   recompilation entirely, including across R sessions.
#'
#' @section Serialization: Exposed function objects in `$functions` are
#'   compiled bindings and, like any compiled function from this package, do
#'   not survive `saveRDS()`/`readRDS()`. After restoring a saved
#'   [`StanModel`] or [`StanFit`], call `$expose_stan_functions()` again to
#'   repopulate `$functions` -- this rebuilds from the on-disk cache and
#'   does not recompile.
#'
#' @seealso [stan_model()] for the `compile_standalone` argument, which
#'   exposes functions as part of the model's own compilation.
#'
NULL
