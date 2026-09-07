# Generates R->C++ SEXP wrappers for a Stan `functions` block from stanc's
# debug-ast output. Returns list(code, functions): `code` is the generated C++
# (one `extern "C" SEXP <fn>_sexp(...)` per function + a registry) and
# `functions` is data.frame(name, is_rng, args, arg_types, returntype).
#
# Wrappers use stanr::as_cpp/as_sexp (cpp11 plus vendored interop
# specializations), so they compile with plain `R CMD SHLIB` (no attribute
# processor). RNG fns are detected by scanning each body for a `_rng` call;
# their wrappers take an extra `seed` arg. Wrappers call
# `model_namespace::<fn>`; callers rewrite to unqualified for external_cpp.
.stanr_functions_to_cpp_wrappers <- function(model_code) {
  if (!is.character(model_code) || length(model_code) != 1 || is.na(model_code)) {
    stop("model_code must be a single, non-missing string.", call. = FALSE)
  }

  # 1. Run stanc with debug-ast
  ctx <- stanc_ctx()
  res <- ctx$call("stanc", "model", model_code, as.array("debug-ast"))
  if (!is.null(res$errors)) {
    stop(paste(res$errors, collapse = "\n"), call. = FALSE)
  }
  ast <- res$result

  # 2. Extract the `functionblock` (first top-level node)
  start <- regexpr("(functionblock", ast, fixed = TRUE)[[1]]
  if (start < 0L) {
    return(list(code = character(), functions = NULL))
  }
  chars <- strsplit(substr(ast, start, nchar(ast)), "", fixed = TRUE)[[1]]
  depth <- 0L
  fb_end <- NA_integer_
  for (i in seq_along(chars)) {
    if (chars[i] == "(") {
      depth <- depth + 1L
    } else if (chars[i] == ")") {
      depth <- depth - 1L
      if (depth == 0L) {
        fb_end <- i
        break
      }
    }
  }
  fb <- substr(ast, start, start + fb_end - 1L)

  # 3. For each FunDef, keep the signature (up to `(body`) and the body
  sig_starts <- gregexpr("(FunDef", fb, fixed = TRUE)[[1]]
  body_pos <- gregexpr("(body", fb, fixed = TRUE)[[1]]
  if (sig_starts[[1]] < 0L) {
    return(list(code = character(), functions = NULL))
  }

  signatures <- character()
  bodies <- character()
  for (k in seq_along(sig_starts)) {
    s <- sig_starts[[k]]
    b <- body_pos[body_pos > s][1]
    e <- if (k < length(sig_starts)) sig_starts[[k + 1L]] else nchar(fb) + 1L
    # Signature: everything before `(body`, missing the FunDef's own closing
    # paren (which comes after the body); append it to balance.
    signatures <- c(signatures, paste0(substr(fb, s, b - 1L), ")"))
    bodies <- c(bodies, substr(fb, b, e - 1L))
  }

  # 4. Parse each FunDef signature into a named list with C++ types
  parse_fundef <- function(sig) {
    tokens <- regmatches(
      sig, gregexpr("\\(|\\)|[^()[:space:]]+", sig, perl = TRUE)
    )[[1]]
    i <- 1L

    scalar_cpp <- function(tag) {
      switch(tag,
        UInt = "int",
        UReal = "double",
        UComplex = "std::complex<double>",
        UVector = "Eigen::Matrix<double,-1,1>",
        URowVector = "Eigen::Matrix<double,1,-1>",
        UMatrix = "Eigen::Matrix<double,-1,-1>",
        UComplexVector = "Eigen::Matrix<std::complex<double>,-1,1>",
        UComplexRowVector = "Eigen::Matrix<std::complex<double>,1,-1>",
        UComplexMatrix = "Eigen::Matrix<std::complex<double>,-1,-1>",
        stop("Unknown AST type: ", tag, call. = FALSE)
      )
    }

    parse_type <- function() {
      if (tokens[i] == "(") {
        i <<- i + 1L
        tag <- tokens[i]
        i <<- i + 1L
        if (tag == "UArray") {
          elem <- parse_type()
          i <<- i + 1L  # ")"
          paste0("std::vector<", elem, ">")
        } else if (tag == "UTuple") {
          i <<- i + 1L  # "(" of element list
          elems <- character()
          while (tokens[i] != ")") elems <- c(elems, parse_type())
          i <<- i + 1L  # ")" of element list
          i <<- i + 1L  # ")" of UTuple
          paste0("std::tuple<", paste(elems, collapse = ", "), ">")
        } else {
          cpp <- scalar_cpp(tag)
          i <<- i + 1L
          cpp
        }
      } else {
        cpp <- scalar_cpp(tokens[i])
        i <<- i + 1L
        cpp
      }
    }

    parse_name <- function() {
      i <<- i + 1L  # "(" of outer list
      i <<- i + 1L  # "(" of (name X)
      stopifnot(tokens[i] == "name")
      i <<- i + 1L
      nm <- tokens[i]
      i <<- i + 1L  # X
      i <<- i + 1L  # ")" of (name X)
      while (tokens[i] != ")") i <<- i + 1L  # skip (id_loc ...)
      i <<- i + 1L  # ")" of outer list
      nm
    }

    # --- FunDef ---
    i <- i + 1L  # "(" of FunDef
    stopifnot(tokens[i] == "FunDef")
    i <- i + 1L

    # returntype: (returntype (ReturnType X)) or (returntype Void)
    i <- i + 1L  # "(" of returntype
    stopifnot(tokens[i] == "returntype")
    i <- i + 1L
    if (tokens[i] == "(") {
      i <- i + 1L  # "(" of ReturnType
      stopifnot(tokens[i] == "ReturnType")
      i <- i + 1L
      returntype <- parse_type()
      i <- i + 1L  # ")" of ReturnType
    } else {
      stopifnot(tokens[i] == "Void")
      returntype <- "void"
      i <- i + 1L
    }
    i <- i + 1L  # ")" of returntype

    # funname: (funname ((name X) (id_loc ...)))
    i <- i + 1L  # "(" of funname
    stopifnot(tokens[i] == "funname")
    i <- i + 1L
    name <- parse_name()
    i <- i + 1L  # ")" of funname

    # arguments: (arguments ((AutoDiffable TYPE (name X) (id_loc ...)) ...))
    i <- i + 1L  # "(" of arguments
    i <- i + 1L  # arguments tag
    stopifnot(tokens[i] == "arguments")
    i <- i + 1L  # "(" of argument list
    i <- i + 1L  # first arg (or ")" if empty)
    args <- character()
    arg_types <- character()
    while (tokens[i] == "(") {
      i <- i + 1L  # "(" of the arg
      # Skip the AD marker (AutoDiffable, DataOnly, ...).
      i <- i + 1L
      arg_types <- c(arg_types, parse_type())
      args <- c(args, parse_name())
      i <- i + 1L  # ")" of the arg
      i <- i + 1L  # next arg (or ")" if done)
    }
    i <- i + 1L  # ")" of argument list
    i <- i + 1L  # ")" of arguments

    list(returntype = returntype, name = name, args = args, arg_types = arg_types)
  }

  fdecls <- lapply(signatures, parse_fundef)

  # 5. Detect RNG functions by scanning each body for a `_rng` call
  for (k in seq_along(fdecls)) {
    fdecls[[k]]$is_rng <- grepl("[A-Za-z0-9_]_rng\\b", bodies[[k]])
  }

  # 6. Build one SEXP wrapper per function
  # Function lives in `model_namespace` (model TU) or at file scope
  # (standalone-only); default to `model_namespace::`, callers rewrite it.
  build_wrapper <- function(info) {
    ret <- info$returntype
    name <- info$name
    arg_names <- info$args
    arg_types <- info$arg_types

    sexp_params <- if (length(arg_names)) {
      paste0("SEXP ", arg_names, "_sexp")
    } else {
      character()
    }
    get_lines <- if (length(arg_names)) {
      paste0(
        "  const auto ", arg_names, " = stanr::as_cpp<", arg_types,
        ">(", arg_names, "_sexp);"
      )
    } else {
      character()
    }
    call_args <- if (info$is_rng) {
      paste(c(arg_names, "base_rng__", "pstream__"), collapse = ", ")
    } else {
      paste(c(arg_names, "pstream__"), collapse = ", ")
    }

    if (ret == "void") {
      body <- c(
        paste0("  model_namespace::", name, "(", call_args, ");"),
        "  return R_NilValue;"
      )
    } else {
      body <- paste0(
        "  return stanr::as_sexp(model_namespace::", name, "(", call_args, "));"
      )
    }

    if (info$is_rng) {
      wrapper <- c(
        paste0("extern \"C\" SEXP ", name, "_sexp(",
               paste(c(sexp_params, "SEXP seed_sexp"), collapse = ", "), ") {"),
        "  BEGIN_CPP11",
        "  stan::rng_t base_rng__ = stan::services::util::create_rng(",
        "      static_cast<unsigned int>(stanr::as_cpp<int>(seed_sexp)), 0);",
        get_lines,
        body,
        "  END_CPP11",
        "}"
      )
    } else {
      wrapper <- c(
        paste0("extern \"C\" SEXP ", name, "_sexp(",
               paste(sexp_params, collapse = ", "), ") {"),
        "  BEGIN_CPP11",
        get_lines,
        body,
        "  END_CPP11",
        "}"
      )
    }

    paste(wrapper, collapse = "\n")
  }

  wrappers <- vapply(fdecls, build_wrapper, character(1))

  # 7. Registry
  names_vec <- vapply(fdecls, `[[`, character(1), "name")
  is_rng_vec <- vapply(fdecls, `[[`, logical(1), "is_rng")
  args_vec <- vapply(fdecls, function(f) paste(f$args, collapse = ","), character(1))

  # A name colliding with the registry symbol would be a C++ redefinition.
  reserved <- "stanr_exposed_functions"
  if (reserved %in% names_vec) {
    stop(
      "Stan function `", reserved,
      "` collides with a reserved/internal stanr export name; rename ",
      "the Stan function to expose it.",
      call. = FALSE
    )
  }

  reg_lines <- c(
    "extern \"C\" SEXP stanr_exposed_functions(void) {",
    "  BEGIN_CPP11",
    # Direct-list-init (no parens): with parens, a single-element {x} list
    # ambiguously prefers the explicit r_vector(R_xlen_t size) constructor
    # over the initializer_list one when x converts to R_xlen_t (true for
    # bool/int/double elements), silently building a zero-length vector.
    "  cpp11::writable::strings names{",
    paste0("    ", paste(sprintf('"%s"', names_vec), collapse = ", "), "};"),
    "  cpp11::writable::logicals is_rng{",
    paste0("    ", paste(ifelse(is_rng_vec, "true", "false"), collapse = ", "), "};"),
    "  cpp11::writable::strings args{",
    paste0("    ", paste(sprintf('"%s"', args_vec), collapse = ", "), "};"),
    "  return cpp11::writable::list({",
    "    cpp11::named_arg(\"name\") = names,",
    "    cpp11::named_arg(\"is_rng\") = is_rng,",
    "    cpp11::named_arg(\"args\") = args",
    "  });",
    "  END_CPP11",
    "}"
  )

  header <- c(
    "#include <stan/model/model_header.hpp>",
    "#include <cpp11.hpp>",
    "#include <cpp11/declarations.hpp>",
    "#include <stanr/cpp11_tuple_interop.hpp>",
    "#include <stanr/model_methods.hpp>",
    "static std::ostream* pstream__ = &stanr::r_console_stream();"
  )

  code <- paste(c(header, wrappers, paste(reg_lines, collapse = "\n")),
                collapse = "\n\n")

  functions_df <- data.frame(
    name = names_vec,
    is_rng = is_rng_vec,
    args = args_vec,
    arg_types = vapply(fdecls, function(f) {
      paste(f$arg_types, collapse = ",")
    }, character(1)),
    returntype = vapply(fdecls, `[[`, character(1), "returntype"),
    stringsAsFactors = FALSE
  )

  list(code = code, functions = functions_df)
}

# Wrapper declarations for the functions a program exposes. Both backends
# build their `$functions` env from this, so both refuse the same input the
# same way: a program with no `functions` block, or one declaring none.
.stanr_exposed_function_wrappers <- function(code) {
  gen <- .stanr_functions_to_cpp_wrappers(code)
  if (is.null(gen$functions) || !nrow(gen$functions)) {
    stop(
      "The Stan program has no `functions` block to expose.",
      call. = FALSE
    )
  }
  gen
}

# Compiles a Stan `functions` block into a callable env via `R CMD SHLIB`:
# stanc -> standalone C++ + generated SEXP wrappers, then dyn.load. Wrappers
# call `model_namespace::<fn>`, which the standalone TU defines.
.compile_standalone_functions_environment <- function(
  code,
  stan_file = NULL,
  external_cpp = NULL,
  cpp_options = list(),
  verbose = FALSE,
  precompiled_headers = TRUE
) {
  stanc_out <- stanc(
    code,
    standalone_functions = TRUE,
    external_cpp = external_cpp
  )
  # Keep only the model_namespace impl, dropping the `// [[stan::function]]`
  # wrapper stubs (unused now).
  impl <- sub(
    "\\n// \\[\\[stan::function\\]\\].*$",
    "",
    stanc_out,
    perl = TRUE
  )

  gen <- .stanr_exposed_function_wrappers(code)
  wrapper_section <- gen$code

  # external_cpp is at file scope before `model_namespace`; unqualify calls.
  if (length(external_cpp) > 0) {
    namespace_pos <- regexpr(
      "namespace model_namespace",
      impl,
      fixed = TRUE
    )[[1]]
    for (fn_name in gen$functions$name) {
      first_pos <- regexpr(paste0("\\b", fn_name, "\\b"), impl)[[1]]
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

  # Wrappers include <stan/model/model_header.hpp>; the standalone impl
  # already includes it, so the include guard dedupes.
  full_code <- paste(impl, wrapper_section, sep = "\n")

  cpp_option_assignments <- .stanr_parse_cpp_options(cpp_options)
  extra_assignments <- cpp_option_assignments

  build_dir <- .stanr_build_scratch_dir()
  cpp_file <- file.path(build_dir, "stanr_functions.cpp")
  writeLines(full_code, cpp_file)

  base_cppflags <- .stanr_base_cppflags()
  pch_enabled <- FALSE
  if (
    precompiled_headers &&
      length(external_cpp) == 0 &&
      !.stanr_cpp_options_block_pch(extra_assignments)
  ) {
    pch_flags <- .stanr_pch_flags(base_cppflags, verbose)
    pch_enabled <- nzchar(pch_flags)
    cppflags <- paste(pch_flags, base_cppflags)
  } else {
    cppflags <- base_cppflags
  }

  if (verbose) {
    message("[stanr] Compiling Stan functions...")
  }
  compile_functions <- function(compilation_cppflags) {
    .stanr_compile(
      cpp_file = cpp_file,
      cppflags = compilation_cppflags,
      libs = .stanr_tbb_libs(),
      extra_assignments = extra_assignments,
      verbose = verbose
    )
  }
  lib_file <- .stanr_compile_with_pch_retry(
    compile_functions,
    cppflags,
    base_cppflags,
    pch_enabled,
    verbose
  )

  compiled_env <- new.env()
  .stanr_load_functions_build(lib_file, compiled_env)
  compiled_env
}

# Populates a target env from a compiled functions env. The env already has
# each exposed fn bound as a .Call wrapper (explicit formals, trailing `seed`
# for `_rng` fns) plus `stanr_exposed_functions`.
.stanr_build_functions_env <- function(
  compiled_env,
  target_env,
  global,
  global_env = globalenv()
) {
  registry <- compiled_env$stanr_exposed_functions()

  # Clear stale bindings from a previous build.
  rm(list = ls(target_env), envir = target_env)

  for (i in seq_along(registry[["name"]])) {
    name <- registry[["name"]][[i]]
    fn <- compiled_env[[name]]
    assign(name, fn, envir = target_env)
    if (global) {
      assign(name, fn, envir = global_env)
    }
  }

  invisible(target_env)
}
