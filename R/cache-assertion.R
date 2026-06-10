.typedr_cached_assertion_prefix <- ".typedr_cached_assertion_"

.typedr_cacheable_assertion <- function(expr) {
  is.call(expr)
}

.typedr_assertion_cache_key <- function(expr) {
  paste(deparse(expr, width.cutoff = 500L), collapse = " ")
}

.typedr_apply_assertion_cache <- function(body, eval_env) {
  cache_env <- new.env(parent = emptyenv())
  bindings <- list()
  sources <- list()
  counter <- 0L

  register <- function(source_expr) {
    key <- .typedr_assertion_cache_key(source_expr)
    if (exists(key, envir = cache_env, inherits = FALSE)) {
      return(get(key, envir = cache_env))
    }

    counter <<- counter + 1L
    sym_nm <- paste0(.typedr_cached_assertion_prefix, counter)
    sym <- as.name(sym_nm)
    assertion <- eval(source_expr, envir = eval_env)
    bindings[[sym_nm]] <<- assertion
    sources[[sym_nm]] <<- source_expr
    assign(key, sym, envir = cache_env)
    sym
  }

  rewrite <- function(expr) {
    if (!is.call(expr)) {
      return(expr)
    }

    call_nm <- call_name(expr)
    if (!is_null(call_nm) && call_nm %in% c("check_arg", "check_output")) {
      if (length(expr) >= 3L) {
        assertion <- expr[[3L]]
        if (.typedr_cacheable_assertion(assertion)) {
          source_expr <- assertion
          cached_sym <- register(assertion)
          assertion_label <- call2("quote", source_expr)
          bind <- isTRUE(expr$.bind)
          if (identical(call_nm, "check_output")) {
            expr <- call2(
              "check_output",
              expr[[2L]],
              cached_sym,
              .assertion_expr = assertion_label
            )
          } else if (bind) {
            expr <- call2(
              "check_arg",
              expr[[2L]],
              cached_sym,
              .bind = TRUE,
              .assertion_expr = assertion_label
            )
          } else {
            expr <- call2(
              "check_arg",
              expr[[2L]],
              cached_sym,
              .assertion_expr = assertion_label
            )
          }
        }
      }
      return(expr)
    }

    if (!is_null(call_nm) && identical(call_nm, "check_dependent_arg")) {
      return(expr)
    }

    expr[-1L] <- lapply(expr[-1L], rewrite)
    expr
  }

  list(
    body = rewrite(body),
    bindings = bindings,
    sources = sources
  )
}

.typedr_build_typed_function <- function(fmls, body, eval_env) {
  cached <- .typedr_apply_assertion_cache(body, eval_env)

  fn_env <- if (length(cached$bindings)) {
    env <- new.env(parent = eval_env)
    for (nm in names(cached$bindings)) {
      assign(nm, cached$bindings[[nm]], envir = env)
    }
    env
  } else {
    eval_env
  }

  fn <- new_function(fmls, cached$body, env = fn_env)
  if (length(cached$sources)) {
    attr(fn, "typedr_assertion_sources") <- cached$sources
  }
  fn
}

.typedr_restore_assertion_sources <- function(expr, sources) {
  if (is.call(expr)) {
    call_nm <- call_name(expr)
    if (!is_null(call_nm) && call_nm %in% c("check_arg", "check_output")) {
      expr_names <- names2(expr)
      if (".assertion_expr" %in% expr_names) {
        label <- expr$.assertion_expr
        if (is.call(label) && identical(call_name(label), "quote")) {
          label <- label[[2L]]
        }
        if (identical(call_nm, "check_arg") && isTRUE(expr$.bind)) {
          return(call2("check_arg", expr[[2L]], label, .bind = TRUE))
        }
        return(call2(call_nm, expr[[2L]], label))
      }
    }
  }

  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (nm %in% names(sources)) {
      return(sources[[nm]])
    }
    return(expr)
  }

  if (!is.call(expr)) {
    return(expr)
  }

  expr[-1L] <- lapply(expr[-1L], function(part) {
    .typedr_restore_assertion_sources(part, sources)
  })
  expr
}

.typedr_function_for_print <- function(fn) {
  sources <- attr(fn, "typedr_assertion_sources", exact = TRUE)
  if (is_null(sources) || !length(sources)) {
    return(fn)
  }

  new_function(
    fn_fmls(fn),
    .typedr_restore_assertion_sources(body(fn), sources),
    env = environment(fn)
  )
}
