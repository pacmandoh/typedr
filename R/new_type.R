.typedr_truncate_text <- function(x, max_chars = 80L) {
  x <- paste(x, collapse = " ")
  x <- gsub("[[:space:]]+", " ", x)
  if (nchar(x, type = "width") <= max_chars) {
    return(x)
  }

  paste0(strtrim(x, width = max_chars - 3L), "...")
}

.typedr_deparse <- function(x, max_chars = 80L) {
  .typedr_truncate_text(
    deparse(x, width.cutoff = 60L),
    max_chars = max_chars
  )
}

.typedr_diagnostic_label <- function(x, max_chars = 56L) {
  .typedr_truncate_text(expr_deparse(x), max_chars = max_chars)
}

.typedr_assertion_diagnostic_label <- function(x, max_chars = 48L) {
  operator <- if (is_call(x, "|")) {
    "|"
  } else if (is_call(x, "&")) {
    "&"
  } else {
    return(.typedr_diagnostic_label(x, max_chars = max_chars))
  }

  flatten <- function(node) {
    if (is_call(node, operator)) {
      return(c(flatten(node[[2]]), flatten(node[[3]])))
    }
    expr_deparse(node)
  }

  .typedr_summarize_labels(
    flatten(x),
    separator = paste0(" ", operator, " "),
    max_items = 2L,
    max_chars = max_chars
  )
}

.typedr_error_call <- function(x, max_chars = 56L) {
  if (is_null(x) || !is_call(x)) {
    return(x)
  }
  label <- paste(expr_deparse(x), collapse = " ")
  label <- gsub("[[:space:]]+", " ", label)
  if (nchar(label, type = "width") <= max_chars) {
    return(x)
  }

  if (is_call(x, c("|", "&"))) {
    return(call2("Type"))
  }

  nm <- call_name(x)
  if (!is_null(nm) && !is.na(nm) && nzchar(nm)) {
    return(call2(nm))
  }

  call2("Type")
}

.typedr_summarize_labels <- function(
  labels,
  separator = " | ",
  max_items = 3L,
  max_chars = 80L
) {
  labels <- vapply(
    labels,
    .typedr_truncate_text,
    character(1),
    max_chars = max_chars
  )
  shown <- character()

  for (label in labels) {
    candidate <- paste(c(shown, label), collapse = separator)
    if (
      length(shown) >= max_items || nchar(candidate, type = "width") > max_chars
    ) {
      break
    }
    shown <- c(shown, label)
  }

  remaining <- length(labels) - length(shown)
  summary <- paste(shown, collapse = separator)
  if (remaining > 0L) {
    summary <- paste0(summary, separator, "... (", remaining, " more)")
  }
  summary
}

#' Build an assertion factory
#'
#' `as_assertion_factory()` wraps a checking function and turns it into a typedr
#' assertion factory. The wrapped function must take the value to check as its
#' first argument and return the checked value, return a predicate result, or
#' throw an error if the value is invalid. Errors that are not already typedr
#' errors are wrapped in a standard typedr custom assertion error. When a
#' non-typedr error already has a useful message, that message becomes the
#' typedr error message instead of being repeated as a parent error. Generated
#' wrapper names are replaced with the public assertion-factory call in typedr
#' errors, and long predicate expressions or values are shortened in diagnostic
#' bullets. Exceptionally long union or intersection calls use the neutral
#' `Type()` label; long single-factory calls fall back to the factory name
#' (for example `Character()`).
#'
#' @param f A function whose first argument is the value to check. Additional
#'   arguments become arguments of the assertion factory.
#' @param mode How to interpret the return value of `f`. `"assertion"` expects
#'   `f` to return the checked value. `"predicate"` expects `f` to return a
#'   scalar logical. `"auto"` keeps value-returning assertions working and
#'   treats scalar logical values that are not the original value as predicates.
#' @param message Optional message to use when a predicate assertion fails or
#'   when a non-typedr error is wrapped.
#' @param typedr_fast Optional fast-path specification for native assertion
#'   factories. When the factory is invoked without extra `...` conditions and
#'   without blocked formals such as `each` or `levels`, generated assertions
#'   take a success-only fast path before falling back to the standard checker.
#' @return A function with class `assertion_factory`.
#'
#' @export
#' @importFrom rlang call2 expr fn_fmls_syms fn_fmls new_function pairlist2 is_null caller_env names2 sym try_fetch
#' @importFrom cli cli_abort
as_assertion_factory <- function(
  f,
  mode = c("auto", "assertion", "predicate"),
  message = NULL,
  typedr_fast = NULL
) {
  mode <- match.arg(mode)
  if (!is_null(message) && (!is_character(message) || length(message) != 1L)) {
    cli_abort(
      "`message` must be a single string or `NULL`.",
      class = c(
        "typedr_input_error",
        "typedr_assertion_factory_error",
        "typedr_error"
      )
    )
  }

  # create a function with arguments being the additional args to f and dots
  f_call <- call2(expr(f), expr(value), !!!fn_fmls_syms(f)[-1])
  dots_call <- call2(
    "process_assertion_factory_dots",
    sym("..."),
    .typedr_assertion_call = sym(".typedr_assertion_call")
  )
  predicate <- .typedr_predicate_label(f)
  message_expr <- if (is_null(message)) quote(NULL) else message
  predicate_expr <- if (is_null(predicate)) quote(NULL) else predicate
  typedr_fast_expr <- if (is_null(typedr_fast)) quote(NULL) else typedr_fast

  res <- new_function(
    pairlist2(!!!fn_fmls(f)[-1], ... = ),
    expr({
      .typedr_assertion_call <- .typedr_error_call(
        .typedr_factory_call(sys.function())
      )
      .typedr_factory_mode <- !!mode
      .typedr_factory_message <- !!message_expr
      .typedr_factory_predicate <- !!predicate_expr
      f_call <- substitute(!!f_call)
      # remove if empty
      f_call <- Filter(function(value) !identical(value, expr(expr = )), f_call)

      footer <- !!dots_call
      fast_enabled <- .typedr_fast_factory_eligible(
        !!typedr_fast_expr,
        f_call,
        footer
      )
      fast_args <- if (fast_enabled) {
        .typedr_fast_call_args(f_call)
      } else {
        list()
      }

      fast_header <- NULL
      if (fast_enabled) {
        fast_header <- .typedr_fast_assertion_header(
          !!typedr_fast_expr,
          fast_args
        )
      }

      header <- call2(
        "{",
        expr(f <- !!f), # so the substituted definition is readable
        substitute(
          value <- .typedr_run_assertion_check(
            function() F_CALL,
            value,
            mode = MODE,
            message = MESSAGE,
            predicate = PREDICATE,
            call = .typedr_assertion_call
          ),
          list(
            F_CALL = f_call,
            MODE = .typedr_factory_mode,
            MESSAGE = .typedr_factory_message,
            PREDICATE = .typedr_factory_predicate
          )
        )
      )

      body_parts <- list(fast_header, header, footer, expr(value))
      body_parts <- body_parts[!vapply(body_parts, is.null, logical(1))]
      body <- as.call(c(list(as.name("{")), body_parts))
      assertion <- new_function(pairlist2(value = ), body)
      class(assertion) <- c("typedr_assertion", "function")
      assertion
    })
  )

  class(res) <- "assertion_factory"
  attr(res, "typedr_type_function") <- f
  environment(res) <- caller_env()
  res
}

.typedr_factory_call <- function(factory) {
  env <- environment(factory)
  if (is_null(env)) {
    return(NULL)
  }

  nms <- names(env)
  for (nm in nms) {
    candidate <- try_fetch(
      get(nm, envir = env, inherits = FALSE),
      error = identity
    )
    if (!inherits(candidate, "error") && identical(candidate, factory)) {
      return(call2(nm))
    }
  }

  NULL
}

.typedr_predicate_label <- function(f) {
  body <- body(f)
  if (is_null(body)) {
    return(NULL)
  }

  expr <- if (is_call(body, "{")) {
    parts <- as.list(body)
    if (length(parts) < 2L) {
      return(NULL)
    }
    parts[[length(parts)]]
  } else {
    body
  }

  .typedr_deparse(expr)
}

.typedr_run_assertion_check <- function(
  check,
  value,
  mode = c("auto", "assertion", "predicate"),
  message = NULL,
  predicate = NULL,
  call = caller_env()
) {
  mode <- match.arg(mode)
  has_message <- !is_null(message)

  result <- try_fetch(check(), error = identity)
  if (inherits(result, "error")) {
    if (inherits(result, "typedr_error")) {
      if (is_call(call)) {
        result$call <- .typedr_error_call(call)
        attr(result$call, "srcref") <- NULL
      }
      rlang::cnd_signal(result)
    }
    result_message <- conditionMessage(result)
    bullets <- if (has_message) {
      if (identical(message, result_message)) {
        message
      } else {
        c(
          message,
          "i" = result_message
        )
      }
    } else {
      result_message
    }
    cli_abort(
      bullets,
      class = c(
        "typedr_custom_assertion_error",
        "typedr_assertion_error",
        "typedr_error"
      ),
      call = .typedr_error_call(call)
    )
  }
  message <- message %||% "Custom assertion failed."

  is_predicate <- is_logical(result) &&
    length(result) == 1L &&
    (mode == "predicate" || (mode == "auto" && !identical(result, value)))

  if (is_predicate) {
    if (isTRUE(result)) {
      return(value)
    }
    predicate_line <- if (!is_null(predicate)) {
      sprintf(
        "`%s` evaluated to %s.",
        .typedr_truncate_text(predicate),
        .typedr_deparse(result)
      )
    } else {
      sprintf("custom predicate evaluated to %s.", .typedr_deparse(result))
    }
    cli_abort(
      c(
        message,
        "x" = predicate_line,
        "i" = sprintf("value: %s", .typedr_deparse(value))
      ),
      class = c(
        "typedr_custom_assertion_error",
        "typedr_assertion_error",
        "typedr_error"
      ),
      call = .typedr_error_call(call)
    )
  }

  if (mode == "predicate") {
    cli_abort(
      c(
        "Custom assertion predicate must return a scalar logical.",
        "x" = .typedr_compare(
          typeof(result),
          "logical",
          x_arg = "typeof(result)",
          y_arg = "expected"
        )
      ),
      class = c(
        "typedr_custom_assertion_error",
        "typedr_assertion_error",
        "typedr_error"
      ),
      call = .typedr_error_call(call)
    )
  }

  result
}

#' Process additional assertion conditions
#'
#' Developer helper used by assertion factories to turn additional conditions in
#' `...` into checking expressions. It is exported for generated typedr code and
#' is not intended for direct interactive use.
#'
#' @param ... Additional assertion conditions. Named arguments compare
#'   `name(value)` with the supplied value; unnamed arguments must be formulas.
#' @param .typedr_assertion_call Internal assertion call used in generated error
#'   messages.
#' @return A `{` expression containing the generated checks, or `NULL` when no
#'   conditions are supplied.
#' @export
process_assertion_factory_dots <- function(..., .typedr_assertion_call = NULL) {
  args <- list(...)
  if (!length(args)) {
    return(NULL)
  }
  assertion_call <- if (is_null(.typedr_assertion_call)) {
    quote(NULL)
  } else {
    call2("quote", .typedr_assertion_call)
  }
  nms <- names2(args)
  exprs <- vector("list", length(args))
  for (i in seq_along(args)) {
    ## is the ith argument named ?
    if (!nms[[i]] %in% c("", "...")) {
      fun <- sym(nms[[i]])
      exprs[[i]] <- bquote(
        if (!identical(.(fun)(value), .(args[[i]]))) {
          cli::cli_abort(
            c(
              .(paste0("`", nms[[i]], "` mismatch")),
              "x" = .typedr_compare(
                .(fun)(value),
                .(args[[i]]),
                x_arg = .(paste0(nms[[i]], "(value)")),
                y_arg = "expected"
              )
            ),
            class = c(
              "typedr_custom_assertion_error",
              "typedr_assertion_error",
              "typedr_error"
            ),
            call = .(assertion_call)
          )
        }
      )
    } else {
      ## is it not a formula ?
      if (!is_call(args[[i]], "~")) {
        cli_abort(
          "Assertions should be either named functions or unnamed formulas.",
          class = c(
            "typedr_input_error",
            "typedr_assertion_factory_error",
            "typedr_error"
          )
        )
      }
      ## is it a 2 sided formula ?
      if (length(args[[i]]) == 3) {
        error <- args[[i]][[2]]
        assertion <- do.call(
          substitute,
          list(args[[i]][[3]], list(. = quote(value)))
        )
      } else {
        error <- "mismatch"
        assertion <- do.call(
          substitute,
          list(args[[i]][[2]], list(. = quote(value)))
        )
      }

      exprs[[i]] <- bquote(
        if (!.(assertion)) {
          cli::cli_abort(
            c(
              .(error),
              "x" = .typedr_compare(
                FALSE,
                TRUE,
                x_arg = .(deparse1(assertion)),
                y_arg = "expected"
              )
            ),
            class = c(
              "typedr_custom_assertion_error",
              "typedr_assertion_error",
              "typedr_error"
            ),
            call = .(assertion_call)
          )
        }
      )
    }
  }
  exprs
  call2("{", !!!exprs)
}

infer_implicit_assignment_call <- function(value) {
  # note : attr(, "class") is different from class()
  cl <- class(value)
  if (identical(cl, c("POSIXct", "POSIXt"))) {
    return(expr(Time()))
  }
  if (inherits(value, "Date")) {
    return(expr(Date()))
  }
  if (is.matrix(value)) {
    return(expr(Matrix()))
  }
  if (is.array(value)) {
    return(expr(Array()))
  }
  if (is_null(value)) {
    return(expr(Null()))
  }
  if (is_pairlist(value)) {
    return(expr(Pairlist()))
  }
  if (is_call(value)) {
    return(expr(Language()))
  }

  if (is_atomic(value) && is_null(attr(value, "class"))) {
    assertion_call <- switch(
      typeof(value),
      "logical" = expr(Logical()),
      "integer" = expr(Integer()),
      "double" = expr(Double()),
      "complex" = expr(Any(typeof = "complex")),
      "character" = expr(Character()),
      "raw" = expr(Raw())
    )
    return(assertion_call)
  }
  if (length(cl) == 1) {
    assertion_call <- switch(
      cl,
      "list" = expr(List()),
      "NULL" = expr(Null()),
      "function" = expr(Function()),
      "environment" = expr(Environment()),
      "name" = expr(Symbol()),
      "pairlist" = expr(Pairlist()),
      "language" = expr(Language()),
      "expression" = expr(Expression()),
      "factor" = expr(Factor()),
      "data.frame" = expr(Data.frame()),
      "matrix" = expr(Matrix()),
      "array" = expr(Array()),
      "date" = expr(Date()),
      call2("Any", class = cl)
    )
    return(assertion_call)
  }
  call2("Any", class = cl)
}

get_assertion <- function(x) {
  x <- as.character(substitute(x))
  find_assertion_call <- function(node) {
    if (!is_call(node)) {
      return(NULL)
    }

    if (
      length(node) >= 2 &&
        is_call(node[[1]]) &&
        any(vapply(
          as.list(node)[-1],
          identical,
          logical(1),
          quote(assigned_value)
        ))
    ) {
      return(node[[1]])
    }

    for (child in as.list(node)) {
      found <- find_assertion_call(child)
      if (!is_null(found)) {
        return(found)
      }
    }

    NULL
  }

  assertion <- try_fetch(
    find_assertion_call(body(activeBindingFunction(x, parent.frame()))),
    error = function(e) {
      cli_abort(
        "Can't retrieve assertion for `{.field {x}}`.",
        class = c("typedr_get_assertion_error", "typedr_error"),
        parent = e
      )
    }
  )

  if (is_null(assertion)) {
    cli_abort(
      "Can't retrieve assertion for `{.field {x}}`.",
      i = "The active binding body does not contain a typedr assertion call.",
      class = c("typedr_get_assertion_error", "typedr_error")
    )
  }

  assertion
}
