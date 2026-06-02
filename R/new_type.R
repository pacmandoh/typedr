#' Build an assertion factory
#'
#' `as_assertion_factory()` wraps a checking function and turns it into a typedr
#' assertion factory. The wrapped function must take the value to check as its
#' first argument and return the checked value, or throw an error if the value is
#' invalid.
#'
#' @param f A function whose first argument is the value to check. Additional
#'   arguments become arguments of the assertion factory.
#' @return A function with class `assertion_factory`.
#'
#' @export
#' @importFrom rlang call2 expr fn_fmls_syms fn_fmls new_function pairlist2 is_null caller_env names2 sym try_fetch
#' @importFrom cli cli_abort
as_assertion_factory <- function(f) {
  # create a function with arguments being the additional args to f and dots
  f_call <- call2(expr(f), expr(value), !!!fn_fmls_syms(f)[-1])
  dots_call <- call2("process_assertion_factory_dots", sym("..."))

  res <- new_function(
    pairlist2(!!!fn_fmls(f)[-1], ... = ),
    expr({
      f_call <- substitute(!!f_call)
      # remove if empty
      f_call <- Filter(function(value) !identical(value, expr(expr = )), f_call)

      header <- call2(
        "{",
        expr(f <- !!f), # so the substituted definition is readable
        substitute(value <- F_CALL, list(F_CALL = f_call))
      )

      # the footer is made of additional assertions derived from `...`
      footer <- !!dots_call

      if (is_null(footer)) {
        body <- call2("{", header, expr(value))
      } else {
        body <- call2("{", header, footer, expr(value))
      }
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

#' Process additional assertion conditions
#'
#' Developer helper used by assertion factories to turn additional conditions in
#' `...` into checking expressions. It is exported for generated typedr code and
#' is not intended for direct interactive use.
#'
#' @param ... Additional assertion conditions. Named arguments compare
#'   `name(value)` with the supplied value; unnamed arguments must be formulas.
#' @return A `{` expression containing the generated checks, or `NULL` when no
#'   conditions are supplied.
#' @export
process_assertion_factory_dots <- function(...) {
  args <- list(...)
  if (!length(args)) {
    return(NULL)
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
            )
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
            )
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
      "matrix" = expr(Matrix()),
      call2("Any", class = cl)
    )
    return(assertion_call)
  }
  call2("Any", class = cl)
}

get_assertion <- function(x) {
  x <- as.character(substitute(x))
  find_assertion_call <- function(node) {
    if (!is.call(node)) {
      return(NULL)
    }

    if (
      length(node) >= 2 &&
        is.call(node[[1]]) &&
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
