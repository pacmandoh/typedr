#' Combine typedr assertions
#'
#' typedr assertions can be combined with `|` and `&`.
#' `a | b` accepts a value when either assertion accepts it. `a & b` accepts a
#' value only when both assertions accept it, in order. `c(a, b)` is also
#' supported as a union for users who prefer R's usual combining idiom, but `|`
#' is usually clearer in function signatures.
#'
#' Combined assertions can be used anywhere a regular typedr assertion can be
#' used: variable declarations, function argument annotations, return types,
#' list element checks, and dependent argument guards. Errors from long unions
#' summarize the allowed candidates instead of printing an unbounded list.
#'
#' @param e1,e2 typedr assertions.
#' @param ... typedr assertions.
#' @return A typedr assertion.
#'
#' @name assertion_combinators
#' @rdname assertion_combinators
NULL

typedr_combine_assertions <- function(assertions, operator, exprs = NULL) {
  if (!length(assertions)) {
    cli_abort(
      "At least one typedr assertion is required.",
      class = c(
        "typedr_combinator_error",
        "typedr_assertion_error",
        "typedr_error"
      )
    )
  }

  ok <- vapply(assertions, inherits, logical(1), "typedr_assertion")
  if (!all(ok)) {
    cli_abort(
      "Can only combine typedr assertions.",
      x = "Argument {which(!ok)[[1]]} is not a typedr assertion.",
      class = c(
        "typedr_combinator_error",
        "typedr_assertion_error",
        "typedr_error"
      )
    )
  }

  operator <- match.arg(operator, c("or", "and"))
  labels <- typedr_assertion_labels(assertions, exprs)

  assertion <- switch(
    operator,
    "or" = typedr_union_assertion(assertions, labels),
    "and" = typedr_intersection_assertion(assertions, labels)
  )
  attr(assertion, "typedr_assertion_label") <- paste(
    labels,
    collapse = if (operator == "or") " | " else " & "
  )
  class(assertion) <- c("typedr_assertion", "function")
  assertion
}

typedr_assertion_labels <- function(assertions, exprs = NULL) {
  if (!is_null(exprs)) {
    return(vapply(exprs, expr_deparse, character(1)))
  }

  labels <- vapply(
    assertions,
    function(assertion) {
      label <- attr(assertion, "typedr_assertion_label", exact = TRUE)
      if (is_null(label)) {
        "<typedr type>"
      } else {
        label
      }
    },
    character(1)
  )
  labels
}

typedr_union_assertion <- function(assertions, labels) {
  function(value) {
    for (i in seq_along(assertions)) {
      res <- try_fetch(assertions[[i]](value), error = identity)
      if (!inherits(res, "error")) {
        return(res)
      }
    }

    candidates <- unlist(
      strsplit(labels, " | ", fixed = TRUE),
      use.names = FALSE
    )
    label_summary <- .typedr_summarize_labels(candidates)
    cli_abort(
      c(
        "Value does not satisfy any allowed {.cls Type()}.",
        "x" = "Expected one of: {.cls {label_summary}}."
      ),
      class = c(
        "typedr_union_error",
        "typedr_combinator_error",
        "typedr_assertion_error",
        "typedr_error"
      )
    )
  }
}

typedr_intersection_assertion <- function(assertions, labels) {
  function(value) {
    res <- value
    for (i in seq_along(assertions)) {
      res <- try_fetch(assertions[[i]](res), error = identity)
      if (inherits(res, "error")) {
        constraints <- strsplit(labels[[i]], " & ", fixed = TRUE)[[1]]
        failed_label <- .typedr_summarize_labels(
          constraints,
          separator = " & ",
          max_items = 2L,
          max_chars = 56L
        )
        cli_abort(
          c(
            "Value does not satisfy all required {.cls Type()} constraints.",
            "x" = "Failed constraint: {failed_label}."
          ),
          class = c(
            "typedr_intersection_error",
            "typedr_combinator_error",
            "typedr_assertion_error",
            "typedr_error"
          )
        )
      }
    }
    res
  }
}

#' @export
#' @rdname assertion_combinators
`|.typedr_assertion` <- function(e1, e2) {
  exprs <- list(substitute(e1), substitute(e2))
  typedr_combine_assertions(list(e1, e2), "or", exprs = exprs)
}

#' @export
#' @rdname assertion_combinators
`&.typedr_assertion` <- function(e1, e2) {
  exprs <- list(substitute(e1), substitute(e2))
  typedr_combine_assertions(list(e1, e2), "and", exprs = exprs)
}

#' @export
#' @rdname assertion_combinators
c.typedr_assertion <- function(...) {
  assertions <- list(...)
  typedr_combine_assertions(assertions, "or")
}
