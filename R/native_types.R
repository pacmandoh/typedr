#' Assertion factories provided by typedr
#'
#' @description
#'
#' These functions are assertion factories: they produce assertions that take an
#' object, check conditions, and return the input, usually unmodified. The
#' assertion factories documented here never modify their inputs.
#'
#' The factory names and the general assertion-factory pattern follow the
#' original [typed](https://github.com/moodymudskipper/typed) package. typedr
#' keeps that interface and modernizes the implementation with rlang predicates,
#' cli errors, and typed condition classes.
#'
#' Additional conditions can be provided through `...`:
#'
#' * Named arguments should name a predicate or accessor to call on the checked
#'   object, with the argument value giving the expected result.
#' * Unnamed arguments should be formulas. The right-hand side is a condition
#'   that can use `value` or `.` as a placeholder for the checked object. In
#'   two-sided formulas, the left-hand side is used as the error message.
#'
#' `Any()` is the most general assertion factory: it doesn't check anything
#' unless additional conditions are provided through `...`. Other assertion
#' factories use the relevant base `is.<type>` function when available, check
#' `typeof()` for atomic types, or check that the class of the checked value
#' contains the relevant class.
#'
#' See advanced examples at the bottom, including uses of `Symbol()` and `Dots()`.
#'
#' @param length Required length of the checked object.
#' @param nrow Required number of rows.
#' @param ncol Required number of columns.
#' @param each Assertion that every item or column must satisfy.
#' @param dim Required dimensions.
#' @param levels Required factor levels.
#' @param data_frame_ok Whether data frames should be accepted by `List()`.
#' @param null_ok Whether `NULL` values should be accepted and not subjected to
#'   any further check.
#' @param ... Additional conditions, see details.
#'
#' @export
#' @return A typedr assertion function.
#' @name assertion_factories
#' @rdname assertion_factories
#' @include new_type.R
#' @examples
#' \dontrun{
#' # fails
#' Integer() ? x <- 1
#' # equivalent to
#' declare("x", Integer(), value = 1)
#'
#' Integer(2) ? x <- 1L
#'
#' # we can use additional conditions in `...`
#' Integer(anyNA = FALSE) ? x <- c(1L, NA, 1L)
#' Integer(anyDuplicated = 0L) ? x <- c(1L, NA, 1L)
#' }
#'
#' Integer(2) ? x <- 11:12
#'
#' \dontrun{
#' # We can also use it directly to test assertions
#' Integer() ? x <- 1
#' # equivalent to
#' declare("x", Integer(), value = 1)
#'
#' Integer(2) ? x <- 1L
#' }
#'
#' \dontrun{
#' # If we want to restrict the quoted expression rather than the value of an
#' # argument, we can use `?~` :
#' identity_sym_only <- ? function(x = ?~ Symbol()) {
#'   x
#' }
#'
#' a <- 1
#' identity_sym_only(a)
#' identity_sym_only(a + a)
#'
#' identity_sym_only
#' }
#'
#' \dontrun{
#' integer_list <- ? function(... = ? Integer()) {
#'   list(...)
#' }
#'
#' integer_list(1L, 2L, "a")
#'
#' integer_pair <- ? function(... = ? Dots(2, each = Integer())) {
#'   list(...)
#' }
#'
#' integer_pair(1L, 2L, 3L)
#' integer_pair(1L, "a", "a")
#'
#' x <- 1
#' y <- 2
#' symbol_list1 <- ? function(... = ? Dots(2, Symbol())) {
#'   list(...)
#' }
#' symbol_list1(quote(x), quote(y))
#' symbol_list1(x, y)
#'
#' symbol_list2 <- ? function(... = ?~ Dots(2, Symbol())) {
#'   list(...)
#' }
#' symbol_list2(x, x + y)
#' symbol_list2(x, y)
#' }
Any <- as_assertion_factory(function(value, length = NULL) {
  if (!is_null(length) && length(value) != length) {
    length <- as.integer(length)
    .typedr_abort_assertion(c(
      "length mismatch",
      "x" = .typedr_compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Logical <- as_assertion_factory(function(
  value,
  length = NULL,
  null_ok = FALSE
) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!is_logical(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "logical",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  if (!is_null(length) && length(value) != length) {
    length <- as.integer(length)
    .typedr_abort_assertion(c(
      "length mismatch",
      "x" = .typedr_compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Integer <- as_assertion_factory(function(
  value,
  length = NULL,
  null_ok = FALSE
) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!is_integer(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "integer",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  if (!is_null(length) && length(value) != length) {
    length <- as.integer(length)
    .typedr_abort_assertion(c(
      "length mismatch",
      "x" = .typedr_compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Double <- as_assertion_factory(function(value, length = NULL, null_ok = FALSE) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!is_double(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "double",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  if (!is_null(length) && length(value) != length) {
    length <- as.integer(length)
    .typedr_abort_assertion(c(
      "length mismatch",
      "x" = .typedr_compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Character <- as_assertion_factory(function(
  value,
  length = NULL,
  null_ok = FALSE
) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!is_character(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "character",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  if (!is_null(length) && length(value) != length) {
    length <- as.integer(length)
    .typedr_abort_assertion(c(
      "length mismatch",
      "x" = .typedr_compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Raw <- as_assertion_factory(function(value, length = NULL, null_ok = FALSE) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!is_raw(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "raw",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  if (!is_null(length) && length(value) != length) {
    length <- as.integer(length)
    .typedr_abort_assertion(c(
      "length mismatch",
      "x" = .typedr_compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
List <- as_assertion_factory(function(
  value,
  length = NULL,
  each,
  data_frame_ok = TRUE,
  null_ok = FALSE
) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!is_list(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "list",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  if (!is_null(length) && length(value) != length) {
    length <- as.integer(length)
    .typedr_abort_assertion(c(
      "length mismatch",
      "x" = .typedr_compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"
      )
    ))
  }

  if (!is_missing(each)) {
    nms <- names2(value)
    for (i in seq_along(value)) {
      try_fetch(each(value[[i]]), error = function(e) {
        if (nms[[i]] == "") {
          .typedr_abort_assertion(
            sprintf("element %s failed assertion.", i),
            class = "typedr_element_error",
            parent = e
          )
        } else {
          .typedr_abort_assertion(
            sprintf('element %s ("%s") failed assertion.', i, nms[[i]]),
            class = "typedr_element_error",
            parent = e
          )
        }
      })
    }
  }

  if (!data_frame_ok && is.data.frame(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        is.data.frame(value),
        FALSE,
        x_arg = "is.data.frame(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Null <- as_assertion_factory(function(value) {
  if (!is_null(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "NULL",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Closure <- as_assertion_factory(function(value, null_ok = FALSE) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (typeof(value) != "closure") {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "closure",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Special <- as_assertion_factory(function(value, null_ok = FALSE) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (typeof(value) != "special") {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "special",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Builtin <- as_assertion_factory(function(value, null_ok = FALSE) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (typeof(value) != "builtin") {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "builtin",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Environment <- as_assertion_factory(function(value, null_ok = FALSE) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!is_environment(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "environment",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Symbol <- as_assertion_factory(function(value, null_ok = FALSE) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!is_symbol(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "symbol",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Pairlist <- as_assertion_factory(function(
  value,
  length = NULL,
  each,
  null_ok = TRUE
) {
  if (is_null(value)) {
    if (null_ok) {
      return(NULL)
    } else {
      .typedr_abort_assertion("`value` can't be NULL")
    }
  }
  if (!is_pairlist(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "pairlist",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  if (!is_missing(each)) {
    nms <- names2(value)
    for (i in seq_along(value)) {
      try_fetch(each(value[[i]]), error = function(e) {
        if (nms[[i]] == "") {
          .typedr_abort_assertion(
            sprintf("element %s failed assertion.", i),
            class = "typedr_element_error",
            parent = e
          )
        } else {
          .typedr_abort_assertion(
            sprintf('element %s ("%s") failed assertion.', i, nms[[i]]),
            class = "typedr_element_error",
            parent = e
          )
        }
      })
    }
  }
  if (!is_null(length) && length(value) != length) {
    length <- as.integer(length)
    .typedr_abort_assertion(c(
      "length mismatch",
      "x" = .typedr_compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Language <- as_assertion_factory(function(value, null_ok = FALSE) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (typeof(value) != "language") {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "language",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Expression <- as_assertion_factory(function(
  value,
  length = NULL,
  null_ok = FALSE
) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (typeof(value) != "expression") {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        typeof(value),
        "expression",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    ))
  }
  if (!is_null(length) && length(value) != length) {
    length <- as.integer(length)
    .typedr_abort_assertion(c(
      "length mismatch",
      "x" = .typedr_compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

# function, factor, matrix, array, data.frame, date, time

#' @export
#' @rdname assertion_factories
Function <- as_assertion_factory(function(value, null_ok = FALSE) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!is_function(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        is_function(value),
        TRUE,
        x_arg = "is_function(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Factor <- as_assertion_factory(function(
  value,
  length = NULL,
  levels,
  null_ok = FALSE
) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!is.factor(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        is.factor(value),
        TRUE,
        x_arg = "is.factor(value)",
        y_arg = "expected"
      )
    ))
  }
  if (!is_null(length) && length(value) != length) {
    length <- as.integer(length)
    .typedr_abort_assertion(c(
      "length mismatch",
      "x" = .typedr_compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"
      )
    ))
  }
  if (!is_missing(levels) && !identical(levels(value), levels)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        levels(value),
        levels,
        x_arg = "levels(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Data.frame <- as_assertion_factory(function(
  value,
  nrow,
  ncol,
  each,
  null_ok = FALSE
) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!is.data.frame(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        class(value),
        "data.frame",
        x_arg = "class(value)",
        y_arg = "expected to contain"
      )
    ))
  }
  if (!is_missing(nrow) && nrow(value) != nrow) {
    nrow <- as.integer(nrow)
    .typedr_abort_assertion(c(
      "Row number mismatch",
      "x" = .typedr_compare(
        nrow(value),
        nrow,
        x_arg = "nrow(value)",
        y_arg = "expected"
      )
    ))
  }
  if (!is_missing(ncol) && ncol(value) != ncol) {
    ncol <- as.integer(ncol)
    .typedr_abort_assertion(c(
      "Column number mismatch",
      "x" = .typedr_compare(
        ncol(value),
        ncol,
        x_arg = "ncol(value)",
        y_arg = "expected"
      )
    ))
  }

  if (!is_missing(each)) {
    nms <- names2(value)
    for (i in seq_along(value)) {
      try_fetch(each(value[[i]]), error = function(e) {
        .typedr_abort_assertion(
          sprintf('column %s ("%s") failed assertion.', i, nms[[i]]),
          class = "typedr_column_error",
          parent = e
        )
      })
    }
  }
  value
})

#' @export
#' @rdname assertion_factories
Matrix <- as_assertion_factory(function(value, nrow, ncol, null_ok = FALSE) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!is.matrix(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        class(value),
        "matrix",
        x_arg = "class(value)",
        y_arg = "expected to contain"
      )
    ))
  }
  if (!is_missing(nrow) && nrow(value) != nrow) {
    nrow <- as.integer(nrow)
    .typedr_abort_assertion(c(
      "Row number mismatch",
      "x" = .typedr_compare(
        nrow(value),
        nrow,
        x_arg = "nrow(value)",
        y_arg = "expected"
      )
    ))
  }
  if (!is_missing(ncol) && ncol(value) != ncol) {
    ncol <- as.integer(ncol)
    .typedr_abort_assertion(c(
      "Column number mismatch",
      "x" = .typedr_compare(
        ncol(value),
        ncol,
        x_arg = "ncol(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Array <- as_assertion_factory(function(value, dim, null_ok = FALSE) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!is.array(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        is.array(value),
        TRUE,
        x_arg = "is.array(value)",
        y_arg = "expected"
      )
    ))
  }
  if (!is_missing(dim) && !identical(dim(value), as.integer(dim))) {
    dim <- as.integer(dim)
    .typedr_abort_assertion(c(
      "dimension mismatch",
      "x" = .typedr_compare(
        dim(value),
        dim,
        x_arg = "dim(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Date <- as_assertion_factory(function(value, length = NULL, null_ok = FALSE) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!"Date" %in% class(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        class(value),
        "Date",
        x_arg = "class(value)",
        y_arg = "expected to contain"
      )
    ))
  }

  if (!is_null(length) && length(value) != length) {
    length <- as.integer(length)
    .typedr_abort_assertion(c(
      "length mismatch",
      "x" = .typedr_compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Time <- as_assertion_factory(function(value, length = NULL, null_ok = FALSE) {
  if (null_ok && is_null(value)) {
    return(NULL)
  }
  if (!"POSIXct" %in% class(value)) {
    .typedr_abort_assertion(c(
      "type mismatch",
      "x" = .typedr_compare(
        class(value),
        "POSIXct",
        x_arg = "class(value)",
        y_arg = "expected to contain"
      )
    ))
  }

  if (!is_null(length) && length(value) != length) {
    length <- as.integer(length)
    .typedr_abort_assertion(c(
      "length mismatch",
      "x" = .typedr_compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"
      )
    ))
  }
  value
})

#' @export
#' @rdname assertion_factories
Dots <- as_assertion_factory(function(value, length = NULL, each) {
  if (!is_null(length) && length(value) != length) {
    length <- as.integer(length)
    .typedr_abort_assertion(c(
      "length mismatch",
      "x" = .typedr_compare(
        length(value),
        length,
        x_arg = "length(value)",
        y_arg = "expected"
      )
    ))
  }

  if (!is_missing(each)) {
    nms <- names2(value)
    for (i in seq_along(value)) {
      try_fetch(each(value[[i]]), error = function(e) {
        if (nms[[i]] == "") {
          .typedr_abort_assertion(
            sprintf("element %s failed assertion.", i),
            class = "typedr_element_error",
            parent = e
          )
        } else {
          .typedr_abort_assertion(
            sprintf('element %s ("%s") failed assertion.', i, nms[[i]]),
            class = "typedr_element_error",
            parent = e
          )
        }
      })
    }
  }
  value
})
