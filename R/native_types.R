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
#' @param each Assertion that every item or column must satisfy. When several
#'   items fail, typedr reports the first location and the number of remaining
#'   failures while preserving the first underlying error as the parent.
#' @param dim Required dimensions.
#' @param levels Required factor levels.
#' @param data_frame_ok Whether data frames should be accepted by `List()`.
#' @param allow_null Whether `NULL` values should be accepted and not subjected to
#'   any further check.
#' @param ... Additional conditions, see details.
#'
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
NULL

.typedr_plural <- function(x) {
  paste0(x, "s")
}

.typedr_each_failure_label <- function(i, name, kind) {
  if (name == "") {
    sprintf("%s %s", kind, i)
  } else {
    name <- .typedr_truncate_text(name, max_chars = 32L)
    sprintf("%s %s (%s)", kind, i, encodeString(name, quote = '"'))
  }
}

.typedr_check_each <- function(
  value,
  each,
  kind = "element",
  class = "typedr_element_error"
) {
  nms <- names2(value)
  first_failure <- NULL
  first_error <- NULL
  failure_count <- 0L

  for (i in seq_along(value)) {
    err <- try_fetch(
      {
        each(value[[i]])
        NULL
      },
      error = identity
    )

    if (inherits(err, "error")) {
      failure_count <- failure_count + 1L
      first_error <- first_error %||% err
      if (is_null(first_failure)) {
        first_failure <- .typedr_each_failure_label(i, nms[[i]], kind)
      }
    }
  }

  if (failure_count == 0L) {
    return(invisible(NULL))
  }

  if (failure_count == 1L) {
    .typedr_abort_assertion(
      sprintf("%s failed assertion.", first_failure),
      class = class,
      parent = first_error
    )
  }

  .typedr_abort_assertion(
    c(
      sprintf(
        "%s %s failed assertion.",
        failure_count,
        .typedr_plural(kind)
      ),
      "x" = sprintf(
        "First failure: %s; and %s more.",
        first_failure,
        failure_count - 1L
      )
    ),
    class = class,
    parent = first_error
  )
}

.typedr_check_typeof <- function(value, expected) {
  if (typeof(value) == expected) {
    return(invisible(NULL))
  }

  .typedr_abort_assertion(c(
    "type mismatch",
    "x" = .typedr_compare(
      typeof(value),
      expected,
      x_arg = "typeof(value)",
      y_arg = "expected"
    )
  ))
}

.typedr_check_length <- function(value, expected) {
  if (is_null(expected) || length(value) == expected) {
    return(invisible(NULL))
  }

  expected <- as.integer(expected)
  .typedr_abort_assertion(c(
    "length mismatch",
    "x" = .typedr_compare(
      length(value),
      expected,
      x_arg = "length(value)",
      y_arg = "expected"
    )
  ))
}

.typedr_check_class <- function(value, expected) {
  if (expected %in% class(value)) {
    return(invisible(NULL))
  }

  .typedr_abort_assertion(c(
    "type mismatch",
    "x" = .typedr_compare(
      class(value),
      expected,
      x_arg = "class(value)",
      y_arg = "expected to contain"
    )
  ))
}

.typedr_check_shape <- function(value, expected_nrow, expected_ncol) {
  if (!is_missing(expected_nrow) && nrow(value) != expected_nrow) {
    expected_nrow <- as.integer(expected_nrow)
    .typedr_abort_assertion(c(
      "Row number mismatch",
      "x" = .typedr_compare(
        nrow(value),
        expected_nrow,
        x_arg = "nrow(value)",
        y_arg = "expected"
      )
    ))
  }

  if (!is_missing(expected_ncol) && ncol(value) != expected_ncol) {
    expected_ncol <- as.integer(expected_ncol)
    .typedr_abort_assertion(c(
      "Column number mismatch",
      "x" = .typedr_compare(
        ncol(value),
        expected_ncol,
        x_arg = "ncol(value)",
        y_arg = "expected"
      )
    ))
  }

  invisible(NULL)
}

#' @export
#' @rdname assertion_factories
Any <- as_assertion_factory(
  function(value, length = NULL) {
    .typedr_check_length(value, length)
    value
  }
)

#' @export
#' @rdname assertion_factories
Logical <- as_assertion_factory(
  function(value, length = NULL, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_typeof(value, "logical")
    .typedr_check_length(value, length)
    value
  }
)

#' @export
#' @rdname assertion_factories
Integer <- as_assertion_factory(
  function(value, length = NULL, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_typeof(value, "integer")
    .typedr_check_length(value, length)
    value
  }
)

#' @export
#' @rdname assertion_factories
Double <- as_assertion_factory(
  function(value, length = NULL, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_typeof(value, "double")
    .typedr_check_length(value, length)
    value
  }
)

#' @export
#' @rdname assertion_factories
Character <- as_assertion_factory(
  function(value, length = NULL, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_typeof(value, "character")
    .typedr_check_length(value, length)
    value
  }
)

#' @export
#' @rdname assertion_factories
Raw <- as_assertion_factory(
  function(value, length = NULL, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_typeof(value, "raw")
    .typedr_check_length(value, length)
    value
  }
)

#' @export
#' @rdname assertion_factories
List <- as_assertion_factory(
  function(
    value,
    length = NULL,
    each,
    data_frame_ok = TRUE,
    allow_null = FALSE
  ) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_typeof(value, "list")
    .typedr_check_length(value, length)

    if (!is_missing(each)) {
      .typedr_check_each(value, each)
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
  }
)

#' @export
#' @rdname assertion_factories
Null <- as_assertion_factory(
  function(value) {
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
  }
)

#' @export
#' @rdname assertion_factories
Closure <- as_assertion_factory(
  function(value, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_typeof(value, "closure")
    value
  }
)

#' @export
#' @rdname assertion_factories
Special <- as_assertion_factory(
  function(value, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_typeof(value, "special")
    value
  }
)

#' @export
#' @rdname assertion_factories
Builtin <- as_assertion_factory(
  function(value, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_typeof(value, "builtin")
    value
  }
)

#' @export
#' @rdname assertion_factories
Environment <- as_assertion_factory(
  function(value, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_typeof(value, "environment")
    value
  }
)

#' @export
#' @rdname assertion_factories
Symbol <- as_assertion_factory(
  function(value, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_typeof(value, "symbol")
    value
  }
)

#' @export
#' @rdname assertion_factories
Pairlist <- as_assertion_factory(
  function(
    value,
    length = NULL,
    each,
    allow_null = TRUE
  ) {
    if (is_null(value)) {
      if (allow_null) {
        return(NULL)
      } else {
        .typedr_abort_assertion("`value` can't be NULL")
      }
    }
    .typedr_check_typeof(value, "pairlist")
    if (!is_missing(each)) {
      .typedr_check_each(value, each)
    }
    .typedr_check_length(value, length)
    value
  }
)

#' @export
#' @rdname assertion_factories
Language <- as_assertion_factory(
  function(value, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_typeof(value, "language")
    value
  }
)

#' @export
#' @rdname assertion_factories
Expression <- as_assertion_factory(
  function(
    value,
    length = NULL,
    allow_null = FALSE
  ) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_typeof(value, "expression")
    .typedr_check_length(value, length)
    value
  }
)

#' @export
#' @rdname assertion_factories
Function <- as_assertion_factory(
  function(value, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
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
  }
)

#' @export
#' @rdname assertion_factories
Factor <- as_assertion_factory(
  function(
    value,
    length = NULL,
    levels,
    allow_null = FALSE
  ) {
    if (allow_null && is_null(value)) {
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
    .typedr_check_length(value, length)
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
  }
)

#' @export
#' @rdname assertion_factories
Data.frame <- as_assertion_factory(
  function(value, nrow, ncol, each, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_class(value, "data.frame")
    .typedr_check_shape(value, nrow, ncol)

    if (!is_missing(each)) {
      .typedr_check_each(
        value,
        each,
        kind = "column",
        class = "typedr_column_error"
      )
    }
    value
  }
)

#' @export
#' @rdname assertion_factories
Matrix <- as_assertion_factory(
  function(value, nrow, ncol, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_class(value, "matrix")
    .typedr_check_shape(value, nrow, ncol)
    value
  }
)

#' @export
#' @rdname assertion_factories
Array <- as_assertion_factory(
  function(value, dim, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
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
  }
)

#' @export
#' @rdname assertion_factories
Date <- as_assertion_factory(
  function(value, length = NULL, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_class(value, "Date")
    .typedr_check_length(value, length)
    value
  }
)

#' @export
#' @rdname assertion_factories
Time <- as_assertion_factory(
  function(value, length = NULL, allow_null = FALSE) {
    if (allow_null && is_null(value)) {
      return(NULL)
    }
    .typedr_check_class(value, "POSIXct")
    .typedr_check_length(value, length)
    value
  }
)

#' @export
#' @rdname assertion_factories
Dots <- as_assertion_factory(
  function(value, length = NULL, each) {
    .typedr_check_length(value, length)

    if (!is_missing(each)) {
      .typedr_check_each(value, each)
    }
    value
  }
)
