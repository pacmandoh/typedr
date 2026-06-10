.typedr_state <- new.env(parent = emptyenv())

.typedr_state$warned_once <- list()
.typedr_state$type_printers <- list()

#' Register a custom value printer for a typedr type
#'
#' Use `type_printer()` to customize how typed values are displayed by typedr's
#' print methods.
#'
#' @param type An assertion factory, such as `Character`, or an assertion call,
#'   such as `Character()`.
#' @param printer A function called as `printer(value, max_items = max_items)`.
#' @return `printer`, invisibly.
#'
#' @export
type_printer <- function(type, printer) {
  type_expr <- enexpr(type)
  check_function(printer)

  key <- .typedr_type_printer_key(type_expr)
  .typedr_state$type_printers[[key]] <- printer

  invisible(printer)
}

.typedr_type_printer_key <- function(type) {
  if (is_call(type)) {
    return(call_name(type))
  }
  if (is_symbol(type)) {
    return(as_name(type))
  }
  if (is_character(type) && length(type) == 1L) {
    return(type)
  }
  cli_abort(
    "Can't derive a type printer key.",
    class = c("typedr_type_printer_error", "typedr_error")
  )
}

.typedr_get_type_printer <- function(assertion) {
  if (is_null(assertion)) {
    return(NULL)
  }
  key <- .typedr_type_printer_key(assertion)
  .typedr_state$type_printers[[key]]
}

.warn_once <- function(id, msg, type = c("warn", "tips"), call = caller_env()) {
  type <- arg_match(type)
  if (isTRUE(.typedr_state$warned_once[[id]])) {
    return(invisible(NULL))
  }
  if (type == "warn") {
    .cli_warn_bullets(msg, call = call)
  } else {
    cli_bullets(msg)
  }
  .typedr_state$warned_once[[id]] <- TRUE
  invisible(NULL)
}

.cli_warn_bullets <- function(
  msg,
  call = caller_env(),
  class = NULL,
  .envir = parent.frame()
) {
  formatted <- format_warning(msg, .envir = .envir)
  lines <- strsplit(formatted, "\n", fixed = TRUE)[[1]]
  if (length(lines) > 1L) {
    lines[-1L] <- paste0("  ", lines[-1L])
    formatted <- paste(lines, collapse = "\n")
  }
  rlang::warn(formatted, class = class, call = call)
}

.capitalize <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

.format_type <- function(x) {
  typedr <- .capitalize(typeof(x))
  format_inline("{.cls {typedr}()}")
}

.typedr_compare <- function(actual, expected, x_arg, y_arg = "expected") {
  paste0(
    "`",
    .typedr_truncate_text(x_arg, max_chars = 40L),
    "`: ",
    col_green(.typedr_deparse(actual)),
    ", ",
    "`",
    .typedr_truncate_text(y_arg, max_chars = 40L),
    "`: ",
    col_green(.typedr_deparse(expected))
  )
}

.typedr_assertion_error_class <- function(message) {
  header <- unname(message[[1]])
  switch(
    header,
    "type mismatch" = "typedr_type_mismatch",
    "length mismatch" = "typedr_length_mismatch",
    "Row number mismatch" = "typedr_shape_mismatch",
    "Column number mismatch" = "typedr_shape_mismatch",
    "dimension mismatch" = "typedr_shape_mismatch",
    "`value` can't be NULL" = "typedr_null_mismatch",
    "typedr_assertion_mismatch"
  )
}

.typedr_abort_assertion <- function(
  message,
  class = NULL,
  parent = NULL,
  call = caller_env()
) {
  if (is_null(class)) {
    class <- .typedr_assertion_error_class(message)
  }

  cli_abort(
    message,
    class = c(class, "typedr_assertion_error", "typedr_error"),
    parent = parent,
    call = call
  )
}

.is_assign_stmt <- function(expr) {
  is_call(expr, c("<-", "="))
}

.typedr_is_wrapped_null <- function(x) {
  inherits(x, "typedr_null")
}

.typedr_unwrap <- function(x) {
  if (.typedr_is_wrapped_null(x)) {
    return(NULL)
  }
  x
}

.typedr_peel_value <- function(x) {
  if (!inherits(x, "typedr_value")) {
    return(x)
  }
  if (.typedr_is_wrapped_null(x)) {
    return(NULL)
  }
  attr(x, "typedr_name") <- NULL
  attr(x, "typedr_assertion") <- NULL
  attr(x, "typedr_const") <- NULL
  class(x) <- setdiff(class(x), "typedr_value")
  x
}

.typedr_is_interactive <- function() {
  interactive()
}

.typedr_inform_declare_unset <- function(
  x,
  assertion_quoted,
  call = caller_env(),
  value_missing = FALSE
) {
  if (
    !value_missing || !.typedr_is_interactive() || !identical(call, globalenv())
  ) {
    return(invisible(NULL))
  }
  assertion_label <- .typedr_assertion_diagnostic_label(assertion_quoted)
  cli_inform(c(
    "i" = "Declared {.field `{x}`} as {.cls {assertion_label}} ({.emph unset})."
  ))
}

.apply_typedr_attrs <- function(val, name, assertion_call, const) {
  if (is_null(val)) {
    # R deprecates `structure(NULL, *)`; keep metadata on an empty carrier instead.
    out <- structure(list(), class = c("typedr_value", "typedr_null"))
  } else {
    out <- structure(val, class = c("typedr_value", class(val)))
  }

  attr(out, "typedr_name") <- name
  attr(out, "typedr_assertion") <- assertion_call
  attr(out, "typedr_const") <- isTRUE(const)
  out
}
