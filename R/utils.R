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

.warn_once <- function(id, msg, type = c("warn", "tips")) {
  type <- arg_match(type)
  if (isTRUE(.typedr_state$warned_once[[id]])) {
    return(invisible(NULL))
  }
  if (type == "warn") {
    cli_warn(msg)
  } else {
    cli_bullets(col_grey(msg))
  }
  .typedr_state$warned_once[[id]] <- TRUE
  invisible(NULL)
}

.capitalize <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

.format_type <- function(x) {
  typedr <- .capitalize(typeof(x))
  format_inline("{.cls {typedr}()}")
}

.typedr_deparse <- function(x) {
  paste(deparse(x, width.cutoff = 60L), collapse = "\n")
}

.typedr_compare <- function(actual, expected, x_arg, y_arg = "expected") {
  paste0(
    "`", x_arg, "`: ", col_green(.typedr_deparse(actual)), "\n",
    "`", y_arg, "`: ", col_green(.typedr_deparse(expected))
  )
}

.typedr_assertion_error_class <- function(message) {
  header <- unname(message[[1]])
  switch(header,
    "type mismatch" = "typedr_type_mismatch",
    "length mismatch" = "typedr_length_mismatch",
    "Row number mismatch" = "typedr_shape_mismatch",
    "Column number mismatch" = "typedr_shape_mismatch",
    "dimension mismatch" = "typedr_shape_mismatch",
    "`value` can't be NULL" = "typedr_null_mismatch",
    "typedr_assertion_mismatch"
  )
}

.typedr_abort_assertion <- function(message,
                                     class = NULL,
                                     parent = NULL,
                                     call = caller_env()) {
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

.apply_typedr_attrs <- function(val, name, assertion_call, const) {
  structure(
    val,
    typedr_name = name,
    typedr_assertion = assertion_call,
    typedr_const = isTRUE(const),
    class = c("typedr_value", class(val))
  )
}
