#' Print typedr objects with expanded output
#'
#' typedr extends the print output inherited from the original typed interface
#' with cli-formatted summaries for typed functions, assertion factories,
#' assertions, and typed values. These helpers call typedr's print methods with
#' convenient presets. They are useful when the default output is truncated or
#' when you want to inspect all typed arguments, the whole rewritten function
#' body, or value metadata.
#'
#' @param x A typedr function or typedr value. Defaults to `.Last.value`.
#' @param ... Additional arguments passed to `print()`.
#' @param max_args Maximum number of typed arguments to display.
#' @param fn_indent Number of spaces used to indent function bodies.
#' @param fn_wrap Width used when wrapping function body output.
#' @param fn_limit_lines Maximum number of function body lines to display.
#' @param fn_lineno Whether to show function body line numbers.
#' @param fn_color Whether to colorize function body output.
#' @param highlight Syntax highlight style, usually a named list such as
#'   `vsc_dark_plus()`.
#' @return `x`, invisibly.
#'
#' @name print_typedr
#' @rdname print_typedr
#' @export
print_all_args <- function(x = .Last.value, ...) {
  check_function(x)

  old <- options(typedr.print.max_args = .Machine$integer.max)
  on.exit(options(old), add = TRUE)
  print(x, ...)
  invisible(x)
}

#' @rdname print_typedr
#' @export
print_whole_fn <- function(x = .Last.value, ...) {
  check_function(x)

  old <- options(typedr.print.fn_limit_lines = .Machine$integer.max)
  on.exit(options(old), add = TRUE)
  print(x, ...)
  invisible(x)
}

#' @rdname print_typedr
#' @export
print_whole_value <- function(x = .Last.value, ...) {
  print(x, ..., max_items = .Machine$integer.max, full_value = TRUE)
  invisible(x)
}

#' @rdname print_typedr
#' @export
print_stats <- function(x = .Last.value) {
  check_function(x)
  .stats_typedr_fn(x)
}

#' @rdname print_typedr
#' @export
print_typedr <- function(
  x = .Last.value,
  ...,
  max_args = getOption("typedr.print.max_args", 8),
  fn_indent = getOption("typedr.print.fn_indent", 2),
  fn_wrap = getOption("typedr.print.fn_wrap", 60),
  fn_limit_lines = getOption("typedr.print.fn_limit_lines", 20),
  fn_lineno = getOption("typedr.print.fn_lineno", FALSE),
  fn_color = getOption("typedr.print.fn_color", TRUE),
  highlight = getOption("typedr.print.highlight", vsc_dark_plus())
) {
  options(
    typedr.print.max_args = max_args,
    typedr.print.fn_indent = fn_indent,
    typedr.print.fn_wrap = fn_wrap,
    typedr.print.fn_limit_lines = fn_limit_lines,
    typedr.print.fn_lineno = fn_lineno,
    typedr.print.fn_color = fn_color,
    typedr.print.highlight = highlight
  )

  print(x, ...)
  invisible(x)
}
