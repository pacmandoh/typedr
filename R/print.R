#' @export
print.assertion_factory <- function(x, ...) {
  type_fn <- attr(x, "typedr_type_function", exact = TRUE) %||% x
  fmls <- fn_fmls(type_fn)
  fmls <- fmls[setdiff(names(fmls), c("value", "..."))]

  cli_text("{.strong {col_grey('<typedr type factory>')}}")
  fn_color <- getOption("typedr.print.fn_color", TRUE)
  fn_out <- pretty_fn(
    x,
    lineno = FALSE,
    color = fn_color,
    wrap = getOption("typedr.print.fn_wrap", 60),
    indent = getOption("typedr.print.factory_fn_indent", 4),
    limit_lines = getOption(
      "typedr.print.factory_fn_limit_lines",
      .Machine$integer.max
    ),
    style = getOption("typedr.print.highlight", vsc_dark_plus())
  )
  if (length(fmls)) {
    args <- vapply(
      names(fmls),
      function(arg) {
        def <- fmls[[arg]]
        def_val <- if (is_missing(def) || identical(def, expr(expr = ))) {
          ""
        } else {
          expr_deparse(def)
        }
        if (nzchar(def_val)) {
          format_inline("{.arg {arg}} {col_green('=')} {.field {def_val}}")
        } else {
          format_inline("{.arg {arg}}")
        }
      },
      character(1)
    )
    cli_text("{.strong Arguments:}")
    cli_bullets(set_names(args, rep("*", length(args))))
  } else {
    cli_text("{.strong Arguments:} {col_grey('<none>')}")
  }

  .typedr_print_fn_meta(
    truncated = attr(fn_out, "typedr_fn_truncated", exact = TRUE),
    color = attr(fn_out, "typedr_fn_color", exact = TRUE)
  )

  invisible(x)
}

#' @export
print.typedr_assertion <- function(x, ...) {
  cli_text("{.strong {col_grey('<typedr type>')}}")
  fn_color <- getOption("typedr.print.fn_color", TRUE)
  fn_out <- pretty_fn(
    x,
    lineno = FALSE,
    color = fn_color,
    wrap = getOption("typedr.print.fn_wrap", 60),
    indent = getOption("typedr.print.fn_indent", 2),
    limit_lines = getOption("typedr.print.fn_limit_lines", 20),
    style = getOption("typedr.print.highlight", vsc_dark_plus())
  )
  .typedr_print_fn_meta(
    truncated = attr(fn_out, "typedr_fn_truncated", exact = TRUE),
    color = attr(fn_out, "typedr_fn_color", exact = TRUE)
  )
  invisible(x)
}

#' @export
print.typedr <- function(x, ...) {
  max_args <- getOption("typedr.print.max_args", 8)
  fn_indent <- getOption("typedr.print.fn_indent", 2)
  fn_wrap <- getOption("typedr.print.fn_wrap", 60)
  fn_limit_lines <- getOption("typedr.print.fn_limit_lines", 20)
  fn_lineno <- getOption("typedr.print.fn_lineno", FALSE)
  fn_color <- getOption("typedr.print.fn_color", TRUE)
  highlight <- getOption("typedr.print.highlight", vsc_dark_plus()) # R/utils-print.R

  fmls <- fn_fmls(x)
  return_type <- attr(x, "return_type", exact = TRUE)
  return_type <- if (is_null(return_type) || identical(return_type, NA)) {
    "Any()"
  } else {
    expr_deparse(return_type)
  }
  arg_types <- attr(x, "arg_types", exact = TRUE) %||% list()
  args_truncated <- length(arg_types) > max_args

  cli_text("{.strong {col_grey('<typedr function>')}}")
  fn_out <- pretty_fn(
    # R/utils-print.R
    .typedr_function_for_print(x),
    lineno = fn_lineno,
    color = fn_color,
    wrap = fn_wrap,
    indent = fn_indent,
    style = highlight,
    limit_lines = fn_limit_lines
  )
  cli_text("{.strong Return:} {.cls {return_type}}")
  if (length(arg_types)) {
    cli_text("{.strong Arguments:}")
    args_all <- names(arg_types)
    n_all <- length(args_all)
    args_show <- if (n_all > max_args) args_all[seq_len(max_args)] else args_all
    lines <- vapply(
      args_show,
      function(arg) {
        cls <- expr_deparse(arg_types[[arg]])
        def <- fmls[[arg]]
        def_val <- if (is_missing(def) || identical(def, expr(expr = ))) {
          ""
        } else {
          expr_deparse(def)
        }
        if (nzchar(def_val)) {
          format_inline(
            "{.arg {arg}}: {.cls {cls}} {.emph (default: {.field {def_val}})}"
          )
        } else {
          format_inline("{.arg {arg}}: {.cls {cls}}")
        }
      },
      character(1)
    )
    cli_bullets(set_names(lines, rep("*", length(lines))))
    if (n_all > max_args) {
      remaining <- n_all - max_args
      cli_bullets(c(
        " " = col_grey("{.emph ... and {remaining} more args ...}")
      ))
    }
  }

  .typedr_print_fn_meta(
    truncated = attr(fn_out, "typedr_fn_truncated", exact = TRUE),
    args_truncated = args_truncated,
    has_args = length(arg_types) > 0L,
    color = attr(fn_out, "typedr_fn_color", exact = TRUE)
  )

  invisible(x)
}

#' @export
print.typedr_value <- function(
  x,
  ...,
  max_items = getOption("typedr.max_items", 20),
  full_value = FALSE
) {
  max_items <- max(1L, as.integer(max_items)[[1]])
  full_value <- isTRUE(full_value)
  truncated <- FALSE
  mark_truncated <- function() {
    truncated <<- TRUE
  }

  # Strip typedr_value metadata before inspecting the underlying value.
  if (.typedr_is_wrapped_null(x)) {
    # R/utils.R
    untyped <- NULL
  } else {
    untyped <- structure(x, class = setdiff(class(x), "typedr_value"))
    attr(untyped, "typedr_name") <- NULL
    attr(untyped, "typedr_assertion") <- NULL
    attr(untyped, "typedr_const") <- NULL
  }

  format_len <- function(n) {
    format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
  }

  # Preview atomic vectors.
  preview_vec <- function(v, n = 10) {
    len <- length(v)
    if (len == 0) {
      return("c()")
    }
    i <- seq_len(min(len, n))
    elems <- v[i]
    fmt <- if (is_character(elems)) sprintf('"%s"', elems) else format(elems)
    paste(fmt, collapse = ", ")
  }

  preview_items <- function(v) {
    fmt <- if (is_character(v)) sprintf('"%s"', v) else trimws(format(v))
    paste(fmt, collapse = ", ")
  }

  preview_classed_items <- function(v) {
    if (inherits(v, c("Date", "POSIXct", "POSIXlt")) || is.factor(v)) {
      return(as.character(v))
    }
    # nocov start
    format(v)
    # nocov end
  }

  preview_classed <- function(v, n = 10) {
    len <- length(v)
    if (len == 0) {
      return("c()")
    }
    paste(preview_classed_items(utils::head(v, n)), collapse = ", ")
  }

  preview_expr <- function(expr) {
    paste(expr_deparse(expr), collapse = " ")
  }

  preview_expression <- function(v, n = 10) {
    len <- length(v)
    if (len == 0) {
      return("c()")
    }
    idx <- seq_len(min(len, n))
    paste(
      vapply(as.list(v)[idx], preview_expr, character(1)),
      collapse = ", "
    )
  }

  print_classed_atomic <- function(v, type, n = 10) {
    len <- length(v)
    n_show <- min(len, n)
    cli_text("{.field value}: {.cls {type}} [{format_len(len)}]")
    cli_text("{.field data}: {preview_classed(v, n_show)}")
    if (!full_value && len > n_show) {
      mark_truncated()
      cli_text(col_grey(format_inline(
        "{.emph # ... with {format_len(len - n_show)} more values}"
      )))
    }
  }

  vec_ptype <- function(v) {
    if (is.factor(v)) {
      "fct"
    } else if (inherits(v, "POSIXct")) {
      "dttm"
    } else if (inherits(v, "Date")) {
      "date"
    } else {
      switch(
        typeof(v),
        "integer" = "int",
        "double" = "dbl",
        "character" = "chr",
        "logical" = "lgl",
        "raw" = "raw",
        typeof(v)
      )
    }
  }

  format_cell <- function(x) {
    if (length(x) == 0 || (length(x) == 1 && is_na(x))) {
      return("NA")
    }
    if (is_character(x)) {
      return(sprintf('"%s"', paste(x, collapse = ",")))
    }
    paste(as.character(x), collapse = ",")
  }

  preview_data_frame <- function(df, n = 10) {
    nr <- nrow(df)
    nc <- ncol(df)
    if (!nr || !nc) {
      return(character())
    }

    n_show <- if (full_value) nr else min(nr, n)
    cols <- names2(df)
    types <- vapply(
      df,
      function(col) paste0("<", vec_ptype(col), ">"),
      character(1)
    )
    body <- lapply(seq_len(nc), function(j) {
      vapply(
        seq_len(n_show),
        function(i) format_cell(df[[j]][[i]]),
        character(1)
      )
    })

    widths <- vapply(
      seq_len(nc),
      function(j) {
        max(nchar(c(cols[[j]], types[[j]], body[[j]])), 1L)
      },
      integer(1)
    )

    fmt_row <- function(values) {
      paste(
        vapply(
          seq_len(nc),
          function(j) {
            format(values[[j]], width = widths[[j]], justify = "right")
          },
          character(1)
        ),
        collapse = " "
      )
    }

    lines <- c(
      fmt_row(cols),
      col_grey(format_inline("{.emph {fmt_row(types)}}"))
    )

    rows <- vapply(
      seq_len(n_show),
      function(i) {
        fmt_row(vapply(body, `[[`, character(1), i))
      },
      character(1)
    )
    lines <- c(lines, rows)
    if (!full_value && nr > n_show) {
      mark_truncated()
      lines <- c(
        lines,
        col_grey(format_inline(
          "{.emph # ... with {format_len(nr - n_show)} more rows}"
        ))
      )
    }
    lines
  }

  preview_grid <- function(values, row_labels, col_labels) {
    values <- as.matrix(values)
    nr <- nrow(values)
    nc <- ncol(values)
    cells <- apply(values, c(1, 2), format_cell)

    row_w <- max(nchar(row_labels), 0L)
    col_w <- vapply(
      seq_len(nc),
      function(j) {
        max(nchar(c(col_labels[[j]], cells[, j])), 1L)
      },
      integer(1)
    )

    header <- paste0(
      strrep(" ", row_w),
      if (row_w) " " else "",
      paste(
        vapply(
          seq_len(nc),
          function(j) {
            format(col_labels[[j]], width = col_w[[j]], justify = "right")
          },
          character(1)
        ),
        collapse = " "
      )
    )

    rows <- vapply(
      seq_len(nr),
      function(i) {
        paste0(
          format(row_labels[[i]], width = row_w, justify = "right"),
          " ",
          paste(
            vapply(
              seq_len(nc),
              function(j) {
                format(cells[[i, j]], width = col_w[[j]], justify = "right")
              },
              character(1)
            ),
            collapse = " "
          )
        )
      },
      character(1)
    )

    c(header, rows)
  }

  preview_matrix <- function(m, n = 10) {
    nr <- nrow(m)
    nc <- ncol(m)
    n_row <- if (full_value) nr else min(nr, max(1L, floor(sqrt(n))))
    n_col <- if (full_value) nc else min(nc, max(1L, floor(n / n_row)))
    cells <- m[seq_len(n_row), seq_len(n_col), drop = FALSE]
    lines <- c(
      col_grey(format_inline(
        "{.emph preview: rows 1-{n_row}, cols 1-{n_col}}"
      )),
      preview_grid(
        cells,
        row_labels = paste0("[", seq_len(n_row), ",]"),
        col_labels = paste0("[,", seq_len(n_col), "]")
      )
    )
    if (!full_value && (nr > n_row || nc > n_col)) {
      mark_truncated()
      lines <- c(
        lines,
        col_grey(format_inline(
          "{.emph # ... {format_len(nr - n_row)} more rows, {format_len(nc - n_col)} more cols}"
        ))
      )
    }
    lines
  }

  preview_array <- function(a, n = 10) {
    len <- length(a)
    dm <- dim(a)
    clean_array <- structure(a, class = NULL)
    if (full_value || len <= n) {
      return(utils::capture.output(print(clean_array)))
    }
    if (is_null(dm) || length(dm) < 3L) {
      mark_truncated()
      return(c(
        col_grey(format_inline("{.emph preview: flattened values}")),
        format_inline(
          "{.field data}: {preview_items(utils::head(as.vector(a), n))}"
        ),
        col_grey(format_inline(
          "{.emph # ... with {format_len(len - n)} more values}"
        ))
      ))
    }

    slice <- a[,, 1, drop = TRUE]
    if (!is.matrix(slice)) {
      slice <- matrix(as.vector(slice), nrow = dm[[1]])
    }
    lines <- c(
      col_grey(format_inline("{.emph preview: slice [, , 1]}")),
      preview_matrix(slice, n)
    )

    extra_slices <- prod(dm[-c(1, 2)]) - 1L
    if (extra_slices > 0L) {
      mark_truncated()
      lines <- c(
        lines,
        col_grey(format_inline(
          "{.emph # ... with {format_len(extra_slices)} more slices}"
        ))
      )
    }

    if (len > n) {
      mark_truncated()
      lines <- c(
        lines,
        col_grey(format_inline("{.emph preview: flattened values}")),
        format_inline(
          "{.field data}: {preview_items(utils::head(as.vector(a), n))}"
        ),
        col_grey(format_inline(
          "{.emph # ... with {format_len(len - n)} more values}"
        ))
      )
    }
    lines
  }

  type_summary <- function(v) {
    if (is.factor(v)) {
      format_inline("{.cls factor} [{format_len(length(v))}]")
    } else if (inherits(v, "POSIXct")) {
      format_inline("{.cls POSIXct} [{format_len(length(v))}]")
    } else if (inherits(v, "Date")) {
      format_inline("{.cls Date} [{format_len(length(v))}]")
    } else if (is.matrix(v)) {
      label <- paste(typeof(v), "matrix")
      format_inline(
        "{.cls {label}} {format_len(nrow(v))} x {format_len(ncol(v))}"
      )
    } else if (is.array(v)) {
      label <- paste(typeof(v), "array")
      format_inline(
        "{.cls {label}} dim {paste(format_len(dim(v)), collapse = ' x ')}"
      )
    } else if (inherits(v, "data.frame")) {
      format_inline(
        "{.cls data.frame} {format_len(nrow(v))} x {format_len(ncol(v))}"
      )
    } else if (is_list(v)) {
      format_inline("{.cls list} [{format_len(length(v))}]")
    } else {
      label <- typeof(v)
      format_inline("{.cls {label}} [{format_len(length(v))}]")
    }
  }

  preview_atomic_lines <- function(v, n = 10) {
    len <- length(v)
    if (len == 0) {
      return(format_inline("{.field data}: c()"))
    }

    preview <- function(x, n_preview) {
      if (is.factor(x) || inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
        preview_classed(x, n_preview)
      } else {
        preview_items(x)
      }
    }

    if (len <= n) {
      return(format_inline("{.field data}: {preview(v, n)}"))
    }

    n_show <- min(len, n)
    mark_truncated()
    c(
      format_inline("{.field data}: {preview(utils::head(v, n_show), n_show)}"),
      col_grey(format_inline(
        "{.emph # ... with {format_len(len - n_show)} more values}"
      ))
    )
  }

  preview_tree_lines <- function(v, n = 10) {
    if (
      (is_atomic(v) && is_null(dim(v))) ||
        is.factor(v) ||
        inherits(v, c("Date", "POSIXct", "POSIXlt"))
    ) {
      return(preview_atomic_lines(v, n))
    }

    character()
  }

  print_expression <- function(v, n = 10) {
    len <- length(v)
    cli_text("{.field value}: {.cls expression} [{format_len(len)}]")
    cli_text("{.field data}: {preview_expression(v, n)}")
    if (!full_value && len > n) {
      mark_truncated()
      cli_text(col_grey(format_inline(
        "{.emph # ... with {format_len(len - n)} more expressions}"
      )))
    }
  }

  print_pairlist <- function(v, n = 10) {
    len <- length(v)
    cli_text("{.field value}: {.cls pairlist} [{format_len(len)}]")
    lines <- utils::capture.output(print(v))
    if (length(lines)) {
      cli_verbatim(utils::head(lines, n))
    }
    if (!full_value && length(lines) > n) {
      mark_truncated()
      cli_text(col_grey(format_inline(
        "{.emph # ... with {format_len(length(lines) - n)} more lines}"
      )))
    }
  }

  print_function_value <- function(v) {
    cli_text("{.field value}: {.cls closure}")
    fmls <- names2(fn_fmls(v))
    cli_text(
      "{.field args}: {if (length(fmls)) paste(fmls, collapse = ', ') else col_grey('<none>')}"
    )
    cli_text("{.field body}: {preview_expr(fn_body(v))}")
  }

  preview_list_items <- function(v, n = 10, depth = 1L, max_depth = 2L) {
    len <- length(v)
    if (len == 0) {
      return(character())
    }
    nms <- names2(v)
    idx <- seq_len(min(len, n))
    lines <- unlist(
      lapply(idx, function(i) {
        nm <- if (nms[[i]] == "") paste0("[[", i, "]]") else nms[[i]]
        item <- v[[i]]
        head <- paste0(
          strrep("  ", depth - 1L),
          "* ",
          format_inline("{.field {nm}}"),
          ": ",
          type_summary(item)
        )
        child_is_list <- is_list(item) &&
          !inherits(item, "data.frame") &&
          depth < max_depth
        child <- if (child_is_list) {
          preview_list_items(
            item,
            n = n,
            depth = depth + 1L,
            max_depth = max_depth
          )
        } else {
          preview_tree_lines(item, n = n)
        }
        if (length(child) && !child_is_list) {
          child <- paste0(strrep("  ", depth), child)
        }
        c(head, child)
      }),
      use.names = FALSE
    )

    if (len > length(idx)) {
      mark_truncated()
      lines <- c(
        lines,
        col_grey(
          format_inline(
            "{strrep('  ', depth - 1L)}{.emph # ... with {format_len(len - length(idx))} more elements}"
          )
        )
      )
    }

    lines
  }

  nm <- attr(x, "typedr_name")
  assertion <- attr(x, "typedr_assertion")
  const <- isTRUE(attr(x, "typedr_const"))
  custom_printer <- .typedr_get_type_printer(assertion)

  # value block
  if (!is_null(custom_printer)) {
    custom_output <- try_fetch(
      custom_printer(untyped, max_items = max_items),
      error = function(e) {
        cli_abort(
          "Custom typedr value printer failed.",
          class = c("typedr_type_printer_error", "typedr_error"),
          parent = e
        )
      }
    )
    if (is_character(custom_output)) {
      cli_text(custom_output)
    }
  } else if (is.factor(untyped)) {
    print_classed_atomic(untyped, "factor", max_items)
    cli_text("{.field levels}: {col_green(preview_items(levels(untyped)))}")
  } else if (inherits(untyped, "POSIXct")) {
    print_classed_atomic(untyped, "POSIXct", max_items)
  } else if (inherits(untyped, "Date")) {
    print_classed_atomic(untyped, "Date", max_items)
  } else if (typeof(untyped) == "expression") {
    print_expression(untyped, max_items)
  } else if (typeof(untyped) == "pairlist") {
    print_pairlist(untyped, max_items)
  } else if (typeof(untyped) == "closure") {
    print_function_value(untyped)
  } else if (is.matrix(untyped)) {
    cli_text(
      "{.field value}: {.cls {typeof(untyped)} matrix} {format_len(nrow(untyped))} x {format_len(ncol(untyped))}"
    )
    cli_verbatim(preview_matrix(untyped, max_items))
  } else if (is.array(untyped)) {
    cli_text(
      "{.field value}: {.cls {typeof(untyped)} array} dim {paste(format_len(dim(untyped)), collapse = ' x ')}"
    )
    cli_verbatim(preview_array(untyped, max_items))
  } else if (inherits(untyped, "data.frame")) {
    cli_text(
      "{.field value}: {.cls data.frame} {format_len(nrow(untyped))} x {format_len(ncol(untyped))}"
    )
    cli_verbatim(preview_data_frame(untyped, max_items))
  } else if (is_list(untyped)) {
    len <- length(untyped)
    cli_text("{.field value}: {.cls list} [{format_len(len)}]")
    items <- preview_list_items(untyped, max_items)
    if (length(items)) {
      cli_verbatim(items)
    }
  } else if (is_atomic(untyped) && is_null(dim(untyped))) {
    len <- length(untyped)
    type <- typeof(untyped)
    if (len <= max_items) {
      cli_text(
        "{.field value}: {.cls {type}} [{format_len(len)}]
        {preview_vec(untyped, n = max_items)}"
      )
    } else {
      cli_text("{.field value}: {.cls {type}} [{format_len(len)}]")
      cli_text("{.field data}: {preview_vec(untyped, n = max_items)}")
      mark_truncated()
      cli_text(col_grey(format_inline(
        "{.emph # ... with {format_len(len - max_items)} more values}"
      )))
    }
  } else if (!isS4(untyped)) {
    cli_text("{.field value}: {col_blue(format(untyped))}")
  } else {
    cli_text("{.field value}: <S4 {class(untyped)[1]}>")
  }

  # meta bullets
  meta <- c(
    "*" = sprintf(
      "{.field assertion}: %s",
      if (!is_null(assertion)) {
        format_inline("{.cls {expr_deparse(assertion)}}")
      } else {
        col_grey("<none>")
      }
    ),
    "*" = sprintf(
      "{.field const}: %s",
      if (const) col_red("TRUE") else col_grey("FALSE")
    )
  )
  cli_bullets(meta)

  if (truncated) {
    cli_text(col_grey("Run `typedr::print_whole_value()` to see full value."))
  }

  invisible(x)
}
