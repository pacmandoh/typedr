capture_typedr_cli <- function(expr) {
  out <- character()
  append <- function(x) {
    out <<- c(out, x)
  }

  with_mocked_bindings(
    {
      force(expr)
      paste(out, collapse = "\n")
    },
    cli_text = function(...) {
      append(cli::format_inline(..., .envir = parent.frame()))
    },
    cli_bullets = function(text, ...) {
      append(unname(cli::format_inline(
        text,
        .envir = parent.frame(),
        collapse = FALSE
      )))
    },
    cli_verbatim = function(text, ...) {
      append(text)
    },
    .package = "typedr"
  )
}

test_that("typedr function printing exposes the compact public sections", {
  old <- options(
    cli.num_colors = 1,
    typedr.print.fn_color = FALSE,
    typedr.print.fn_lineno = FALSE,
    typedr.print.max_args = 8,
    typedr.print.fn_limit_lines = 20
  )
  on.exit(options(old), add = TRUE)

  f <- Character()?function(a1 = 1?Double(), c = ?Double(), d) {
    a1 + c + d
  }

  out <- capture_typedr_cli(print(f))

  expect_match(out, "<typedr function>", fixed = TRUE)
  expect_match(out, "function (a1 = 1, c, d)", fixed = TRUE)
  expect_match(out, "check_arg(a1, Double())", fixed = TRUE)
  expect_match(out, "check_output(a1 + c + d, Character())", fixed = TRUE)
  expect_match(out, "Return: <Character()>", fixed = TRUE)
  expect_match(out, "Arguments:", fixed = TRUE)
  expect_match(out, "`a1`: <Double()>", fixed = TRUE)
  expect_match(out, "default: 1", fixed = TRUE)
})

test_that("typedr function printing folds long argument lists", {
  old <- options(
    cli.num_colors = 1,
    typedr.print.fn_color = FALSE,
    typedr.print.max_args = 2,
    typedr.print.fn_limit_lines = 20
  )
  on.exit(options(old), add = TRUE)

  f <- ?function(a = ?Double(), b = ?Double(), c = ?Double()) {
    a + b + c
  }

  out <- capture_typedr_cli(print(f))

  expect_no_match(out, "`c`: <Double()>", fixed = TRUE)
  expect_match(out, "and 1 more args", fixed = TRUE)
})

test_that("assertion factories print as typedr types", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  out <- capture_typedr_cli(print(Character))

  expect_match(out, "<typedr type factory>", fixed = TRUE)
  expect_match(out, "function (", fixed = TRUE)
  expect_match(out, "f_call <- substitute", fixed = TRUE)
  expect_match(out, "Arguments:", fixed = TRUE)
  expect_match(out, "length", fixed = TRUE)
})

test_that("typedr value printing omits the old value header and previews long vectors", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  declare("x_print_short", Character(), value = c("a", "b"))
  short_out <- capture_typedr_cli(print(x_print_short, max_items = 5))

  expect_no_match(short_out, "<typedr> value", fixed = TRUE)
  expect_match(short_out, "value: <character> [2]", fixed = TRUE)
  expect_match(short_out, '"a", "b"', fixed = TRUE)
  expect_match(short_out, "assertion: <Character()>", fixed = TRUE)

  declare("x_print_long", Character(), value = c("a", "b", "c", "d", "e", "f"))
  long_out <- capture_typedr_cli(print(x_print_long, max_items = 4))

  expect_match(long_out, "value: <character> [6]", fixed = TRUE)
  expect_match(long_out, 'data: "a", "b", "c", "d"', fixed = TRUE)
  expect_match(long_out, "# ... with 2 more values", fixed = TRUE)
  expect_match(long_out, "print_whole_value()", fixed = TRUE)

  full_out <- capture_typedr_cli(print_whole_value(x_print_long))
  expect_match(full_out, '"a", "b", "c", "d", "e", "f"', fixed = TRUE)
})

test_that("typedr value printing handles shaped and non-atomic values", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  declare("x_print_matrix", Matrix(), value = matrix(1:4, nrow = 2))
  matrix_out <- capture_typedr_cli(print(x_print_matrix))
  expect_match(matrix_out, "value: <integer matrix> 2 x 2", fixed = TRUE)
  expect_match(matrix_out, "preview: rows 1-2, cols 1-2", fixed = TRUE)
  expect_match(matrix_out, "[1,]", fixed = TRUE)

  no_row_names <- matrix(1:4, nrow = 2)
  rownames(no_row_names) <- NULL
  no_row_names <- structure(no_row_names, class = c("typedr_value", "matrix"))
  attr(no_row_names, "typedr_assertion") <- quote(Matrix())
  attr(no_row_names, "typedr_const") <- FALSE
  no_row_names_out <- capture_typedr_cli(print(no_row_names))
  expect_match(no_row_names_out, "preview: rows 1-2, cols 1-2", fixed = TRUE)

  declare("x_print_array", Array(), value = array(1:8, dim = c(2, 2, 2)))
  array_out <- capture_typedr_cli(print(x_print_array))
  expect_match(array_out, "value: <integer array> dim 2 x 2 x 2", fixed = TRUE)
  expect_match(array_out, ", , 1", fixed = TRUE)
  expect_match(array_out, ", , 2", fixed = TRUE)

  declare("x_print_df", Data.frame(), value = data.frame(a = 1, b = 2))
  df_out <- capture_typedr_cli(print(x_print_df))
  expect_match(df_out, "value: <data.frame> 1 x 2", fixed = TRUE)
  expect_match(df_out, "<dbl>", fixed = TRUE)

  many_col_df <- data.frame(
    id = seq_len(25),
    name = paste0("item_", seq_len(25)),
    group = factor(rep(c("low", "mid", "high"), length.out = 25)),
    day = as.Date("2026-01-01") + seq_len(25),
    score = seq_len(25) / 10,
    flag = rep(c(TRUE, FALSE), length.out = 25),
    note = sprintf("note_%02d", seq_len(25))
  )
  declare("x_print_many_col_df", Data.frame(), value = many_col_df)
  many_col_out <- capture_typedr_cli(print(x_print_many_col_df, max_items = 5))
  expect_match(many_col_out, "value: <data.frame> 25 x 7", fixed = TRUE)
  expect_match(many_col_out, "<fct>", fixed = TRUE)
  expect_match(many_col_out, "<date>", fixed = TRUE)
  expect_match(many_col_out, "more rows", fixed = TRUE)
  expect_match(many_col_out, "print_whole_value()", fixed = TRUE)

  declare(
    "x_print_list",
    List(),
    value = list(
      a = 1,
      b = c("x", "y"),
      nested = list(ok = TRUE, when = as.Date("2026-06-01"))
    )
  )
  list_out <- capture_typedr_cli(print(x_print_list))
  expect_match(list_out, "value: <list> [3]", fixed = TRUE)
  expect_match(list_out, "* a: <double> [1]", fixed = TRUE)
  expect_match(list_out, "data: 1", fixed = TRUE)
  expect_match(list_out, "* b: <character> [2]", fixed = TRUE)
  expect_match(list_out, "* nested: <list> [2]", fixed = TRUE)
  expect_match(list_out, "* when: <Date> [1]", fixed = TRUE)
})

test_that("typedr value printing keeps classed atomic values readable", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  declare("x_print_factor", Factor(), value = factor(c("low", "high", "low")))
  factor_out <- capture_typedr_cli(print(x_print_factor))
  expect_match(factor_out, "value: <factor> [3]", fixed = TRUE)
  expect_match(factor_out, "data: low, high, low", fixed = TRUE)
  expect_match(factor_out, "levels:", fixed = TRUE)

  declare(
    "x_print_date",
    Date(),
    value = as.Date(c("2026-06-01", "2026-06-02"))
  )
  date_out <- capture_typedr_cli(print(x_print_date))
  expect_match(date_out, "value: <Date> [2]", fixed = TRUE)
  expect_match(date_out, "2026-06-01", fixed = TRUE)

  declare(
    "x_print_time",
    Time(),
    value = as.POSIXct("2026-06-01 12:34:56", tz = "UTC")
  )
  time_out <- capture_typedr_cli(print(x_print_time))
  expect_match(time_out, "value: <POSIXct> [1]", fixed = TRUE)

  declare(
    "x_print_factor_long",
    Factor(),
    value = factor(rep(c("low", "high"), 4))
  )
  factor_long_out <- capture_typedr_cli(print(
    x_print_factor_long,
    max_items = 3
  ))
  expect_match(factor_long_out, "value: <factor> [8]", fixed = TRUE)
  expect_match(factor_long_out, "# ... with 5 more values", fixed = TRUE)
  expect_match(factor_long_out, "print_whole_value()", fixed = TRUE)
})

test_that("custom type printers override only the value block", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  Email <- as_assertion_factory(function(value) {
    Character(length = 1)(value)
    value
  })

  factory_out <- capture_typedr_cli(print(Email))
  expect_match(factory_out, "<typedr type factory>", fixed = TRUE)
  expect_match(factory_out, "function (...)", fixed = TRUE)
  expect_match(factory_out, "Character(length = 1)(value)", fixed = TRUE)
  expect_match(factory_out, "Arguments: <none>", fixed = TRUE)

  default_out <- paste(capture.output(print.default(Email)), collapse = "\n")
  expect_match(default_out, "function (...)", fixed = TRUE)
  expect_match(default_out, "assertion_factory", fixed = TRUE)
  expect_match(default_out, "Character(length = 1)(value)", fixed = TRUE)

  assertion_out <- capture_typedr_cli(print(Email()))
  expect_match(assertion_out, "<typedr type>", fixed = TRUE)
  expect_match(assertion_out, "function (value)", fixed = TRUE)

  type_printer(Email, function(value, max_items = 20) {
    sprintf("{.field value}: {.cls Email} %s", value)
  })

  declare("x_print_email", Email(), value = "a@example.com")
  email_out <- capture_typedr_cli(print(x_print_email))

  expect_match(email_out, "value: <Email> a@example.com", fixed = TRUE)
  expect_match(email_out, "assertion: <Email()>", fixed = TRUE)
})

test_that("data values use default foreground color", {
  old <- options(cli.num_colors = 256)
  on.exit(options(old), add = TRUE)

  declare(
    "x_print_plain_data",
    Factor(),
    value = factor(rep(c("low", "high"), 4))
  )
  out <- capture_typedr_cli(print(x_print_plain_data, max_items = 3))

  expect_match(out, "\033\\[[0-9;]*mdata\033\\[[0-9;]*m: low, high, low")
  expect_no_match(out, "\033\\[[0-9;]*mlow, high, low")
})

test_that("typedr function printing handles missing metadata and helper wrappers", {
  old <- options(
    cli.num_colors = 1,
    typedr.print.fn_color = FALSE,
    typedr.print.fn_lineno = FALSE,
    typedr.print.max_args = 1,
    typedr.print.fn_limit_lines = 5
  )
  on.exit(options(old), add = TRUE)

  plain <- function(x = 1) {
    x
  }
  class(plain) <- c("typedr", "function")

  plain_out <- capture_typedr_cli(print(plain))
  expect_match(plain_out, "<typedr function>", fixed = TRUE)
  expect_match(plain_out, "Return: <Any()>", fixed = TRUE)

  f <- ?function(a = ?Double(), b = ?Double(), c = ?Double()) {
    a + b + c
  }

  folded_out <- capture_typedr_cli(print(f))
  expect_match(folded_out, "and 2 more args", fixed = TRUE)

  all_args_out <- capture_typedr_cli(print_all_args(f))
  expect_match(all_args_out, "`a`: <Double()>", fixed = TRUE)
  expect_match(all_args_out, "`c`: <Double()>", fixed = TRUE)

  whole_fn_out <- capture_typedr_cli(print_whole_fn(f))
  expect_match(whole_fn_out, "<typedr function>", fixed = TRUE)

  typedr_out <- capture_typedr_cli(
    print_typedr(
      f,
      max_args = 3,
      fn_color = FALSE,
      fn_lineno = FALSE,
      fn_limit_lines = 20
    )
  )
  expect_match(typedr_out, "`c`: <Double()>", fixed = TRUE)
})

test_that("typedr value printing covers empty and scalar atomic branches", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  declare("x_print_empty_chr", Character(), value = character())
  empty_chr_out <- capture_typedr_cli(print(x_print_empty_chr))
  expect_match(empty_chr_out, "value: <character> [0]", fixed = TRUE)
  expect_match(empty_chr_out, "c()", fixed = TRUE)

  declare("x_print_logical", Logical(), value = c(TRUE, FALSE, NA))
  logical_out <- capture_typedr_cli(print(x_print_logical))
  expect_match(logical_out, "value: <logical> [3]", fixed = TRUE)
  expect_match(logical_out, "TRUE")
  expect_match(logical_out, "FALSE")
  expect_match(logical_out, "NA")

  declare("x_print_raw", Raw(), value = charToRaw("abc"))
  raw_out <- capture_typedr_cli(print(x_print_raw))
  expect_match(raw_out, "value: <raw> [3]", fixed = TRUE)
})

test_that("typedr value printing covers empty data frames and lists", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  declare("x_print_empty_df", Data.frame(), value = data.frame())
  empty_df_out <- capture_typedr_cli(print(x_print_empty_df))
  expect_match(empty_df_out, "value: <data.frame> 0 x 0", fixed = TRUE)

  declare("x_print_empty_list", List(), value = list())
  empty_list_out <- capture_typedr_cli(print(x_print_empty_list))
  expect_match(empty_list_out, "value: <list> [0]", fixed = TRUE)
})

test_that("typedr value printing covers truncated matrices and flattened arrays", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  declare(
    "x_print_big_matrix",
    Matrix(),
    value = matrix(seq_len(100), nrow = 10)
  )
  big_matrix_out <- capture_typedr_cli(print(x_print_big_matrix, max_items = 4))
  expect_match(big_matrix_out, "value: <integer matrix> 10 x 10", fixed = TRUE)
  expect_match(big_matrix_out, "more rows", fixed = TRUE)
  expect_match(big_matrix_out, "more cols", fixed = TRUE)
  expect_match(big_matrix_out, "print_whole_value()", fixed = TRUE)

  declare(
    "x_print_flat_array",
    Array(),
    value = array(seq_len(20), dim = c(20))
  )
  flat_array_out <- capture_typedr_cli(print(x_print_flat_array, max_items = 5))
  expect_match(flat_array_out, "value: <integer array> dim 20", fixed = TRUE)
  expect_match(flat_array_out, "preview: flattened values", fixed = TRUE)

  full_array_out <- capture_typedr_cli(print_whole_value(x_print_flat_array))
  expect_match(full_array_out, "value: <integer array> dim 20", fixed = TRUE)
})

test_that("typedr value printing covers fallback objects", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  x_print_call <- quote(1 + 2)
  class(x_print_call) <- c("typedr_value", class(x_print_call))
  attr(x_print_call, "typedr_name") <- "x_print_call"
  attr(x_print_call, "typedr_assertion") <- quote(Language())
  attr(x_print_call, "typedr_const") <- FALSE

  call_out <- capture_typedr_cli(print(x_print_call))
  expect_match(call_out, "value:", fixed = TRUE)
  expect_match(call_out, "1 + 2", fixed = TRUE)

  x_print_plain_typed <- structure(
    1,
    class = c("typedr_value", class(1)),
    typedr_const = FALSE
  )
  plain_typed_out <- capture_typedr_cli(print(x_print_plain_typed))
  expect_match(plain_typed_out, "assertion: <none>", fixed = TRUE)
})

test_that("custom type printer errors are structured", {
  BadPrinter <- as_assertion_factory(function(value) {
    Character(length = 1)(value)
    value
  })

  type_printer(BadPrinter, function(value, max_items = 20) {
    stop("printer failed")
  })

  declare("x_print_bad_printer", BadPrinter(), value = "x")

  expect_error(
    capture_typedr_cli(print(x_print_bad_printer)),
    class = "typedr_type_printer_error"
  )
})

test_that("assertion factory printing includes required arguments", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  RequiredArg <- as_assertion_factory(function(value, pattern) {
    Character(length = 1)(value)
    value
  })

  out <- capture_typedr_cli(print(RequiredArg))

  expect_match(out, "<typedr type factory>", fixed = TRUE)
  expect_match(out, "`pattern`", fixed = TRUE)
})

test_that("pretty function formatting covers fallback highlighting and alternate outputs", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  fn <- function(alpha = 1, beta) {
    if (alpha > 1) {
      return("large")
    }
    beta + alpha # comment
  }

  out <- with_mocked_bindings(
    {
      .typedr_state$warned_once$prettycode_missing <- FALSE
      pretty_fn(
        fn,
        lineno = TRUE,
        color = TRUE,
        output = "string",
        width_align = 3,
        limit_lines = 5
      )
    },
    is_installed = function(pkg) FALSE,
    cli_bullets = function(...) NULL,
    .package = "typedr"
  )

  expect_match(out, "  1 function", fixed = TRUE)
  expect_match(out, "lines folded", fixed = TRUE)

  vector_out <- pretty_fn(
    fn,
    lineno = TRUE,
    alt_grey = FALSE,
    color = FALSE,
    output = "vector",
    width_align = 2,
    limit_lines = 20
  )

  expect_true(is.character(vector_out))
  expect_match(vector_out[[1]], " 1 function", fixed = TRUE)

  long_fn <- function(alpha = 1) {
    alpha <- alpha + 1
    alpha <- alpha + 1
    alpha <- alpha + 1
    alpha <- alpha + 1
    alpha <- alpha + 1
    alpha
  }

  cli_out <- capture_typedr_cli(pretty_fn(
    long_fn,
    lineno = TRUE,
    color = FALSE,
    limit_lines = 5
  ))
  expect_match(cli_out, "print_whole_fn()", fixed = TRUE)
})

test_that("internal highlighting helpers handle empty input and no-op paths", {
  expect_identical(.highlight_typedr_basic(character()), character())
  expect_identical(
    .highlight_typedr_basic("plain_text", style = list()),
    "plain_text"
  )
  expect_identical(
    .highlight_typedr_basic(
      c("# comment", "(", "NULL", "mean(1)"),
      style = list(
        comment = function(x) paste0("comment:", x),
        null = function(x) paste0("null:", x),
        call = function(x) paste0("call:", x),
        bracket = list(function(x) paste0("bracket:", x))
      )
    ),
    c(
      "comment:# comment",
      "bracket:(",
      "null:NULL",
      "call:meanbracket:(1bracket:)"
    )
  )
  expect_identical(.color_symbol_formals("123", style = vsc_dark_plus()), "123")
  expect_identical(
    .color_symbol_formals(character(), style = vsc_dark_plus()),
    character()
  )
  expect_identical(
    .color_symbol_formals("x <- 1", style = vsc_dark_plus()),
    "x <- 1"
  )
  expect_identical(.adjust_indent("  x", to = 4, from = 4), "  x")
})

test_that("print_stats emits function statistics", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  f <- ?function(x = ?Double(), y = 2, ...) {
    msg <- "value"
    if (TRUE) {
      return(x + y)
    }
    msg
  }

  out <- capture_typedr_cli(print_stats(f))

  expect_match(out, "<typedr>` function Stats", fixed = TRUE)
  expect_match(out, "Signature:", fixed = TRUE)
  expect_match(out, "Args:", fixed = TRUE)
  expect_match(out, "+ ...", fixed = TRUE)
  expect_match(out, "Types:", fixed = TRUE)
  expect_match(out, "Address & Size", fixed = TRUE)
  expect_match(out, "Body / tokens", fixed = TRUE)
  expect_match(out, "Top calls:", fixed = TRUE)
  expect_match(out, "Version:", fixed = TRUE)

  ret <- NULL
  capture_typedr_cli(ret <- print_stats(f))
  expect_identical(ret, f)

  typed_return <- Double()?function(x = ?Double()) {
    1.5
  }
  typed_return_out <- capture_typedr_cli(print_stats(typed_return))
  expect_match(typed_return_out, "Double()", fixed = TRUE)
  expect_match(typed_return_out, "numbers = 1", fixed = TRUE)

  function_literal <- function() {
    function(a = 1) a
  }
  function_literal_out <- capture_typedr_cli(print_stats(function_literal))
  expect_match(function_literal_out, "calls", fixed = TRUE)

  symbol_body <- function() x
  symbol_out <- capture_typedr_cli(print_stats(symbol_body))
  expect_no_match(symbol_out, "Top calls:", fixed = TRUE)
})

test_that("type printer keys support characters and reject invalid types", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  type_printer("EmailByName", function(value, max_items = 20) {
    sprintf("{.field value}: {.cls EmailByName} %s", value)
  })

  x <- structure(
    "named@example.com",
    typedr_name = "x_print_named_email",
    typedr_assertion = "EmailByName",
    typedr_const = FALSE,
    class = c("typedr_value", "character")
  )

  out <- capture_typedr_cli(print(x))
  expect_match(out, "value: <EmailByName> named@example.com", fixed = TRUE)

  expect_null(.typedr_get_type_printer(NULL))
  expect_error(
    .typedr_type_printer_key(list("bad")),
    class = "typedr_type_printer_error"
  )
})

test_that("warn_once warns, tips, and suppresses repeated ids", {
  .typedr_state$warned_once$unit_warn_once <- NULL
  .typedr_state$warned_once$unit_tip_once <- NULL

  expect_warning(
    .warn_once("unit_warn_once", c("!" = "careful"), type = "warn"),
    "careful"
  )
  expect_silent(.warn_once("unit_warn_once", c("!" = "careful"), type = "warn"))

  out <- capture_typedr_cli(.warn_once(
    "unit_tip_once",
    c("i" = "tip"),
    type = "tips"
  ))
  expect_match(out, "tip", fixed = TRUE)
  expect_equal(
    capture_typedr_cli(.warn_once(
      "unit_tip_once",
      c("i" = "tip"),
      type = "tips"
    )),
    ""
  )
})

test_that("typedr value printing covers classed, shaped, nested, and S4 edge cases", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  declare("x_print_empty_factor", Factor(), value = factor(character()))
  empty_factor_out <- capture_typedr_cli(print(x_print_empty_factor))
  expect_match(empty_factor_out, "value: <factor> [0]", fixed = TRUE)
  expect_match(empty_factor_out, "data: c()", fixed = TRUE)

  declare(
    "x_print_long_time",
    Time(),
    value = as.POSIXct("2026-06-01", tz = "UTC") + 0:6
  )
  long_time_out <- capture_typedr_cli(print(x_print_long_time, max_items = 3))
  expect_match(long_time_out, "value: <POSIXct> [7]", fixed = TRUE)
  expect_match(long_time_out, "more values", fixed = TRUE)

  declare(
    "x_print_empty_matrix",
    Matrix(),
    value = matrix(numeric(), nrow = 0, ncol = 2)
  )
  empty_matrix_out <- capture_typedr_cli(print(x_print_empty_matrix))
  expect_match(empty_matrix_out, "value: <double matrix> 0 x 2", fixed = TRUE)
  expect_match(empty_matrix_out, "[,1]", fixed = TRUE)

  declare(
    "x_print_nested_long_list",
    List(),
    value = list(
      nested = list(a = 1, b = 2, c = 3),
      tail = 4,
      extra = 5
    )
  )
  nested_out <- capture_typedr_cli(print(
    x_print_nested_long_list,
    max_items = 1
  ))
  expect_match(nested_out, "* nested: <list> [3]", fixed = TRUE)
  expect_match(nested_out, "more elements", fixed = TRUE)
  expect_match(nested_out, "print_whole_value()", fixed = TRUE)

  declare(
    "x_print_list_summaries",
    List(),
    value = list(
      factor = factor("a"),
      time = as.POSIXct("2026-06-01", tz = "UTC"),
      matrix = matrix(1:4, nrow = 2),
      array = array(1:8, dim = c(2, 2, 2)),
      df = data.frame(a = 1),
      empty = character(),
      long = letters[1:5],
      nested = list(child = list(grandchild = 1))
    )
  )
  summary_out <- capture_typedr_cli(print(
    x_print_list_summaries,
    max_items = 10
  ))
  expect_match(summary_out, "* factor: <factor> [1]", fixed = TRUE)
  expect_match(summary_out, "* time: <POSIXct> [1]", fixed = TRUE)
  expect_match(summary_out, "* matrix: <integer matrix> 2 x 2", fixed = TRUE)
  expect_match(
    summary_out,
    "* array: <integer array> dim 2 x 2 x 2",
    fixed = TRUE
  )
  expect_match(summary_out, "* df: <data.frame> 1 x 1", fixed = TRUE)
  expect_match(summary_out, "data: c()", fixed = TRUE)
  expect_match(summary_out, '"a", "b", "c", "d", "e"', fixed = TRUE)
  expect_match(summary_out, "* child: <list> [1]", fixed = TRUE)

  declare("x_print_long_vector_list", List(), value = list(long = letters[1:5]))
  long_vector_list_out <- capture_typedr_cli(print(
    x_print_long_vector_list,
    max_items = 3
  ))
  expect_match(long_vector_list_out, '"a", "b", "c"', fixed = TRUE)
  expect_match(long_vector_list_out, "with 2 more values", fixed = TRUE)

  declare(
    "x_print_mixed_df",
    Data.frame(),
    value = data.frame(
      when = as.POSIXct("2026-06-01", tz = "UTC"),
      day = as.Date("2026-06-02"),
      raw = as.raw(1),
      complex = 1 + 2i,
      missing = NA_real_
    )
  )
  mixed_df_out <- capture_typedr_cli(print(x_print_mixed_df))
  expect_match(mixed_df_out, "<dttm>", fixed = TRUE)
  expect_match(mixed_df_out, "<date>", fixed = TRUE)
  expect_match(mixed_df_out, "<raw>", fixed = TRUE)
  expect_match(mixed_df_out, "<complex>", fixed = TRUE)
  expect_match(mixed_df_out, "NA", fixed = TRUE)

  declare(
    "x_print_vector_slice_array",
    Array(),
    value = array(seq_len(4), dim = c(2, 1, 2))
  )
  vector_slice_out <- capture_typedr_cli(print(
    x_print_vector_slice_array,
    max_items = 3
  ))
  expect_match(vector_slice_out, "preview: slice [, , 1]", fixed = TRUE)

  setClass("TypedrPrintS4", slots = c(x = "numeric"))
  s4 <- new("TypedrPrintS4", x = 1)
  attr(s4, "typedr_assertion") <- quote(Any())
  attr(s4, "typedr_const") <- FALSE

  s4_out <- capture_typedr_cli(print.typedr_value(s4))
  expect_match(s4_out, "<S4", fixed = TRUE)
})
