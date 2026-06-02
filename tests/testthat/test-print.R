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
      append(unname(cli::format_inline(text, .envir = parent.frame(), collapse = FALSE)))
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

  f <- Character() ? function(a1 = 1 ? Double(), c = ? Double(), d) {
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
  expect_match(out, "`c`: <Double()>", fixed = TRUE)
  expect_no_match(out, "<typedr> value", fixed = TRUE)
  expect_no_match(out, "Run `typedr::print_stats()`", fixed = TRUE)
})

test_that("typedr function printing folds long argument lists", {
  old <- options(
    cli.num_colors = 1,
    typedr.print.fn_color = FALSE,
    typedr.print.max_args = 2,
    typedr.print.fn_limit_lines = 20
  )
  on.exit(options(old), add = TRUE)

  f <- ? function(a = ? Double(), b = ? Double(), c = ? Double()) {
    a + b + c
  }

  out <- capture_typedr_cli(print(f))

  expect_match(out, "`a`: <Double()>", fixed = TRUE)
  expect_match(out, "`b`: <Double()>", fixed = TRUE)
  expect_no_match(out, "`c`: <Double()>", fixed = TRUE)
  expect_match(out, "and 1 more args", fixed = TRUE)
  expect_match(out, "print_all_args()", fixed = TRUE)
})

test_that("assertion factories print as typedr types", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  out <- capture_typedr_cli(print(Character))

  expect_match(out, "<typedr type factory>", fixed = TRUE)
  expect_match(out, "function (", fixed = TRUE)
  expect_match(out, "f_call <- substitute", fixed = TRUE)
  expect_no_match(out, "typedr_type_function", fixed = TRUE)
  expect_match(out, "Arguments:", fixed = TRUE)
  expect_match(out, "length", fixed = TRUE)
  expect_match(out, "null_ok", fixed = TRUE)
})

test_that("typedr value printing omits the old value header and previews long vectors", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  declare("x_print_short", Character(), value = c("a", "b"))
  short_out <- capture_typedr_cli(print(x_print_short, max_items = 5))

  expect_no_match(short_out, "<typedr> value", fixed = TRUE)
  expect_match(short_out, "value: <character> [2]", fixed = TRUE)
  expect_match(short_out, '"a", "b"', fixed = TRUE)
  expect_no_match(short_out, "name: `x_print_short`", fixed = TRUE)
  expect_match(short_out, "assertion: <Character()>", fixed = TRUE)
  expect_match(short_out, "const: FALSE", fixed = TRUE)

  declare("x_print_long", Character(), value = c("a", "b", "c", "d", "e", "f"))
  long_out <- capture_typedr_cli(print(x_print_long, max_items = 4))

  expect_match(long_out, "value: <character> [6]", fixed = TRUE)
  expect_match(long_out, 'data: "a", "b", "c", "d"', fixed = TRUE)
  expect_match(long_out, "# ... with 2 more values", fixed = TRUE)
  expect_match(long_out, "print_whole_value()", fixed = TRUE)
  expect_no_match(long_out, "total", fixed = TRUE)

  full_out <- capture_typedr_cli(print_whole_value(x_print_long))
  expect_match(full_out, '"a", "b", "c", "d", "e", "f"', fixed = TRUE)
  expect_no_match(full_out, "print_whole_value()", fixed = TRUE)
})

test_that("typedr value printing handles shaped and non-atomic values", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  declare("x_print_matrix", Matrix(), value = matrix(1:4, nrow = 2))
  matrix_out <- capture_typedr_cli(print(x_print_matrix))
  expect_match(matrix_out, "value: <integer matrix> 2 x 2", fixed = TRUE)
  expect_match(matrix_out, "preview: rows 1-2, cols 1-2", fixed = TRUE)
  expect_match(matrix_out, "[1,]", fixed = TRUE)

  declare("x_print_array", Array(), value = array(1:8, dim = c(2, 2, 2)))
  array_out <- capture_typedr_cli(print(x_print_array))
  expect_match(array_out, "value: <integer array> dim 2 x 2 x 2", fixed = TRUE)
  expect_match(array_out, ", , 1", fixed = TRUE)
  expect_match(array_out, ", , 2", fixed = TRUE)

  declare("x_print_df", Data.frame(), value = data.frame(a = 1, b = 2))
  df_out <- capture_typedr_cli(print(x_print_df))
  expect_match(df_out, "value: <data.frame> 1 x 2", fixed = TRUE)
  expect_match(df_out, "<dbl>", fixed = TRUE)
  expect_match(df_out, "a", fixed = TRUE)
  expect_match(df_out, "b", fixed = TRUE)
  expect_no_match(df_out, "# A data.frame", fixed = TRUE)

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
  expect_match(many_col_out, '"item_1"', fixed = TRUE)
  expect_match(many_col_out, "more rows", fixed = TRUE)
  expect_match(many_col_out, "print_whole_value()", fixed = TRUE)

  declare("x_print_list", List(), value = list(
    a = 1,
    b = c("x", "y"),
    nested = list(ok = TRUE, when = as.Date("2026-06-01"))
  ))
  list_out <- capture_typedr_cli(print(x_print_list))
  expect_match(list_out, "value: <list> [3]", fixed = TRUE)
  expect_match(list_out, "* a: <double> [1]", fixed = TRUE)
  expect_match(list_out, "data: 1", fixed = TRUE)
  expect_match(list_out, "* b: <character> [2]", fixed = TRUE)
  expect_match(list_out, 'data: "x", "y"', fixed = TRUE)
  expect_match(list_out, "* nested: <list> [2]", fixed = TRUE)
  expect_match(list_out, "* ok: <logical> [1]", fixed = TRUE)
  expect_match(list_out, "* when: <Date> [1]", fixed = TRUE)
  expect_no_match(list_out, "name: `x_print_list`", fixed = TRUE)
})

test_that("typedr value printing keeps classed atomic values readable", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  declare("x_print_factor", Factor(), value = factor(c("low", "high", "low")))
  factor_out <- capture_typedr_cli(print(x_print_factor))
  expect_match(factor_out, "value: <factor> [3]", fixed = TRUE)
  expect_match(factor_out, "data: low, high, low", fixed = TRUE)
  expect_match(factor_out, "levels:", fixed = TRUE)
  expect_match(factor_out, "low", fixed = TRUE)

  declare("x_print_date", Date(), value = as.Date(c("2026-06-01", "2026-06-02")))
  date_out <- capture_typedr_cli(print(x_print_date))
  expect_match(date_out, "value: <Date> [2]", fixed = TRUE)
  expect_match(date_out, "2026-06-01", fixed = TRUE)

  declare("x_print_time", Time(), value = as.POSIXct("2026-06-01 12:34:56", tz = "UTC"))
  time_out <- capture_typedr_cli(print(x_print_time))
  expect_match(time_out, "value: <POSIXct> [1]", fixed = TRUE)
  expect_match(time_out, "2026", fixed = TRUE)

  declare("x_print_factor_long", Factor(), value = factor(rep(c("low", "high"), 4)))
  factor_long_out <- capture_typedr_cli(print(x_print_factor_long, max_items = 3))
  expect_match(factor_long_out, "value: <factor> [8]", fixed = TRUE)
  expect_match(factor_long_out, "data: low, high, low", fixed = TRUE)
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
  expect_match(factory_out, "f_call <- substitute(f(value))", fixed = TRUE)
  expect_match(factory_out, "Character(length = 1)(value)", fixed = TRUE)
  expect_match(factory_out, "Arguments: <none>", fixed = TRUE)

  default_out <- paste(capture.output(print.default(Email)), collapse = "\n")
  expect_match(default_out, "function (...)", fixed = TRUE)
  expect_match(default_out, "attr(,\"class\")", fixed = TRUE)
  expect_match(default_out, "assertion_factory", fixed = TRUE)
  expect_match(default_out, "attr(,\"typedr_type_function\")", fixed = TRUE)
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
  expect_no_match(email_out, "name: `x_print_email`", fixed = TRUE)
  expect_match(email_out, "assertion: <Email()>", fixed = TRUE)
})

test_that("data values use default foreground color", {
  old <- options(cli.num_colors = 256)
  on.exit(options(old), add = TRUE)

  declare("x_print_plain_data", Factor(), value = factor(rep(c("low", "high"), 4)))
  out <- capture_typedr_cli(print(x_print_plain_data, max_items = 3))

  expect_match(out, "\033\\[[0-9;]*mdata\033\\[[0-9;]*m: low, high, low")
  expect_no_match(out, "\033\\[[0-9;]*mlow, high, low")
})
