# fmt: skip file
test_that("typedr function printing shows body, return, args, and helper wrappers", {
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
  expect_match(out, "Return: <Character()>", fixed = TRUE)
  expect_match(out, "`a1`: <Double()>", fixed = TRUE)
  expect_match(out, "(default: 1)", fixed = TRUE)

  fold_f <- ? function(a = ? Double(), b = ? Double(), c = ? Double()) a + b + c
  options(typedr.print.max_args = 2)
  folded_args <- capture_typedr_cli(print(fold_f))
  expect_match(folded_args, "and 1 more args", fixed = TRUE)

  plain <- function(x = 1) x
  class(plain) <- c("typedr", "function")
  plain_out <- capture_typedr_cli(print(plain))
  expect_match(plain_out, "Return: <Any()>", fixed = TRUE)

  long_f <- ? function(a = ? Double(), b = ? Double(), c = ? Double()) {
    a + b + c
    a + b + c
    a + b + c
    a + b + c
    a + b + c
    a + b + c
  }
  options(typedr.print.max_args = 1, typedr.print.fn_limit_lines = 5)
  folded_out <- capture_typedr_cli(print(long_f))
  expect_match(folded_out, "and 2 more args", fixed = TRUE)
  whole_pos <- regexpr("print_whole_fn", folded_out, fixed = TRUE)[1]
  all_args_pos <- regexpr("print_all_args", folded_out, fixed = TRUE)[1]
  expect_true(whole_pos > 0 && all_args_pos > 0 && whole_pos < all_args_pos)

  all_args_out <- capture_typedr_cli(print_all_args(long_f))
  expect_match(all_args_out, "`c`: <Double()>", fixed = TRUE)
  expect_match(capture_typedr_cli(print_whole_fn(long_f)), "<typedr function>", fixed = TRUE)
  expect_match(
    capture_typedr_cli(print_typedr(
      long_f,
      max_args = 3,
      fn_color = FALSE,
      fn_lineno = FALSE,
      fn_limit_lines = 20
    )),
    "`c`: <Double()>",
    fixed = TRUE
  )
})

test_that("assertion factories and typedr types print compact summaries", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  out <- capture_typedr_cli(print(Character))
  expect_match(out, "<typedr type factory>", fixed = TRUE)
  expect_match(out, "Arguments:", fixed = TRUE)
  expect_match(out, "length", fixed = TRUE)

  RequiredArg <- as_assertion_factory(function(value, pattern) {
    Character(length = 1)(value)
    value
  })
  expect_match(capture_typedr_cli(print(RequiredArg)), "`pattern`", fixed = TRUE)

  Email <- as_assertion_factory(function(value) {
    Character(length = 1)(value)
    value
  })
  expect_match(capture_typedr_cli(print(Email)), "Arguments: <none>", fixed = TRUE)
  expect_match(capture_typedr_cli(print(Email())), "<typedr type>", fixed = TRUE)
})

test_that("typedr value printing previews common types, empties, and truncation", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  declare("x_short", Character(), value = c("a", "b"))
  short_out <- capture_typedr_cli(print(x_short, max_items = 5))
  expect_match(short_out, "value: <character> [2]", fixed = TRUE)
  expect_match(short_out, '"a", "b"', fixed = TRUE)
  expect_match(short_out, "assertion: <Character()>", fixed = TRUE)

  declare("x_long", Character(), value = letters[1:6])
  long_out <- capture_typedr_cli(print(x_long, max_items = 4))
  expect_match(long_out, "# ... with 2 more values", fixed = TRUE)
  expect_match(long_out, "print_whole_value()", fixed = TRUE)
  expect_match(
    capture_typedr_cli(print_whole_value(x_long)),
    '"a", "b", "c", "d", "e", "f"',
    fixed = TRUE
  )

  declare("x_empty_chr", Character(), value = character())
  expect_match(
    capture_typedr_cli(print(x_empty_chr)),
    "value: <character> [0]",
    fixed = TRUE
  )

  declare("x_logical", Logical(), value = c(TRUE, FALSE, NA))
  expect_match(capture_typedr_cli(print(x_logical)), "NA", fixed = TRUE)

  declare("x_raw", Raw(), value = charToRaw("abc"))
  expect_match(capture_typedr_cli(print(x_raw)), "value: <raw> [3]", fixed = TRUE)

  declare("x_factor", Factor(), value = factor(c("low", "high", "low")))
  factor_out <- capture_typedr_cli(print(x_factor))
  expect_match(factor_out, "levels:", fixed = TRUE)

  declare("x_empty_factor", Factor(), value = factor(character()))
  expect_match(
    capture_typedr_cli(print(x_empty_factor)),
    "data: c()",
    fixed = TRUE
  )
  expect_match(
    capture_typedr_cli(print(x_factor, max_items = 2)),
    "print_whole_value()",
    fixed = TRUE
  )

  declare("x_date", Date(), value = as.Date(c("2026-06-01", "2026-06-02")))
  expect_match(capture_typedr_cli(print(x_date)), "2026-06-01", fixed = TRUE)

  declare("x_time", Time(), value = as.POSIXct("2026-06-01 12:34:56", tz = "UTC"))
  expect_match(capture_typedr_cli(print(x_time)), "POSIXct", fixed = TRUE)

  declare("x_expr", Expression(1), value = expression(amount / 100))
  expr_out <- capture_typedr_cli(print(x_expr))
  expect_match(expr_out, "value: <expression> [1]", fixed = TRUE)
  expect_match(expr_out, "amount / 100", fixed = TRUE)

  declare("x_empty_expr", Expression(), value = expression())
  expect_match(
    capture_typedr_cli(print(x_empty_expr)),
    "data: c()",
    fixed = TRUE
  )

  declare("x_long_expr", Expression(), value = expression(a, b, c))
  expect_match(
    capture_typedr_cli(print(x_long_expr, max_items = 1)),
    "more expressions",
    fixed = TRUE
  )

  declare("x_pairlist", Pairlist(), value = formals(function(x) x))
  pairlist_out <- capture_typedr_cli(print(x_pairlist))
  expect_match(pairlist_out, "value: <pairlist> [1]", fixed = TRUE)
  expect_match(pairlist_out, "$x", fixed = TRUE)

  declare(
    "x_long_pairlist",
    Pairlist(),
    value = formals(function(a, b, c, d, e) TRUE)
  )
  expect_match(
    capture_typedr_cli(print(x_long_pairlist, max_items = 1)),
    "more lines",
    fixed = TRUE
  )

  declare("x_closure", Closure(), value = function(x) sqrt(x))
  closure_out <- capture_typedr_cli(print(x_closure))
  expect_match(closure_out, "value: <closure>", fixed = TRUE)
  expect_match(closure_out, "args: x", fixed = TRUE)
  expect_match(closure_out, "sqrt(x)", fixed = TRUE)

  declare("x_matrix", Matrix(), value = matrix(1:4, nrow = 2))
  expect_match(capture_typedr_cli(print(x_matrix)), "[1,]", fixed = TRUE)

  no_row_names <- matrix(1:4, nrow = 2)
  rownames(no_row_names) <- NULL
  no_row_names <- structure(no_row_names, class = c("typedr_value", "matrix"))
  attr(no_row_names, "typedr_assertion") <- quote(Matrix())
  attr(no_row_names, "typedr_const") <- FALSE
  expect_match(
    capture_typedr_cli(print(no_row_names)),
    "preview: rows 1-2, cols 1-2",
    fixed = TRUE
  )

  declare("x_array", Array(), value = array(1:8, dim = c(2, 2, 2)))
  expect_match(capture_typedr_cli(print(x_array)), ", , 1", fixed = TRUE)

  declare("x_df", Data.frame(), value = data.frame(num = 1, label = "x"))
  expect_match(capture_typedr_cli(print(x_df)), "<chr>", fixed = TRUE)

  declare("x_empty_df", Data.frame(), value = data.frame())
  expect_match(
    capture_typedr_cli(print(x_empty_df)),
    "value: <data.frame> 0 x 0",
    fixed = TRUE
  )

  declare("x_list", List(), value = list(a = 1, b = c("x", "y")))
  expect_match(capture_typedr_cli(print(x_list)), "* a: <double> [1]", fixed = TRUE)

  declare("x_empty_list", List(), value = list())
  expect_match(capture_typedr_cli(print(x_empty_list)), "value: <list> [0]", fixed = TRUE)

  declare("x_big_matrix", Matrix(), value = matrix(seq_len(100), nrow = 10))
  big_matrix_out <- capture_typedr_cli(print(x_big_matrix, max_items = 4))
  expect_match(big_matrix_out, "more rows", fixed = TRUE)
  expect_match(big_matrix_out, "more cols", fixed = TRUE)

  declare("x_flat_array", Array(), value = array(seq_len(20), dim = c(20)))
  expect_match(
    capture_typedr_cli(print(x_flat_array, max_items = 5)),
    "preview: flattened values",
    fixed = TRUE
  )

  x_null <- structure(list(), class = c("typedr_value", "typedr_null"))
  attr(x_null, "typedr_name") <- "nullable_x"
  attr(x_null, "typedr_assertion") <- quote(Null())
  attr(x_null, "typedr_const") <- FALSE
  expect_match(capture_typedr_cli(print(x_null)), "value: NULL", fixed = TRUE)

  declare(
    "x_list_types",
    List(),
    value = list(
      factor = factor("a"),
      time = as.POSIXct("2026-06-01", tz = "UTC"),
      day = as.Date("2026-06-01"),
      matrix = matrix(1:4, nrow = 2),
      array = array(1:8, dim = c(2, 2, 2)),
      df = data.frame(a = 1),
      long = letters[1:5],
      fn = function() 1
    )
  )
  list_types_out <- capture_typedr_cli(print(x_list_types, max_items = 6))
  expect_match(list_types_out, "* factor: <factor> [1]", fixed = TRUE)
  expect_match(list_types_out, "* matrix: <integer matrix> 2 x 2", fixed = TRUE)
  expect_match(list_types_out, "* df: <data.frame> 1 x 1", fixed = TRUE)
  expect_match(list_types_out, "with 2 more elements", fixed = TRUE)

  declare("x_empty_in_list", List(), value = list(empty = character()))
  empty_list_out <- capture_typedr_cli(print(x_empty_in_list))
  expect_match(empty_list_out, "* empty: <character> [0]", fixed = TRUE)
  expect_match(empty_list_out, "data: c()", fixed = TRUE)

  declare("x_long_in_list", List(), value = list(long = letters[1:5]))
  expect_match(
    capture_typedr_cli(print(x_long_in_list, max_items = 3)),
    "with 2 more values",
    fixed = TRUE
  )
})

test_that("typedr value printing covers nested lists, mixed frames, and fallbacks", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  declare(
    "x_many_col_df",
    Data.frame(),
    value = data.frame(
      id = 1:25,
      group = factor(rep(c("low", "mid", "high"), length.out = 25)),
      day = as.Date("2026-01-01") + 1:25,
      flag = rep(c(TRUE, FALSE), length.out = 25)
    )
  )
  many_col_out <- capture_typedr_cli(print(x_many_col_df, max_items = 5))
  expect_match(many_col_out, "more rows", fixed = TRUE)

  declare(
    "x_nested_list",
    List(),
    value = list(
      nested = list(a = 1, b = 2, c = 3),
      tail = 4
    )
  )
  nested_out <- capture_typedr_cli(print(x_nested_list, max_items = 1))
  expect_match(nested_out, "more elements", fixed = TRUE)

  declare(
    "x_mixed_df",
    Data.frame(),
    value = data.frame(
      when = as.POSIXct("2026-06-01", tz = "UTC"),
      day = as.Date("2026-06-02"),
      raw = as.raw(1),
      complex = 1 + 2i,
      missing = NA_real_
    )
  )
  mixed_df_out <- capture_typedr_cli(print(x_mixed_df))
  expect_match(mixed_df_out, "<dttm>", fixed = TRUE)
  expect_match(mixed_df_out, "<complex>", fixed = TRUE)

  declare(
    "x_slice_array",
    Array(),
    value = array(seq_len(4), dim = c(2, 1, 2))
  )
  expect_match(
    capture_typedr_cli(print(x_slice_array, max_items = 3)),
    "preview: slice [, , 1]",
    fixed = TRUE
  )

  x_call <- quote(1 + 2)
  class(x_call) <- c("typedr_value", class(x_call))
  attr(x_call, "typedr_assertion") <- quote(Language())
  attr(x_call, "typedr_const") <- FALSE
  expect_match(capture_typedr_cli(print(x_call)), "1 + 2", fixed = TRUE)

  x_plain <- structure(1, class = c("typedr_value", class(1)), typedr_const = FALSE)
  expect_match(capture_typedr_cli(print(x_plain)), "assertion: <none>", fixed = TRUE)

  setClass("TypedrPrintS4", slots = c(x = "numeric"))
  s4 <- new("TypedrPrintS4", x = 1)
  attr(s4, "typedr_assertion") <- quote(Any())
  attr(s4, "typedr_const") <- FALSE
  expect_match(capture_typedr_cli(print.typedr_value(s4)), "<S4", fixed = TRUE)
})

test_that("custom type printers override value display and surface printer errors", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  Email <- as_assertion_factory(function(value) {
    Character(length = 1)(value)
    value
  })
  type_printer(Email, function(value, max_items = 20) {
    sprintf("value: <Email> %s", value)
  })
  declare("x_email", Email(), value = "a@example.com")
  expect_match(capture_typedr_cli(print(x_email)), "value: <Email>", fixed = TRUE)

  BadPrinter <- as_assertion_factory(function(value) {
    Character(length = 1)(value)
    value
  })
  type_printer(BadPrinter, function(value, max_items = 20) stop("printer failed"))
  declare("x_bad_printer", BadPrinter(), value = "x")
  expect_error(
    capture_typedr_cli(print(x_bad_printer)),
    class = "typedr_type_printer_error"
  )
})

test_that("pretty_fn supports folding, alternate outputs, and fn meta footer", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  fn <- function(alpha = 1, beta) {
    if (alpha > 1) return("large")
    beta + alpha # comment
  }

  string_out <- with_mocked_bindings(
    pretty_fn(
      fn,
      lineno = TRUE,
      color = TRUE,
      output = "string",
      width_align = 3,
      limit_lines = 5
    ),
    is_installed = function(pkg) FALSE,
    cli_bullets = function(...) NULL,
    .package = "typedr"
  )
  expect_match(string_out, "lines folded", fixed = TRUE)

  vector_out <- pretty_fn(
    fn,
    lineno = TRUE,
    alt_grey = FALSE,
    color = FALSE,
    output = "vector",
    width_align = 2,
    limit_lines = 20
  )
  expect_match(vector_out[[1]], " 1 function", fixed = TRUE)

  long_fn <- function(alpha = 1) {
    alpha <- alpha + 1
    alpha <- alpha + 1
    alpha <- alpha + 1
    alpha <- alpha + 1
    alpha <- alpha + 1
    alpha
  }
  cli_out <- capture_typedr_cli({
    fn_out <- pretty_fn(long_fn, color = FALSE, limit_lines = 5)
    .typedr_print_fn_meta(
      truncated = attr(fn_out, "typedr_fn_truncated", exact = TRUE),
      color = attr(fn_out, "typedr_fn_color", exact = TRUE)
    )
  })
  expect_match(cli_out, "print_whole_fn()", fixed = TRUE)

  old256 <- options(cli.num_colors = 256)
  on.exit(options(old256), add = TRUE)
  folded <- pretty_fn(long_fn, color = FALSE, output = "vector", limit_lines = 5)
  folded_i <- grep("lines folded", folded)
  marker <- folded[folded_i + c(-1L, 0L, 1L)]
  expect_match(marker[[2]], "lines folded", fixed = TRUE)
  expect_true(all(grepl("\033\\[90m", marker)))

  .typedr_state$warned_once$prettycode_missing <- NULL
  on.exit(.typedr_state$warned_once$prettycode_missing <- NULL, add = TRUE)
  meta_out <- function() {
    capture_typedr_cli(.typedr_print_fn_meta(truncated = TRUE, color = TRUE))
  }
  first <- with_mocked_bindings(meta_out(), is_installed = function(pkg) FALSE, .package = "typedr")
  second <- with_mocked_bindings(meta_out(), is_installed = function(pkg) FALSE, .package = "typedr")
  expect_match(first, "prettycode", fixed = TRUE)
  expect_no_match(second, "prettycode")
  expect_match(second, "print_whole_fn()", fixed = TRUE)
})

test_that("internal highlighting helpers cover edge paths", {
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
  expect_identical(.color_symbol_formals(character(), style = vsc_dark_plus()), character())
  expect_identical(.adjust_indent("  x", to = 4, from = 4), "  x")
})

test_that("print_stats summarizes typed functions", {
  old <- options(cli.num_colors = 1)
  on.exit(options(old), add = TRUE)

  f <- ? function(x = ? Double(), y = 2, ...) {
    if (TRUE) return(x + y)
    "unused"
  }
  out <- capture_typedr_cli(print_stats(f))
  expect_match(out, "<typedr>` function Stats", fixed = TRUE)
  expect_match(out, "Top calls:", fixed = TRUE)

  typed_return <- Double() ? function(x = ? Double()) 1.5
  expect_match(
    capture_typedr_cli(print_stats(typed_return)),
    "numbers = 1",
    fixed = TRUE
  )

  symbol_body <- function() x
  expect_no_match(
    capture_typedr_cli(print_stats(symbol_body)),
    "Top calls:",
    fixed = TRUE
  )

  function_literal <- function() {
    function(a = 1) a
  }
  expect_match(
    capture_typedr_cli(print_stats(function_literal)),
    "calls",
    fixed = TRUE
  )
})
