if (!exists("capture_typedr_cli", mode = "function")) {
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
}

test_that("native assertion factories survive broad success and failure cases", {
  cases <- list(
    list(Logical, list(TRUE, c(TRUE, FALSE)), list(1, "x")),
    list(Integer, list(1L, c(1L, 2L)), list(1, TRUE)),
    list(Double, list(1, c(1, 2)), list(1L, "x")),
    list(Character, list("x", c("x", "y")), list(1, TRUE)),
    list(Raw, list(charToRaw("a")), list("a", 1L)),
    list(List, list(list(), list(1, "x")), list(1, matrix(1))),
    list(Null, list(NULL), list(0, FALSE)),
    list(Closure, list(function() NULL), list(sum, 1)),
    list(Builtin, list(sum), list(function() NULL, 1)),
    list(Environment, list(new.env(parent = emptyenv())), list(list(), 1)),
    list(Symbol, list(quote(x)), list(quote(x + y), "x")),
    list(Pairlist, list(pairlist(x = 1)), list(list(x = 1), quote(x))),
    list(Language, list(quote(x + y)), list(quote(x), "x")),
    list(Expression, list(expression(x), expression(x + y)), list(quote(x), "x")),
    list(Factor, list(factor("a"), factor(c("a", "b"))), list("a", 1L)),
    list(Data.frame, list(data.frame(x = 1), data.frame(x = 1:2, y = c("a", "b"))), list(list(x = 1), matrix(1))),
    list(Matrix, list(matrix(1), matrix(1:4, nrow = 2)), list(array(1, c(1, 1, 1)), data.frame(x = 1))),
    list(Array, list(array(1, c(1, 1, 1)), matrix(1)), list(1, data.frame(x = 1))),
    list(Date, list(Sys.Date(), as.Date(c("2026-01-01", "2026-01-02"))), list("2026-01-01", 1)),
    list(Time, list(Sys.time(), as.POSIXct(c("2026-01-01", "2026-01-02"), tz = "UTC")), list(Sys.Date(), "2026-01-01"))
  )

  for (case in cases) {
    factory <- case[[1]]
    assertion <- factory()

    for (value in case[[2]]) {
      expect_error(assertion(value), NA)
      expect_identical(assertion(value), value)
    }

    for (value in case[[3]]) {
      expect_error(assertion(value), class = "typedr_assertion_error")
    }
  }
})

test_that("shape, length, null, and nested assertions remain strict", {
  expect_error(Character(length = 2)(c("a", "b")), NA)
  expect_error(Character(length = 2)("a"), class = "typedr_length_mismatch")
  expect_error(Character(null_ok = TRUE)(NULL), NA)
  expect_error(Character(null_ok = FALSE)(NULL), class = "typedr_type_mismatch")

  expect_error(Matrix(nrow = 2, ncol = 2)(matrix(1:4, nrow = 2)), NA)
  expect_error(Matrix(nrow = 3)(matrix(1:4, nrow = 2)), class = "typedr_shape_mismatch")
  expect_error(Array(dim = c(2, 2, 2))(array(seq_len(8), c(2, 2, 2))), NA)
  expect_error(Array(dim = c(2, 2))(array(seq_len(8), c(2, 2, 2))), class = "typedr_shape_mismatch")

  expect_error(List(each = Character())(list("a", "b")), NA)
  expect_error(List(each = Character())(list("a", 1)), class = "typedr_element_error")
  expect_error(Pairlist(each = Double())(pairlist(a = 1, b = 2)), NA)
  expect_error(Pairlist(each = Double())(pairlist(a = 1, b = 2L)), class = "typedr_element_error")
  expect_error(Data.frame(each = Integer())(data.frame(a = 1L, b = 2L)), NA)
  expect_error(Data.frame(each = Integer())(data.frame(a = 1L, b = 2)), class = "typedr_column_error")
  expect_error(Dots(each = Character())(list("a", "b")), NA)
  expect_error(Dots(each = Character())(list("a", 1)), class = "typedr_element_error")
})

test_that("active bindings keep declared values and argument bindings honest under repeated assignment", {
  declare("stress_number", Double(), value = 0)

  for (i in seq_len(20)) {
    stress_number <- i / 10
    expect_equal(stress_number, i / 10, ignore_attr = TRUE)
  }

  expect_error(stress_number <- 1L, class = "typedr_assign_error")
  expect_equal(stress_number, 2, ignore_attr = TRUE)

  declare("stress_const", Character(), value = "locked", const = TRUE)
  expect_equal(stress_const, "locked", ignore_attr = TRUE)
  expect_error(stress_const <- "open", class = "typedr_constant_error")

  f <- function(x) {
    check_arg(x, Double(), .bind = TRUE)
    x <- x + 1
    expect_error(x <- 1L, class = "typedr_assign_error")
    x
  }
  expect_equal(f(1), 2)
  expect_error(f(1L), class = "typedr_type_error")
})

test_that("question mark syntax holds across repeated runtime checks", {
  f <- Double() ? function(x = ? Double(), y = ? Double()) {
    x + y
  }

  for (i in seq_len(25)) {
    x <- i + 0
    expect_equal(f(x, i / 2), x + i / 2)
  }

  expect_error(f(1L, 2), class = "typedr_type_error")

  bad_return <- Character() ? function(x = ? Double()) {
    x + 1
  }
  expect_error(bad_return(1), class = "typedr_return_error")
})

test_that("custom assertion factories, type printers, and printing compose without leaking", {
  Email <- as_assertion_factory(function(value) {
    Character(length = 1)(value)
    if (!grepl("@", value)) {
      cli::cli_abort("not an email")
    }
    value
  })

  expect_error(Email()("a@example.com"), NA)
  expect_error(Email()("bad"), "not an email")

  type_printer(Email, function(value, max_items = 20) {
    sprintf("{.field value}: {.cls Email} %s", value)
  })

  declare("stress_email", Email(), value = "a@example.com")
  out <- capture_typedr_cli(print(stress_email))
  expect_match(out, "value: <Email> a@example.com", fixed = TRUE)
  expect_match(out, "assertion: <Email()>", fixed = TRUE)

  expect_match(capture_typedr_cli(print(Email)), "<typedr type factory>", fixed = TRUE)
  expect_match(capture_typedr_cli(print(Email())), "<typedr type>", fixed = TRUE)
})

test_that("value printers smoke test small, large, nested, and shaped values", {
  values <- list(
    character = c("a", "b", "c", "d", "e"),
    factor = factor(rep(c("low", "high"), 6)),
    date = as.Date("2026-01-01") + seq_len(12),
    time = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + seq_len(12),
    matrix = matrix(seq_len(100), nrow = 10),
    array = array(seq_len(27), c(3, 3, 3)),
    data_frame = data.frame(
      id = seq_len(12),
      name = paste0("item_", seq_len(12)),
      flag = rep(c(TRUE, FALSE), 6)
    ),
    list = list(a = 1, b = c("x", "y", "z"), nested = list(ok = TRUE, day = as.Date("2026-01-01")))
  )

  assertions <- list(
    character = Character(),
    factor = Factor(),
    date = Date(),
    time = Time(),
    matrix = Matrix(),
    array = Array(),
    data_frame = Data.frame(),
    list = List()
  )

  for (nm in names(values)) {
    declare(paste0("stress_print_", nm), assertions[[nm]], value = values[[nm]])
    obj <- get(paste0("stress_print_", nm), inherits = FALSE)
    expect_error(capture_typedr_cli(print(obj, max_items = 4)), NA)
    expect_error(capture_typedr_cli(print_whole_value(obj)), NA)
  }
})
