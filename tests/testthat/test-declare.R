# fmt: skip file
test_that("check_output works", {
  expect_equal(check_output(2, Double()), 2)
  expect_error(check_output(2L, Double()), class = "typedr_return_error")
})

test_that("check_output supports custom assertion expressions", {
  expect_equal(
    check_output(2, Double(), .assertion_expr = "Double"),
    2
  )

  expect_error(
    check_output(2L, Double(), .assertion_expr = "Double"),
    class = "typedr_return_error"
  )

  expect_equal(
    check_output(2, Double(), .assertion_expr = quote(my_type)),
    2
  )
})

test_that("check_output errors have structured classes", {
  err <- rlang::catch_cnd(
    check_output(2L, Double())
  )

  expect_s3_class(err, "typedr_return_error")
  expect_s3_class(err, "typedr_check_output_error")
  expect_s3_class(err, "typedr_error")
})

test_that("check_output formats the actual return type only on failure", {
  err <- rlang::catch_cnd(check_output(2L, Double()))

  expect_match(conditionMessage(err), "Integer()", fixed = TRUE)
  expect_match(conditionMessage(err), "type mismatch", fixed = TRUE)
})

test_that("typed return errors name the return assertion factory", {
  f <- Character() ? function(x = 1L ? Integer(), y = ? Integer()) {
    x + y
  }

  err <- rlang::catch_cnd(f(, 2L))

  expect_s3_class(err, "typedr_return_error")
  expect_identical(err$parent$call, quote(Character()))
  expect_match(
    conditionMessage(err),
    "Expected <Character()> return value, got <Integer()>.",
    fixed = TRUE
  )
})

test_that("check_output returns the identical value on success", {
  env_value <- environment()
  expect_identical(check_output(env_value, Environment()), env_value)

  list_value <- list(x = 1:3)
  expect_identical(check_output(list_value, List()), list_value)
})

test_that("check_output returns plain values for typedr_value inputs", {
  f <- Integer() ? function(x = 1L ? Integer(), y = ? Integer()) {
    Integer() ? h
    h <- x + y
    return(h)
  }

  out <- f(, 2L)
  expect_equal(out, 3L)
  expect_identical(class(out), "integer")
  expect_false(inherits(out, "typedr_value"))

  f_implicit <- Integer() ? function(x = 1L ? Integer(), y = ? Integer()) {
    Integer() ? h
    h <- x + y
  }
  expect_identical(f(, 2L), f_implicit(, 2L))

  wrapped <- .apply_typedr_attrs(3L, "h", quote(Integer()), FALSE)
  expect_identical(check_output(wrapped, Integer()), 3L)
})

test_that("check_output failure preserves full diagnostic text", {
  err <- rlang::catch_cnd(
    check_output(2L, Double(), .assertion_expr = quote(Double()))
  )
  lines <- strsplit(conditionMessage(err), "\n", fixed = TRUE)[[1]]

  expect_identical(err$parent$call, quote(Double()))
  expect_s3_class(err$parent, "typedr_type_mismatch")
  expect_length(lines, 5L)
  expect_equal(lines[[1]], "Return value does not satisfy the required <Type()>.")
  expect_match(
    lines[[2]],
    "Expected <Double()> return value, got <Integer()>.",
    fixed = TRUE
  )
  expect_equal(lines[[3]], "Caused by error in `Double()`:")
  expect_equal(lines[[4]], "! type mismatch")
  expect_match(
    lines[[5]],
    "`typeof(value)`: \"integer\", `expected`: \"double\"",
    fixed = TRUE
  )
})

test_that("check_arg works", {
  x <- 1
  y <- 2

  expect_equal(check_arg(x, Double()), NULL)
  expect_equal(check_arg(y, Double(), .bind = TRUE), NULL)

  expect_equal(y <- 3, 3)
  expect_equal(y, 3)

  expect_error(y <- 3L, class = "typedr_assign_error")
  expect_equal(y, 3)

  expect_error(check_arg(x, Integer()), class = "typedr_type_error")
  expect_error(
    check_arg(y, Integer(), .bind = TRUE),
    class = "typedr_type_error"
  )
})

test_that("check_arg handles non-symbol argument expressions", {
  expect_equal(
    check_arg(1 + 1, Double()),
    NULL
  )

  expect_error(
    check_arg(1L + 1L, Double()),
    class = "typedr_type_error"
  )
})

test_that("check_arg errors have structured classes", {
  x <- 1

  err <- rlang::catch_cnd(
    check_arg(x, Integer())
  )

  expect_s3_class(err, "typedr_type_error")
  expect_s3_class(err, "typedr_check_arg_error")
  expect_s3_class(err, "typedr_error")
})

test_that("unknown assertion headers use generic assertion mismatch class", {
  expect_identical(
    .typedr_assertion_error_class(c("something else")),
    "typedr_assertion_mismatch"
  )
})

test_that("declare works", {
  expect_equal(declare("x", value = 1), 1, ignore_attr = TRUE)
  expect_equal(
    declare("x", value = data.frame(a = 1)),
    data.frame(a = 1),
    ignore_attr = TRUE
  )

  foobar_obj <- structure(1, class = c("foo", "bar"))
  expect_equal(declare("x", value = foobar_obj), foobar_obj, ignore_attr = TRUE)

  expect_equal(declare("x", Double(), value = 1), 1, ignore_attr = TRUE)
  expect_equal(declare("x", Double, value = 1), 1, ignore_attr = TRUE)

  expect_equal(x <- 2, 2, ignore_attr = TRUE)
  expect_error(x <- 2L, class = "typedr_assign_error")

  expect_error(
    declare("x", Double(), value = 1L),
    class = "typedr_initial_error"
  )

  expect_equal(
    declare("x", Double(), value = 1, const = TRUE),
    1,
    ignore_attr = TRUE
  )
  expect_equal(x, 1, ignore_attr = TRUE)
  expect_error(x <- 2, class = "typedr_constant_error")

  expect_error(
    declare("x", Double(), value = 1L, const = TRUE),
    class = "typedr_initial_error"
  )

  expect_equal(Double() ? x <- 1, 1, ignore_attr = TRUE)
  expect_error(Double() ? x <- 1L, class = "typedr_initial_error")
})

test_that("declare supports nullable typed values without structure(NULL) warnings", {
  expect_no_warning(declare("nullable_x", Null(), value = NULL))
  expect_null(nullable_x)

  nullable_x <- NULL
  expect_null(nullable_x)

  expect_no_warning(
    declare("nullable_y", Character(allow_null = TRUE), value = NULL)
  )
  expect_null(nullable_y)

  nullable_y <- "ok"
  expect_equal(nullable_y, "ok", ignore_attr = TRUE)

  expect_error(nullable_y <- 1L, class = "typedr_assign_error")
})

test_that("declare supports missing initial value", {
  declare("z", Double())

  expect_null(z)

  z <- 1
  expect_equal(z, 1, ignore_attr = TRUE)

  expect_error(z <- 1L, class = "typedr_assign_error")
  expect_equal(z, 1, ignore_attr = TRUE)
})

test_that("declare unset inform reports assertion and name", {
  inform <- rlang::catch_cnd(
    .typedr_inform_declare_unset("inform_z", quote(Double()))
  )

  expect_s3_class(inform, "rlang_message")
  expect_match(conditionMessage(inform), "Declared", fixed = TRUE)
  expect_match(conditionMessage(inform), "`inform_z`", fixed = TRUE)
  expect_match(conditionMessage(inform), "Double()", fixed = TRUE)
  expect_match(conditionMessage(inform), "unset", fixed = TRUE)
})

test_that("declare skips unset inform outside the global environment", {
  calls <- 0L
  with_mocked_bindings(
    {
      f <- function() {
        declare("in_func", Double())
        in_func <- 1
        in_func
      }
      expect_equal(f(), 1, ignore_attr = TRUE)
    },
    .typedr_inform_declare_unset = function(x, assertion_quoted) {
      calls <<- calls + 1L
    },
    .package = "typedr"
  )
  expect_equal(calls, 0L)
})

test_that("declare does not inform when an initial value is supplied", {
  expect_null(rlang::catch_cnd(declare("with_value", Double(), value = 1)))
  expect_equal(with_value, 1, ignore_attr = TRUE)
})

test_that("declare supports assertion factories", {
  declare("factory_x", Double, value = 1)

  expect_equal(factory_x, 1, ignore_attr = TRUE)

  factory_x <- 2
  expect_equal(factory_x, 2, ignore_attr = TRUE)

  expect_error(factory_x <- 2L, class = "typedr_assign_error")
})

test_that("declare constants can be read repeatedly", {
  declare("k", Double(), value = 1, const = TRUE)

  expect_equal(k, 1, ignore_attr = TRUE)
  expect_error(k <- 2, class = "typedr_constant_error")
})

test_that("declare errors have structured classes", {
  err <- rlang::catch_cnd(
    declare("bad_x", Double(), value = 1L)
  )

  expect_s3_class(err, "typedr_initial_error")
  expect_s3_class(err, "typedr_declare_error")
  expect_s3_class(err, "typedr_error")
})

test_that("values are declared in separate environments", {
  typedr::Integer() ? a
  typedr::Integer() ? b

  a <- 1L
  b <- 2L

  expect_equal(a, 1L, ignore_attr = TRUE)
  expect_equal(b, 2L, ignore_attr = TRUE)
})

test_that("active bindings stay honest under repeated assignment", {
  declare("stress_number", Double(), value = 0)

  for (i in seq_len(3)) {
    stress_number <- i / 10
    expect_equal(stress_number, i / 10, ignore_attr = TRUE)
  }

  expect_error(stress_number <- 1L, class = "typedr_assign_error")

  declare("stress_const", Character(), value = "locked", const = TRUE)
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
