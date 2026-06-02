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

  expect_equal(Double()?x <- 1, 1, ignore_attr = TRUE)
  expect_error(Double()?x <- 1L, class = "typedr_initial_error")
})

test_that("declare supports missing initial value", {
  declare("z", Double())

  expect_null(z)

  z <- 1
  expect_equal(z, 1, ignore_attr = TRUE)

  expect_error(z <- 1L, class = "typedr_assign_error")
  expect_equal(z, 1, ignore_attr = TRUE)
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
  typedr::Integer()?a
  typedr::Integer()?b

  a <- 1L
  b <- 2L

  expect_equal(a, 1L, ignore_attr = TRUE)
  expect_equal(b, 2L, ignore_attr = TRUE)
})
