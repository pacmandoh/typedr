test_that("check_output works", {
  expect_equal(check_output(2, Double()), 2)
  expect_error(check_output(2L, Double()), class = "typedr_return_error")
})

test_that("check_arg works", {
  x <- 1
  y <- 2
  expect_equal(check_arg(x, Double()), NULL)
  expect_equal(check_arg(y, Double(), .bind = TRUE), NULL)
  expect_equal(y <- 3, 3)
  expect_error(y <- 3L, class = "typedr_assign_error")
  # commenting because according to rhub doesn't work on
  # Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  expect_error(check_arg(x, Integer()), class = "typedr_type_error")
  expect_error(check_arg(y, Integer(), .bind = TRUE), class = "typedr_type_error")
})

test_that("declare works", {
  expect_equal(declare("x", value = 1), 1, ignore_attr = TRUE)
  expect_equal(declare("x", value = data.frame(a = 1)), data.frame(a = 1), ignore_attr = TRUE)
  sys_time <- Sys.time()
  expect_equal(declare("x", value = sys_time), sys_time, ignore_attr = TRUE)
  foobar_obj <- structure(1, class = c("foo", "bar"))
  expect_equal(declare("x", value = foobar_obj), foobar_obj, ignore_attr = TRUE)
  # expect_error(? x <- stop("!!!"))

  expect_equal(declare("x", Double(), value = 1), 1, ignore_attr = TRUE)
  expect_equal(declare("x", Double, value = 1), 1, ignore_attr = TRUE)
  expect_equal(x <- 2, 2, ignore_attr = TRUE)
  expect_error(x <- 2L, class = "typedr_assign_error")
  expect_error(declare("x", Double(), value = 1L), class = "typedr_initial_error")

  expect_equal(declare("x", Double(), value = 1, const = TRUE), 1, ignore_attr = TRUE)
  expect_equal(x, 1, ignore_attr = TRUE)
  expect_error(x <- 2, class = "typedr_constant_error")
  expect_error(declare("x", Double(), value = 1L, const = TRUE), class = "typedr_initial_error")

  expect_equal(Double() ? x <- 1, 1, ignore_attr = TRUE)
  expect_error(Double() ? x <- 1L, class = "typedr_initial_error")
})

test_that("values are declared in separate environments", {
  typedr::Integer() ? a
  typedr::Integer() ? b

  a <- 1L
  b <- 2L
  expect_equal(a, 1L, ignore_attr = TRUE)
})
