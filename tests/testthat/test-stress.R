test_that("active bindings keep declared values and argument bindings honest under repeated assignment", {
  declare("stress_number", Double(), value = 0)

  for (i in seq_len(3)) {
    stress_number <- i / 10
    expect_equal(stress_number, i / 10, ignore_attr = TRUE)
  }

  expect_error(stress_number <- 1L, class = "typedr_assign_error")
  expect_equal(stress_number, 0.3, ignore_attr = TRUE)

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
  f <- Double()?function(x = ?Double(), y = ?Double()) {
    x + y
  }

  for (i in seq_len(3)) {
    x <- i + 0
    expect_equal(f(x, i / 2), x + i / 2)
  }

  expect_error(f(1L, 2), class = "typedr_type_error")

  bad_return <- Character()?function(x = ?Double()) {
    x + 1
  }
  expect_error(bad_return(1), class = "typedr_return_error")
})
