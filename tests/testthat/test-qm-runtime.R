# fmt: skip file
test_that("lazy argument annotations check expressions rather than values", {
  f <- ? function(x = ?~ Symbol()) {
    TRUE
  }

  expect_true(f(a_name_that_does_not_exist))
  expect_error(f(a_name_that_does_not_exist + 1), class = "typedr_type_error")
})

test_that("bound argument annotations keep checking reassignment", {
  f_ok <- ? function(x = 1 ?+ Double()) {
    x <- 2
    x
  }

  f_bad <- ? function(x = 1 ?+ Double()) {
    x <- 2L
    x
  }

  expect_equal(f_ok(), 2, ignore_attr = TRUE)
  expect_error(f_bad(), class = "typedr_assign_error")
})

test_that("regular dots annotations check each supplied value", {
  f <- ? function(... = ? Double()) {
    length(list(...))
  }

  expect_equal(f(1, 2), 2)
  expect_error(f(1, 2L), class = "typedr_type_error")
})

test_that("lazy dots annotations check supplied expressions", {
  f <- ? function(... = ?~ Symbol()) {
    length(enexprs(...))
  }

  expect_equal(f(a, b), 2)
  expect_error(f(a + b), class = "typedr_type_error")
})

test_that("Dots annotations check the dots container", {
  f <- ? function(... = ? Dots(2, each = Double())) {
    sum(...)
  }

  expect_equal(f(1, 2), 3)
  expect_error(f(1), class = "typedr_type_error")
  expect_error(f(1, 2L), class = "typedr_type_error")
})

test_that("typed return checks cover implicit and explicit returns", {
  implicit_ok <- Integer() ? function(x = ? Logical()) {
    if (x) {
      1L
    } else {
      2L
    }
  }

  explicit_bad <- Integer() ? function() {
    if (TRUE) {
      return(1)
    }
    2L
  }

  expect_equal(implicit_ok(TRUE), 1L)
  expect_equal(implicit_ok(FALSE), 2L)
  expect_error(explicit_bad(), class = "typedr_return_error")
})

test_that("function assignment with typed return stores typedr metadata", {
  expect_no_error(
    f_runtime_meta <- Double() ? function(x = ? Double()) {
      x + 1
    }
  )

  expect_s3_class(f_runtime_meta, "typedr")
  expect_identical(attr(f_runtime_meta, "return_type"), expr(Double()))
  expect_identical(attr(f_runtime_meta, "arg_types")$x, expr(Double()))
  expect_equal(f_runtime_meta(1), 2)
  expect_error(f_runtime_meta(1L), class = "typedr_type_error")
})

test_that("missing typed function arguments fail before assertion evaluation", {
  f <- ? function(
    id = ? Integer() | Character(),
    amount = ? Double(1)
  ) {
    TRUE
  }

  expect_warning(
    expect_error(f(), class = "typedr_missing_argument_error"),
    NA
  )
  expect_warning(
    expect_error(f(id = 1L), class = "typedr_missing_argument_error"),
    NA
  )
})

test_that("missing dependent arguments fail before assertion evaluation", {
  f <- ? function(
    a1 = 1L ? Integer(),
    a2 = ? a1:Integer() ~ Double()
  ) {
    TRUE
  }

  expect_warning(
    expect_error(f(), class = "typedr_missing_argument_error"),
    NA
  )
  expect_warning(
    expect_error(f(a1 = 1L), class = "typedr_missing_argument_error"),
    NA
  )
})

test_that("missing dependent arguments support warning severity", {
  f <- ? function(
    a1 = 1L ? Integer(),
    a2 = ? a1:Integer() ~ Warning(Double())
  ) {
    TRUE
  }

  expect_warning(
    expect_true(f()),
    class = "typedr_dependency_warning"
  )
})
