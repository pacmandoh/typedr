test_that("as_assertion_factory works", {
  Numeric <- as_assertion_factory(function(value) {
    if (!is.numeric(value)) {
      stop("!!!", call. = FALSE)
    }
    value
  })
  expect_equal(Numeric()(1), 1)
  expect_error(Numeric()("a"))
  expect_equal(Numeric(anyNA = FALSE)(1), 1)
  expect_error(Numeric(anyNA = TRUE)(1))
  expect_equal(Numeric(~ . == 1)(1), 1)
  expect_error(Numeric(~ . == 1)("a"))
  expect_equal(Numeric("!!!" ~ . == 1)(1), 1)
  expect_error(Numeric("!!!" ~ . == 1)("a"))
  expect_error(Numeric("foo")(1))

  declare("x", Double())
  x <- 1
  get_assertion(x)
  expect_error(get_assertion(x), NA)
})
