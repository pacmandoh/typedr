test_that("question mark works", {
  # outside of function def
  expect_equal(? x <- 1, 1, ignore_attr = TRUE)
  expect_equal(? (x) <- 1, 1, ignore_attr = TRUE)
  expect_equal(Double() ? x, NULL, ignore_attr = TRUE)
  expect_equal(Double() ? x <- 1, 1, ignore_attr = TRUE)
  expect_equal(Double() ? (x) <- 1, 1, ignore_attr = TRUE)
  expect_equal((Double() ? x <- 1), 1, ignore_attr = TRUE)

  # regular help
  expect_no_error(?mean)

  # function def
  expect_no_error(
    fun <- Double() ? function(x = ?~ Symbol(), y = ?+Double(), z = 1 ? Double()) {
      ?mean
      ? foo <- 1
      ? (foo) <- 1
      Double() ? bar <- 1
      Double() ? (bar) <- 1
      Double() ? baz
      baz <- 1
      if (TRUE) {
        return(foo)
      }
      foo
    }
  )
  expect_no_error(? function(... = ? Double()) {})
  expect_no_error(? function(... = ?~ Double()) {})
  expect_no_error(? function(... = ? Dots(2)) {})
  expect_no_error(? function(... = ?~ Dots(2)) {})
  expect_no_error(
    Function() ? fun1 <- Double() ? function() {
      1
    }
  )

  expect_error(? function(... = ?+Double()) {}, class = "typedr_dots_bind_error")

  # unary `?` + assignment with non-symbol LHS of assignment
  expect_error(
    ? (1) <- 2,
    class = "typedr_input_error"
  )

  # non-unary: `assertion ? <rhs>` with non-symbol <rhs>
  # e.g. trying to declare a call as a variable name
  expect_error(
    {
      x <- 1L
      y <- 2
      Double() ? x + y
    },
    class = "typedr_assign_error"
  )

  # non-unary: `assertion ? <rhs>` with numeric rhs (not a symbol)
  expect_error(
    Double() ? 1L,
    class = "typedr_return_error"
  )

  # unary `?` with non-symbol RHS errors (value-context)
  expect_error(
    ? list(a = "a"),
    class = "typedr_value_context_error"
  )

  # invalid LHS assertion expression errors (lhs error)
  expect_error(
    (stop("boom")) ? 1,
    class = "typedr_lhs_error"
  )

  # RHS evaluation error surfaces as rhs-eval error
  expect_error(
    Integer() ? unknown_var + 1L,
    class = "typedr_rhs_eval_error"
  )

  # binary `assertion ? name` declares binding (smoke) and wrong use of `?+` outside formals errors
  expect_no_error(Double() ? v_smoke)
  expect_null(v_smoke)

  expect_error(
    Double() ? h + x,
    class = "typedr_rhs_eval_error"
  )

  expect_error(
    Double() ? ~x,
    class = "typedr_return_error"
  )

  # binary `assertion ? <expr>` fails type check via check_output (mismatch)
  expect_error(
    {
      x <- 1
      y <- 2
      Integer() ? (x + y)
    },
    class = "typedr_return_error"
  )
})

test_that("Irrelevant return() calls are not wrapped", {
  expect_no_error({
    fun <- Function() ? function() {
      function() {
        return()
      }
    }
  })

  expect_no_error({
    fun <- Integer() ? function() {
      ret <- local({
        return(1.0)
      })
      as.integer(10)
    }
  })
})
# detach("package:typedr");covr::report()

# `?` <- typedr::`?`
