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
  expect_equal(fun(alpha, 1), 1, ignore_attr = TRUE)
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

  # binary `assertion ? <expr>` succeeds through check_output
  expect_equal(
    {
      xi <- 1L
      yi <- 2L
      Integer() ? (xi + yi)
    },
    3L,
    ignore_attr = TRUE
  )
})

test_that("question mark rewrites nested body declarations in typed functions", {
  expect_no_error(
    f <- Integer() ? function() {
      ?mean
      ? local_inferred <- 1L
      Integer() ? local_typed <- 2L
      Integer() ? local_unassigned
      local_unassigned <- 3L
      local_inferred + local_typed + local_unassigned
    }
  )

  expect_equal(f(), 6L, ignore_attr = TRUE)
})

test_that("question mark wraps explicit return values", {
  expect_no_error(
    f <- Integer() ? function(x = ? Integer()) {
      return(x)
    }
  )

  expect_equal(f(1L), 1L, ignore_attr = TRUE)
  expect_error(f(1), class = "typedr_type_error")

  expect_no_error(
    g <- Integer() ? function() {
      return(1)
    }
  )
  expect_error(g(), class = "typedr_return_error")
})

test_that("question mark checks implicit final return values", {
  expect_no_error(
    f <- Integer() ? function(x = ? Integer()) {
      x
    }
  )

  expect_equal(f(1L), 1L, ignore_attr = TRUE)
  expect_error(f(1), class = "typedr_type_error")

  expect_no_error(
    g <- Integer() ? function() {
      1
    }
  )
  expect_error(g(), class = "typedr_return_error")
})

test_that("question mark validates later active binding assignments", {
  Integer() ? typed_qm_value

  expect_null(typed_qm_value)

  typed_qm_value <- 1L
  expect_equal(typed_qm_value, 1L, ignore_attr = TRUE)

  expect_error(
    typed_qm_value <- 1,
    class = "typedr_assign_error"
  )
  expect_equal(typed_qm_value, 1L, ignore_attr = TRUE)
})

test_that("question mark can declare constants with inferred types", {
  expect_equal(? typed_qm_inferred <- 1L, 1L, ignore_attr = TRUE)
  expect_equal(typed_qm_inferred, 1L, ignore_attr = TRUE)

  expect_error(
    typed_qm_inferred <- 1,
    class = "typedr_assign_error"
  )
})

test_that("question mark handles nested expressions inside typed functions", {
  expect_no_error(
    f <- Integer() ? function(x = ? Integer()) {
      y <- x + 1L
      if (y > 1L) {
        return(y)
      }
      y
    }
  )

  expect_equal(f(1L), 2L, ignore_attr = TRUE)
  expect_error(f(1), class = "typedr_type_error")
})

test_that("question mark reports return type failures inside branches", {
  expect_no_error(
    f <- Integer() ? function(flag = ? Logical()) {
      if (flag) {
        return(1L)
      }
      1
    }
  )

  expect_equal(f(TRUE), 1L, ignore_attr = TRUE)
  expect_error(f(FALSE), class = "typedr_return_error")
})

test_that("question mark keeps help calls valid in nested contexts", {
  expect_no_error(
    f <- Function() ? function() {
      ?mean
      function() {
        ?sum
        NULL
      }
    }
  )

  expect_true(is.function(f()))
})
