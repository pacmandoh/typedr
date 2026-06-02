test_that("dependent argument annotations error when a matching guard fails", {
  f <- ? function(
    a1 = ? Integer() | Character(),
    a2 = ? a1:Integer() ~ Double()
  ) {
    TRUE
  }

  expect_true(f(1L, 1))
  expect_error(f(1L, "a"), class = "typedr_dependency_error")
  expect_true(f("a"))
  expect_true(f("a", "not checked"))
})

test_that("dependent argument annotations print as dependent checks", {
  f <- ? function(
    a1 = ? Integer() | Character(),
    a2 = ? a1:Integer() ~ Double()
  ) {
    TRUE
  }
  out <- paste(deparse(body(f)), collapse = "\n")

  expect_match(out, "check_dependent_arg(a2", fixed = TRUE)
  expect_no_match(out, "check_arg(enexpr(a2), a1:Integer())", fixed = TRUE)
})

test_that("union argument errors keep the parent chain shallow", {
  f <- ? function(
    a1 = ? Integer() | Character(),
    a2 = ? a1:Integer() ~ Double()
  ) {
    TRUE
  }
  err <- rlang::catch_cnd(f(a1 = TRUE, a2 = 1), "error")

  expect_s3_class(err, "typedr_type_error")
  expect_s3_class(err$parent, "typedr_union_error")
  expect_null(err$parent$parent)
})

test_that("dependent argument annotations can warn instead of erroring", {
  f <- ? function(
    a1 = ? Integer() | Character(),
    a2 = ? a1:Integer() ~ Warning(Double())
  ) {
    TRUE
  }

  expect_true(f(1L, 1))
  expect_warning(
    expect_true(f(1L, "a")),
    class = "typedr_dependency_warning"
  )
  expect_true(f("a", "not checked"))
})

test_that("dependent argument annotations accept explicit Error wrapper", {
  f <- ? function(
    a1 = ? Integer() | Character(),
    a2 = ? a1:Integer() ~ Error(Double())
  ) {
    TRUE
  }

  expect_true(f(1L, 1))
  expect_error(f(1L, "a"), class = "typedr_dependency_error")
})

test_that("dependent guards support multi-argument unions", {
  f <- ? function(
    a1 = ? Any(),
    a2 = ? Any(),
    a3 = ? Any(),
    out = ? a1:Integer() | a2:Character() | a3:Logical() ~ Double()
  ) {
    TRUE
  }

  expect_true(f("a", 1, 1, "not checked"))
  expect_error(f(1L, 1, 1, "a"), class = "typedr_dependency_error")
  expect_error(f("a", "b", 1, "a"), class = "typedr_dependency_error")
  expect_error(f("a", 1, TRUE, "a"), class = "typedr_dependency_error")
  expect_true(f("a", 1, 1, 1))
})

test_that("dependent guards accept assertion factories", {
  f <- ? function(
    a1 = ? Any(),
    out = ? a1:Integer ~ Double()
  ) {
    TRUE
  }

  expect_true(f(1L, 1))
  expect_error(f(1L, "bad"), class = "typedr_dependency_error")
  expect_true(f("a", "not checked"))
})

test_that("dependent guards support multi-argument intersections", {
  f <- ? function(
    a1 = ? Any(),
    a2 = ? Any(),
    out = ? a1:Integer() & a2:Character() ~ Double()
  ) {
    TRUE
  }

  expect_true(f(1L, "a", 1))
  expect_error(f(1L, "a", "bad"), class = "typedr_dependency_error")
  expect_true(f(1L, 1, "not checked"))
  expect_true(f("a", "b", "not checked"))
})

test_that("dependent guards support parentheses and negation", {
  f <- ? function(
    a1 = ? Any(),
    a2 = ? Any(),
    out = ? !(a1:Integer() | a2:Character()) ~ Double()
  ) {
    TRUE
  }

  expect_error(f("a", 1, "bad"), class = "typedr_dependency_error")
  expect_true(f(1L, 1, "not checked"))
  expect_true(f("a", "b", "not checked"))
})

test_that("Missing guards treat omitted and NULL arguments as missing", {
  f <- ? function(
    a1 = NULL ? (Null() | Integer()),
    a2 = ? !a1:Missing() ~ Double()
  ) {
    TRUE
  }

  expect_true(f())
  expect_true(f(a1 = NULL))
  expect_true(f(a1 = 1L, a2 = 1))
  expect_error(f(a1 = 1L, a2 = 1L), class = "typedr_dependency_error")
})

test_that("dependent fallbacks warn when inactive arguments are supplied", {
  f <- ? function(
    a1 = NULL ? (Null() | Integer()),
    a2 = ? !a1:Missing() ~ Double() / Warning()
  ) {
    TRUE
  }

  expect_true(f())
  expect_true(f(a1 = NULL))
  expect_warning(
    expect_true(f(a2 = 1)),
    class = "typedr_dependency_inactive_warning"
  )
  expect_warning(
    expect_true(f(a1 = NULL, a2 = 1)),
    class = "typedr_dependency_inactive_warning"
  )
  expect_true(f(a1 = 1L, a2 = 1))
  expect_error(f(a1 = 1L, a2 = 1L), class = "typedr_dependency_error")
})

test_that("dependent fallbacks error when inactive arguments are supplied", {
  f <- ? function(
    a1 = NULL ? (Null() | Integer()),
    a2 = ? !a1:Missing() ~ Double() / Error()
  ) {
    TRUE
  }

  expect_true(f())
  expect_error(
    f(a1 = NULL, a2 = 1),
    class = "typedr_dependency_inactive_error"
  )
  expect_true(f(a1 = 1L, a2 = 1))
})

test_that("dependent fallbacks validate fallback syntax", {
  expect_error(
    ? function(
      a1 = NULL ? (Null() | Integer()),
      a2 = ? !a1:Missing() ~ Double() / Ignore()
    ) {
      TRUE
    },
    class = "typedr_guard_fallback_error"
  )
  expect_error(
    ? function(
      a1 = NULL ? (Null() | Integer()),
      a2 = ? !a1:Missing() ~ Double() / (Warning() | Error())
    ) {
      TRUE
    },
    class = "typedr_guard_fallback_error"
  )
})

test_that("dependent guards report invalid guard syntax", {
  bad_guard <- ? function(
    a1 = ? Any(),
    a2 = ? Integer() ~ Double()
  ) {
    TRUE
  }

  bad_lhs <- ? function(
    a1 = ? Any(),
    a2 = ? 1:Integer() ~ Double()
  ) {
    TRUE
  }

  bad_rhs <- ? function(
    a1 = ? Any(),
    a2 = ? a1:identity ~ Double()
  ) {
    TRUE
  }

  expect_error(bad_guard(1, 1), class = "typedr_guard_error")
  expect_error(bad_lhs(1, 1), class = "typedr_guard_error")
  expect_error(bad_rhs(1, 1), class = "typedr_guard_error")
})

test_that("dependent guards report guard argument evaluation failures", {
  f <- ? function(
    a1 = ? Any(),
    a2 = ? typedr_missing_guard_arg:Integer() ~ Double()
  ) {
    TRUE
  }

  expect_error(f(1, 1), class = "typedr_guard_error")
})

test_that("Missing guards report argument evaluation failures", {
  expect_error(
    typedr:::typedr_arg_is_missing(quote(x), rlang::env()),
    class = "typedr_guard_error"
  )

  env <- rlang::env()
  rlang::env_bind_active(env, x = function() {
    stop("boom")
  })

  expect_error(
    typedr:::typedr_arg_is_missing(quote(x), env),
    class = "typedr_guard_error"
  )
})

test_that("dependent annotations reject dots and bound arguments", {
  expect_error(
    ? function(... = ? a1:Integer() ~ Double()) {},
    class = "typedr_dots_guard_error"
  )
  expect_error(
    ? function(a1 = ? Any(), a2 = ?+ (a1:Integer() ~ Double())) {},
    class = "typedr_guard_bind_error"
  )
})
