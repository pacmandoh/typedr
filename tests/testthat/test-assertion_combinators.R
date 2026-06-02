test_that("typedr assertions support union with |", {
  number <- Integer() | Double()

  expect_equal(number(1L), 1L)
  expect_equal(number(1), 1)
  expect_error(number("a"), class = "typedr_union_error")
})

test_that("union errors do not expose nested candidate failures", {
  err <- rlang::catch_cnd((Integer() | Character())(TRUE), "error")

  expect_s3_class(err, "typedr_union_error")
  expect_null(err$parent)
})

test_that("typedr assertions support union with c()", {
  number <- c(Integer(), Double())

  expect_equal(number(1L), 1L)
  expect_equal(number(1), 1)
  expect_error(number("a"), class = "typedr_union_error")
})

test_that("typedr assertion unions keep readable labels", {
  union <- Integer() | Double()
  nested_union <- c(union, Character())

  expect_equal(
    attr(union, "typedr_assertion_label"),
    "Integer() | Double()"
  )
  expect_equal(
    attr(nested_union, "typedr_assertion_label"),
    "Integer() | Double() | <typedr type>"
  )
})

test_that("typedr assertions support intersection with &", {
  positive_integer <- Integer() & Any(... = ~ . > 0L)

  expect_equal(positive_integer(1L), 1L)
  expect_error(positive_integer(0L), class = "typedr_intersection_error")
  expect_error(positive_integer(1), class = "typedr_intersection_error")
})

test_that("intersection errors do not expose nested candidate failures", {
  err <- rlang::catch_cnd((Integer() & Character())(1L), "error")

  expect_s3_class(err, "typedr_intersection_error")
  expect_null(err$parent)
})

test_that("typedr assertions support cascaded unions", {
  scalar_value <- Integer() | Double() | Character() | Logical()

  expect_equal(scalar_value(1L), 1L)
  expect_equal(scalar_value(1), 1)
  expect_equal(scalar_value("a"), "a")
  expect_equal(scalar_value(TRUE), TRUE)
  expect_error(scalar_value(list(1)), class = "typedr_union_error")
})

test_that("typedr assertions support cascaded intersections", {
  small_positive_integer <- Integer() &
    Any(... = ~ . > 0L) &
    Any(... = ~ . < 10L)

  expect_equal(small_positive_integer(1L), 1L)
  expect_error(small_positive_integer(0L), class = "typedr_intersection_error")
  expect_error(small_positive_integer(10L), class = "typedr_intersection_error")
  expect_error(small_positive_integer(1), class = "typedr_intersection_error")
})

test_that("union assertions work in function arguments", {
  f <- ? function(x = ? Integer() | Double()) {
    x
  }

  expect_equal(f(1L), 1L)
  expect_equal(f(1), 1)
  expect_error(f("a"), class = "typedr_type_error")
})

test_that("c() union assertions work in function arguments", {
  f <- ? function(x = ? c(Integer(), Double())) {
    x
  }

  expect_equal(f(1L), 1L)
  expect_equal(f(1), 1)
  expect_error(f("a"), class = "typedr_type_error")
})

test_that("intersection assertions work in function arguments", {
  f <- ? function(x = ? Integer() & Any(... = ~ . > 0L)) {
    x
  }

  expect_equal(f(1L), 1L)
  expect_error(f(0L), class = "typedr_type_error")
  expect_error(f(1), class = "typedr_type_error")
})

test_that("combining typedr assertions rejects non-assertions", {
  expect_error(
    typedr:::typedr_combine_assertions(list(), "or"),
    class = "typedr_combinator_error"
  )
  expect_error(Integer() | 1, class = "typedr_combinator_error")
  expect_error(Integer() & 1, class = "typedr_combinator_error")
  expect_error(c(Integer(), 1), class = "typedr_combinator_error")
})

test_that("cascaded combined assertions work in function arguments", {
  union_fun <- ? function(x = ? Integer() | Double() | Character()) {
    x
  }
  intersection_fun <- ? function(
    x = ? Integer() & Any(... = ~ . > 0L) & Any(... = ~ . < 10L)
  ) {
    x
  }

  expect_equal(union_fun(1L), 1L)
  expect_equal(union_fun(1), 1)
  expect_equal(union_fun("a"), "a")
  expect_error(union_fun(TRUE), class = "typedr_type_error")

  expect_equal(intersection_fun(1L), 1L)
  expect_error(intersection_fun(0L), class = "typedr_type_error")
  expect_error(intersection_fun(10L), class = "typedr_type_error")
})

test_that("combined assertions work for return values", {
  f <- (Integer() | Double()) ? function(use_integer = FALSE) {
    if (use_integer) {
      1L
    } else {
      1
    }
  }
  g <- (Integer() | Double()) ? function() "a"

  expect_equal(f(TRUE), 1L)
  expect_equal(f(FALSE), 1)
  expect_error(g(), class = "typedr_return_error")
})

test_that("combined assertions work in declare and bound arguments", {
  declare("typed_number", Integer() | Double(), value = 1L)
  expect_equal(typed_number, 1L, ignore_attr = TRUE)
  expect_no_error(typed_number <- 1)
  expect_error(typed_number <- "a", class = "typedr_assign_error")

  f <- ? function(x = ?+ (Integer() | Double())) {
    x <- 1
    x
  }

  expect_equal(f(1L), 1, ignore_attr = TRUE)
  expect_error(f("a"), class = "typedr_type_error")
})
