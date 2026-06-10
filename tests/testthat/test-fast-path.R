# fmt: skip file
test_that("native assertions use fast path on success", {
  expect_equal(Integer()(1L), 1L)
  expect_equal(Double()(1), 1)
  expect_equal(Character()("x"), "x")
  expect_equal(Logical()(TRUE), TRUE)
  expect_equal(Raw()(charToRaw("a")), charToRaw("a"))
  expect_equal(Null()(NULL), NULL)
})

test_that("fast path failures match slow path error classes", {
  expect_error(Integer()(1), class = "typedr_type_mismatch")
  expect_error(Integer(2)(1L), class = "typedr_length_mismatch")
  expect_error(Double()(1L), class = "typedr_type_mismatch")
})

test_that("fast path is disabled for dotted factory calls", {
  expect_no_fast_path(with_slow_path(Integer))
})

test_that("fast path is disabled for blocked factory formals", {
  expect_no_fast_path(List(data_frame_ok = FALSE))
  expect_no_fast_path(List(each = Double()))
  expect_no_fast_path(Factor(levels = c("a", "b")))
})

test_that("custom assertion factories stay on slow path", {
  Custom <- as_assertion_factory(function(value) {
    value
  })

  expect_no_fast_path(Custom())
  expect_equal(Custom()(1), 1)
})

test_that("generated native assertions include fast and slow checks", {
  body_text <- paste(deparse(body(Integer())), collapse = "\n")
  expect_match(body_text, ".typedr_fast_try", fixed = TRUE)
  expect_match(body_text, ".typedr_run_assertion_check", fixed = TRUE)
})

test_that("fast path rejects data frames for List(data_frame_ok = FALSE)", {
  expect_error(
    List(data_frame_ok = FALSE)(data.frame(x = 1)),
    class = "typedr_type_mismatch"
  )
})

test_that("slow path allow_null branches stay reachable when fast path disabled", {
  expect_null(with_slow_path(Integer, allow_null = TRUE)(NULL))
  expect_null(with_slow_path(Logical, allow_null = TRUE)(NULL))
  expect_null(with_slow_path(Double, allow_null = TRUE)(NULL))
  expect_null(with_slow_path(Character, allow_null = TRUE)(NULL))
  expect_null(with_slow_path(Raw, allow_null = TRUE)(NULL))
  expect_null(List(allow_null = TRUE, data_frame_ok = FALSE)(NULL))
  expect_null(Null(~TRUE)(NULL))
  expect_null(with_slow_path(Closure, allow_null = TRUE)(NULL))
  expect_null(with_slow_path(Special, allow_null = TRUE)(NULL))
  expect_null(with_slow_path(Builtin, allow_null = TRUE)(NULL))
  expect_null(with_slow_path(Environment, allow_null = TRUE)(NULL))
  expect_null(with_slow_path(Symbol, allow_null = TRUE)(NULL))
  expect_null(Pairlist(each = Integer(), allow_null = TRUE)(NULL))
  expect_null(with_slow_path(Language, allow_null = TRUE)(NULL))
  expect_null(with_slow_path(Expression, allow_null = TRUE)(NULL))
  expect_null(with_slow_path(Function, allow_null = TRUE)(NULL))
  expect_null(Factor(allow_null = TRUE, levels = "a")(NULL))
  expect_null(Data.frame(allow_null = TRUE, each = Integer())(NULL))
  expect_null(slow_matrix(allow_null = TRUE)(NULL))
  expect_null(slow_array(allow_null = TRUE)(NULL))
  expect_null(with_slow_path(Date, allow_null = TRUE)(NULL))
  expect_null(with_slow_path(Time, allow_null = TRUE)(NULL))
})

test_that("slow path success returns value when fast path disabled", {
  expect_equal(with_slow_path(Logical)(TRUE), TRUE)
  expect_equal(with_slow_path(Integer)(1L), 1L)
  expect_equal(with_slow_path(Double)(1.5), 1.5)
  expect_equal(with_slow_path(Raw)(charToRaw("a")), charToRaw("a"))
  expect_equal(with_slow_path(Character)("x"), "x")
  expect_identical(
    with_slow_path(Closure, allow_null = FALSE)(function() {}),
    function() {}
  )
  expect_identical(with_slow_path(Special, allow_null = FALSE)(`if`), `if`)
  expect_identical(with_slow_path(Builtin, allow_null = FALSE)(`+`), `+`)
  expect_identical(
    with_slow_path(Environment, allow_null = FALSE)(emptyenv()),
    emptyenv()
  )
  expect_identical(with_slow_path(Symbol, allow_null = FALSE)(quote(x)), quote(x))
  expect_identical(
    with_slow_path(Language, allow_null = FALSE)(quote(x + y)),
    quote(x + y)
  )
  expect_identical(
    with_slow_path(Expression)(expression(1 + 1)),
    expression(1 + 1)
  )
  expect_identical(
    with_slow_path(Function, allow_null = FALSE)(identity),
    identity
  )
  expect_identical(slow_matrix()(matrix(1)), matrix(1))
  expect_identical(slow_array()(array(1)), array(1))
  expect_equal(with_slow_path(Date)(as.Date("2020-01-01")), as.Date("2020-01-01"))
  expect_identical(
    with_slow_path(Time)(as.POSIXct("2020-01-01")),
    as.POSIXct("2020-01-01")
  )
})
