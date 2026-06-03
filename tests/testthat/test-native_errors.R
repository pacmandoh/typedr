condition_from <- function(expr) {
  tryCatch(
    force(expr),
    error = identity
  )
}

expect_typedr_error <- function(expr, class) {
  err <- condition_from(expr)
  expect_s3_class(err, class)
  expect_s3_class(err, "typedr_assertion_error")
  expect_s3_class(err, "typedr_error")
  invisible(err)
}

test_that("native assertions use typed mismatch classes", {
  expect_typedr_error(Logical()(1), "typedr_type_mismatch")
  expect_typedr_error(Integer()(1), "typedr_type_mismatch")
  expect_typedr_error(Double()(1L), "typedr_type_mismatch")
  expect_typedr_error(Character()(1), "typedr_type_mismatch")
  expect_typedr_error(Raw()(1), "typedr_type_mismatch")
  expect_typedr_error(List()(1), "typedr_type_mismatch")
  expect_typedr_error(Null()(1), "typedr_type_mismatch")
  expect_typedr_error(Closure()(1), "typedr_type_mismatch")
  expect_typedr_error(Special()(1), "typedr_type_mismatch")
  expect_typedr_error(Builtin()(1), "typedr_type_mismatch")
  expect_typedr_error(Environment()(1), "typedr_type_mismatch")
  expect_typedr_error(Symbol()(1), "typedr_type_mismatch")
  expect_typedr_error(Pairlist()(1), "typedr_type_mismatch")
  expect_typedr_error(Language()(1), "typedr_type_mismatch")
  expect_typedr_error(Expression()(1), "typedr_type_mismatch")
  expect_typedr_error(Function()(1), "typedr_type_mismatch")
  expect_typedr_error(Factor()(1), "typedr_type_mismatch")
  expect_typedr_error(Matrix()(1), "typedr_type_mismatch")
  expect_typedr_error(Array()(1), "typedr_type_mismatch")
  expect_typedr_error(Data.frame()(1), "typedr_type_mismatch")
  expect_typedr_error(Date()(1), "typedr_type_mismatch")
  expect_typedr_error(Time()(1), "typedr_type_mismatch")
})

test_that("native assertions use length, shape, and null mismatch classes", {
  expect_typedr_error(Any(2)(1), "typedr_length_mismatch")
  expect_typedr_error(Logical(2)(TRUE), "typedr_length_mismatch")
  expect_typedr_error(Expression(2)(expression(a)), "typedr_length_mismatch")
  expect_typedr_error(Date(2)(Sys.Date()), "typedr_length_mismatch")
  expect_typedr_error(Time(2)(Sys.time()), "typedr_length_mismatch")
  expect_typedr_error(Dots(2)(list(1)), "typedr_length_mismatch")

  expect_typedr_error(Matrix(2)(matrix(1)), "typedr_shape_mismatch")
  expect_typedr_error(Matrix(, 2)(matrix(1)), "typedr_shape_mismatch")
  expect_typedr_error(Data.frame(2)(data.frame(x = 1)), "typedr_shape_mismatch")
  expect_typedr_error(
    Data.frame(, 2)(data.frame(x = 1)),
    "typedr_shape_mismatch"
  )
  expect_typedr_error(Array(c(1, 2))(matrix(1)), "typedr_shape_mismatch")

  expect_typedr_error(Pairlist(allow_null = FALSE)(NULL), "typedr_null_mismatch")
})

test_that("container each failures preserve parent assertion errors", {
  list_err <- expect_typedr_error(
    List(each = Double())(list(a = 1L)),
    "typedr_element_error"
  )
  expect_s3_class(list_err$parent, "typedr_type_mismatch")
  expect_match(
    conditionMessage(list_err),
    'element 1 \\("a"\\) failed assertion'
  )

  pairlist_err <- expect_typedr_error(
    Pairlist(each = Double())(pairlist(1L)),
    "typedr_element_error"
  )
  expect_s3_class(pairlist_err$parent, "typedr_type_mismatch")
  expect_match(conditionMessage(pairlist_err), "element 1 failed assertion")

  data_frame_err <- expect_typedr_error(
    Data.frame(each = Double())(data.frame(a = 1L)),
    "typedr_column_error"
  )
  expect_s3_class(data_frame_err$parent, "typedr_type_mismatch")
  expect_match(
    conditionMessage(data_frame_err),
    'column 1 \\("a"\\) failed assertion'
  )

  dots_err <- expect_typedr_error(
    Dots(each = Double())(list(1L)),
    "typedr_element_error"
  )
  expect_s3_class(dots_err$parent, "typedr_type_mismatch")
  expect_match(conditionMessage(dots_err), "element 1 failed assertion")
})

test_that("get_assertion failures are tidy typedr errors", {
  err <- condition_from(get_assertion(not_an_active_binding))

  expect_s3_class(err, "typedr_get_assertion_error")
  expect_s3_class(err, "typedr_error")
  expect_s3_class(err$parent, "error")
  expect_match(conditionMessage(err), "Can't retrieve assertion")
})
