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

test_that("native assertion errors never expose generated wrapper calls", {
  cases <- list(
    Logical = function() Logical()(1),
    Integer = function() Integer()(1),
    Double = function() Double()(1L),
    Character = function() Character()(1),
    Raw = function() Raw()(1),
    List = function() List()(1),
    Null = function() Null()(1),
    Closure = function() Closure()(1),
    Special = function() Special()(1),
    Builtin = function() Builtin()(1),
    Environment = function() Environment()(1),
    Symbol = function() Symbol()(1),
    Pairlist = function() Pairlist()(1),
    Language = function() Language()(1),
    Expression = function() Expression()(1),
    Function = function() Function()(1),
    Factor = function() Factor()(1),
    Matrix = function() Matrix()(1),
    Array = function() Array()(1),
    Data.frame = function() Data.frame()(1),
    Date = function() Date()(1),
    Time = function() Time()(1)
  )

  for (name in names(cases)) {
    err <- rlang::catch_cnd(cases[[name]](), "error")

    expect_no_match(conditionMessage(err), "`f()`", fixed = TRUE)
    expect_identical(err$call, as.call(list(as.name(name))))
  }
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
  expect_identical(data_frame_err$call, quote(Data.frame()))
  expect_identical(data_frame_err$parent$call, quote(Double()))
  expect_match(
    conditionMessage(data_frame_err),
    'column 1 \\("a"\\) failed assertion'
  )
  expect_no_match(conditionMessage(data_frame_err), "`f()`", fixed = TRUE)

  dots_err <- expect_typedr_error(
    Dots(each = Double())(list(1L)),
    "typedr_element_error"
  )
  expect_s3_class(dots_err$parent, "typedr_type_mismatch")
  expect_match(conditionMessage(dots_err), "element 1 failed assertion")
})

test_that("nested assertion errors hide generated factory wrapper calls", {
  err <- rlang::catch_cnd(Data.frame(each = Double()) ? x <- iris, "error")

  expect_s3_class(err, "typedr_initial_error")
  expect_identical(err$parent$call, quote(Data.frame(each = Double())))
  expect_identical(err$parent$parent$call, quote(Double()))
  expect_match(conditionMessage(err), "Caused by error in `Double()`", fixed = TRUE)
  expect_no_match(conditionMessage(err), "`f()`", fixed = TRUE)
})

test_that("container each failures are summarized when many items fail", {
  df <- as.data.frame(setNames(rep(list(1L), 6), letters[1:6]))
  data_frame_err <- expect_typedr_error(
    Data.frame(each = Double())(df),
    "typedr_column_error"
  )

  expect_match(conditionMessage(data_frame_err), "6 columns failed assertion.", fixed = TRUE)
  expect_match(conditionMessage(data_frame_err), 'column 1 ("a")', fixed = TRUE)
  expect_match(conditionMessage(data_frame_err), 'column 5 ("e")', fixed = TRUE)
  expect_match(conditionMessage(data_frame_err), "and 1 more", fixed = TRUE)
  expect_no_match(conditionMessage(data_frame_err), 'column 6 ("f")', fixed = TRUE)
  expect_identical(data_frame_err$parent$call, quote(Double()))

  list_err <- expect_typedr_error(
    List(each = Double())(setNames(rep(list(1L), 6), letters[1:6])),
    "typedr_element_error"
  )
  expect_match(conditionMessage(list_err), "6 elements failed assertion.", fixed = TRUE)
  expect_match(conditionMessage(list_err), 'element 5 ("e")', fixed = TRUE)
  expect_match(conditionMessage(list_err), "and 1 more", fixed = TRUE)
  expect_no_match(conditionMessage(list_err), "`f()`", fixed = TRUE)

  pairlist_err <- expect_typedr_error(
    Pairlist(each = Double())(as.pairlist(setNames(rep(list(1L), 6), letters[1:6]))),
    "typedr_element_error"
  )
  expect_match(conditionMessage(pairlist_err), "6 elements failed assertion.", fixed = TRUE)

  dots_err <- expect_typedr_error(
    Dots(each = Double())(setNames(rep(list(1L), 6), letters[1:6])),
    "typedr_element_error"
  )
  expect_match(conditionMessage(dots_err), "6 elements failed assertion.", fixed = TRUE)
})

test_that("assertion factory call inference falls back cleanly", {
  expect_null(.typedr_factory_call(length))

  factory <- Double
  env <- new.env(parent = emptyenv())
  environment(factory) <- env

  expect_null(.typedr_factory_call(factory))
})

test_that("get_assertion failures are tidy typedr errors", {
  err <- condition_from(get_assertion(not_an_active_binding))

  expect_s3_class(err, "typedr_get_assertion_error")
  expect_s3_class(err, "typedr_error")
  expect_s3_class(err$parent, "error")
  expect_match(conditionMessage(err), "Can't retrieve assertion")
})
