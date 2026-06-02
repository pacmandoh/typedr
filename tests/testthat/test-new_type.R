test_that("as_assertion_factory builds assertion factories", {
  Numeric <- as_assertion_factory(function(value, length = NULL) {
    if (!is.numeric(value)) {
      cli::cli_abort("not numeric")
    }
    if (!is.null(length) && length(value) != length) {
      cli::cli_abort("bad length")
    }
    value
  })

  expect_s3_class(Numeric, "assertion_factory")
  expect_true(is.function(Numeric))

  assertion <- Numeric(length = 2)
  expect_true(is.function(assertion))
  expect_equal(assertion(c(1, 2)), c(1, 2))
  expect_error(assertion(1), "bad length")
  expect_error(Numeric()("x"), "not numeric")
})

test_that("as_assertion_factory preserves generated assertion shape", {
  Numeric <- as_assertion_factory(function(value, length = NULL) {
    value
  })

  assertion <- Numeric(length = 1)
  body_text <- paste(deparse(body(assertion)), collapse = "\n")

  expect_match(body_text, "f <- function", fixed = TRUE)
  expect_match(body_text, "value <- f(value, length = 1)", fixed = TRUE)
})

test_that("named dots add value-derived checks", {
  ScalarNoMissing <- as_assertion_factory(function(value) {
    value
  })

  assertion <- ScalarNoMissing(length = 1L, anyNA = FALSE)

  expect_equal(assertion(1), 1)
  expect_error(assertion(c(1, 2)), class = "typedr_custom_assertion_error")
  expect_error(assertion(NA), class = "typedr_custom_assertion_error")
})

test_that("formula dots add custom checks with and without custom messages", {
  Fruit <- as_assertion_factory(function(value) {
    if (!is.character(value)) {
      cli::cli_abort("not character")
    }
    value
  })

  fruit <- Fruit(~ value %in% c("apple", "pear"))
  named_fruit <- Fruit("not fruit" ~ . %in% c("apple", "pear"))

  expect_equal(fruit("apple"), "apple")
  expect_error(fruit("potato"), class = "typedr_custom_assertion_error")
  expect_error(named_fruit("potato"), "not fruit", class = "typedr_custom_assertion_error")
})

test_that("malformed dots fail with typedr input errors", {
  AnyFactory <- as_assertion_factory(function(value) value)

  expect_error(
    AnyFactory("not a formula"),
    class = "typedr_input_error"
  )
})

test_that("process_assertion_factory_dots handles empty dots", {
  expect_null(process_assertion_factory_dots())
})

test_that("process_assertion_factory_dots returns a block expression", {
  expr <- process_assertion_factory_dots(length = 1L, anyNA = FALSE)

  expect_true(is_call(expr, "{"))
  expect_length(as.list(expr), 3)
})

test_that("implicit assignment inference uses expected assertion calls", {
  expect_identical(infer_implicit_assignment_call(TRUE), expr(Logical()))
  expect_identical(infer_implicit_assignment_call(1L), expr(Integer()))
  expect_identical(infer_implicit_assignment_call(1), expr(Double()))
  expect_identical(infer_implicit_assignment_call("x"), expr(Character()))
  expect_identical(infer_implicit_assignment_call(charToRaw("a")), expr(Raw()))

  expect_identical(infer_implicit_assignment_call(list()), expr(List()))
  expect_identical(infer_implicit_assignment_call(NULL), expr(Null()))
  expect_identical(infer_implicit_assignment_call(function() NULL), expr(Function()))
  expect_identical(infer_implicit_assignment_call(new.env()), expr(Environment()))
  expect_identical(infer_implicit_assignment_call(quote(x)), expr(Symbol()))
  expect_identical(infer_implicit_assignment_call(pairlist(x = 1)), expr(Pairlist()))
  expect_identical(infer_implicit_assignment_call(quote(x + y)), expr(Language()))
  expect_identical(infer_implicit_assignment_call(expression(x)), expr(Expression()))
  expect_identical(infer_implicit_assignment_call(factor("a")), expr(Factor()))
  expect_identical(infer_implicit_assignment_call(data.frame(x = 1)), expr(Data.frame()))
  expect_identical(infer_implicit_assignment_call(matrix(1)), expr(Matrix()))
  expect_identical(infer_implicit_assignment_call(array(1)), expr(Array()))
  expect_identical(infer_implicit_assignment_call(Sys.Date()), expr(Date()))
  expect_identical(infer_implicit_assignment_call(Sys.time()), expr(Time()))
})

test_that("implicit assignment inference falls back to Any for unsupported classes", {
  obj <- structure(1, class = "custom_class")
  expect_identical(infer_implicit_assignment_call(obj), call2("Any", class = "custom_class"))

  obj_multi <- structure(1, class = c("custom_a", "custom_b"))
  expect_identical(infer_implicit_assignment_call(obj_multi), call2("Any", class = c("custom_a", "custom_b")))
})

test_that("declare can use assertion factories directly", {
  expect_equal(declare("typed_from_factory", Double, value = 1), 1, ignore_attr = TRUE)
  expect_error(typed_from_factory <- 1L, class = "typedr_assign_error")
})

test_that("get_assertion returns the active binding assertion call", {
  declare("typed_for_assertion_lookup", Integer())

  expect_no_error(get_assertion(typed_for_assertion_lookup))
  expect_identical(get_assertion(typed_for_assertion_lookup), expr(Integer()))
})

test_that("get_assertion errors when active binding has no typedr assertion call", {
  e <- new.env(parent = emptyenv())

  f <- local({
    val <- NULL
    function(assigned_value) {
      if (!missing(assigned_value)) {
        val <<- assigned_value
      }
      val
    }
  })

  makeActiveBinding("plain_binding", f, e)
  e$get_assertion_ <- get_assertion

  expect_error(
    with(e, get_assertion_(plain_binding)),
    class = "typedr_get_assertion_error"
  )
})

test_that("get_assertion tolerates instrumented active binding bodies", {
  instrumented_env <- new.env(parent = emptyenv())
  instrumented_fun <- local({
    val <- NULL
    function(assigned_value) {
      covr_count()
      if (!missing(assigned_value)) {
        covr_count()
        tmp <- tryCatch(Integer()(assigned_value), error = identity)
        val <<- tmp
      }
      val
    }
  })
  makeActiveBinding("instrumented_binding", instrumented_fun, instrumented_env)

  instrumented_env$get_assertion_ <- get_assertion
  expect_identical(
    with(instrumented_env, get_assertion_(instrumented_binding)),
    expr(Integer())
  )
})
