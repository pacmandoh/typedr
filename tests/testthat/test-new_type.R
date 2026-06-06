test_that("as_assertion_factory builds assertion factories", {
  Numeric <- as_assertion_factory(function(value, length = NULL) {
    if (!is_bare_numeric(value)) {
      cli::cli_abort("not numeric")
    }
    if (!is_null(length) && length(value) != length) {
      cli::cli_abort("bad length")
    }
    value
  })

  expect_s3_class(Numeric, "assertion_factory")
  expect_true(is_function(Numeric))

  assertion <- Numeric(length = 2)
  expect_true(is_function(assertion))
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
  expect_match(body_text, ".typedr_run_assertion_check", fixed = TRUE)
  expect_match(body_text, "f(value,", fixed = TRUE)
  expect_match(body_text, "length = 1", fixed = TRUE)
})

test_that("as_assertion_factory auto-wraps simple predicates", {
  Positive <- as_assertion_factory(function(value) {
    value > 0
  })

  expect_equal(Positive()(1), 1)
  err <- rlang::catch_cnd(Positive()(0), "error")

  expect_s3_class(err, "typedr_custom_assertion_error")
  expect_match(conditionMessage(err), "value > 0", fixed = TRUE)
  expect_match(conditionMessage(err), "value: 0", fixed = TRUE)
})

test_that("as_assertion_factory uses custom predicate messages", {
  Positive <- as_assertion_factory(
    function(value) value > 0,
    message = "`value` must be positive."
  )

  err <- rlang::catch_cnd(Positive()(0), "error")

  expect_s3_class(err, "typedr_custom_assertion_error")
  expect_match(conditionMessage(err), "`value` must be positive.", fixed = TRUE)
  expect_match(conditionMessage(err), "value > 0", fixed = TRUE)
})

test_that("custom predicate diagnostics truncate long expressions and values", {
  LongPredicate <- as_assertion_factory(function(value) {
    value %in% paste0("allowed-value-", seq_len(100))
  })

  err <- rlang::catch_cnd(
    LongPredicate()(paste(rep("x", 100), collapse = "")),
    "error"
  )
  lines <- strsplit(conditionMessage(err), "\n", fixed = TRUE)[[1]]

  expect_true(all(nchar(lines, type = "width") < 140L))
  expect_match(conditionMessage(err), "...", fixed = TRUE)
})

test_that("as_assertion_factory supports explicit predicate mode", {
  IsTrue <- as_assertion_factory(
    function(value) value,
    mode = "predicate"
  )
  BadPredicate <- as_assertion_factory(
    function(value) "not logical",
    mode = "predicate"
  )

  expect_true(IsTrue()(TRUE))
  expect_error(IsTrue()(FALSE), class = "typedr_custom_assertion_error")
  expect_error(BadPredicate()(1), class = "typedr_custom_assertion_error")
})

test_that("as_assertion_factory wraps ordinary errors without repeated parents", {
  BaseError <- as_assertion_factory(function(value) {
    stop("plain failure", call. = FALSE)
  })
  BaseErrorWithMessage <- as_assertion_factory(
    function(value) stop("plain failure", call. = FALSE),
    message = "`value` failed the custom check."
  )
  BaseErrorWithSameMessage <- as_assertion_factory(
    function(value) stop("plain failure", call. = FALSE),
    message = "plain failure"
  )
  RlangError <- as_assertion_factory(function(value) {
    rlang::abort("rlang failure", class = "custom_rlang_error")
  })

  base_err <- rlang::catch_cnd(BaseError()(1), "error")
  custom_msg_err <- rlang::catch_cnd(BaseErrorWithMessage()(1), "error")
  same_msg_err <- rlang::catch_cnd(BaseErrorWithSameMessage()(1), "error")
  rlang_err <- rlang::catch_cnd(RlangError()(1), "error")

  expect_s3_class(base_err, "typedr_custom_assertion_error")
  expect_null(base_err$parent)
  expect_match(conditionMessage(base_err), "plain failure", fixed = TRUE)
  expect_no_match(
    conditionMessage(base_err),
    "Custom assertion failed.",
    fixed = TRUE
  )

  expect_s3_class(custom_msg_err, "typedr_custom_assertion_error")
  expect_null(custom_msg_err$parent)
  expect_match(
    conditionMessage(custom_msg_err),
    "`value` failed the custom check.",
    fixed = TRUE
  )

  expect_s3_class(same_msg_err, "typedr_custom_assertion_error")
  expect_null(same_msg_err$parent)
  expect_equal(
    length(gregexpr(
      "plain failure",
      conditionMessage(same_msg_err),
      fixed = TRUE
    )[[1]]),
    1L
  )

  expect_s3_class(rlang_err, "typedr_custom_assertion_error")
  expect_null(rlang_err$parent)
  expect_match(conditionMessage(rlang_err), "rlang failure", fixed = TRUE)
})

test_that("as_assertion_factory keeps custom assertion errors concise", {
  EvenInteger <- as_assertion_factory(function(value) {
    Integer(length = 1)(value)
    if (value %% 2L != 0L) {
      stop("`value` must be even.", call. = FALSE)
    }
    value
  })

  err <- rlang::catch_cnd(EvenInteger()(3L), "error")

  expect_s3_class(err, "typedr_custom_assertion_error")
  expect_null(err$parent)
  expect_equal(conditionMessage(err), "`value` must be even.")
})

test_that("as_assertion_factory validates custom messages", {
  expect_error(
    as_assertion_factory(function(value) TRUE, message = c("a", "b")),
    class = "typedr_input_error"
  )
})

test_that("assertion factory helpers cover predicate label fallbacks", {
  expect_null(.typedr_predicate_label(sum))
  expect_null(.typedr_predicate_label(function(value) {}))

  err <- rlang::catch_cnd(
    .typedr_run_assertion_check(
      function() FALSE,
      1,
      mode = "predicate",
      predicate = NULL
    ),
    "error"
  )

  expect_s3_class(err, "typedr_custom_assertion_error")
  expect_match(
    conditionMessage(err),
    "custom predicate evaluated to FALSE.",
    fixed = TRUE
  )
})

test_that("as_assertion_factory preserves existing typedr assertion errors", {
  err <- rlang::catch_cnd(Integer()("x"), "error")

  expect_s3_class(err, "typedr_type_mismatch")
  expect_null(err$parent)
})

test_that("custom assertion errors use factory calls instead of generated wrappers", {
  PositiveDouble <- as_assertion_factory(function(value) {
    Double(length = 1)(value)
    value
  })

  err <- rlang::catch_cnd(PositiveDouble()(1L), "error")

  expect_s3_class(err, "typedr_type_mismatch")
  expect_identical(err$call, quote(PositiveDouble()))
  expect_no_match(conditionMessage(err), "`f()`", fixed = TRUE)
})

test_that("as_assertion_factory removes generated source refs when rethrowing typedr errors", {
  PositiveDouble <- as_assertion_factory(function(value) {
    value <- Double(length = 1)(value)
    if (value <= 0) {
      cli::cli_abort("`value` must be positive.")
    }
    value
  })

  err <- rlang::catch_cnd(PositiveDouble()(1L), "error")

  expect_s3_class(err, "typedr_type_mismatch")
  expect_null(attr(err$call, "srcref"))
})

test_that("auto mode preserves logical values returned as checked values", {
  Identity <- as_assertion_factory(function(value) value)

  expect_false(Identity()(FALSE))
  expect_true(Identity()(TRUE))
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
    if (!is_character(value)) {
      cli::cli_abort("not character")
    }
    value
  })

  fruit <- Fruit(~ value %in% c("apple", "pear"))
  named_fruit <- Fruit("not fruit" ~ . %in% c("apple", "pear"))

  expect_equal(fruit("apple"), "apple")
  expect_error(fruit("potato"), class = "typedr_custom_assertion_error")
  expect_error(
    named_fruit("potato"),
    "not fruit",
    class = "typedr_custom_assertion_error"
  )
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

test_that("additional assertion dots use factory calls instead of generated wrappers", {
  any_named <- Any(anyNA = FALSE)
  any_formula <- Any(... = ~ . > 0)

  named_err <- rlang::catch_cnd(any_named(NA), "error")
  formula_err <- rlang::catch_cnd(any_formula(-1), "error")

  expect_s3_class(named_err, "typedr_custom_assertion_error")
  expect_s3_class(formula_err, "typedr_custom_assertion_error")
  expect_identical(named_err$call, quote(Any()))
  expect_identical(formula_err$call, quote(Any()))
  expect_no_match(conditionMessage(named_err), "`f()`", fixed = TRUE)
  expect_no_match(conditionMessage(formula_err), "`f()`", fixed = TRUE)
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
  expect_identical(
    infer_implicit_assignment_call(function() NULL),
    expr(Function())
  )
  expect_identical(
    infer_implicit_assignment_call(new.env()),
    expr(Environment())
  )
  expect_identical(infer_implicit_assignment_call(quote(x)), expr(Symbol()))
  expect_identical(
    infer_implicit_assignment_call(pairlist(x = 1)),
    expr(Pairlist())
  )
  expect_identical(
    infer_implicit_assignment_call(quote(x + y)),
    expr(Language())
  )
  expect_identical(
    infer_implicit_assignment_call(expression(x)),
    expr(Expression())
  )
  expect_identical(infer_implicit_assignment_call(factor("a")), expr(Factor()))
  expect_identical(
    infer_implicit_assignment_call(data.frame(x = 1)),
    expr(Data.frame())
  )
  expect_identical(infer_implicit_assignment_call(matrix(1)), expr(Matrix()))
  expect_identical(infer_implicit_assignment_call(array(1)), expr(Array()))
  expect_identical(infer_implicit_assignment_call(Sys.Date()), expr(Date()))
  expect_identical(infer_implicit_assignment_call(Sys.time()), expr(Time()))
})

test_that("implicit assignment inference falls back to Any for unsupported classes", {
  obj <- structure(1, class = "custom_class")
  expect_identical(
    infer_implicit_assignment_call(obj),
    call2("Any", class = "custom_class")
  )

  obj_multi <- structure(1, class = c("custom_a", "custom_b"))
  expect_identical(
    infer_implicit_assignment_call(obj_multi),
    call2("Any", class = c("custom_a", "custom_b"))
  )
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
