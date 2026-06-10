# fmt: skip file
test_that("typed functions cache assertion factories at definition time", {
  f <- Integer() ? function(x = ? Integer()) {
    x
  }

  body_text <- paste(deparse(body(f)), collapse = "\n")
  expect_match(body_text, ".typedr_cached_assertion_", fixed = TRUE)
  expect_equal(f(1L), 1L)
  expect_error(f(1), class = "typedr_type_error")
})

test_that("print restores cached assertion source expressions", {
  old <- options(
    cli.num_colors = 1,
    typedr.print.fn_color = FALSE,
    typedr.print.fn_lineno = FALSE
  )
  on.exit(options(old), add = TRUE)

  f <- Double() ? function(x = ? Integer()) {
    x + 0
  }
  out <- capture_typedr_cli(print(f))

  expect_match(out, "check_arg(x, Integer())", fixed = TRUE)
  expect_match(out, "check_output", fixed = TRUE)
  expect_no_match(out, ".typedr_cached_assertion_", fixed = TRUE)
})

test_that("dependent argument checks stay uncached in generated bodies", {
  f <- ? function(
    a1 = ? Integer() | Character(),
    a2 = ? a1:Integer() ~ Double()
  ) {
    TRUE
  }
  out <- paste(deparse(body(f)), collapse = "\n")

  expect_match(out, "check_dependent_arg(a2", fixed = TRUE)
  expect_match(out, ".typedr_cached_assertion_", fixed = TRUE)
})

test_that("dotted factories disable native fast path", {
  dotted <- Integer(is.integer = TRUE)
  expect_no_fast_path(dotted)
  expect_equal(dotted(1L), 1L)
})

test_that("functions without cacheable assertions skip cache env", {
  f <- ? function(x) {
    x
  }
  expect_null(attr(f, "typedr_assertion_sources", exact = TRUE))
})

test_that("print restores bind checks from cached assertions", {
  old <- options(
    cli.num_colors = 1,
    typedr.print.fn_color = FALSE,
    typedr.print.fn_lineno = FALSE
  )
  on.exit(options(old), add = TRUE)

  f <- ? function(x = ?+ Integer()) {
    x
  }
  out <- capture_typedr_cli(print(f))
  expect_match(out, "check_arg(x, Integer(), .bind = TRUE)", fixed = TRUE)
})

test_that("restore replaces cached assertion symbols for display", {
  sources <- list(.typedr_cached_assertion_1 = quote(Integer()))
  restored <- typedr:::.typedr_restore_assertion_sources(
    quote(check_arg(x, .typedr_cached_assertion_1)),
    sources
  )
  expect_identical(restored, quote(check_arg(x, Integer())))
})

test_that("restore leaves non-call leaves unchanged", {
  expect_identical(
    typedr:::.typedr_restore_assertion_sources(1L, list()),
    1L
  )
})
