# fmt: skip file
load_typedr_quietly <- function() {
  old <- getOption("typedr.quiet")
  options(typedr.quiet = TRUE)
  on.exit(options(typedr.quiet = old), add = TRUE)

  if (!"package:typedr" %in% search()) {
    suppressPackageStartupMessages(
      library(typedr, warn.conflicts = FALSE)
    )
  }
}

test_that(".typedr_detect_masks reports typedr masks", {
  load_typedr_quietly()
  masks <- typedr:::.typedr_detect_masks()
  expect_true("?" %in% names(masks))
  expect_true("declare" %in% names(masks))
})

test_that("typedr startup message lists lone single-package masks", {
  masks <- list(
    "?" = c("package:typedr", "package:utils"),
    declare = c("package:typedr", "package:base")
  )
  lines <- typedr:::.typedr_mask_lines(masks)

  expect_length(lines, 2L)
  expect_match(lines[[1L]], "typedr::?", fixed = TRUE)
  expect_true(length(grep("typedr::?()", lines, fixed = TRUE)) == 0L)
  expect_match(lines[[2L]], "typedr::declare()", fixed = TRUE)
})

test_that("typedr_startup_message includes package name", {
  load_typedr_quietly()
  startup_msg <- typedr:::typedr_startup_message()
  expect_match(startup_msg, "typedr", fixed = TRUE)
})

test_that("warn.conflicts = FALSE suppresses default masking messages", {
  if ("package:typedr" %in% search()) {
    detach("package:typedr", character.only = TRUE)
  }

  old_quiet <- getOption("typedr.quiet")
  on.exit(options(typedr.quiet = old_quiet), add = TRUE)
  options(typedr.quiet = TRUE)

  out <- paste(
    capture.output(
      suppressPackageStartupMessages(
        library(typedr, warn.conflicts = FALSE)
      )
    ),
    collapse = "\n"
  )
  if (length(grep("The following object is masked", out, fixed = TRUE)) > 0L) {
    fail("typedr emitted default masking messages")
  }
})

test_that("typedr_inform_startup respects typedr.quiet", {
  old <- getOption("typedr.quiet")
  on.exit(options(typedr.quiet = old), add = TRUE)

  options(typedr.quiet = TRUE)
  expect_silent(typedr:::.typedr_inform_startup())
})

test_that(".typedr_mask_lines collapses bulk single-package masks", {
  masks <- list(
    "?" = c("package:typedr", "package:foo", "package:utils"),
    declare = c("package:typedr", "package:foo", "package:base"),
    Integer = c("package:typedr", "package:foo"),
    Character = c("package:typedr", "package:foo")
  )
  lines <- typedr:::.typedr_mask_lines(masks)

  expect_length(lines, 3L)
  expect_match(lines[[1L]], "2 objects from foo", fixed = TRUE)
  expect_match(lines[[2L]], "typedr::?", fixed = TRUE)
  expect_match(lines[[3L]], "typedr::declare()", fixed = TRUE)
})

test_that(".typedr_mask_lines collapses repeated masks from same package", {
  masks <- list(
    Integer = c("package:typedr", "package:base"),
    Character = c("package:typedr", "package:base"),
    Double = c("package:typedr", "package:base")
  )
  lines <- typedr:::.typedr_mask_lines(masks)

  expect_length(lines, 1L)
  expect_match(lines[[1L]], "3 objects from base", fixed = TRUE)
})

test_that(".typedr_mask_lines handles empty input", {
  expect_length(typedr:::.typedr_mask_lines(list()), 0L)
})

test_that("typedr_startup_message returns header only without masks", {
  msg <- with_mocked_bindings(
    typedr:::typedr_startup_message(),
    .typedr_detect_masks = function() {
      list()
    },
    .package = "typedr"
  )
  expect_equal(length(strsplit(msg, "\n", fixed = TRUE)[[1L]]), 1L)
})

test_that("typedr_startup_message omits masks with conflicted attached", {
  msg <- with_mocked_bindings(
    typedr:::typedr_startup_message(),
    .typedr_is_attached = function(x) {
      identical(x, "conflicted")
    },
    .package = "typedr"
  )
  expect_equal(length(strsplit(msg, "\n", fixed = TRUE)[[1L]]), 1L)
})

test_that(".typedr_confirm_conflict returns NULL for non-conflicts", {
  expect_null(typedr:::.typedr_confirm_conflict("package:typedr", "declare"))

  f <- function() 1
  attach(new.env(parent = emptyenv()), name = "package:typedr_covr_a")
  attach(new.env(parent = emptyenv()), name = "package:typedr_covr_b")
  on.exit({
    detach("package:typedr_covr_a", character.only = TRUE)
    detach("package:typedr_covr_b", character.only = TRUE)
  }, add = TRUE)
  assign("f", f, envir = as.environment("package:typedr_covr_a"))
  assign("f", f, envir = as.environment("package:typedr_covr_b"))
  expect_null(typedr:::.typedr_confirm_conflict(
    c("package:typedr_covr_a", "package:typedr_covr_b"),
    "f"
  ))
})

test_that(".typedr_invert handles empty input", {
  expect_equal(typedr:::.typedr_invert(list()), list())
})

test_that("typedr_inform_startup informs startup message", {
  old <- getOption("typedr.quiet")
  on.exit(options(typedr.quiet = old), add = TRUE)
  options(typedr.quiet = FALSE)

  expect_message(typedr:::.typedr_inform_startup(), "typedr")
})

test_that("typedr_inform_startup skips empty message", {
  old <- getOption("typedr.quiet")
  on.exit(options(typedr.quiet = old), add = TRUE)
  options(typedr.quiet = FALSE)

  expect_silent(with_mocked_bindings(
    typedr:::.typedr_inform_startup(),
    typedr_startup_message = function() "",
    .package = "typedr"
  ))
})

test_that("onAttach and onDetach run in test environment", {
  expect_invisible(typedr:::.onAttach(libname = NULL, pkgname = "typedr"))
  expect_invisible(typedr:::.onDetach(libname = NULL, pkgname = "typedr"))
})
