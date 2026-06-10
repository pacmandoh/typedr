#' typedr: Support Types for Variables, Arguments, and Return Values
#'
#' @description
#' typedr is a modernized refactor of the
#' [typed](https://github.com/moodymudskipper/typed) package for R. It supports
#' runtime type constraints for variables, function arguments, and function
#' return values.
#'
#' typedr keeps typed's core syntax and model: `?` declarations, assertion
#' factories, typed functions, and active bindings for assignment checks. The
#' implementation has been refreshed around rlang and cli so expression
#' handling, condition metadata, error messages, and print output are more
#' structured and easier to inspect. Diagnostics use public assertion names,
#' summarize repeated failures, and shorten long labels or values so generated
#' implementation details do not dominate the message.
#'
#' @section Attach:
#' Load with `library(typedr, warn.conflicts = FALSE)`, as recommended for typedr
#' and for the original [typed](https://github.com/moodymudskipper/typed) package.
#' This suppresses R's default masking messages for every export (including `?`,
#' `declare()`, and assertion factories such as `Integer()`), and typedr prints
#' its own short startup summary instead. The mask list is detected dynamically
#' from the search path. Symbols that only mask the same package are collapsed
#' into one line; a single-symbol mask or a symbol that masks multiple packages
#' is listed individually.
#'
#' `library(typedr)` without `warn.conflicts = FALSE` still works, but R prints
#' its own masking messages and typedr's summary may duplicate them.
#' See [declare] for syntax.
#'
#' Set `options(typedr.quiet = TRUE)` to suppress typedr's startup summary.
#'
#' @section Relationship with typed:
#' typedr is derived from and deeply grateful to
#' [moodymudskipper/typed](https://github.com/moodymudskipper/typed). The
#' original package established the interface and idea that typedr carries
#' forward. typedr's purpose is to continue that work with modern rlang/cli
#' internals, structured typedr error classes, and richer printing for typed
#' objects.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import cli
## usethis namespace: end
NULL
