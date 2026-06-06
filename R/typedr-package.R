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
