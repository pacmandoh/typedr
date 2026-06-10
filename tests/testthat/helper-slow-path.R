# fmt: skip file

# A passing formula dot; disables native fast path via factory `...`.
.slow_path_dot <- quote(~TRUE)

.is_missing_formal <- function(x) {
  is.symbol(x) && !nzchar(as.character(x))
}

#' Call an assertion factory without native fast path.
#'
#' Fills unset factory formals with their defaults, then appends a no-op
#' formula dot (`~ TRUE`) so it reaches `...` instead of binding to a typed
#' formal such as `length` or `allow_null`.
#'
#' Factories whose defaults are `missing` (e.g. `Matrix(nrow, ncol)`) need a
#' dedicated helper such as [slow_matrix()] instead.
with_slow_path <- function(factory, ...) {
  user_args <- list(...)
  if (length(user_args)) {
    nms <- names(user_args)
    if (is.null(nms) || any(nms == "" | is.na(nms))) {
      stop("with_slow_path(): pass factory arguments by name.", call. = FALSE)
    }
  }

  fmls <- formals(factory)
  fml_names <- names(fmls)
  fml_names <- fml_names[fml_names != "..."]

  call_args <- lapply(fml_names, function(nm) {
    if (nm %in% names(user_args)) {
      user_args[[nm]]
    } else {
      fmls[[nm]]
    }
  })
  names(call_args) <- fml_names

  if (any(vapply(call_args, .is_missing_formal, logical(1L)))) {
    stop(
      "with_slow_path(): use a dedicated helper for factories with ",
      "missing defaults (e.g. slow_matrix()).",
      call. = FALSE
    )
  }

  do.call(factory, c(call_args, list(.slow_path_dot)))
}

#' @rdname with_slow_path
slow_matrix <- function(allow_null = FALSE) {
  do.call(Matrix, alist(, , allow_null = allow_null, ~TRUE))
}

#' @rdname with_slow_path
slow_array <- function(allow_null = FALSE) {
  do.call(Array, alist(, allow_null = allow_null, ~TRUE))
}

expect_no_fast_path <- function(assertion) {
  body_text <- paste(deparse(body(assertion)), collapse = "\n")
  expect_no_match(body_text, ".typedr_fast_try", fixed = TRUE)
}
