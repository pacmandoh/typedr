.typedr_fast_blocked_formals <- c("each", "levels")

.typedr_fast_factory_eligible <- function(typedr_fast, f_call, footer) {
  if (is_null(typedr_fast) || !is_null(footer)) {
    return(FALSE)
  }

  args <- as.list(f_call)
  args[[1L]] <- NULL
  if (length(args) > 0L) {
    args[[1L]] <- NULL
  }
  arg_names <- names2(args)

  if (any(.typedr_fast_blocked_formals %in% arg_names)) {
    return(FALSE)
  }

  if ("data_frame_ok" %in% arg_names && isFALSE(args$data_frame_ok)) {
    return(FALSE)
  }

  TRUE
}

.typedr_fast_call_args <- function(f_call) {
  args <- as.list(f_call)
  args[[1L]] <- NULL
  if (length(args) > 0L) {
    args[[1L]] <- NULL
  }
  args
}

.typedr_fast_assertion_header <- function(spec, params) {
  params_expr <- if (length(params) == 0L) {
    quote(list())
  } else {
    as.call(c(list(quote(list)), params))
  }
  fast_try_call <- call2(
    ".typedr_fast_try",
    sym("value"),
    spec = spec,
    params = params_expr
  )
  call2(
    "{",
    call2("<-", sym("fast"), fast_try_call),
    call2(
      "if",
      call2("$", sym("fast"), sym("ok")),
      call2("return", call2("$", sym("fast"), sym("value")))
    )
  )
}

.typedr_fast_try <- function(value, spec, params = list()) {
  length <- params$length
  allow_null <- params$allow_null %||% FALSE
  params$length <- NULL
  params$allow_null <- NULL

  if (isTRUE(allow_null) && is_null(value)) {
    return(list(ok = TRUE, value = NULL))
  }

  ok <- switch(
    spec$kind,
    null = is_null(value),
    any = {
      if (!is_null(length) && length(value) != length) {
        FALSE
      } else {
        TRUE
      }
    },
    atomic = {
      if (typeof(value) != spec$typeof) {
        FALSE
      } else if (!is_null(length) && length(value) != length) {
        FALSE
      } else {
        TRUE
      }
    },
    typeof = {
      if (typeof(value) != spec$typeof) {
        FALSE
      } else if (!is_null(length) && length(value) != length) {
        FALSE
      } else {
        TRUE
      }
    },
    class = {
      if (!(spec$class %in% class(value))) {
        FALSE
      } else if (!is_null(length) && length(value) != length) {
        FALSE
      } else {
        TRUE
      }
    },
    is_function = is_function(value),
    is_factor = {
      if (!is.factor(value)) {
        FALSE
      } else if (!is_null(length) && length(value) != length) {
        FALSE
      } else {
        TRUE
      }
    },
    list = {
      if (typeof(value) != "list") {
        FALSE
      } else if (!is_null(length) && length(value) != length) {
        FALSE
      } else {
        TRUE
      }
    },
    matrix = {
      if (!("matrix" %in% class(value))) {
        FALSE
      } else if (
        !is_null(params$nrow) && nrow(value) != params$nrow
      ) {
        FALSE
      } else if (
        !is_null(params$ncol) && ncol(value) != params$ncol
      ) {
        FALSE
      } else {
        TRUE
      }
    },
    data.frame = {
      if (!("data.frame" %in% class(value))) {
        FALSE
      } else if (
        !is_null(params$nrow) && nrow(value) != params$nrow
      ) {
        FALSE
      } else if (
        !is_null(params$ncol) && ncol(value) != params$ncol
      ) {
        FALSE
      } else {
        TRUE
      }
    },
    array = {
      if (!is.array(value)) {
        FALSE
      } else if (
        !is_null(params$dim) &&
          !identical(dim(value), as.integer(params$dim))
      ) {
        FALSE
      } else {
        TRUE
      }
    },
    pairlist = {
      if (typeof(value) != "pairlist") {
        FALSE
      } else if (!is_null(length) && length(value) != length) {
        FALSE
      } else {
        TRUE
      }
    },
    FALSE
  )

  if (isTRUE(ok)) {
    list(ok = TRUE, value = value)
  } else {
    list(ok = FALSE)
  }
}
