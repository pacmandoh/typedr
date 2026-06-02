#' Set variable, argument, and return types
#'
#' Use `?` to set a function's return type, argument types, or variable types
#' in the body of the function. `declare` is an alternative to set a variable's
#' type. This syntax follows the original
#' [typed](https://github.com/moodymudskipper/typed) package; typedr keeps the
#' user-facing model while using rlang and cli internally for expression
#' rewriting, active bindings, and structured errors.
#'
#' @section Set a variable's type:
#'
#' When used to set a variable's type, `?` maps
#' to `declare` so that `assertion ? var` calls `declare("var", assertion)`,
#' `assertion ? var <- value` calls `declare("var", assertion, value)`, and
#' `assertion ? (var) <- value` calls
#' `declare("var", assertion, value, const = TRUE)`.
#'
#' In those cases an active binding is defined so `var` returns `value` (or
#' `NULL` if none was provided). If `const` is `FALSE` (the default), the
#' value can later be changed by assigning to `var`, but assigning a value that
#' doesn't satisfy the assertion triggers an error.
#'
#' @section Set a function's return type:
#'
#' The syntaxes `assertion ? function(<args>) {<body>}` and
#' `fun <- assertion ? function(<args>) {<body>}` create a function of class
#' `c("typedr", "function")`.
#' The returned function will have its body modified so that return values are
#' wrapped inside a `check_output()` call. Printing the function will display
#' the return type.
#'
#' @section Set a function argument's type:
#'
#' When using the above syntax, or if we don't want to force a return type, the
#' simpler `? function(<args>) {<body>}` or `fun <- ? function(<args>) {<body>}`
#' syntax, we can set argument types by providing arguments as
#' `arg = default_value ? assertion` or `arg = ? assertion`. When the function is
#' called, argument types are checked.
#'
#' By default, arguments are only checked at the top of the function. They can be
#' assigned values that don't satisfy the assertion later in the function body.
#' To prevent this, use `arg = default_value ? +assertion` or
#' `arg = ? +assertion`.
#'
#' Note that it is easy to forget the `?` before `function`.
#'
#' If we'd rather check the quoted argument rather than the argument's value,
#' we can type `arg = default_value ? ~assertion` or
#' `arg = ? ~assertion`. A possible use case might be `arg = ? ~ Symbol()`.
#'
#' Dots can be checked too: `... = ? assertion` makes sure that every argument
#' passed through `...` satisfies the assertion.
#'
#' The special assertion factory `Dots()` can also be used. In that case the
#' checks apply to `list(...)` rather than to each element individually. For
#' instance, `function(... = ? Dots(2))` makes sure that `...` receives 2 values.
#'
#' The returned function will have its body modified so the arguments are
#' checked by `check_arg()` calls at the top. Printing the function will display
#' the argument types.
#'
#' @param lhs Left-hand side of the `?` operator.
#' @param rhs Right-hand side of the `?` operator.
#'
#' @export
#' @return
#' `declare()` (and `?` when it maps to `declare()`) returns `value` invisibly
#' and is usually called for side effects.
#' `assertion ? function(<args>) {<body>}` returns a typed function, of class `c("typedr", "function")`.
#' `fun <- assertion ? function(<args>) {<body>}` returns a typed function and
#' binds it to `fun` in the local environment.
#'
#' @examples
#' Integer() ? function(x = ? Integer()) {
#'   Integer() ? y <- 2L
#'   res <- x + y
#'   res
#' }
#' @rdname declare
#' @importFrom rlang caller_env enexpr enexprs is_missing is_symbol as_name
#' @importFrom rlang is_call call2 expr eval_bare fn_body fn_fmls fn_fmls_names
#' @importFrom rlang try_fetch is_null sym as_label new_function env_bind set_names
#' @importFrom cli cli_abort
`?` <- function(lhs, rhs) {
  call <- caller_env()
  lhs <- enexpr(lhs)
  rhs <- enexpr(rhs)
  unary_qm_lgl <- is_missing(rhs)
  if (unary_qm_lgl) {
    rhs <- lhs
    lhs <- NA
  }

  qmark_error_call <- call2("?", expr(lhs), expr(rhs))

  # Helper: convert a symbol to its name string, error if not a symbol
  .sym_name <- function(x) {
    if (is_symbol(x)) {
      return(as_name(x))
    }
    x_type <- .format_type(x) # R/utils.R
    cli_abort(
      c(
        "Invalid use of `{.field ?}`.",
        i = "Please use `{.field ?+}` or `{.field ?~}` inside function arguments definitions.",
        x = "In `{.field rhs}` or `{.field lhs}` at `{.field {x}}`: expected {.cls Symbol()}, got {x_type}."
      ),
      class = c("typedr_input_error", "typedr_qmark_error", "typedr_error"),
      call = qmark_error_call
    )
  }

  # ? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ? CASE 1: `?` used to annotate a function definition on the RHS
  if (is_call(rhs, "function")) {
    # TODO(source-ref display): This was an unfinished attempt to keep the
    # original function srcref for printing/error context. It is disabled
    # because `rhs_raw` is not defined on this path, and current printing uses
    # the rewritten function body instead.
    # rhs_lines <- try_fetch(
    #   {
    #     rhs_sr <- attr(rhs_raw, "srcref", exact = TRUE)
    #     if (!is_null(rhs_sr)) as.character(rhs_sr) else NULL
    #   },
    #   error = function(e) NULL
    # )

    value <- eval_bare(rhs, env = call)
    body <- fn_body(value)

    # Recursively rewrite `?` calls inside function body to declare or help
    .modify_qm_calls <- function(x) {
      if (!is_call(x) || !is_call(x, "?")) {
        return(x)
      }
      n <- length(x)
      if (n == 2) {
        rhs_i <- x[[2]]
        if (.is_assign_stmt(rhs_i)) { # R/utils.R
          const <- is_call(rhs_i[[2]], "(")
          nm <- .sym_name(if (const) rhs_i[[c(2, 2)]] else rhs_i[[2]])
          return(call2("declare", nm, value = rhs_i[[3]], const = const))
        }
        return(call2("help", as_label(rhs_i)))
      }
      lhs_i <- x[[2]]
      rhs_i <- x[[3]]
      if (.is_assign_stmt(rhs_i)) { # R/utils.R
        const <- is_call(rhs_i[[2]], "(")
        nm <- .sym_name(if (const) rhs_i[[c(2, 2)]] else rhs_i[[2]])
        return(call2("declare", nm, lhs_i, value = rhs_i[[3]], const = const))
      }
      call2("declare", .sym_name(rhs_i), lhs_i)
    }

    body <- .modify_qm_calls(body)

    # Insert check_arg() calls for annotated formals
    fmls <- fn_fmls(value)
    nms <- fn_fmls_names(value)

    annotated_fmls_lgl <- vapply(fmls, function(z) is_call(z, "?"), logical(1))
    args_are_annotated <- any(annotated_fmls_lgl)

    if (args_are_annotated) {
      annotations <- annotations_attr <- lapply(fmls[annotated_fmls_lgl], function(z) z[[length(z)]])

      bind_lgl <- vapply(annotations, function(z) is_call(z, "+"), logical(1))
      lazy_lgl <- vapply(annotations, function(z) is_call(z, "~"), logical(1))

      annotations_attr[bind_lgl] <- lapply(annotations_attr[bind_lgl], `[[`, 2)
      annotations[bind_lgl | lazy_lgl] <- lapply(annotations[bind_lgl | lazy_lgl], `[[`, 2)

      arg_assertion_factory_calls <- Map(function(arg_nm, ann, bind, lazy) {
        if (identical(arg_nm, "...")) {
          if (bind) {
            cli_abort(
              "Can't bind `{.field {{...}}}` with `{.field ?+}`.",
              class = c("typedr_dots_bind_error", "typedr_qmark_error", "typedr_error"),
              call = qmark_error_call
            )
          }
          is_dots <- is_call(ann, "Dots")
          if (is_dots) ann[[1]] <- expr(Dots)
          dots_sym <- sym("...")
          container <- if (lazy) call2("enexprs", dots_sym) else call2("list", dots_sym)
          return(if (is_dots) {
            expr(check_arg(!!container, !!ann))
          } else if (lazy) {
            expr(check_arg(!!container, List(each = !!ann)))
          } else {
            expr(check_arg(!!container, List(each = !!ann)))
          })
        }
        if (bind) {
          return(expr(check_arg(!!sym(arg_nm), !!ann, .bind = TRUE)))
        }
        target <- if (lazy) expr(enexpr(!!sym(arg_nm))) else expr(!!sym(arg_nm))
        expr(check_arg(!!target, !!ann))
      }, nms[annotated_fmls_lgl], annotations, bind_lgl, lazy_lgl)

      fmls[annotated_fmls_lgl] <- lapply(fmls[annotated_fmls_lgl], function(z) {
        if (length(z) == 2) expr(expr = ) else z[[2]]
      })

      # prepend checks to body; preserve existing `{}` blocks
      body_exprs <- if (is_call(body, "{")) as.list(body)[-1] else list(body)
      body <- call2("{", !!!arg_assertion_factory_calls, !!!body_exprs)
    }

    # Insert check_output() if a return type is provided
    if (lhs_is_assignment <- .is_assign_stmt(lhs)) { # R/utils.R
      return_assertion_factory <- lhs[[3]]
    } else if (lhs_is_qm <- is_call(lhs, "?")) {
      return_assertion_factory <- lhs[[c(3, 3)]]
    } else {
      return_assertion_factory <- lhs
    }

    if (!unary_qm_lgl) {
      modify_return_calls <- function(x) {
        if (!is_call(x)) {
          return(x)
        }
        if (is_call(x, "return")) {
          x[[2]] <- expr(check_output(!!x[[2]], !!return_assertion_factory))
          return(x)
        }
        if (is_call(x, c("if", "for", "while", "repeat", "{"))) {
          x[] <- lapply(x, modify_return_calls)
        }
        x
      }

      body <- modify_return_calls(body)
      last_call <- body[[length(body)]]
      if (!is_call(last_call) || !is_call(last_call, "return")) {
        body[[length(body)]] <- expr(check_output(!!last_call, !!return_assertion_factory))
      }
    }

    # Build the new function with attributes and class
    f <- new_function(fmls, body, env = call)

    if (args_are_annotated) {
      attr(f, "arg_types") <- annotations_attr
    }
    attr(f, "return_type") <- return_assertion_factory
    class(f) <- c("typedr", "function")

    if (lhs_is_assignment) {
      var_nm <- .sym_name(lhs[[2]])
      env_bind(call, !!!set_names(list(f), var_nm))
      return(invisible(f))
    } else if (lhs_is_qm) {
      var_nm <- .sym_name(lhs[[c(3, 2)]])
      fun_checker <- lhs[[2]]
      eval_bare(expr(declare(!!var_nm, !!fun_checker, !!f)), env = call)
      return(invisible(f))
    } else {
      return(f)
    }
  }

  # ? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ? CASE 2: `?` maps to declare/help outside of function-definition RHS
  if (unary_qm_lgl) {
    if (.is_assign_stmt(rhs)) { # R/utils.R
      const <- is_call(rhs[[2]], "(")
      nm <- .sym_name(if (const) rhs[[c(2, 2)]] else rhs[[2]])
      return(eval_bare(call2("declare", nm, value = rhs[[3]], const = const), env = call))
    }
    if (is_symbol(rhs)) {
      return(eval_bare(call2("help", as_label(rhs)), env = call))
    }
    cli_abort(
      c(
        "Invalid use of `{.field ?}` in a value position.",
        i = "Use `? name <- value` to declare a typed variable,
          or put `{.field ?}` before or in `function(...)`.",
        x = "Got `rhs`: `{as_label(rhs)}` (only a symbol like `? mean` is allowed here)."
      ),
      class = c("typedr_value_context_error", "typedr_qmark_error", "typedr_error"),
      call = qmark_error_call
    )
  } else {
    # if (is_call(rhs, c("+", "~"))) {
    #   cli_abort(
    #     c(
    #       "Please use `{.field ?+}` or `{.field ?~}` inside function arguments definitions.",
    #       i = "`function(x = ?+ Type(), ...) {{ ... }}`",
    #       i = "`function(x = ?~ Type(), ...) {{ ... }}`"
    #     ),
    #     class = c("typedr_qmark_bind_context_error", "typedr_qmark_error", "typedr_error"),
    #     call = qmark_error_call
    #   )
    # }

    # * 1) assertion ? (name) <- value  /  assertion ? name <- value
    if (.is_assign_stmt(rhs)) { # R/utils.R
      const <- is_call(rhs[[2]], "(")
      nm <- .sym_name(if (const) rhs[[c(2, 2)]] else rhs[[2]])
      return(eval_bare(call2("declare", nm, lhs, value = rhs[[3]], const = const), env = call))
    }

    # * 2) assertion ? name -> declare(name, assertion)
    if (is_symbol(rhs)) {
      return(eval_bare(call2("declare", .sym_name(rhs), lhs), env = call))
    }

    # * 3) Integer() ? 1, Integer() ? x + y
    assertion_fn <- try_fetch(eval_bare(lhs, env = call), error = identity)
    if (inherits(assertion_fn, "error")) {
      cli_abort(
        "Invalid assertion on the left of `{.field ?}`.",
        class = c("typedr_lhs_error", "typedr_qmark_error", "typedr_error"),
        parent = assertion_fn, call = qmark_error_call
      )
    }

    value <- try_fetch(eval_bare(rhs, env = call), error = identity)
    if (inherits(value, "error")) {
      cli_abort(
        "Failed to evaluate the right-hand side expression.",
        class = c("typedr_rhs_eval_error", "typedr_qmark_error", "typedr_error"),
        parent = value, call = qmark_error_call
      )
    }

    check_output(value, assertion_fn, .assertion_expr = lhs)
    value
  }
}
