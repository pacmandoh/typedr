vsc_dark_plus <- function() {
  mk <- make_ansi_style
  list(
    reserved = mk("#C586C0"),
    number = mk("#B5CEA8"),
    null = mk("#569CD6"),
    operator = mk("#4FC1FF"),
    call = mk("#DCDCAA"),
    string = mk("#CE9178"),
    comment = function(x) style_italic(mk("#6A9955")(x)),
    bracket = c(mk("#0A7B83"), mk("#FFD866"), mk("#FF5555")),
    formals = function(x) style_italic(mk("#9CDCFE")(x))
  )
}

.highlight_typedr_basic <- function(lines, style = vsc_dark_plus()) {
  if (!length(lines)) {
    return(lines)
  }

  style_or_id <- function(name) style[[name]] %||% function(x) x
  col_reserved <- style_or_id("reserved")
  col_number <- style_or_id("number")
  col_null <- style_or_id("null")
  col_operator <- style_or_id("operator")
  col_call <- style_or_id("call")
  col_string <- style_or_id("string")
  col_comment <- style_or_id("comment")
  bracket_style <- style[["bracket"]] %||% list(function(x) x)

  reserved <- c(
    "function", "if", "else", "for", "in", "while", "repeat", "break",
    "next", "return"
  )
  constants <- c("NULL", "NA", "NaN", "Inf", "TRUE", "FALSE")
  token_re <- paste0(
    '"(?:[^"\\\\]|\\\\.)*"',
    "|'(?:[^'\\\\]|\\\\.)*'",
    "|#[^\n]*",
    "|\\b(?:", paste(c(reserved, constants), collapse = "|"), ")\\b",
    "|\\b[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?[iL]?\\b",
    "|\\b[A-Za-z.][A-Za-z0-9_.]*(?=\\s*\\()",
    "|\\?\\+|\\?~|::|:::|<-|->|<=|>=|==|!=|&&|\\|\\||[?+*/^$@~!:<>|&=\\-]",
    "|[(){}\\[\\],]"
  )

  colour_token <- function(token, bracket_i) {
    if (grepl("^#", token)) {
      return(list(text = col_comment(token), bracket_i = bracket_i))
    }
    if (grepl("^(\"|')", token)) {
      return(list(text = col_string(token), bracket_i = bracket_i))
    }
    if (token %in% reserved) {
      return(list(text = col_reserved(token), bracket_i = bracket_i))
    }
    if (token %in% constants) {
      return(list(text = col_null(token), bracket_i = bracket_i))
    }
    if (grepl("^[0-9]", token)) {
      return(list(text = col_number(token), bracket_i = bracket_i))
    }
    if (grepl("^[(){}\\[\\],]$", token)) {
      f <- bracket_style[[((bracket_i - 1L) %% length(bracket_style)) + 1L]]
      return(list(text = f(token), bracket_i = bracket_i + 1L))
    }
    if (grepl("^[A-Za-z.][A-Za-z0-9_.]*$", token)) {
      return(list(text = col_call(token), bracket_i = bracket_i))
    }
    list(text = col_operator(token), bracket_i = bracket_i)
  }

  vapply(
    lines,
    function(line) {
      m <- gregexpr(token_re, line, perl = TRUE)[[1]]
      if (identical(m[1], -1L)) {
        return(line)
      }

      lens <- attr(m, "match.length")
      out <- character(length(m) * 2 + 1)
      pos <- 1L
      k <- 1L
      bracket_i <- 1L

      for (i in seq_along(m)) {
        start <- m[[i]]
        len <- lens[[i]]
        if (start > pos) {
          out[[k]] <- substr(line, pos, start - 1L)
          k <- k + 1L
        }

        token <- substr(line, start, start + len - 1L)
        coloured <- colour_token(token, bracket_i)
        out[[k]] <- coloured$text
        bracket_i <- coloured$bracket_i
        k <- k + 1L
        pos <- start + len
      }

      if (pos <= nchar(line)) {
        out[[k]] <- substr(line, pos, nchar(line))
      } else {
        k <- k - 1L
      }

      paste0(out[seq_len(k)], collapse = "")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

.color_symbol_formals <- function(lines, style) {
  code <- paste(lines, collapse = "\n")

  str <- "(?:\"(?:[^\"\\\\]|\\\\.)*\"|'(?:[^'\\\\]|\\\\.)*')"
  ansi <- "\\x1B\\[[0-9;]*m"
  skip <- function(p) paste0("(?:", str, "|", ansi, ")(*SKIP)(*F)|", p)
  pat <- paste0(
    "([\\(,](?:\\s*", ansi, ")*\\s*)",
    "([A-Za-z.][A-Za-z0-9_.]*)",
    "(?:\\s*", ansi, ")*\\s*(?=(?:=|,|\\)))"
  )
  m <- gregexpr(skip(pat), code, perl = TRUE)

  if (identical(m[[1]][1], -1)) {
    return(lines)
  }

  s <- attr(m[[1]], "capture.start")
  l <- attr(m[[1]], "capture.length")
  s2 <- if (is_null(dim(s))) s[2] else s[, 2]
  ll <- if (is_null(dim(l))) l[2] else l[, 2]
  if (!length(s2)) {
    return(lines)
  }
  ord <- order(s2, decreasing = TRUE)
  s2 <- s2[ord]
  ll <- ll[ord]
  nb <- nchar(code, type = "bytes")
  last <- nb + 1
  out <- vector("list", length(s2) * 2 + 1)
  k <- 1
  colf <- style$formals %||% function(x) x
  for (i in seq_along(s2)) {
    a <- s2[i]
    b <- a + ll[i] - 1
    if (last > b + 1) {
      out[[k]] <- substr(code, b + 1, last - 1)
      k <- k + 1
    }
    out[[k]] <- colf(substr(code, a, b))
    k <- k + 1
    last <- a
  }
  if (last > 1) out[[k]] <- substr(code, 1, last - 1)

  strsplit(paste0(rev(out[seq_len(k)]), collapse = ""), "\n", fixed = TRUE)[[1]]
}

.adjust_indent <- function(lines, to = 4, from = 4) {
  to <- as.integer(to)[1]
  from <- as.integer(from)[1]
  if (!is.finite(to) || to < 0 || !is.finite(from) || from < 0 || to == from) {
    return(lines)
  }
  m <- regexpr("^ *", lines)
  nsp <- attr(m, "match.length")
  lvl <- nsp %/% from
  rem <- nsp %% from
  new_nsp <- pmax.int(0, lvl * to + rem)
  ifelse(nsp == 0, lines,
    paste0(strrep(" ", new_nsp), substring(lines, nsp + 1))
  )
}

.maybe_fold <- function(
    xs, indent, lineno, max_total = 20) {
  n <- length(xs)
  if (n <= max_total) {
    return(xs)
  }
  keep <- as.integer(max_total / 2)

  hidden <- n - 2 * keep
  marker <- sprintf("... %d lines folded ...", hidden)
  marker <- if (lineno) {
    c(
      paste0(strrep(" ", nchar(length(xs)) - 1), cli::symbol$arrow_up),
      paste0(strrep(" ", indent), marker),
      paste0(strrep(" ", nchar(length(xs)) - 1), cli::symbol$arrow_down)
    )
  } else {
    c(
      paste0(strrep(" ", indent), cli::symbol$arrow_up),
      paste0(strrep(" ", indent), marker),
      paste0(strrep(" ", indent), cli::symbol$arrow_down)
    )
  }

  c(
    xs[seq_len(keep)],
    marker,
    xs[seq.int(to = n, length.out = keep)]
  )
}

pretty_fn <- function(
    fn, lineno = TRUE,
    alt_grey = TRUE, color = TRUE,
    output = c("cli", "vector", "string"),
    width_align = NULL, wrap = 60,
    indent = 2, limit_lines = 20, style = vsc_dark_plus()) {
  check_function(fn)
  check_bool(lineno)
  check_bool(alt_grey)
  check_bool(color)
  check_number_whole(wrap)
  check_number_whole(indent, min = 0)
  check_number_whole(limit_lines, min = 5)

  output <- arg_match(output)

  attributes(fn) <- NULL
  lines <- deparse(fn, width.cutoff = wrap)
  llr <- length(lines)

  if (!is_null(indent) && as.integer(indent)[1] != 4) {
    lines <- .adjust_indent(lines, to = as.integer(indent)[1], from = 4)
  }

  use_prettycode <- is_installed("prettycode")
  if (!use_prettycode && color) {
    .warn_once( # R/utils.R
      id = "prettycode_missing",
      msg = c(
        "!" = "{.pkg prettycode} is not installed, using basic {.pkg typedr} syntax highlighting.",
        "i" = "Install it with {.code install.packages('prettycode')} for fuller R syntax highlighting.",
        "{col_grey('This warning is displayed once per session.')}"
      )
    )
  }

  if (color) {
    lines <- if (use_prettycode) {
      prettycode::highlight(lines, style = style)
    } else {
      .highlight_typedr_basic(lines, style = style)
    }
    lines <- .color_symbol_formals(lines, style = style)
  }

  if (lineno) {
    n <- seq_along(lines)
    width <- if (is_null(width_align)) nchar(length(lines)) else as.integer(width_align)
    idx <- sprintf(paste0("%", width, "d"), n)
    if (alt_grey) idx[n %% 2 == 0] <- col_grey(idx[n %% 2 == 0])
    lines <- paste0(idx, " ", lines)
  }

  lines <- .maybe_fold(lines, indent, lineno, max_total = limit_lines)
  lln <- length(lines)

  if (output == "cli") {
    cli_verbatim(c(
      lines,
      if (lln < llr) info <- col_grey(format_inline("Run `typedr::print_whole_fn()` to see whole function."))
    ))
    invisible(lines)
  } else if (output == "vector") {
    return(lines)
  } else {
    return(paste(lines, collapse = "\n"))
  }
}

.stats_typedr_fn <- function(x, top_k = 8L, width_cutoff = 60L) {
  fmt_bytes <- function(b) {
    u <- c("B", "KB", "MB", "GB", "TB")
    i <- 1L
    b <- as.numeric(b)
    while (b >= 1024 && i < length(u)) {
      b <- b / 1024
      i <- i + 1L
    }
    sprintf(if (b < 10 && i > 1) "%.1f %s" else "%.0f %s", b, u[i])
  }

  env_lab <- function(e) {
    nm <- environmentName(e)
    if (!nzchar(nm)) nm <- if (identical(e, .GlobalEnv)) "global" else "<unnamed>"
    nm
  }

  env_depth <- function(e) {
    d <- 0L
    while (!identical(e, emptyenv())) {
      d <- d + 1L
      e <- parent.env(e)
    }
    d
  }

  fmls <- fn_fmls(x)
  n_args <- length(fmls)
  n_defaults <- sum(!vapply(fmls, function(z) identical(z, expr(expr = )), logical(1)))
  has_dots <- any(names(fmls) == "...")
  arg_types <- attr(x, "arg_types", exact = TRUE) %||% list()

  n_annot <- sum(!vapply(arg_types, .is_assign_stmt, logical(1)))
  ret_type <- attr(x, "return_type", exact = TRUE)
  ret_label <- if (is_null(ret_type) || identical(ret_type, NA)) {
    "Any()"
  } else {
    paste(expr_deparse(ret_type), collapse = "")
  }

  sizes <- list(fn = utils::object.size(x), formals = utils::object.size(fmls), body = utils::object.size(body(x)))
  addrs <- list(fn = obj_address(x), formals = obj_address(fmls), body = obj_address(body(x)))

  counts <- new.env(parent = emptyenv())
  counts$calls_total <- 0L
  counts$returns <- 0L
  counts$strings <- 0L
  counts$numbers <- 0L
  counts$logical <- 0L
  counts$symbols <- 0L
  counts$by_head <- new.env(parent = emptyenv())

  push <- function(stk, v) {
    stk[[length(stk) + 1L]] <- v
    stk
  }
  pop <- function(stk) {
    v <- stk[[length(stk)]]
    stk[[length(stk)]] <- NULL
    list(stk = stk, v = v)
  }

  stk <- list(body(x))
  while (length(stk)) {
    pp <- pop(stk)
    stk <- pp$stk
    node <- pp$v
    if (is_call(node)) {
      counts$calls_total <- counts$calls_total + 1L
      hd_key <- deparse1(node[[1L]])
      cur <- get0(hd_key, envir = counts$by_head, inherits = FALSE, ifnotfound = 0L)
      assign(hd_key, cur + 1L, envir = counts$by_head)
      if (identical(hd_key, "return")) counts$returns <- counts$returns + 1L
      if (length(node) > 1L) for (i in seq.int(2L, length(node))) stk <- push(stk, node[[i]])
    } else if (is_pairlist(node)) {
      for (i in seq_along(node)) stk <- push(stk, node[[i]])
    } else if (is_symbol(node)) {
      counts$symbols <- counts$symbols + 1L
    } else if (is_character(node)) {
      counts$strings <- counts$strings + length(node)
    } else if (is_double(node)) {
      counts$numbers <- counts$numbers + length(node)
    } else if (is_logical(node)) {
      counts$logical <- counts$logical + length(node)
    }
  }

  heads <- ls(envir = counts$by_head, all.names = TRUE)
  freq <- if (length(heads)) {
    vapply(heads, function(k) get(k, envir = counts$by_head, inherits = FALSE), integer(1))
  } else {
    integer()
  }
  ord <- order(freq, decreasing = TRUE)
  heads <- heads[ord]
  freq <- freq[ord]
  kk <- min(as.integer(top_k), length(freq))
  top_calls <- if (kk) {
    paste0("`", heads[seq_len(kk)], "`", " [", col_green(freq[seq_len(kk)]), "]")
  } else {
    character()
  }

  body_lines <- length(deparse(body(x), width.cutoff = as.integer(width_cutoff)))

  e_cur <- environment(x)
  e_name <- env_lab(e_cur)
  e_depth <- env_depth(e_cur)

  cli_text("{col_yellow('-')} {.strong `<typedr>` function Stats}")
  cli_bullets(c(
    "i" = format_inline("Signature: {.code fn(}{paste(names(fmls), collapse = ', ')}{.code )}
      {cli::symbol$arrow_right} {.cls {ret_label}}"),
    "i" = format_inline("Args: {.field {n_args}} total,
      {.field {n_defaults}} with defaults{if (has_dots) ', + ...' else ''}"),
    "i" = format_inline("Types: {.field {n_annot}} annotated")
  ))
  cli_text("{col_yellow('-')} {.strong Address & Size}")
  cli_bullets(c(
    "i" = format_inline("Env: {.cls {e_name}} (depth: {.field {e_depth}})"),
    "i" = format_inline("Memory: fn = {.field {fmt_bytes(sizes$fn)}} {col_grey('/')}
      formals = {.field {fmt_bytes(sizes$formals)}} {col_grey('/')} body = {.field {fmt_bytes(sizes$body)}}"),
    "i" = format_inline("Address: fn<{addrs$fn}> {col_grey('/')}
      formals<{addrs$formals}> {col_grey('/')} body<{addrs$body}>")
  ))
  cli_text("{col_yellow('-')} {.strong Body {col_grey('/')} tokens}")
  cli_bullets(c(
    "i" = format_inline("Body: {.field {body_lines}} lines,
      {.field {counts$calls_total}} calls, {.field {counts$returns}} returns"),
    "i" = format_inline("Literals: strings = {.field {counts$strings}},
      numbers = {.field {counts$numbers}}, logical = {.field {counts$logical}}")
  ))
  if (length(top_calls)) cli_bullets(c("i" = format_inline("Top calls: {{top_calls}}")))
  pkg_vs <- col_grey(paste0("{{typedr}} (", as.character(utils::packageVersion("typedr")), ")"))
  cli_bullets(c("i" = format_inline("Version: {pkg_vs}")))
  invisible(x)
}
