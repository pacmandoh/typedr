typedr_startup_message <- function() {
  version <- .typedr_highlight_version(utils::packageVersion("typedr"))
  heart <- cli::col_red(cli::symbol$heart)
  header <- cli::format_inline(
    "{heart} {.pkg typedr} {col_grey({version})}"
  )

  if (.typedr_is_attached("conflicted")) {
    return(header)
  }

  mask_lines <- .typedr_mask_lines(.typedr_detect_masks())
  if (length(mask_lines) == 0L) {
    return(header)
  }

  paste(c(header, mask_lines), collapse = "\n")
}

.typedr_mask_lines <- function(x) {
  if (length(x) == 0L) {
    return(character())
  }

  info <- cli::col_yellow(cli::symbol$info)
  by_pkg <- list()
  multi <- character()

  for (sym in names(x)) {
    others <- gsub("^package:", "", x[[sym]][-1L])
    if (length(others) > 1L) {
      multi <- c(multi, sym)
    } else if (length(others) == 1L) {
      by_pkg[[others]] <- c(by_pkg[[others]], sym)
    }
  }

  lines <- character()

  for (pkg in sort(names(by_pkg))) {
    n <- length(by_pkg[[pkg]])
    if (n > 1L) {
      lines <- c(lines, .typedr_mask_summary_line(info, pkg, n))
    }
  }

  for (sym in names(x)) {
    if (sym %in% multi) {
      lines <- c(lines, .typedr_mask_detail_line(sym, x[[sym]], info))
      next
    }

    others <- gsub("^package:", "", x[[sym]][-1L])
    if (length(others) == 1L && length(by_pkg[[others]]) == 1L) {
      lines <- c(lines, .typedr_mask_detail_line(sym, x[[sym]], info))
    }
  }

  lines
}

.typedr_mask_summary_line <- function(info, pkg, n) {
  obj_word <- if (n == 1L) "object" else "objects"
  cli::format_inline(
    "{info} {.pkg typedr} {col_grey(\"masks\")} {col_yellow({n})} {col_grey({obj_word})} {col_grey(\"from\")} {.pkg {pkg}}"
  )
}

.typedr_mask_detail_line <- function(sym, pkgs, info) {
  others <- gsub("^package:", "", pkgs[-1L])
  typed_label <- .typedr_conflict_sym_label(sym, pkg = "typedr")
  other_labels <- vapply(
    others,
    function(pkg) {
      .typedr_conflict_sym_label(sym, pkg = pkg)
    },
    character(1L)
  )
  other_text <- paste(other_labels, collapse = ", ")
  cli::format_inline(
    "{info} {.field {typed_label}} {col_grey(\"masks\")} {col_grey({other_text})}"
  )
}

.typedr_detect_masks <- function() {
  envs <- grep("^package:", search(), value = TRUE)
  envs <- rlang::set_names(envs)

  objs <- .typedr_invert(lapply(envs, .typedr_ls_env))
  conflicts <- objs[vapply(objs, function(obj) length(obj) > 1L, logical(1L))]

  typed_names <- "package:typedr"
  conflicts <- conflicts[vapply(
    conflicts,
    function(pkg) any(pkg %in% typed_names),
    logical(1L)
  )]

  masks <- Map(.typedr_confirm_conflict, conflicts, names(conflicts))
  masks[!vapply(masks, is.null, logical(1L))]
}

.typedr_conflict_sym_label <- function(name, pkg = NULL) {
  if (identical(name, "?")) {
    paste0(pkg %||% "typedr", "::?")
  } else {
    paste0(pkg %||% "typedr", "::", name, "()")
  }
}

.typedr_confirm_conflict <- function(packages, name) {
  objs <- lapply(packages, function(pkg) get(name, pos = pkg))
  objs <- objs[vapply(objs, is.function, logical(1L))]

  if (length(objs) <= 1L) {
    return(NULL)
  }

  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1L) {
    return(NULL)
  }

  packages[order(vapply(
    packages,
    function(pkg) {
      match(pkg, search())
    },
    integer(1L)
  ))]
}

.typedr_ls_env <- function(env) {
  ls(pos = env)
}

.typedr_invert <- function(x) {
  if (length(x) == 0L) {
    return(list())
  }

  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}

.typedr_highlight_version <- function(x) {
  x <- as.character(x)

  is_dev <- function(piece) {
    piece <- suppressWarnings(as.numeric(piece))
    !is.na(piece) & piece >= 9000
  }

  pieces <- strsplit(x, ".", fixed = TRUE)
  pieces <- lapply(pieces, function(part) {
    ifelse(is_dev(part), cli::col_red(part), part)
  })
  vapply(pieces, paste, collapse = ".", FUN.VALUE = character(1L))
}

.typedr_inform_startup <- function() {
  if (isTRUE(getOption("typedr.quiet"))) {
    return(invisible())
  }

  msg <- typedr_startup_message()
  if (is_null(msg) || !nzchar(msg)) {
    return(invisible())
  }

  rlang::inform(msg, class = "packageStartupMessage")
  invisible()
}

.typedr_is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

.typedr_is_loading_for_tests <- function() {
  identical(Sys.getenv("TESTTHAT"), "true") ||
    (!interactive() &&
      identical(Sys.getenv("DEVTOOLS_LOAD"), "typedr"))
}
