# fmt: skip file
if (!"package:typedr" %in% search()) {
  pkgload::load_all(".", quiet = TRUE)
}

capture_typedr_cli <- function(expr) {
  out <- character()
  append <- function(x) {
    out <<- c(out, x)
  }

  with_mocked_bindings(
    {
      force(expr)
      paste(out, collapse = "\n")
    },
    cli_text = function(...) {
      append(cli::format_inline(..., .envir = parent.frame()))
    },
    cli_bullets = function(text, ...) {
      append(unname(cli::format_inline(
        text,
        .envir = parent.frame(),
        collapse = FALSE
      )))
    },
    cli_verbatim = function(text, ...) {
      append(text)
    },
    .package = "typedr"
  )
}
