#' Set up a package to use typedr
#'
#' This sets up your package so it can use typedr. It adds typedr to
#' `DESCRIPTION`, ensures the package documentation file exists, and imports
#' typedr's exported helpers.
#'
#' typedr is a modernized rlang/cli refactor of the original
#' [typed](https://github.com/moodymudskipper/typed) package, so this helper is
#' intended for packages that want typed-style runtime checks with typedr's
#' structured errors and richer print methods.
#'
#' @return `NULL`, invisibly. Called for side effects.
#' @export

# nocov start
use_typedr <- function() {
  check_installed("usethis")
  check_installed("desc")

  usethis::use_package("typedr")
  pkg_doc_path <- sprintf(
    "R/%s-package.R",
    desc::desc(usethis::proj_get())$get_field("Package")
  )
  if (!file.exists(pkg_doc_path)) {
    usethis::use_package_doc(open = FALSE)
  }
  usethis::use_import_from(
    "typedr",
    getNamespaceExports("typedr"),
    load = FALSE
  )
  invisible(NULL)
}
# nocov end
