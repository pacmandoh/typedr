# deal with the conflicting `?` in the  devtools_shims env
# follow up in https://github.com/r-lib/pkgload/issues/265
# nocov start

.onAttach <- function(...) {
  if (isNamespaceLoaded("pkgload")) {
    # global_entrace()
    attr(`?`, "original") <- utils::getFromNamespace("shim_question", "pkgload")
    utils::getFromNamespace("assignInNamespace", "utils")(
      "shim_question",
      `?`,
      "pkgload"
    )
  }
}

.onDetach <- function(...) {
  if (isNamespaceLoaded("pkgload")) {
    original <- attr(
      utils::getFromNamespace("shim_question", "pkgload"),
      "original"
    )
    if (!is_null(original)) {
      utils::getFromNamespace("assignInNamespace", "utils")(
        original,
        `?`,
        "pkgload"
      )
    }
  }
}
# nocov end
