.onAttach <- function(libname, pkgname) {
  .typedr_on_attach_pkgload()

  if (.typedr_is_loading_for_tests()) {
    return(invisible())
  }

  .typedr_inform_startup()
  invisible()
}

.onDetach <- function(libname, pkgname) {
  .typedr_on_detach_pkgload()
  invisible()
}

.typedr_assign_pkgload_shim <- function(value) {
  if (!isNamespaceLoaded("pkgload")) {
    return(invisible())
  }

  ns <- asNamespace("pkgload")
  if (!exists("shim_question", envir = ns, inherits = FALSE)) {
    return(invisible())
  }

  if (bindingIsLocked("shim_question", ns)) {
    unlockBinding("shim_question", ns)
    on.exit(lockBinding("shim_question", ns), add = TRUE)
  }

  assignInNamespace("shim_question", value, "pkgload")
  invisible()
}

.typedr_on_attach_pkgload <- function() {
  # deal with the conflicting `?` in the devtools_shims env
  # follow up in https://github.com/r-lib/pkgload/issues/265
  # nocov start
  if (!isNamespaceLoaded("pkgload")) {
    return(invisible())
  }

  typedr_q <- get("?", envir = asNamespace("typedr"), inherits = FALSE)
  original <- getFromNamespace("shim_question", "pkgload")
  attr(typedr_q, "original") <- original
  .typedr_assign_pkgload_shim(typedr_q)
  # nocov end
}

.typedr_on_detach_pkgload <- function() {
  # nocov start
  if (!isNamespaceLoaded("pkgload")) {
    return(invisible())
  }

  shim <- getFromNamespace("shim_question", "pkgload")
  original <- attr(shim, "original")
  if (!is_null(original)) {
    .typedr_assign_pkgload_shim(original)
  }
  # nocov end
}
