#' @keywords internal
.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        c(
            "Currently, the default options are:",
            paste(
                "\n- location for datasets is",
                getOption("IssueTrackeR.dataset.dir")
            ),
            paste("\n- owner:", getOption("IssueTrackeR.owner")),
            paste("\n- repo:", getOption("IssueTrackeR.repo"))
        )
    )
}

#' @keywords internal
.onLoad <- function(libname, pkgname) {
    reset_options(verbose = FALSE)
}
