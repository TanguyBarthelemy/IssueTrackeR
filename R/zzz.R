#' @keywords internal
.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        c(
            "Currently, the default options are:",
            paste("\n- location for datasets is", getOption("IssueTrackeR.dataset.dir")),
            paste("\n- owner:", getOption("IssueTrackeR.owner")),
            paste("\n- repo:", getOption("IssueTrackeR.repo"))
        )
    )
}

#' @keywords internal
.onLoad <- function(libname, pkgname) {
    dataset_dir <- file.path(tempdir(), "data") |>
        normalizePath(mustWork = FALSE)

    # nolint start undesirable_function_linter
    options(IssueTrackeR.dataset.dir = dataset_dir)
    options(IssueTrackeR.owner = "rjdverse")
    options(IssueTrackeR.repo = "rjdemetra")
    # nolint end
}
