
#' @keywords internal
.onLoad <- function(libname, pkgname) {
    dataset_path <- file.path(tempdir(), "data") |>
        normalizePath(mustWork = FALSE)

    # nolint start undesirable_function_linter
    options(IssueTrackeR.dataset.path = dataset_path)
    options(IssueTrackeR.username = "rjdverse")
    options(IssueTrackeR.repo = "rjdemetra")
    # nolint end
}
