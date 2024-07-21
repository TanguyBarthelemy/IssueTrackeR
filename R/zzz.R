
#' @keywords internal
.onLoad <- function(libname, pkgname) {
    dataset_path <- file.path(tempdir(), "data") |>
        normalizePath(mustWork = FALSE)

    options(IssueTrackeR.dataset.path = dataset_path)
    options(IssueTrackeR.username = "tidyverse")
    options(IssueTrackeR.repo = "dplyr")
}
