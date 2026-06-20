#' @title Reset options
#'
#' @returns `NULL` invisibly
#' @export
#'
#' @examples
#' getOption("IssueTrackeR.owner")
#' options(IssueTrackeR.owner = "TanguyBarthelemy")
#' getOption("IssueTrackeR.owner")
#' reset_options()
#' getOption("IssueTrackeR.owner")
reset_options <- function() {
    dataset_dir <- file.path(tempdir(), "data") |>
        normalizePath(mustWork = FALSE)

    # nolint start undesirable_function_linter
    options(IssueTrackeR.dataset.dir = dataset_dir)
    options(IssueTrackeR.owner = "rjdverse")
    options(IssueTrackeR.repo = "rjdemetra")
    # nolint end
    return(invisible(NULL))
}
