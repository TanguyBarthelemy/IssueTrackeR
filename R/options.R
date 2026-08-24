#' @title Reset options
#'
#' @param verbose A logical value indicating whether to print additional
#' information. Default is \code{TRUE}.
#'
#' @returns `NULL` invisibly
#' @export
#'
#' @examples
#' getOption("IssueTrackeR.owner")
#' reset_options()
#' getOption("IssueTrackeR.owner")
reset_options <- function(verbose = TRUE) {
    dataset_dir <- file.path(tempdir(), "data") |>
        normalizePath(mustWork = FALSE)

    # nolint start undesirable_function_linter
    options(IssueTrackeR.dataset.dir = dataset_dir)
    options(IssueTrackeR.owner = "rjdverse")
    options(IssueTrackeR.repo = "rjdemetra")
    # nolint end

    if (verbose) {
        cat(
            "Reset the default options to:",
            paste(
                "\n- location for datasets is",
                dataset_dir
            ),
            paste("\n- owner: rjdverse"),
            paste("\n- repo: rjdemetra"),
            "\n"
        )
    }

    return(invisible(NULL))
}
