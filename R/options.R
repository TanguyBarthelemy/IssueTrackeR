#' @title Reset options
#'
#' @param verbose A boolean indicating whether to print additional
#' information. Default is \code{TRUE}.
#'
#' @returns `NULL` invisibly
#' @export
#'
#' @examples
#' set.seed(5L)
#' getOption("IssueTrackeR.dataset.name")
#' reset_options()
#' getOption("IssueTrackeR.dataset.name")
reset_options <- function(verbose = TRUE) {
    dataset_dir <- file.path(tempdir(), "data") |>
        normalizePath(mustWork = FALSE)

    # nolint start undesirable_function_linter
    options(IssueTrackeR.dataset.dir = dataset_dir)
    new_name <- letters |>
        sample(size = 5L, replace = TRUE) |>
        paste(collapse = "")
    options(IssueTrackeR.dataset.name = new_name)
    options(IssueTrackeR.selector = init_selector())
    # nolint end

    if (verbose) {
        cat(
            "Reset the default options to:",
            paste("\n- location for datasets: ", dataset_dir),
            paste("\n- name for the datasets: ", new_name),
            paste("\n- selector: NULL")
        )
        cat("\n")
    }

    return(invisible(NULL))
}
