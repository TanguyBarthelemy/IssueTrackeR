#' @title Update database
#'
#' @description
#' Update the different local database (issues, labels and milestones) with the
#' online reference.
#'
#' @param datasets_name A named character string of length 4, specifying the
#' names of the different datasets which will be written. The names
#' \code{datasets_name} have to be \code{"open"}, \code{"closed"},
#' \code{"labels"} and \code{"milestones"}.
#' Defaults to \code{
#' c(open = "open_issues.yaml",
#'   closed = "closed_issues.yaml",
#'   labels = "list_labels.yaml",
#'   milestones = "list_milestones.yaml")
#' }.
#' @inheritParams get
#' @param \dots Additional arguments for connecting to the GitHub repository:
#' * \code{repo} A character string specifying the GitHub repository name.
#' Defaults to the package option \code{IssueTrackeR.repo}.
#' * \code{owner} A character string specifying the GitHub owner.
#' Defaults to the package option \code{IssueTrackeR.owner}.
#' (See the documentation of \code{\link[IssueTrackeR]{get}} to have more
#' information on theses parameters):
#'
#' @returns invisibly (with \code{invisible()}) \code{TRUE}.
#' @export
#'
#' @examplesIf gh::gh_token_exists() && gh::gh_rate_limit()$remaining > 0
#' \donttest{
#' jdemetra_selector <- init_selector(
#'     source = "GitHub",
#'     owner = "jdemetra",
#'     repo = "jdplus-main",
#'     state = "all"
#' )
#'
#' update_database(
#'     selector = jdemetra_selector,
#'     dataset_dir = tempdir()
#' )
#' }
#'
update_database <- function(
    selector,
    dataset_dir,
    verbose = TRUE,
    ...
) {
    issues <- get_issues(selector, verbose = verbose,...)
    write_to_dataset(
        x = issues,
        dataset_dir = dataset_dir,
        dataset_name = "list_issues.yaml",
        verbose = verbose
    )

    list_labels <- get_labels(selector, verbose = verbose, ...)
    write_to_dataset(
        x = list_labels,
        dataset_dir = dataset_dir,
        dataset_name = "list_labels.yaml",
        verbose = verbose
    )

    milestones <- get_milestones(selector, verbose = verbose,...)
    write_to_dataset(
        x = milestones,
        dataset_dir = dataset_dir,
        dataset_name = "list_milestones.yaml",
        verbose = verbose
    )

    return(invisible(TRUE))
}
