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
#' @inheritParams get_issues
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
#' @examples
#'
#' \donttest{
#' update_database()
#' }
#'
update_database <- function(
    dataset_dir = getOption("IssueTrackeR.dataset.dir"),
    datasets_name = c(
        open = "open_issues.yaml",
        closed = "closed_issues.yaml",
        labels = "list_labels.yaml",
        milestones = "list_milestones.yaml"
    ),
    verbose = TRUE,
    ...
) {
    issues_open <- get_issues(
        source = "online",
        state = "open",
        verbose = verbose,
        ...
    )
    write_issues_to_dataset(
        issues = issues_open,
        dataset_dir = dataset_dir,
        dataset_name = datasets_name[["open"]],
        verbose = verbose
    )

    issues_closed <- get_issues(
        source = "online",
        state = "closed",
        verbose = verbose,
        ...
    )
    write_issues_to_dataset(
        issues = issues_closed,
        dataset_dir = dataset_dir,
        dataset_name = datasets_name[["closed"]],
        verbose = verbose
    )

    list_labels <- get_labels(source = "online", verbose = verbose, ...)
    write_labels_to_dataset(
        labels = list_labels,
        dataset_dir = dataset_dir,
        dataset_name = datasets_name[["labels"]],
        verbose = verbose
    )

    milestones <- get_milestones(source = "online", verbose = verbose, ...)
    write_milestones_to_dataset(
        milestones = milestones,
        dataset_dir = dataset_dir,
        dataset_name = datasets_name[["milestones"]],
        verbose = verbose
    )

    return(invisible(TRUE))
}
