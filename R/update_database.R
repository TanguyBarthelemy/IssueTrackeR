#' Update database
#'
#' @description
#' Update the different local database (issues, labels and milestones) with the
#' online reference.
#'
#' @param \dots Additional arguments for the functions
#' \code{write_XXX_to_dataset} where XXX is \code{"issues"}, \code{"labels"} or
#' \code{"milestones"}.
#'
#' @returns invisibly (with \code{invisible()}) \code{TRUE}.
#' @export
#'
#' @examples
#' update_database()
#'
update_database <- function(...) {
    get_issues(source = "online", state = "open", ...) |>
        write_issues_to_dataset(source = "online",
                                dataset_name = "open_issues.yaml", ...)
    get_issues(source = "online", state = "closed", ...) |>
        write_issues_to_dataset(source = "online",
                                dataset_name = "closed_issues.yaml", ...)

    write_labels_to_dataset(source = "online", ...)
    write_milestones_to_dataset(source = "online", ...)

    return(invisible(TRUE))
}
