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
    write_issues_to_dataset(source = "online", ...)
    write_labels_to_dataset(source = "online", ...)
    write_milestones_to_dataset(source = "online", ...)

    return(invisible(TRUE))
}
