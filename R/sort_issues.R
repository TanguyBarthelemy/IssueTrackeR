
simple_sort <- function(issues, sorting_variables) {

    if (length(issues) == 0L) {
        return(new_issues())
    }

    sorted_issues <- issues

    for (sorting_variable in rev(sorting_variables)) {
        if (sorting_variable[["object"]] == "milestones") {

            milestones <- get_milestones()
            index_milestones <- order(milestones[[sorting_variable[["field"]]]])
            sorted_milestones_titles <- milestones[["title"]][index_milestones]

            ref_issues <- sorted_issues
            sorted_issues <- new_issues()
            for (milestone in sorted_milestones_titles) {
                sorted_group <- ref_issues |>
                    IssueTrackeR::filter_issues(
                        fields = "milestone",
                        values = milestone
                    )
                sorted_issues <- c(sorted_issues, sorted_group)
            }
            sorted_issues <- c(sorted_issues, no_milestones(ref_issues))
        } else if (sorting_variable[["object"]] == "issues") {
            sorted_index <- order(vapply(
                X = sorted_issues,
                FUN = base::`[[`,
                sorting_variable[["field"]],
                FUN.VALUE = integer(1L)
            ))
            sorted_issues <- sorted_issues[sorted_index]
        } else {
            stop("Object non accepted.")
        }
    }

    return(sorted_issues)
}

#' @title Sort issues
#'
#' @description
#' Sorting issues with some constraint and order on the labels, the title, the
#' milestones and/or the body.
#'
#' @param x a \code{IssuesTB} object.
#' @param decreasing logical. Should the sort be increasing or decreasing?
#' @param sorting_variables a list containing the quantitative variables to sort
#' the issues. The filters are applied in the order of the variables supplied.
#' @param filtering_factors a list containing constraints for sorting issues by
#' sub-group in order of priority
#' @param ... Unused argument
#'
#' @returns a \code{IssuesTB} object sorted.
#' @details
#' In the order of the constraints imposed by the \code{filtering_factors}
#' argument, the function will first filter by constraint. For each constraint,
#' the function will then sort according to the quantitative variables supplied
#' in \code{sorting_variables}.
#'
#' For example, the following call:
#'
#' \code{
#' sort(
#'     x = issues,
#'     sorting_variables = list(list(object = "milestones", field = "due_on"),
#'                              list(object = "issues", field = "created_at")),
#'     filtering_factors = list(list(values = "bug",
#'                                   fields = "labels",
#'                                   values_logic_gate = "OR"),
#'                              list(values = "package", fields = "title")),
#'     decreasing = TRUE
#' )
#' }
#'
#' will behave as follows:
#'     1) It will select all the issues that have "bug" as a label, then sort
#'     them according to the chronological order of milestones (according to
#'     deadlines) and the chronological order of issue creation dates
#'     2) Among the remaining issues, it will filter the issues that have
#'     \code{"package"} in the title and apply the same sorting.
#'     3) Finally, among all the remaining issues (not sorted until now), the
#'     function will apply the same sorting.
#'     4) The function returns the global list of sorted issues.
#'
#' The argument filtering_factors is a list of constraint following the same
#' naming convention as the \code{\link[IssueTrackeR]{filter_issues}}. So the
#' constraints are represented by named lists with the various arguments (apart
#' from \code{x}) to the \code{\link[IssueTrackeR]{filter_issues}}
#' (\code{values}, \code{fields}, \code{fields_logic_gate},
#' \code{values_logic_gate} and \code{negate}).
#'
#' @export
#' @examples
#' write_milestones_to_dataset()
#' all_issues <- get_issues(source = "online", verbose = FALSE)
#' sort(
#'     x = all_issues,
#'     sorting_variables = list(list(object = "milestones", field = "due_on"),
#'                              list(object = "issues", field = "created_at")),
#'     filtering_factors = list(list(values = "bug",
#'                                   fields = "labels",
#'                                   values_logic_gate = "OR"),
#'                              list(values = "package", fields = "title"))
#' )
#'
#' @exportS3Method sort IssuesTB
#' @method sort IssuesTB
#'
sort.IssuesTB <- function(x, decreasing = FALSE,
                          sorting_variables = list(),
                          filtering_factors = list(),
                          ...) {

    remaining_issues <- x
    selected_issues <- new_issues()

    for (filtering_factor in filtering_factors) {
        filtering_factor[["negate"]] <- isTRUE(filtering_factor[["negate"]])
        filtered_issues <- do.call(
            what = filter_issues,
            args = c(list(x = remaining_issues), filtering_factor)
        )
        sorted_issues <- simple_sort(filtered_issues, sorting_variables)
        selected_issues <- c(selected_issues, sorted_issues)
        filtering_factor[["negate"]] <- !filtering_factor[["negate"]]
        remaining_issues <- do.call(
            what = filter_issues,
            args = c(list(x = remaining_issues), filtering_factor)
        )
    }

    sorted_issues <- simple_sort(remaining_issues, sorting_variables)
    selected_issues <- c(selected_issues, sorted_issues)

    if (decreasing) {
        selected_issues <- rev(selected_issues)
    }
    return(selected_issues)
}
