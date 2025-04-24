
#' @title Sorting issues
#'
#' @description
#' Sort issues based on quantitative variables
#'
#' @inheritParams write_issues_to_dataset
#' @inheritParams write_milestones_to_dataset
#' @param sorting_variables a list of variable (represented by couples of
#' \code{object} and \code{field} in the form
#' \code{c(object = "object_1", field = "field_1")})
#' @param \dots Additional arguments passed to \code{\link{get_milestones}}.
#' Only used if \code{milestones} is missing.
#'
#' @returns the same list of issues as \code{issues} (class \code{IssuesTB})
#' but in a sorted order.
#'
#' @keywords internal
#'
simple_sort <- function(issues, sorting_variables, milestones, ...) {

    if (length(issues) == 0L) {
        return(new_issues())
    }

    sorted_issues <- issues

    for (sorting_variable in rev(sorting_variables)) {
        if (sorting_variable[["object"]] == "milestones") {

            if (missing(milestones)) {
                milestones <- get_milestones(...)
            }

            if (nrow(milestones) > 0L) {

                field <- sorting_variable[["field"]]
                index_milestones <- base::order(milestones[[field]])
                sorted_titles <- milestones[["title"]][index_milestones]

                ref_issues <- sorted_issues
                sorted_issues <- new_issues()
                for (title in sorted_titles) {
                    sorted_group <- IssueTrackeR::filter_issues(
                        x = ref_issues,
                        fields = "milestone",
                        values = title
                    )
                    sorted_issues <- c(sorted_issues, sorted_group)
                }
                sorted_issues <- c(sorted_issues, no_milestones(ref_issues))
            }

        } else if (sorting_variable[["object"]] == "issues") {
            sorted_index <- base::order(vapply(
                X = sorted_issues,
                FUN = base::`[[`,
                sorting_variable[["field"]],
                FUN.VALUE = double(1L)
            ))
            sorted_issues <- sorted_issues[sorted_index]
        } else {
            stop("Object non accepted.", call. = FALSE)
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
#' @param \dots Additional arguments related to milestones for the function
#' \code{simple_sort}.
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
#' \preformatted{
#' sort(
#'     x = issues,
#'     sorting_variables = list(c(object = "milestones", field = "due_on"),
#'                              c(object = "issues", field = "created_at")),
#'     filtering_factors = list(list(values = "bug",
#'                                   fields = "labels",
#'                                   values_logic_gate = "OR"),
#'                              list(values = "package", fields = "title")),
#'     decreasing = TRUE
#' )
#' }
#'
#' will behave as follows:
#'
#' * 1) It will select all the issues that have "bug" as a label, then sort
#'     them according to the chronological order of milestones (according to
#'     deadlines) and the chronological order of issue creation dates
#' * 2) Among the remaining issues, it will filter the issues that have
#'     \code{"package"} in the title and apply the same sorting.
#' * 3) Finally, among all the remaining issues (not sorted until now), the
#'     function will apply the same sorting.
#' * 4) The function returns the global list of sorted issues.
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
#'
#' \donttest{
#' # Get the milestones of the prject
#' milestones <- get_milestones("online")
#' write_milestones_to_dataset(milestones)
#'
#' all_issues <- get_issues(source = "online", verbose = FALSE)
#' sorted_issues <- sort(
#'     x = all_issues,
#'     sorting_variables = list(list(object = "milestones", field = "due_on"),
#'                              list(object = "issues", field = "created_at")),
#'     filtering_factors = list(list(values = "bug",
#'                                   fields = "labels",
#'                                   values_logic_gate = "OR"),
#'                              list(values = "package", fields = "title")),
#'     milestones = milestones
#' )
#' }
#'
#' @exportS3Method sort IssuesTB
#' @method sort IssuesTB
#'
sort.IssuesTB <- function(x,
                          decreasing = FALSE,
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
        sorted_issues <- simple_sort(filtered_issues, sorting_variables, ...)
        selected_issues <- c(selected_issues, sorted_issues)
        filtering_factor[["negate"]] <- !filtering_factor[["negate"]]
        remaining_issues <- do.call(
            what = filter_issues,
            args = c(list(x = remaining_issues), filtering_factor)
        )
    }

    sorted_issues <- simple_sort(remaining_issues, sorting_variables, ...)
    selected_issues <- c(selected_issues, sorted_issues)

    if (decreasing) {
        selected_issues <- rev(selected_issues)
    }
    return(selected_issues)
}
