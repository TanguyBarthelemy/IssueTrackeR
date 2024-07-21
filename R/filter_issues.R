apply_logic <- function(x, logic_gate = c("AND", "OR")) {
    logic_gate <- match.arg(logic_gate)
    return(
        switch(
            EXPR = logic_gate,
            AND = all(x),
            OR = any(x)
        )
    )
}

aggregate_vector <- function(x, y, logic_gate = c("AND", "OR")) {
    logic_gate <- match.arg(logic_gate)
    return(
        switch(
            EXPR = logic_gate,
            AND = x & y,
            OR = x | y
        )
    )
}

#' @title Text in body of an issue(s)
#'
#' @param x a \code{IssueTB} or \code{IssuesTB} object.
#' @param values a vector \code{string}
#' @param logic_gate the logic operator which will aggregate the different
#' assertion related to values.
#' @param ... Unused argument
#'
#' @returns a boolean specifying if the \code{pattern} is contained in the body.
#' @details
#' For different values of \code{values}, the assertion will be constructed with
#' "OR gate" if the logic_gate argument is "OR" and "AND gate" if it is "AND".
#' This means that the call \code{body_contains(x = issue_1, values =
#' c("README", "package"), logic_gate = "OR")} will say whether the issue
#' contains the string "README" OR "package" in its body.
#' Whereas the call \code{body_contains(x = issue_1, values =
#' c("README", "package"), logic_gate = "AND")} will say whether the issue
#' contains the string "README" AND "package" in its body.
#'
#' @export
#'
#' @rdname contains
#'
#' @examples
#' all_issues <- get_issues(type = "online", verbose = FALSE)
#' issue_1 <- all_issues[[1L]]
#' body_contains(x = issue_1, values = "README")
#' body_contains(
#'     x = issue_1,
#'     values = c("README", "package"),
#'     logic_gate = "AND"
#' )
#'
body_contains <- function(x, ...){
    UseMethod("body_contains", x)
}

#' @rdname contains
#' @exportS3Method body_contains IssueTB
#' @method body_contains IssueTB
#' @export
body_contains.IssueTB <- function(x, values, logic_gate = c("AND", "OR"), ...) {
    issue <- x

    logic_gate <- match.arg(logic_gate)
    text_in_body <- (
        !is.null(issue[["body"]])
        && apply_logic(
            x = vapply(
                X = values,
                FUN = grepl,
                FUN.VALUE = logical(1L),
                x = issue[["body"]],
                ignore.case = TRUE
            ),
            logic_gate = logic_gate
        )
    )
    return(text_in_body)
}

#' @rdname contains
#' @exportS3Method body_contains IssuesTB
#' @method body_contains IssuesTB
#' @export
body_contains.IssuesTB <- function(x, ...) {
    issues <- x

    text_in_bodies <- vapply(
        X = issues,
        FUN = body_contains,
        ...,
        FUN.VALUE = logical(1L)
    )
    return(text_in_bodies)
}

#' @rdname contains
#' @exportS3Method body_contains default
#' @method body_contains default
#' @export
body_contains.default <- function(x, ...) {
    stop("This function requires a IssueTB or IssuesTB object.")
}


#' @title Text in title of an issue
#'
#' @param x a \code{IssueTB} or \code{IssuesTB} object.
#' @param values a vector \code{string}
#' @param logic_gate the logic operator which will aggregate the different
#' assertion related to values.
#' @param ... Unused argument
#'
#' @returns a boolean specifying if the \code{values} are contained in the
#' title.
#' @details
#' For different values of \code{values}, the assertion will be constructed with
#' "OR gate" if the logic_gate argument is "OR" and "AND gate" if it is "AND".
#' This means that the call \code{title_contains(x = issue_1, values =
#' c("README", "package"), logic_gate = "OR")} will say whether the issue
#' contains the string "README" OR "package" in its title.
#' Whereas the call \code{title_contains(x = issue_1, values =
#' c("README", "package"), logic_gate = "AND")} will say whether the issue
#' contains the string "README" AND "package" in its title.
#'
#' @rdname contains
#'
#' @export
#'
#' @examples
#' all_issues <- get_issues(type = "online", verbose = FALSE)
#' issue_1 <- all_issues[[1L]]
#' title_contains(x = issue_1, values = "README")
#' title_contains(
#'     x = issue_1,
#'     values = c("README", "package"),
#'     logic_gate = "AND"
#' )
#'
title_contains <- function(x, ...){
    UseMethod("title_contains", x)
}

#' @rdname contains
#' @exportS3Method title_contains IssueTB
#' @method title_contains IssueTB
#' @export
title_contains.IssueTB <- function(x, values, logic_gate = c("AND", "OR"), ...) {
    issue <- x

    logic_gate <- match.arg(logic_gate)
    text_in_title <- (
        !is.null(issue[["title"]])
        && apply_logic(
            x = vapply(
                X = values,
                FUN = grepl,
                FUN.VALUE = logical(1L),
                x = issue[["title"]],
                ignore.case = TRUE
            ),
            logic_gate = logic_gate
        )
    )
    return(text_in_title)
}

#' @rdname contains
#' @exportS3Method title_contains IssuesTB
#' @method title_contains IssuesTB
#' @export
title_contains.IssuesTB <- function(x,...) {
    issues <- x

    text_in_titles <- vapply(
        X = issues,
        FUN = title_contains,
        ...,
        FUN.VALUE = logical(1L)
    )
    return(text_in_titles)
}

#' @rdname contains
#' @exportS3Method title_contains default
#' @method title_contains default
#' @export
title_contains.default <- function(x, ...) {
    stop("This function requires a IssueTB or IssuesTB object.")
}

#' @title Labels in issue
#'
#' @param x a \code{IssueTB} or \code{IssuesTB} object.
#' @param values a \code{string}
#' @param logic_gate the logic operator which will aggregate the different
#' assertion related to values.
#' @param ... Unused argument
#'
#' @returns a boolean specifying if the \code{values} are contained in the
#' labels.
#' @details
#' For different values of \code{values}, the assertion will be constructed with
#' "OR gate" if the logic_gate argument is "OR" and "AND gate" if it is "AND".
#' This means that the call \code{labels_contains(x = issue_1, values =
#' c("medium", "unknown"), logic_gate = "OR")} will say whether the issue
#' contains the labels "medium" OR "unknown".
#' Whereas the call \code{labels_contains(x = issue_1, values =
#' c("medium", "unknown"), logic_gate = "AND")} will say whether the issue
#' contains the labels "medium" AND "unknown".
#'
#' @export
#'
#' @rdname contains
#'
#' @examples
#' all_issues <- get_issues(type = "online", verbose = FALSE)
#' issue_1 <- all_issues[[1L]]
#' labels_contains(issue_1, "medium")
#' labels_contains(
#'     x = issue_1,
#'     values = c("medium", "unknown"),
#'     logic_gate = "AND"
#' )
#'
labels_contains <- function(x, ...){
    UseMethod("labels_contains", x)
}

#' @rdname contains
#' @exportS3Method labels_contains IssueTB
#' @method labels_contains IssueTB
#' @export
labels_contains.IssueTB <- function(x, values, logic_gate = c("AND", "OR"), ...) {
    issue <- x

    logic_gate <- match.arg(logic_gate)
    labels_in_issue <- (
        !is.null(issue[["labels"]])
        && apply_logic(
            x = (values %in% (issue[["labels"]])),
            logic_gate = logic_gate
        )
    )
    return(labels_in_issue)
}

#' @rdname contains
#' @exportS3Method labels_contains IssuesTB
#' @method labels_contains IssuesTB
#' @export
labels_contains.IssuesTB <- function(x, ...) {
    issues <- x

    labels_in_issues <- vapply(
        X = issues,
        FUN = labels_contains,
        ...,
        FUN.VALUE = logical(1L)
    )
    return(labels_in_issues)
}


#' @title Milestone in issue
#'
#' @param x a \code{IssueTB} or \code{IssuesTB} object.
#' @param values a \code{string}
#' @param logic_gate the logic operator which will aggregate the different
#' assertion related to values.
#' @param ... Unused argument
#'
#' @returns a boolean specifying if the \code{values} are contained in the
#' milestones.
#' @details
#' For different values of \code{values}, the assertion will be constructed with
#' "OR gate" if the logic_gate argument is "OR" and "AND gate" if it is "AND".
#' This means that the call
#' \code{
#' milestone_contains(
#'     x = issue_1,
#'     values = c("Fin juin 2024", "Fin juillet 2024"),
#'     logic_gate = "OR"
#' )}
#' will say whether the issue contains the milestones "Fin juin 2024" OR
#' "Fin juillet 2024".
#' Whereas the call
#' \code{
#' milestones_contain(
#'     x = issue_1,
#'     values = c("Fin juin 2024", "Fin juillet 2024"),
#'     logic_gate = "AND"
#' )}
#' will say whether the issue contains the milestones "Fin juin 2024" AND
#' "Fin juillet 2024".
#'
#' @export
#' @rdname contains
#'
#' @examples
#' all_issues <- get_issues(type = "online", verbose = FALSE)
#' issue_1 <- all_issues[[1L]]
#' milestone_contains(x = issue_1, values = "Fin juin 2024")
#' milestone_contains(
#'     x = issue_1,
#'     values = c("Fin juin 2024", "Fin juillet 2024"),
#'     logic_gate = "AND"
#' )
#'
milestone_contains <- function(x, ...){
    UseMethod("milestone_contains", x)
}

#' @rdname contains
#' @exportS3Method milestone_contains IssueTB
#' @method milestone_contains IssueTB
#' @export
milestone_contains.IssueTB <- function(x, values, logic_gate = c("AND", "OR"), ...) {
    issue <- x

    logic_gate <- match.arg(logic_gate)
    milestone_in_issue <- (
        !is.null(issue[["milestone"]])
        && apply_logic(
            x = (values %in% (issue[["milestone"]])),
            logic_gate = logic_gate
        )
    )
    return(milestone_in_issue)
}

#' @rdname contains
#' @exportS3Method milestone_contains IssuesTB
#' @method milestone_contains IssuesTB
#' @export
milestone_contains.IssuesTB <- function(x,...) {
    issues <- x

    milestone_in_issues <- vapply(
        X = issues,
        FUN = milestone_contains,
        ...,
        FUN.VALUE = logical(1L)
    )
    return(milestone_in_issues)
}


#' @title Filter issue or issues
#'
#' @description
#' Filtering issues with some constraint on the labels, the title and the body.
#'
#' @param x a \code{IssueTB} or \code{IssuesTB} object.
#' @param fields a vector \code{string} (contained in
#' \code{c("title", "body", "labels")}).
#' @param values a vector \code{string}.
#' @param negate a boolean indicate the negation of the assertion.
#' @param fields_logic_gate the logic operator which will aggregate the
#' different assertion related to fields (by default \code{"OR"}).
#' @param values_logic_gate the logic operator which will aggregate the
#' different assertion related to values (by default \code{"AND"}).
#'
#' @returns the list of issues that contain the \code{pattern}
#' @details
#' If the argument \code{fields} contains several element, the condition on each
#' fields will be merged with the OR logical operator. On the other hand, if the
#' argument \code{values} contains several differents elements, the conditions
#' will then be merged with the AND logical operator.
#'
#' The function \code{filter_issues} is used to construct assertions. Simple
#' assertions are iterated over values and fields. The
#' \code{values_logic_gate argument} specifies whether the assertions on the
#' values should be aggregated with a logical OR or with an AND logic. The
#' \code{field_logic_gate argument} specifies whether the assertions on the
#' fields should be aggregated with a logical OR or with an AND logic.
#' Finally, the negate argument is used to indicate that you want the opposite
#' of the assertion defined.
#' For example, the following call:
#' filter_issues(
#'     fields = c("body", "title"),
#'     values = c("README", "package"),
#'     values_logic_gate = "AND",
#'     negate = FALSE,
#'     fields_logic_gate = "OR"
#' )
#' indicates issues containing "README" and "package" in the title or body text.
#'
#' The following example:
#' filter_issues(
#'     fields = "labels",
#'     values = c("unknown", "medium"),
#'     values_logic_gate = "OR",
#'     negate = TRUE,
#'     fields_logic_gate = "AND"
#' )
#' designates issues that contain neither "unknown" nor "medium" in their label.
#'
#' Note that in the last example, the fields_logic_gate argument is not
#' important because there is only one field on which to filter.
#'
#' @export
#' @rdname filtering
#'
#' @examples
#' all_issues <- get_issues(type = "online", verbose = FALSE)
#' # Condition: issues containing "README" in its body OR title
#' filtered_issues <- filter_issues(
#'     x = all_issues,
#'     fields = c("body", "title"),
#'     values = "README",
#'     fields_logic_gate = "OR"
#' )
#'
#' # Condition: issues containing neither "unknown" nor "medium" in their label
#' filtered_issues <- filter_issues(
#'     x = all_issues,
#'     fields = "labels",
#'     values = c("unknown", "medium"),
#'     values_logic_gate = "OR",
#'     negate = TRUE,
#'     fields_logic_gate = "AND"
#' )
#'
filter_issues <- function(values,
                          x = get_issues(),
                          fields = c("title", "body"),
                          negate = FALSE,
                          fields_logic_gate = c("OR", "AND"),
                          values_logic_gate = c("AND", "OR")) {
    values_logic_gate <- match.arg(values_logic_gate)
    fields_logic_gate <- match.arg(fields_logic_gate)

    filtering <- rep(
        x = switch(fields_logic_gate, AND = TRUE, OR = FALSE),
        times = length(x)
    )

    if ("title" %in% fields) {
        assertion_title <- title_contains(x = x, values = values,
                                          logic_gate = values_logic_gate)

        filtering <- filtering |>
            aggregate_vector(
                y = assertion_title,
                logic_gate = fields_logic_gate
            )
    }

    if ("body" %in% fields) {
        assertion_body <- body_contains(x = x, values = values,
                                        logic_gate = values_logic_gate)
        filtering <- filtering |>
            aggregate_vector(
                y = assertion_body,
                logic_gate = fields_logic_gate
            )
    }

    if ("labels" %in% fields || "label" %in% fields) {
        assertion_labels <- labels_contains(x = x, values = values,
                                            logic_gate = values_logic_gate)
        filtering <- filtering |>
            aggregate_vector(
                y = assertion_labels,
                logic_gate = fields_logic_gate
            )
    }

    if ("milestone" %in% fields) {
        assertion_milestone <- milestone_contains(x = x, values = values,
                                            logic_gate = values_logic_gate)

        filtering <- filtering |>
            aggregate_vector(
                y = assertion_milestone,
                logic_gate = fields_logic_gate
            )
    }

    if (negate) {
        filtering <- !filtering
    }

    issues_output <- x[filtering]
    class(issues_output) <- "IssuesTB"

    return(issues_output)
}

no_milestones <- function(issues = get_issues()) {
    without_milestone <- issues |>
        lapply(FUN = base::`[[`, "milestone") |>
        vapply(FUN = is.null, FUN.VALUE = logical(1L))
    return(issues[without_milestone])
}

