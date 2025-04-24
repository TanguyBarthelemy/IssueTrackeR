#' @title Compute assertion
#'
#' @description
#' This function allows the user to summarise a set of booleans into a single
#' boolean via a logic gate.
#'
#' @param ... Boolean objects that will be reduced into a single Boolean
#' @param orientation a character string that is either \code{"vector-wise"}
#' (by default) if you want to compute the assertion element by element (is
#' \dots contains vector of same length) or \code{"overall"} if you want to
#' compute the assertion with all the element contained in \dots.
#' @param logic_gate the logic operator which will aggregate the
#' different assertion related to values: \code{"OR"} or \code{"AND"} (by
#' default).
#'
#' @returns a Boolean vector of length 1 if \code{orientation} is
#' \code{"overall"} and of the same length as the elements contained in \dots
#' if \code{orientation} is \code{"vector-wise"}.
#'
#' @details
#' Logic gates work with Booleans in the following way:
#'
#' | AND gate | TRUE  | FALSE |
#' | -------- | ----- | ----- |
#' | TRUE     | TRUE  | FALSE |
#' | FALSE    | FALSE | FALSE |
#'
#' | OR gate | TRUE | FALSE |
#' | ------- | ---- | ----- |
#' | TRUE    | TRUE | TRUE  |
#' | FALSE   | TRUE | FALSE |
#'
#' If the argument \code{orientation} is \code{"overall"}, then all the element
#' of ... will be computed together with logic gate provided by the argument
#' \code{logic_gate}.
#'
#' For example, the following call:
#' \preformatted{
#' logical_reducer(
#'     TRUE, FALSE, c(TRUE, TRUE, TRUE),
#'     logic_gate = "AND", orientation = "overall"
#' )
#' }
#' will be return \code{FALSE} because the second argument (in \dots) has the
#' value \code{FALSE} and with the logic gate AND, the results is \code{FALSE}.
#'
#' On the other hand, if the argument \code{orientation} is \code{"vector-wise"}
#' then the element of \dots must have the same length and their first element
#' will be computed together then their second... and at the end their last
#' element will be computed together  with logic gate provided by the argument
#' \code{logic_gate}.
#'
#' For example, the following call:
#' \preformatted{
#' logical_reducer(
#'     c(TRUE, TRUE, FALSE), c(FALSE, TRUE, FALSE),
#'     logic_gate = "OR", orientation = "vector-wise"
#' )
#' }
#' will be return \code{c(TRUE, TRUE, FALSE)} because the first argument (in
#' \dots) contains \code{c(TRUE, TRUE)} as first and second argument (so with
#' the logic gate OR, the result is \code{TRUE}) and the third value of all
#' element in \dots is \code{FALSE} so the result is \code{FALSE}.
#'
#' @keywords internal
#'
logical_reducer <- function(...,
                            orientation = c("vector-wise", "overall"),
                            logic_gate = c("AND", "OR")) {
    logic_gate <- match.arg(logic_gate)
    orientation <- match.arg(orientation)

    if (orientation == "vector-wise") {
        output <- switch(
            EXPR = logic_gate,
            AND = pmin(...),
            OR = pmax(...)
        ) |> as.logical()
    } else if (orientation == "overall") {
        output <- switch(
            EXPR = logic_gate,
            AND = all(...),
            OR = any(...)
        )
    } else {
        stop("Wrong orientation parameter.", call. = FALSE)
    }
    return(output)
}

#' @title Vectorize \code{grepl}
#'
#' @param pattern character string containing a regular expression (or
#' character string for fixed = TRUE) to be matched in the given character
#' vector. Coerced by as.character to a character string if possible. Missing
#' values are allowed except for \code{regexpr}, \code{gregexpr} and
#' \code{regexec}.
#' @param x a character vector where matches are sought, or an object which can
#' be coerced by as.character to a character vector. Long vectors are supported.
#' @inheritParams base::grepl
#'
#' @returns a matrix with as many rows as \code{length(x)} and as many columns
#' as \code{length(pattern)} containing TRUE or FALSE. Each column is the result
#' of call to \code{\link[base]{grepl}}.
#'
#' @details
#' This function works the same way as \code{\link[base]{grepl}} but with a
#' vectorize pattern. This means that if the argument \code{pattern} has a
#' length >= 2 then the output will be calculated iteratively by pattern. So
#' instead of having a vector as output, we get a matrix with
#' \code{length(pattern)} rows and \code{length(pattern)} columns.
#'
#' For more informations, see the function \code{\link[base]{grepl}}.
#'
#' @keywords internal
#'
vgrepl <- Vectorize(
    FUN = function(pattern,
                   x,
                   ignore.case = TRUE,
                   perl = FALSE,
                   fixed = FALSE,
                   useBytes = FALSE) {
        grepl(
            pattern = pattern,
            x = x,
            ignore.case = ifelse(test = fixed, yes = FALSE, no = ignore.case),
            perl = perl,
            fixed = fixed,
            useBytes = useBytes
        )
    },
    vectorize.args = "pattern"
)

#' @title Does the issue(s) contains this text?
#'
#' @description
#' Check if the issues contains text, values in its title, labels, body and
#' milestone.
#'
#' @param x a \code{IssueTB} or \code{IssuesTB} object.
#' @param values a vector \code{string}. Patterns to look for in the outcome.
#' @param fields a vector \code{string}. The different fields of the issue in
#' which to search for the pattern (among \code{"title"}, \code{"body"},
#' \code{"labels"} and \code{"milestone"}).
#' @param fields_logic_gate the logic operator which will aggregate the
#' different assertion related to fields: \code{"OR"} (by default) or
#' \code{"AND"}.
#' @param values_logic_gate the logic operator which will aggregate the
#' different assertion related to values: \code{"OR"} or \code{"AND"} (by
#' default).
#' @param negate a boolean indicate the negation of the assertion.
#' @param \dots Arguments passed on to \code{\link[IssueTrackeR]{vgrepl}} and
#' therefore to \code{\link[base]{grepl}}
#'
#' @returns a boolean (of length equals 1 if the class of \code{x} is
#' \code{IssueTB} and length superior to 1 if \code{x} if of class
#' \code{IssuesTB}) specifying if the \code{pattern} is contained in the field
#' \code{field} of the issue.
#'
#' @details
#'
#' The contains function in R is designed to check if specific fields of GitHub
#' issues contain certain values, offering a flexible mechanism for constructing
#' complex assertions. The function operates with two main logical gates:
#' \code{fields_logic_gate} and \code{values_logic_gate}.
#'
#' The \code{fields_logic_gate} determines how conditions on multiple fields are
#' combined (either "OR" or "AND"). This means that the call \code{
#' contains(x = issue_1,
#'          fields = c("body", "title"),
#'          values = "README",
#'          fields_logic_gate = "OR")
#' } will say whether the issue \code{issue_1} contains the string
#' \code{"README"} in its title OR in its body.
#'
#' The \code{values_logic_gate} specifies how conditions on multiple values are
#' combined within each field (either "OR" or "AND"). For example the call
#' \preformatted{
#' contains(x = issue_1,
#'          fields = "body",
#'          values = c("README", "package"),
#'          values_logic_gate = "OR")
#' } will say whether the issue \code{issue_1} contains the string
#' \code{"README"} OR \code{"package"} in its body. Whereas the call
#' \preformatted{
#' contains(x = issue_1,
#'          fields = "title",
#'          values = c("README", "package"),
#'          values_logic_gate = "AND")
#' } will say whether the issue \code{issue_1} contains the string
#' \code{"README"} AND \code{"package"} in its body.
#'
#' The function can also negate the condition using the \code{negate} argument,
#' effectively allowing users to negate an assertion.
#'
#' The following example:
#' \preformatted{
#' contains(
#'     x = all_issues,
#'     fields = "labels",
#'     values = c("unknown", "medium"),
#'     values_logic_gate = "OR",
#'     negate = TRUE,
#'     fields_logic_gate = "AND"
#' )
#' }  designates issues that contain neither "unknown" nor "medium" in their
#' label.
#'
#' Note that in the last example, the \code{fields_logic_gate} argument has no
#' importance and is not taken into account because there is only one field on
#' which to filter. In the same way, if the \code{values} argument contains only
#' one element, the \code{values_logic_gate} argument has no importance and is
#' not taken into account.
#'
#' This function is not case-sensitive.
#'
#' @section How assertions with multiple values and multiple fields are built:
#'
#' For the order of logical assertions, as it is easy to add assertions linked
#' by an AND (by piping a new filter_issues), it has been decided that
#' assertions containing AND gates will be distributed and assertions containing
#' OR gates will be factorised. The assertions used by \code{filter_issues} will
#' therefore have the following format:
#' \eqn{(P1 AND Q1) OR (P2 AND Q2)}
#'
#' Thus the following call to filter_issue:
#' \preformatted{
#' filter_issues(
#'     ...,
#'     values = c("v1", "v2"), fields = c("f1", "f2"),
#'     values_logic_gate = "AND", fields_logic_gate = "OR",
#'     ...
#' )
#' }
#' will be represented by the following logical proposition:
#' \eqn{(v1 in f1 AND v2 in f1) OR (v1 in f2 AND v2 in f2)}.
#'
#' This makes it possible to create more complex logical forms by combining AND
#' gates and OR gates.
#'
#' @section Short names:
#' \itemize{
#' \item \code{fields = "b"} for \code{"body"};
#' \item \code{fields = "t"} for \code{"title"};
#' \item \code{fields = "l"} for \code{"labels"};
#' \item \code{fields = "m"} for \code{"milestone"}.
#' }
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#' all_issues <- get_issues(source = "online", verbose = FALSE)
#' issue_1 <- all_issues[[1L]]
#' # This will return TRUE if the issue contains either "README" or "package"
#' # in its body.
#' contains(x = issue_1,
#'          fields = "body",
#'          values = c("README", "package"),
#'          values_logic_gate = "OR")
#' # This will return TRUE if the issue contains "README" in its body AND its
#' # title.
#' contains(x = issue_1,
#'          values = "README",
#'          fields = c("body", "title"),
#'          fields_logic_gate = "AND")
#' }
#'
#' @rdname contains
#'
contains <- function(x, ...) {
    UseMethod("contains", x)
}

#' @rdname contains
#' @exportS3Method contains IssueTB
#' @method contains IssueTB
#' @export
contains.IssueTB <- function(x,
                             values,
                             fields = c("body", "title", "labels", "milestone"),
                             values_logic_gate = c("AND", "OR"),
                             fields_logic_gate = c("OR", "AND"),
                             negate = FALSE,
                             ...) {

    values_logic_gate <- match.arg(values_logic_gate)
    fields_logic_gate <- match.arg(fields_logic_gate)

    fields <- match.arg(
        arg = fields,
        choices = c("body", "title", "labels", "milestone", "b", "t", "l", "m"),
        several.ok = TRUE
    )

    # Cas 1 seul champ
    if (length(fields) == 1L) {

        fields <- switch(
            EXPR = fields,
            b = "body",
            t = "title",
            l = "labels",
            m = "milestone",
            fields
        )

        field_content <- x[[fields]]

        if (is.null(field_content)) {
            text_in_issue <- FALSE
        } else if (fields %in% c("title", "body")) {
            text_in_issue <- vgrepl(
                pattern = values,
                x = field_content,
                ...
            )
        } else if (fields %in% c("labels", "milestone")) {
            text_in_issue <- values %in% field_content
        }

        text_in_issue <- logical_reducer(
            x = text_in_issue,
            orientation = "overall",
            logic_gate = values_logic_gate
        )

        # Cas plusieurs champs
    } else if (length(values) > 1L
               && values_logic_gate == "OR"
               && fields_logic_gate == "AND") {

        text_in_issue <- values |>
            vapply(
                FUN = function(value) {
                    contains(
                        x = x,
                        values = value,
                        fields = fields,
                        fields_logic_gate = fields_logic_gate,
                        negate = FALSE,
                        ...
                    )
                },
                FUN.VALUE = logical(1L)
            ) |>
            logical_reducer(orientation = "overall",
                            logic_gate = values_logic_gate)

        # Autres cas
    } else {
        text_in_issue <- fields |>
            vapply(
                FUN = function(field) {
                    contains(
                        x = x,
                        fields = field,
                        values_logic_gate = values_logic_gate,
                        values = values,
                        negate = FALSE,
                        ...
                    )
                },
                FUN.VALUE = logical(1L)
            ) |>
            logical_reducer(orientation = "overall",
                            logic_gate = fields_logic_gate)
    }

    if (negate) {
        text_in_issue <- !text_in_issue
    }

    return(text_in_issue)
}

#' @rdname contains
#' @exportS3Method contains IssuesTB
#' @method contains IssuesTB
#' @export
contains.IssuesTB <- function(x, values, ...) {
    text_in_issues <- vapply(
        X = x,
        FUN = contains,
        values = values,
        ...,
        FUN.VALUE = logical(1L)
    )

    return(text_in_issues)
}

#' @rdname contains
#' @exportS3Method contains default
#' @method contains default
#' @export
contains.default <- function(x, ...) {
    stop("This function requires a IssueTB or IssuesTB object.", call. = FALSE)
}

#' @title Filter issue or issues
#'
#' @description
#' Filtering issues with some constraint on the labels, the title and the body.
#'
#' @param x a \code{IssuesTB} object.
#' @param \dots Other options used to control filtering behaviour with different
#' fields and values. Passed on to \code{\link[IssueTrackeR]{contains}} as:
#' * \code{values}: a vector \code{string}. Patterns to look for in the outcome.
#' * \code{fields}: a vector \code{string}. The different fields of the issue in
#' which to search for the pattern (among \code{"title"}, \code{"body"},
#' \code{"labels"} and \code{"milestone"})
#' * \code{fields_logic_gate}: the logic operator which will aggregate the
#' different assertion related to fields: \code{"OR"} (by default) or
#' \code{"AND"}.
#' * \code{values_logic_gate}: the logic operator which will aggregate the
#' different assertion related to values: \code{"OR"} or \code{"AND"} (by
#' default).
#' * \code{negate}: a boolean indicate the negation of the assertion.
#'
#' @rdname filter_issues
#'
#' @returns a \code{IssuesTB} object filtered
#' @details
#' This function relies on the function \code{\link[IssueTrackeR]{contains}}.
#' More informations on the filtering in the documentation of the function
#' \code{\link[IssueTrackeR]{contains}}.
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#' all_issues <- get_issues(source = "online", verbose = FALSE)
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
#' }
#'
filter_issues <- function(x, ...) {
    if (missing(x)) {
        return(filter_issues(x = get_issues(), ...))
    }
    UseMethod("filter_issues", x)
}

#' @rdname filter_issues
#' @exportS3Method filter_issues IssuesTB
#' @method filter_issues IssuesTB
#' @export
filter_issues.IssuesTB <- function(x, ...) {
    filtering <- contains(x, ...)
    issues_output <- x[filtering]
    return(issues_output)
}

#' @rdname filter_issues
#' @exportS3Method filter_issues default
#' @method filter_issues default
#' @export
filter_issues.default <- function(x, ...) {
    stop("This function requires a IssuesTB object.", call. = FALSE)
}

#' @title Check issues without milestones
#'
#' @description
#' Filter all the issues without milestones from a list of issues.
#'
#' @param issues a \code{IssuesTB} object.
#'
#' @returns a list of issues without milestones.
#'
#' @keywords internal
#'
no_milestones <- function(issues = get_issues()) {
    without_milestone <- issues |>
        lapply(FUN = base::`[[`, "milestone") |>
        vapply(FUN = is.null, FUN.VALUE = logical(1L))
    return(issues[without_milestone])
}
