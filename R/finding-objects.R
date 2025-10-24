#' @title Check for text in GitHub Issues
#'
#' @description
#' Generic function to filter issues with a given text pattern in the title,
#' body, or comments of a GitHub Issue object or a collection of Issues.
#'
#' @param x An object of class \code{IssuesTB}.
#' @param in_title Boolean. Does the function search for text in the title?
#' (Default \code{TRUE})
#' @param in_body Boolean. Does the function search for text in the body?
#' (Default \code{TRUE})
#' @param in_comments Boolean. Does the function search for text in the
#' comments? (Default \code{TRUE})
#' @param ... Additional arguments passed to [grepl()], such as \code{pattern}
#' and \code{ignore.case}.
#'
#' @returns An object \code{IssuesTB} with issues that satisfy the condition.
#'
#' @examples
#' all_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'     dataset_name = "open_issues.yaml"
#' )
#' with_text(all_issues, pattern = "Excel")
#'
#' @rdname with_text
#' @export
with_text <- function(x, ...) {
    UseMethod("with_text", x)
}

#' @rdname with_text
#' @exportS3Method with_text IssuesTB
#' @method with_text IssuesTB
#' @export
with_text.IssuesTB <- function(
    x,
    ...,
    in_title = TRUE,
    in_body = TRUE,
    in_comments = TRUE
) {
    condition <- FALSE
    if (in_title) {
        condition <- condition | grepl(x = x$title, ...)
    }
    if (in_body) {
        condition <- condition | grepl(x = x$body, ...)
    }
    if (in_comments) {
        condition <- condition |
            vapply(
                X = x$comments,
                FUN = \(.x) any(grepl(x = .x$text, ...)),
                FUN.VALUE = logical(1L)
            )
    }
    return(x[condition, ])
}

#' @title Check for labels in GitHub Issues
#'
#' @description
#' Generic function to filter issues with labels
#'
#' @param x An object of class \code{IssuesTB}.
#' @param ... Additional arguments passed to [grepl()], such as \code{pattern}
#' and \code{ignore.case}.
#'
#' @returns An object \code{IssuesTB} with issues that satisfy the condition.
#'
#' @examples
#' all_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'     dataset_name = "open_issues.yaml"
#' )
#' with_labels(all_issues, pattern = "Bug")
#'
#' @rdname with_labels
#' @export
with_labels <- function(x, ...) {
    UseMethod("with_labels", x)
}

#' @rdname with_labels
#' @exportS3Method with_labels IssuesTB
#' @method with_labels IssuesTB
#' @export
with_labels.IssuesTB <- function(x, ...) {
    condition <- grepl(x = x$labels, ...)
    return(x[condition, ])
}

#' @title Check for comments in GitHub Issues
#'
#' @description
#' Function to filter issues with (or without) comments.
#'
#' @param x An object of class \code{IssuesTB}.
#' @param negate boolean indicating if we are searching for issues WITHOUT
#' comments. Default is \code{FALSE}.
#'
#' @returns An object \code{IssuesTB} with issues that satisfy the condition.
#'
#' @examples
#' all_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'     dataset_name = "open_issues.yaml"
#' )
#' with_comments(all_issues)
#' with_comments(all_issues, negate = TRUE)
#'
#' @rdname with_comments
#' @export
with_comments <- function(x, negate = FALSE) {
    UseMethod("with_comments", x)
}

#' @rdname with_comments
#' @exportS3Method with_comments IssuesTB
#' @method with_comments IssuesTB
#' @export
with_comments.IssuesTB <- function(x, negate = FALSE) {
    condition <- get_nbr_comments(x) > 0
    if (negate) {
        return(x[!condition, ])
    } else {
        return(x[condition, ])
    }
}

#' @title Number of comments
#'
#' @description
#' Number of comments of an Issue or set of issues
#'
#' @param x An object of class \code{IssueTB} or \code{IssuesTB}.
#'
#' @returns An integer or an integer vector with the number of comments of the
#' different issues in \code{x}.
#'
#' @examples
#' all_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'     dataset_name = "open_issues.yaml"
#' )
#' get_nbr_comments(all_issues)
#' get_nbr_comments(all_issues[1L, ])
#'
#' @rdname get_nbr_comments
#' @export
get_nbr_comments <- function(x) {
    UseMethod("get_nbr_comments", x)
}

#' @rdname get_nbr_comments
#' @exportS3Method get_nbr_comments IssueTB
#' @method get_nbr_comments IssueTB
#' @export
get_nbr_comments.IssueTB <- function(x) {
    nbr_comment <- as.numeric(lapply(X = x$comments, FUN = nrow))
    return(nbr_comment)
}

#' @rdname get_nbr_comments
#' @exportS3Method get_nbr_comments IssuesTB
#' @method get_nbr_comments IssuesTB
#' @export
get_nbr_comments.IssuesTB <- function(x) {
    nbr_comment <- nrow(x$comments)
    return(nbr_comment)
}
