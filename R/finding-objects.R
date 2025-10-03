#' @title Check for text in GitHub Issues or sets of Issues
#'
#' @description
#' Generic function to search for a given text pattern in the title, body, or
#' comments of a GitHub Issue object or a collection of Issues.
#'
#' @param x An object of class \code{IssueTB} (a single issue) or
#' \code{IssuesTB} (a \code{data.frame} or \code{tibble} of issues).
#' @param ... Additional arguments passed to [grepl()], such as \code{pattern}
#' and \code{ignore.case}.
#'
#' @return A logical value (`TRUE`/`FALSE`) if `x` is a single issue, or a
#' logical vector for multiple issues.
#'
#' @examples
#' all_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'     dataset_name = "list_issues.yaml"
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
    condition <- F
    if (in_title) {
        condition <- condition | grepl(x = x$title, ...)
    }
    if (in_body) {
        condition <- condition | grepl(x = x$body, ...)
    }
    if (in_comments) {
        condition <- condition |
            sapply(
                X = x$comments,
                FUN = \(.x) any(grepl(x = .x$text, ...))
            )
    }
    return(subset(x, condition))
}

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
    condition <- grepl(x = x$labels, pattern = ...)
    return(subset(x, condition))
}
