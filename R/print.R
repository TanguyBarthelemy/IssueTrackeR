#' @title Display IssueTB and IssuesTB object
#'
#' @description
#' Display IssueTB and IssuesTB with formatted output in the console
#'
#' @param x a \code{IssueTB} or \code{IssuesTB} object.
#' @param \dots Unused argument
#'
#' @details
#' This function displays an issue (\code{IssueTB} object) or a list of issues
#' (\code{IssuesTB} object) with a formatted output.
#'
#' @returns invisibly (with \code{invisible()}) \code{NULL}.
#'
#' @examples
#'
#' \donttest{
#' all_issues <- get_issues(source = "online", verbose = FALSE)
#'
#' # Display one issue
#' print(all_issues[[1]])
#'
#' # Display several issues
#' print(all_issues[1:10])
#' }
#'
#' @rdname print
#'
#' @exportS3Method print IssueTB
#' @method print IssueTB
#'
#' @export
print.IssueTB <- function(x, ...) {
    issue <- x

    cat(crayon::bold("Issue #", issue[["number"]], "\n", sep = ""))
    cat(issue[["owner"]], "/", issue[["repo"]], "\n", sep = "")
    cat(crayon::underline("Labels:"),
        paste(issue[["labels"]], sep = ", "), "\n")
    cat(crayon::underline("Milestone:"), issue[["milestone"]], "\n")
    cat(crayon::underline("Title:"), issue[["title"]], "\n")
    cat(crayon::underline("Text:\n"))
    cat(issue[["body"]], "\n")
    cat("\n")

    return(invisible(issue))
}

#' @rdname print
#' @exportS3Method print IssuesTB
#' @method print IssuesTB
#' @export
print.IssuesTB <- function(x, ...) {
    issues <- x
    cat(crayon::bold(ifelse(
        test = length(issues) > 0L,
        yes = paste("There are", length(issues), "issues."),
        no = "No issues"
    ), "\n"))
    for (issue in issues) {
        cat("\n")
        print(issue)
    }
    return(invisible(issues))
}
