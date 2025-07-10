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

    issue_url <- file.path(
        "https://github.com",
        issue$owner,
        issue$repo,
        "issues",
        issue$number
    )

    cli::cli_h2(paste0(
        "{.href [Issue ",
        issue[["owner"]], "/",
        issue[["repo"]], "#", issue[["number"]],
        "](",
        issue_url,
        ")}"
    ))
  
    cat(
        crayon::underline("Title: "),
        substr(x = issue[["title"]], start = 1, stop = 80),
        "\n",
        crayon::underline("Text:\n"),
        substr(x = issue[["body"]], start = 1, stop = 320),
        "\n...\n\n",
        sep = ""
    )

    return(invisible(issue))
}

#' @rdname print
#' @exportS3Method print IssuesTB
#' @method print IssuesTB
#' @export
print.IssuesTB <- function(x, ...) {
    issues <- x
    cat(crayon::bold(
        ifelse(
            test = nrow(issues) > 0L,
            yes = paste("There are", nrow(issues), "issues."),
            no = "No issues"
        ),
        "\n"
    ))
    for (id_issue in seq_len(nrow(issues))) {
        cat("\n")
        print(issues[id_issue, , drop = TRUE])
    }
    return(invisible(issues))
}
