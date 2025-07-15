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
#' print(all_issues[1, ])
#'
#' # Display several issues
#' print(all_issues[1:10, ])
#'
#' # Display the summary of one issue
#' summary(all_issues[1, ])
#'
#' # Display the summary of
#' summary(all_issues[1:10, ])
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

    issue_desc <- paste0(
        "Issue ",
        issue[["owner"]],
        "/",
        issue[["repo"]],
        "#",
        issue[["number"]]
    )

    cli::cli_h2(
        cli::style_hyperlink(
            text = issue_desc,
            url = issue[["html_url"]]
        )
    )

    cat(
        crayon::underline("Title:"),
        " ",
        substr(x = issue[["title"]], start = 1L, stop = 80L),
        "\n",
        crayon::underline("Text:\n"),
        substr(x = issue[["body"]], start = 1L, stop = 320L),
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
    cat(crayon::bold(
        switch(
            EXPR = as.character(nrow(x)),
            "0" = "No issues",
            "1" = "There is 1 issue.",
            paste("There are", nrow(x), "issues.")
        ),
        "\n"
    ))
    for (id_issue in seq_len(nrow(x))) {
        cat("\n")
        print(x[id_issue, , drop = TRUE])
    }
    return(invisible(x))
}

#' @rdname print
#' @exportS3Method print summary.IssueTB
#' @method print summary.IssueTB
#' @export
print.summary.IssueTB <- function(x, ...) {
    cli::cli_h2(cli::style_hyperlink(
        text = paste0("Issue ", x[["desc"]]),
        url = x[["html_url"]]
    ))

    cat(crayon::underline("Labels:"), " ", sep = "")

    cat(
        vapply(
            X = seq_along(x$labels),
            FUN = function(k) {
                label_style <- combine_styles(
                    make_style(x$label_color[k]),
                    make_style(x$label_bgcolor[k], bg = TRUE)
                )
                cli::style_hyperlink(
                    text = label_style(x$label_name[k]),
                    url = x$label_url[k]
                )
            },
            FUN.VALUE = character(1L)
        ),
        sep = ", "
    )

    cat(
        "\n",
        crayon::underline("State:"),
        " ",
        switch(x[["state_reason"]],
               open = "\U1F7E2 Open",
               completed = "\U2714 Completed",
               not_planned = "\U1F6AB Not planned")
        ,
        "\n",
        crayon::underline("Nb comments:"),
        " ",
        x[["nbr_comments"]],
        "\n\n",
        crayon::underline("Title:"),
        " ",
        x[["title"]],
        "\n",
        crayon::underline("Text:\n"),
        x[["body"]],
        "\n\n",
        sep = ""
    )

    if (x[["nbr_comments"]] > 0L) {
        cat(
            crayon::underline("Comments:\n"),
            paste0(
                "\nComment n°",
                seq_len(x[["nbr_comments"]]),
                " by ",
                x[["comments"]][["author"]],
                ":\n\n",
                x[["comments"]][["text"]],
                "\n"
            ),
            "\n"
        )
    }
}

#' @rdname print
#' @exportS3Method print summary.IssuesTB
#' @method print summary.IssuesTB
#' @export
print.summary.IssuesTB <- function(x, ...) {
    cat(
        crayon::bold(
            if (x$nbr_issues == 0L) "No issues"
            else if (x$nbr_issues == 1L) "There is 1 issue: "
            else paste("There are", x$nbr_issues, "issues:")
        ),
        paste0(
            "\n- ",
            cli::style_hyperlink(
                text = x[["issue_desc"]],
                url = x[["html_url"]]
            ),
            " ",
            x[["state_reason"]]
        ),
        "\n"
    )
}
