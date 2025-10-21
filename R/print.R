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
#' all_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'     dataset_name = "open_issues.yaml"
#' )
#'
#' # Display one issue
#' print(all_issues[1, ])
#'
#' # Display several issues
#' print(all_issues[1:10, ])
#'
#' # Display the summary of one issue
#' summary(all_issues[2, ])
#'
#' # Display the summary of
#' summary(all_issues[1:10, ])
#' @rdname print
#'
#' @exportS3Method print IssueTB
#' @method print IssueTB
#'
#' @export
print.IssueTB <- function(x, ...) {
    issue_desc <- paste0(
        "Issue ",
        x[["owner"]],
        "/",
        x[["repo"]],
        "#",
        x[["number"]]
    )

    cli::cli_h2(
        cli::style_hyperlink(
            text = issue_desc,
            url = x[["html_url"]]
        )
    )

    cat(
        crayon::underline("Title:"),
        " ",
        substr(x = x[["title"]], start = 1L, stop = 80L),
        "\n",
        crayon::underline("Text:\n"),
        ifelse(
            test = nchar(x[["body"]]) > 320L,
            yes = paste0(
                substr(x = x[["body"]], start = 1L, stop = 320L),
                "\n...\n"
            ),
            no = x[["body"]]
        ),
        "\n",
        sep = ""
    )

    return(invisible(x))
}

#' @rdname print
#' @exportS3Method print IssuesTB
#' @method print IssuesTB
#' @export
print.IssuesTB <- function(x, ...) {
    crayon::bold(
        switch(
            EXPR = as.character(nrow(x)),
            "0" = "No issues",
            "1" = "There is 1 issue.",
            paste("There are", nrow(x), "issues.")
        ),
        "\n"
    ) |>
        cat()
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

    if (x[["has_labels"]]) {
        cat(crayon::underline("Labels:"), " ", sep = "")

        cat(
            vapply(
                X = seq_along(x$labels_name),
                FUN = function(k) {
                    label_style <- crayon::combine_styles(
                        crayon::make_style(x$labels_color[k]),
                        crayon::make_style(x$labels_bgcolor[k], bg = TRUE)
                    )
                    cli::style_hyperlink(
                        text = label_style(x$labels_name[k]),
                        url = x$labels_url[k]
                    )
                },
                FUN.VALUE = character(1L)
            ),
            sep = ", "
        )

        cat("\n")
    }

    cat(
        crayon::underline("State:"),
        " ",
        switch(
            x[["state_reason"]],
            open = "\U1F7E2 Open",
            reopened = "\U267B Re-opened",
            completed = "\U2714 Completed",
            not_planned = "\U1F6AB Not planned",
            duplicated = "\U27BF Duplicated"
        ),
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
                "\nComment n\U00B0",
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
    return(invisible(x))
}

#' @rdname print
#' @exportS3Method print summary.IssuesTB
#' @method print summary.IssuesTB
#' @export
print.summary.IssuesTB <- function(x, ...) {
    cat(
        crayon::bold(
            switch(
                EXPR = as.character(x$nbr_issues),
                "0" = "No issues",
                "1" = "There is 1 issue.",
                paste("There are", x$nbr_issues, "issues.")
            )
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
    return(invisible(x))
}

#' @rdname print
#' @exportS3Method print LabelsTB
#' @method print LabelsTB
#' @export
print.LabelsTB <- function(x, ...) {
    x$labels_bgcolor <- x$color
    x$labels_color <- c("grey8", "ivory")[
        isDark(x$labels_bgcolor) + 1L
    ]
    x$labels_url <- paste(
        "https://github.com",
        x$owner,
        x$repo,
        "labels",
        utils::URLencode(x$labels_name),
        sep = "/"
    )
    x$formated_label <- vapply(
        X = seq_len(nrow(x)),
        FUN = function(k) {
            label_style <- crayon::combine_styles(
                crayon::make_style(x$labels_color[k]),
                crayon::make_style(x$labels_bgcolor[k], bg = TRUE)
            )
            cli::style_hyperlink(
                text = label_style(x$name[k]),
                url = x$labels_url[k]
            )
        },
        FUN.VALUE = character(1L)
    )

    couples <- unique(x[, c("owner", "repo")])
    crayon::bold(
        switch(
            EXPR = as.character(nrow(couples)),
            "0" = "No labels",
            "1" = "There is 1 repo.",
            paste("There are", nrow(couples), "repos.")
        )
    ) |>
        cat()

    for (id in seq_len(nrow(couples))) {
        owner_name <- couples[id, "owner"]
        repo_name <- couples[id, "repo"]
        labels_owner <- x[x$owner == owner_name & x$repo == repo_name, ]

        cat(
            paste0(
                "\n- ",
                cli::style_hyperlink(
                    text = paste(owner_name, repo_name, sep = "/"),
                    url = paste(
                        "https://github.com",
                        owner_name,
                        repo_name,
                        sep = "/"
                    )
                ),
                ":"
            ),
            labels_owner$formated_label,
            sep = ", "
        )
    }
    cat("\n")
    return(invisible(x))
}

#' @rdname print
#' @exportS3Method print summary.LabelsTB
#' @method print summary.LabelsTB
#' @export
print.summary.LabelsTB <- function(x, ...) {
    crayon::bold(
        switch(
            EXPR = as.character(nrow(x)),
            "0" = "No labels",
            "1" = "There is 1 label.",
            paste("There are", nrow(x), "labels.")
        )
    ) |>
        cat()

    couples <- unique(x[, c("owner", "repo")])

    for (id in seq_len(nrow(couples))) {
        owner_name <- couples[id, "owner"]
        repo_name <- couples[id, "repo"]
        labels_owner <- x[x$owner == owner_name & x$repo == repo_name, ]

        cat(
            paste0(
                "\n- ",
                cli::style_hyperlink(
                    text = paste(owner_name, repo_name, sep = "/"),
                    url = paste(
                        "https://github.com",
                        owner_name,
                        repo_name,
                        sep = "/"
                    )
                ),
                ":"
            ),
            paste0(labels_owner$formated_label, ": ", labels_owner$description),
            sep = "\n    - "
        )
    }
    return(invisible(x))
}
