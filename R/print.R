#' @title Display IssueTB, IssuesTB or SelectorTB object
#'
#' @description
#' Display IssueTB, IssuesTB or SelectorTB with formatted output in the console
#'
#' @param x An object of class \code{IssueTB}, \code{IssuesTB} or
#'   \code{SelectorTB}.
#' @param \dots Currently not used.
#'
#' @details
#' This function displays an issue (\code{IssueTB} object), a list of issues
#' (\code{IssuesTB} object) or a list of selectors (`SelectorTB` objects) with
#' a formatted output.
#'
#' @returns The object `x` invisibly.
#'
#' @examples
#' issues_selector <- init_selector(
#'     source = "Local",
#'     dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'     dataset_name = NULL
#' )
#' my_issues <- get_issues(selector = issues_selector)
#'
#' # Display one issue
#' print(my_issues[1, ])
#'
#' # Display several issues
#' print(my_issues[1:10, ])
#'
#' # Display a selector
#' print(issues_selector)
#'
#' # Display the summary of one issue
#' summary(my_issues[2, ])
#'
#' # Display the summary of
#' summary(my_issues[1:10, ])
#' @name print-issues
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

#' @rdname print-issues
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

#' @rdname print-issues
#' @exportS3Method print SelectorTB
#' @method print SelectorTB
#' @export
print.SelectorTB <- function(x, ...) {
    cat(
        crayon::bold(
            switch(
                EXPR = as.character(length(x)),
                "0" = "No selectors",
                "1" = "There is 1 selector.",
                paste("There are", length(x), "selectors.")
            )
        ),
        "\n"
    )

    if (is_empty(x)) {
        return(invisible(x))
    }

    for (id_selector in seq_along(x)) {
        selector <- x[[id_selector]]

        txt_selector <- paste(
            crayon::underline(names(selector)),
            as.character(selector),
            sep = ": "
        ) |>
            c(
                crayon::bold(paste0("\nSelector n\U00B0", id_selector)),
                ... = _
            ) |>
            paste("\n")
        cat(txt_selector, sep = "")
    }

    return(invisible(x))
}

#' @rdname print-issues
#' @exportS3Method print summary.IssueTB
#' @method print summary.IssueTB
#' @export
print.summary.IssueTB <- function(x, ...) {
    cli::cli_h2(cli::style_hyperlink(
        text = paste0("Issue ", x[["desc"]]),
        url = x[["html_url"]]
    ))

    if (length(x$label_display) > 0L) {
        cat(crayon::underline("Labels:"), " ", sep = "")
        cat(x$label_display)
        cat("\n")
    }

    cat(
        crayon::underline("State:"),
        " ",
        x[["state_reason"]],
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

#' @rdname print-issues
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
            x[["state_reason"]],
            ifelse(
                test = nzchar(x$label_display),
                no = "",
                yes = paste0("\n", x$label_display, "\n")
            )
        ),
        "\n"
    )
    return(invisible(x))
}

#' @rdname print-issues
#' @exportS3Method print LabelsTB
#' @method print LabelsTB
#' @export
print.LabelsTB <- function(x, ...) {
    x$labels_bgcolor <- x$color
    x$labels_color <- c("grey8", "ivory")[
        is_dark(x$labels_bgcolor) + 1L
    ]
    x$labels_url <- x$url
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
        owner_name <- couples$owner[id]
        repo_name <- couples$repo[id]
        labels_owner <- x[x$owner == owner_name & x$repo == repo_name, ]

        cat(
            paste0(
                "\n- ",
                cli::style_hyperlink(
                    text = file.path(owner_name, repo_name, fsep = "/"),
                    url = file.path(
                        "https://github.com",
                        owner_name,
                        repo_name,
                        fsep = "/"
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

#' @rdname print-issues
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
        owner_name <- couples$owner[id]
        repo_name <- couples$repo[id]
        labels_owner <- x[x$owner == owner_name & x$repo == repo_name, ]

        cat(
            paste0(
                "\n- ",
                cli::style_hyperlink(
                    text = file.path(owner_name, repo_name, fsep = "/"),
                    url = file.path(
                        "https://github.com",
                        owner_name,
                        repo_name,
                        fsep = "/"
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
