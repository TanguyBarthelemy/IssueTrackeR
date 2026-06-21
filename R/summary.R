state_table <- c(
    open = "\U1F7E2 Open",
    reopened = "\U267B Re-opened",
    completed = "\U2714 Completed",
    not_planned = "\U1F6AB Not planned",
    duplicate = "\U27BF Duplicated",
    duplicated = "\U27BF Duplicated"
)

prepare_label_display <- function(x, html_url) {
    if (nrow(x) == 0L) {
        return("")
    }

    labels_name <- x$name
    labels_bgcolor <- x$color

    labels_color <- c("grey8", "ivory")[isDark(x$color) + 1L]
    labels_url <- paste0(
        gsub(
            x = html_url,
            pattern = "\\/[^\\/]*\\/[^\\/]*$",
            replacement = "/labels/"
        ),
        utils::URLencode(labels_name)
    )

    output <- vapply(
        X = seq_along(labels_name),
        FUN = function(k) {
            label_style <- crayon::combine_styles(
                crayon::make_style(labels_color[k]),
                crayon::make_style(labels_bgcolor[k], bg = TRUE)
            )
            cli::style_hyperlink(
                text = label_style(labels_name[k]),
                url = labels_url[k]
            )
        },
        FUN.VALUE = character(1L)
    ) |>
        paste(collapse = ", ")
    return(output)
}

#' @title Compute a summary of an issue or a list of issues
#'
#' @param object a \code{IssueTB} or \code{IssuesTB} object.
#' @param \dots Unused argument
#'
#' @details
#' This function compute the summary of an issue (\code{IssueTB} object) with
#' adding some information (number of comments, ...).
#' For a list of issues (\code{IssuesTB} object), it just summarise the
#' information with statistics by modalities.
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
#' # Summarise one issue
#' summary(all_issues[1, ])
#'
#' # Summarise several issues
#' summary(all_issues[1:10, ])
#' @name summary
#'
#' @exportS3Method summary IssueTB
#' @method summary IssueTB
#'
#' @export
summary.IssueTB <- function(object, ...) {
    object$desc <- paste0(
        object[["owner"]],
        "/",
        object[["repo"]],
        "#",
        object[["number"]]
    )

    object$nbr_comments <- get_nbr_comments(object)
    object$state_reason <- state_table[object[["state_reason"]]]
    object$label_display <- prepare_label_display(
        x = object$labels,
        html_url = object$html_url
    )

    class(object) <- "summary.IssueTB"
    return(object)
}

#' @param with_labels Boolean. Display the labels with the list of issues.
#'   Default is `FALSE`.
#' @rdname summary
#' @exportS3Method summary IssuesTB
#' @method summary IssuesTB
#' @export
summary.IssuesTB <- function(object, with_labels = FALSE, ...) {
    x <- list(
        nbr_issues = nrow(object),
        issue_desc = paste0(
            object[["owner"]],
            "/",
            object[["repo"]],
            "#",
            object[["number"]]
        ),
        html_url = object[["html_url"]],
        state_reason = state_table[object[["state_reason"]]]
    )

    if (with_labels) {
        x$label_display <- Map(
            prepare_label_display,
            object$labels,
            object$html_url
        )
    }

    class(x) <- "summary.IssuesTB"
    return(x)
}

#' @rdname summary
#' @exportS3Method summary LabelsTB
#' @method summary LabelsTB
#' @export
summary.LabelsTB <- function(object, ...) {
    object$labels_bgcolor <- object$color
    object$labels_color <- c("grey8", "ivory")[
        isDark(object$labels_bgcolor) + 1L
    ]
    object$labels_url <- paste(
        "https://github.com",
        object$owner,
        object$repo,
        "labels",
        utils::URLencode(object$labels_name),
        sep = "/"
    )
    object$formated_label <- vapply(
        X = seq_len(nrow(object)),
        FUN = function(k) {
            label_style <- crayon::combine_styles(
                crayon::make_style(object$labels_color[k]),
                crayon::make_style(object$labels_bgcolor[k], bg = TRUE)
            )
            cli::style_hyperlink(
                text = label_style(object$name[k]),
                url = object$labels_url[k]
            )
        },
        FUN.VALUE = character(1L)
    )

    class(object) <- c("summary.LabelsTB", "data.frame")
    return(object)
}
