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
#'     dataset_name = "list_issues.yaml"
#' )
#'
#' # Summarise one issue
#' summary(all_issues[1, ])
#'
#' # Summarise several issues
#' summary(all_issues[1:10, ])
#' @rdname summary
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
    object$nbr_comments <- nrow(object$comments)
    object$has_labels <- length(object$labels) > 0L

    if (object$has_labels) {
        object$label_name <- vapply(
            X = object$labels,
            FUN = `[[`,
            "name",
            FUN.VALUE = character(1L)
        )
        object$label_bgcolor <- paste0(
            "#",
            vapply(
                X = object$labels,
                FUN = `[[`,
                "color",
                FUN.VALUE = character(1L)
            )
        )

        isDark <- function(colr) {
            apply(
                X = grDevices::col2rgb(colr) * c(299L, 587L, 114L),
                FUN = sum,
                MARGIN = 2L
            ) /
                1000L <
                123L
        }

        object$label_color <- c("grey8", "ivory")[
            isDark(object$label_bgcolor) + 1L
        ]
        object$label_url <- vapply(
            X = object$labels,
            FUN = `[[`,
            "url",
            FUN.VALUE = character(1L)
        ) |>
            gsub(
                pattern = "https://api.github.com/repos/",
                replacement = "https://github.com/"
            )
    }

    class(object) <- "summary.IssueTB"
    return(object)
}

#' @rdname summary
#' @exportS3Method summary IssuesTB
#' @method summary IssuesTB
#' @export
summary.IssuesTB <- function(object, ...) {
    state_table <- c(
        open = "\U1F7E2 Open",
        reopened = "\U267B Re-opened",
        completed = "\U2714 Completed",
        not_planned = "\U1F6AB Not planned",
        duplicated = "\U27BF Duplicated"
    )

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
    class(x) <- "summary.IssuesTB"
    return(x)
}
