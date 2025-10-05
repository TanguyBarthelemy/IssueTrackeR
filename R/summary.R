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
summary.IssueTB <- function(object, labels, ...) {
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

        object$labels_name <- object$labels

        if (!missing(labels) && inherits(labels, "LabelsTB")) {

            my_labels <- labels |>
                subset(
                    owner == object$owner
                    & repo == object$repo
                    & name %in% object$labels_name,
                       select = c("name", "color")
                )

            if (nrow(my_labels) == length(object$labels_name)
                & all(object$labels_name %in% my_labels$name)) {

                rownames(my_labels) <- my_labels$name
                object$labels_bgcolor <- my_labels[object$labels_name, "color"]

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
            } else {
                message("Your `labels` don't correspond to the issue.")
            }

        } else {
            message("For colourful links, add a labels argument (resulting from get_labels().")
        }

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
