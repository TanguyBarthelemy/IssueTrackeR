#' @title Who contributes?
#'
#' @description
#' Calculates the contributions of users to a project by counting
#' the number of issues they opened, commented on, and closed.
#'
#' @inheritParams with_labels x
#' @param n Integer. Maximum number of top contributors to display.
#'   Contributions from other users are grouped under "other".
#'   Default: \code{Inf} (shows all contributors individually).
#'
#' @returns A matrix of class \code{ContributionsTB} with users as columns
#' and contribution types (opened, commented, closed) as rows.
#' Each cell contains the count of contributions for that user and type.
#'
#' @examples
#' issues_selector <- init_selector(
#'     source = "Local",
#'     dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'     dataset_name = NULL
#' )
#' my_issues <- get_issues(selector = issues_selector)
#'
#' # Compute contributions for all users
#' contributions <- compute_contribution(my_issues)
#'
#' # Compute contributions for top 5 users
#' top_contributions <- compute_contribution(my_issues, n = 5)
#' @export
#' @name compute_contribution
compute_contribution <- function(x, n) {
    UseMethod(generic = "compute_contribution", object = x)
}

#' @rdname compute_contribution
#' @exportS3Method compute_contribution IssuesTB
#' @method compute_contribution IssuesTB
#' @export
#' @importFrom checkmate assert_number
#' @importFrom utils tail
#' @importFrom utils head
compute_contribution.IssuesTB <- function(x, n = Inf) {
    checkmate::assert_number(n, lower = 0L)
    if (!is.infinite(n)) {
        checkmate::assert_integerish(n, len = 1L, lower = 1L)
    }

    opener <- table(x$creator)
    if (n < length(opener)) {
        head_opener <- opener |> sort(decreasing = TRUE) |> utils::head(n)
        other_opener <- opener |>
            sort(decreasing = TRUE) |>
            utils::tail(-n) |>
            sum()
        opener <- c(head_opener, other = other_opener)
    }
    commenter <- x$comments |>
        lapply(FUN = `[[`, "author") |>
        do.call(what = c) |>
        table()
    if (n < length(commenter)) {
        head_commenter <- commenter |> sort(decreasing = TRUE) |> utils::head(n)
        other_commenter <- commenter |>
            sort(decreasing = TRUE) |>
            utils::tail(-n) |>
            sum()
        commenter <- c(head_commenter, other = other_commenter)
    }
    closer <- table(x$closed_by)
    if (n < length(closer)) {
        head_closer <- closer |> sort(decreasing = TRUE) |> utils::head(n)
        other_closer <- closer |>
            sort(decreasing = TRUE) |>
            utils::tail(-n) |>
            sum()
        closer <- c(head_closer, other = other_closer)
    }

    all_id <- unique(c(names(opener), names(commenter), names(closer)))
    opener[setdiff(all_id, names(opener))] <- 0L
    opener <- opener[all_id]
    commenter[setdiff(all_id, names(commenter))] <- 0L
    commenter <- commenter[all_id]
    closer[setdiff(all_id, names(closer))] <- 0L
    closer <- closer[all_id]

    contributions <- rbind(opener, commenter, closer)
    class(contributions) <- "ContributionsTB"
    return(contributions)
}

#' @rdname compute_contribution
#' @exportS3Method compute_contribution default
#' @method compute_contribution default
#' @export
compute_contribution.default <- function(x, n) {
    stop(
        "This function requires a IssuesTB object.",
        call. = FALSE
    )
}
