#' @title Who contributes?
#' @noRd
#' @name compute_contribution
compute_contribution <- function(x, n) {
    UseMethod(generic = "compute_contribution", object = x)
}

#' @rdname compute_contribution
#' @exportS3Method compute_contribution IssuesTB
#' @method compute_contribution IssuesTB
#' @export
#' @importFrom checkmate assert_number
compute_contribution.IssuesTB <- function(x, n = Inf) {
    checkmate::assert_number(n, lower = 0)
    if (!is.infinite(n)) {
        checkmate::assert_integerish(n, len = 1L, lower = 1L)
    }

    opener <- table(x$creator)

    if (n < length(opener)) {
        opener <- c(
            opener |> sort(decreasing = TRUE) |> head(n),
            other = opener |> sort(decreasing = TRUE) |> tail(-n) |> sum()
        )
    }
    commenter <- x$comments |>
        lapply(FUN = `[[`, "author") |>
        do.call(what = c) |>
        table()
    if (n < length(commenter)) {
        commenter <- c(
            commenter |> sort(decreasing = TRUE) |> head(n),
            other = commenter |> sort(decreasing = TRUE) |> tail(-n) |> sum()
        )
    }
    closer <- table(x$closed_by)
    if (n < length(closer)) {
        closer <- c(
            closer |> sort(decreasing = TRUE) |> head(n),
            other = closer |> sort(decreasing = TRUE) |> tail(-n) |> sum()
        )
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
