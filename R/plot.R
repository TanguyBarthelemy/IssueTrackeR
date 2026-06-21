get_dates_vec <- function(x) {
    min_date <- x |>
        as.Date() |>
        min() |>
        format("%Y-%m") |>
        paste0(... = _, "-01") |>
        as.Date()
    dates <- seq.Date(
        from = min_date,
        to = Sys.Date(),
        by = "month"
    )
    return(dates)
}

bin_count <- function(x, dates = get_dates_vec(x)) {
    groups <- as.Date(x) |>
        cut(breaks = c(dates, max(dates) + 31L)) |>
        table() |>
        as.numeric()
    return(groups)
}

add_n_years <- function(x, n) {
    lt <- as.POSIXlt(x)
    lt$year <- lt$year + n
    as.Date(lt)
}

# Nbr of open issues for at least `lag` years
get_still_open <- function(x, lag = 0L) {
    dates <- get_dates_vec(x$created_at)

    closed <- as.Date(x$closed_at)
    closed[is.na(closed)] <- max(dates) + 32L
    created <- add_n_years(x$created_at, lag)

    keep <- closed > created

    new_created <- bin_count(created[keep], dates)
    new_closed <- bin_count(closed[keep], dates)
    still_open <- cumsum(new_created) - cumsum(new_closed)
    names(still_open) <- dates

    return(still_open)
}

generate_age_mat <- function(x, n = 3L) {
    age_mat <- lapply(
        X = seq_len(n + 1L) - 1L,
        FUN = get_still_open,
        x = x
    ) |>
        do.call(what = cbind)
    age_mat <- age_mat - cbind(age_mat[, -1L], 0L)

    colnames(age_mat)[n + 1L] <- paste0(">", n, "y")
    colnames(age_mat)[seq_len(n)] <- paste0(
        seq_len(n) - 1L,
        "-",
        seq_len(n),
        "y"
    )
    return(age_mat)
}

plot_historic <- function(x, n = 3L) {
    dates <- get_dates_vec(x$created_at)
    age_mat <- generate_age_mat(x, n)

    cols <- grDevices::hcl.colors(
        ncol(age_mat),
        palette = "Viridis",
        rev = TRUE
    )

    plot(
        range(dates),
        c(0L, max(rowSums(age_mat))),
        type = "n",
        xlab = "Date",
        ylab = "Open issues",
        main = "Open Issues by Age"
    )

    cum <- rep(0L, nrow(age_mat))

    for (j in seq_len(ncol(age_mat))) {
        y1 <- cum
        y2 <- cum + age_mat[, j]

        graphics::polygon(
            c(dates, rev(dates)),
            c(y1, rev(y2)),
            col = cols[j],
            border = NA
        )

        cum <- y2
    }

    graphics::legend(
        "topleft",
        legend = colnames(age_mat),
        fill = cols,
        bty = "n"
    )

    return(invisible(NULL))
}

plot_created_closed <- function(x) {
    dates <- get_dates_vec(x$created_at)

    new_created <- bin_count(x$created_at, dates)
    new_closed <- bin_count(x$closed_at, dates)
    still_open <- cumsum(new_created) - cumsum(new_closed)

    ylim <- c(
        -max(new_closed) * 1.2,
        max(c(new_created, still_open)) * 1.2
    )

    plot(
        dates,
        still_open,
        type = "n",
        ylim = ylim,
        xlab = "Date",
        ylab = "Number of issues",
        main = "Evolution of Open Issues"
    )

    graphics::abline(h = 0L, col = "grey70")

    # ouvertures
    graphics::rect(
        xleft = dates - 10L,
        ybottom = 0L,
        xright = dates + 10L,
        ytop = new_created,
        col = "#238636",
        border = NA
    )

    # fermetures
    graphics::rect(
        xleft = dates - 10L,
        ybottom = -new_closed,
        xright = dates + 10L,
        ytop = 0L,
        col = "#DA3633",
        border = NA
    )

    # backlog
    graphics::lines(
        dates,
        still_open,
        lwd = 2L,
        col = "black"
    )

    graphics::legend(
        "topleft",
        legend = c("Still open", "New created", "New closed"),
        col = c("black", "#238636", "#DA3633"),
        lty = c(1L, NA, NA),
        pch = c(NA, 15L, 15L),
        pt.cex = 2L,
        bty = "n"
    )

    return(NULL)
}

#' @title Plot an IssuesTB object
#'
#' @description
#' Visualize the evolution of an issue tracker backlog.
#'
#' Two types of plots are available:
#' \itemize{
#'   \item \code{"historic"}: displays the distribution of open issues by age.
#'   \item \code{"created-closed"}: displays backlog size together with the
#'   numbers of newly created and newly closed issues.
#' }
#'
#' @param x An object of class \code{IssuesTB}.
#' @param type Character string indicating which plot to produce.
#'   Accepted values are \code{"historic"} and \code{"created-closed"}.
#'   The default is \code{"historic"}.
#' @param n Integer specifying the number of age classes to display when
#'   \code{type = "historic"}.
#' @param \dots Currently ignored.
#'
#' @details
#' When \code{type = "historic"}, a stacked area chart is produced showing
#' the number of open issues by age over time. This visualization highlights
#' the evolution and aging of the backlog.
#'
#' The first classes correspond to one-year intervals (\code{0-1y},
#' \code{1-2y}, ..., \code{(n-1)-ny}) and the last class groups all issues
#' older than \code{n} years.
#'
#' When \code{type = "created-closed"}, the total number of open issues is
#' displayed together with the monthly numbers of newly created and newly
#' closed issues. This visualization helps assess whether issue creation
#' and resolution rates are balanced over time.
#'
#' All statistics are aggregated monthly, from the month of the first issue
#' creation to the current date.
#'
#' @returns
#' Invisibly returns \code{x}.
#'
#' @examples
#' all_issues <- rbind(
#'     get_issues(
#'         source = "local",
#'         dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'         dataset_name = "open_issues.yaml"
#'     ),
#'     get_issues(
#'         source = "local",
#'         dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'         dataset_name = "closed_issues.yaml"
#'     )
#' )
#'
#' plot(all_issues, type = "historic")
#' plot(all_issues, type = "created-closed")
#'
#' @rdname plot
#' @method plot IssuesTB
#' @export
plot.IssuesTB <- function(
    x,
    type = c("historic", "created-closed"),
    n = 3L,
    ...
) {
    type <- match.arg(type)
    if (type == "historic") {
        plot_historic(x, n)
    } else if (type == "created-closed") {
        plot_created_closed(x)
    }
    return(invisible(x))
}
