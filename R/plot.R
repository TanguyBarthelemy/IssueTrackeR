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

# Nbr d'issues ouvertes depuis au moins lag annés
get_still_open <- function(x, lag = 0) {
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

generate_age_mat <- function(x, n = 3) {
    age_mat <- lapply(
        X = seq_len(n + 1) - 1,
        FUN = get_still_open,
        x = x
    ) |>
        do.call(what = cbind)
    age_mat <- age_mat - cbind(age_mat[, -1], 0L)

    colnames(age_mat)[n + 1] <- paste0(">", n, "y")
    colnames(age_mat)[seq_len(n)] <- paste0(
        seq_len(n) - 1,
        "-",
        seq_len(n),
        "y"
    )
    return(age_mat)
}


plot_historic <- function(x, n = 3) {
    dates <- get_dates_vec(x$created_at)
    age_mat <- generate_age_mat(x, n)

    cols <- grDevices::hcl.colors(
        ncol(age_mat),
        palette = "Viridis",
        rev = TRUE
    )

    plot(
        range(dates),
        c(0, max(rowSums(age_mat))),
        type = "n",
        xlab = "Date",
        ylab = "Issues ouvertes",
        main = "Anciennete du backlog"
    )

    cum <- rep(0, nrow(age_mat))

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

    legend(
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
        ylab = "Nombre d'issues",
        main = "Backlog et flux"
    )

    graphics::abline(h = 0, col = "grey70")

    # ouvertures
    graphics::rect(
        xleft = dates - 10,
        ybottom = 0,
        xright = dates + 10,
        ytop = new_created,
        col = "#238636",
        border = NA
    )

    # fermetures
    graphics::rect(
        xleft = dates - 10,
        ybottom = -new_closed,
        xright = dates + 10,
        ytop = 0,
        col = "#DA3633",
        border = NA
    )

    # backlog
    graphics::lines(
        dates,
        still_open,
        lwd = 2,
        col = "black"
    )

    graphics::legend(
        "topleft",
        legend = c("Still open", "New open", "New closed"),
        col = c("black", "#238636", "#DA3633"),
        lty = c(1, NA, NA),
        pch = c(NA, 15, 15),
        pt.cex = 2,
        bty = "n"
    )

    return(NULL)
}

#' @title Plot IssuesTB object
#'
#' @description
#' 2 ways to plot the issues:
#'  * The issues still open by layer of ancienneté
#'  * The contribution between new created, closed and still open
#'
#' @param x a \code{IssuesTB} object.
#' @param type a character to indicate which graph to plot. The accepted values
#'  are "history" or "created-closed". The default value is "history".
#' @param n The number of layer displayed in the graph.
#'  Used only if `type = "history`
#' @param \dots Unused argument
#'
#' @details
#' If `type = "history"`, the graph will show the number of issue at the time
#' according to the ancienneté of its creation (in the year, one year ago, two
#' years ago...).
#'
#' If `type == "created-closed`, the number of open issue is accompagné of the
#' contribution of the created and closed issues.
#'
#' The number of issues is displayed by month from the date of the first
#' created issue to today.
#'
#' @returns invisibly (with \code{invisible()}) \code{x}.
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
#' plot(all_issues, type = "history")
#' plot(all_issues, type = "created-close")
#' @rdname plot
#' @exportS3Method plot IssuesTB
#' @method plot IssuesTB
#' @export
plot.IssuesTB <- function(
    x,
    type = c("history", "created-closed"),
    n = 3,
    ...
) {
    type <- match.arg(type)
    if (type == "history") {
        plot_historic(x, n)
    } else {
        plot_created_closed(x)
    }
    return(invisible(x))
}
