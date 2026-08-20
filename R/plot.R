col_open_issues <- "#238636"
col_closed_issues <- "#8250DF"

#' @title Generate Monthly Date Sequence
#'
#' @param x Vector of dates or date-like objects.
#'
#' @returns Vector of `Date` objects from the first day of the minimum month in
#' `x` to the current month.
#'
#' @examples
#' dates <- as.Date(c("2023-03-15", "2023-04-20", "2023-05-05"))
#' IssueTrackeR:::get_dates_vec(dates)
#'
#' @dev
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

#' @title Get Issue Resolution Times
#'
#' @description
#' Calculates the time taken to resolve issues in seconds.
#'
#' @param x An object of class \code{IssuesTB}.
#' @param verbose A logical value indicating whether to print additional
#' information. Default is \code{TRUE}.
#' @param \dots Currently not used.
#'
#' @returns Integer vector of resolution times in seconds.
#'
#' @examples
#' all_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'     dataset_name = "closed_issues.yaml"
#' )
#'
#' IssueTrackeR:::get_resolution_times(all_issues)
#' @dev
#' @name get_resolution_times
get_resolution_times <- function(x, ...) {
    UseMethod("get_resolution_times", x)
}

#' @rdname get_resolution_times
#' @export
#' @exportS3Method get_resolution_times IssuesTB
#' @method get_resolution_times IssuesTB
get_resolution_times.IssuesTB <- function(x, verbose = TRUE, ...) {
    if (all(is.na(x$closed_at))) {
        if (verbose) {
            warning("x contains no closed issues.", call. = FALSE)
        }
        return(invisible(NULL))
    }
    x_solved <- x[!is.na(x$closed_at), ]
    differences <- difftime(
        time1 = x_solved$closed_at,
        time2 = x_solved$created_at,
        units = "secs"
    ) |>
        as.integer()
    return(differences)
}

#' @rdname get_resolution_times
#' @export
#' @exportS3Method get_resolution_times default
#' @method get_resolution_times default
get_resolution_times.default <- function(...) {
    stop(
        "The function requires a IssuesTB object!",
        call. = FALSE
    )
}

#' @title Plot Issue Resolution Time Distribution
#'
#' @description
#' Creates a bar plot showing the distribution of issue resolution times
#' in predefined time categories (< 1 day, 1-7 days, 7-30 days, 1 month-1 year,
#' 1-3 years, > 3 years).
#'
#' @param x An object of class \code{IssuesTB}.
#' @param verbose A logical value indicating whether to print additional
#' information. Default is \code{TRUE}.
#'
#' @returns Invisibly returns `NULL`.
#'
#' @examples
#' all_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'     dataset_name = "closed_issues.yaml"
#' )
#'
#' IssueTrackeR:::plot_resolution_bars(all_issues)
#'
#' @importFrom graphics box
#' @importFrom graphics text
#' @importFrom graphics barplot
#'
#' @dev
plot_resolution_bars <- function(x, verbose = TRUE) {
    resolution_time <- get_resolution_times(x, verbose = FALSE) / 86400.0
    if (length(resolution_time) == 0L) {
        if (verbose) {
            warning("x contains no closed issues.", call. = FALSE)
        }
        return(invisible(NULL))
    }
    resolution_time <- resolution_time[!is.na(resolution_time)]

    breaks <- c(0L, 1L, 7L, 30L, 365L, 3L * 365L, max(resolution_time) + 1L)
    axis_labels <- c(
        "< 1 day",
        "1-7 days",
        "7-30 days",
        "1 month-1 year",
        "1-3 years",
        "> 3 years"
    )
    classes <- cut(
        resolution_time,
        breaks = breaks,
        labels = axis_labels,
        right = FALSE,
        include.lowest = TRUE
    )
    counts <- table(classes)

    bp <- graphics::barplot(
        counts,
        main = "Resolution delay",
        ylab = "Number of issues",
        xlab = NULL,
        col = "grey75",
        border = NA,
        las = 1L,
        ylim = c(0.0, max(counts) * 1.15)
    )

    graphics::text(
        bp,
        counts,
        labels = counts,
        pos = 3L,
        cex = 0.9
    )

    graphics::box()

    return(invisible(NULL))
}

#' @title Plot Empirical Cumulative Distribution of Resolution Times
#'
#' @description
#' Creates an ECDF plot showing the cumulative distribution of issue
#' resolution times on a log scale (1 hour, 1 day, 1 week, 1 month, 1 year,
#' 3 years).
#'
#' @param x An object of class \code{IssuesTB}.
#' @param verbose A logical value indicating whether to print additional
#' information. Default is \code{TRUE}.
#'
#' @returns Invisibly returns `NULL`.
#'
#' @examples
#' all_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'     dataset_name = "closed_issues.yaml"
#' )
#'
#' IssueTrackeR:::plot_resolution_ecdf(all_issues)
#' @importFrom graphics axis
#'
#' @dev
#'
plot_resolution_ecdf <- function(x, verbose = TRUE) {
    resolution_time <- (1L + get_resolution_times(x, verbose = FALSE)) / 3600.0
    if (length(resolution_time) == 0L) {
        if (verbose) {
            warning("x contains no closed issues.", call. = FALSE)
        }
        return(invisible(NULL))
    }
    resolution_time <- resolution_time[!is.na(resolution_time)]

    ticks <- c(
        1L,
        24L,
        24L * 7L,
        24L * 30L,
        24L * 365L,
        3L * 365L * 24L,
        max(resolution_time) + 1L
    )
    axis_labels <- c(
        "1 hour",
        "1 day",
        "1 week",
        "1 month",
        "1 year",
        "3 years",
        "> 3 years"
    )

    x_ecdf <- sort(resolution_time)
    y_ecdf <- seq_along(x_ecdf) / length(x_ecdf)

    cond <- ticks >= min(x_ecdf) & ticks <= max(x_ecdf)

    plot(
        x_ecdf,
        y_ecdf,
        type = "s",
        log = "x",
        xaxt = "n",
        main = "Cumulative distribution",
        xlab = "Time",
        ylab = "Proportion of issues"
    )

    graphics::axis(
        side = 1L,
        at = ticks[cond],
        labels = axis_labels[cond]
    )
    return(invisible(NULL))
}


#' @title Split dates in bins
#'
#' @description
#' Groups observations by date bins and returns the count of observations in
#' each bin.
#'
#' @param x A vector of `Date`
#' @param dates A vector of `Date` objects defining the bin boundaries. If not
#'   provided, the function uses `get_dates_vec(x)` to generate the dates.
#'
#' @returns
#' A integer vector with the number of element in each date bin
#'
#' @examples
#' # With a predefined date vector
#' dates <- as.Date(c("2023-01-01", "2023-02-01", "2023-03-01"))
#' x <- as.Date(c(
#'     "2023-01-15", "2023-01-20", "2023-02-10",
#'     "2023-02-28", "2023-03-15", "2023-03-20"
#' ))
#' IssueTrackeR:::bin_count(x, dates)
#'
#' # Default dates
#' x <- c(
#'     "2023-01-15", "2023-01-20", "2023-02-10",
#'     "2023-02-28", "2023-03-15", "2023-03-20"
#' )
#' IssueTrackeR:::bin_count(x)
#' @details
#'
#' The accepted formats for the argument \code{x} are:
#'
#' \itemize{
#' \item \code{character} objects;
#' \item \code{Date} objects;
#' \item numeric (\code{integer} or \code{double});
#' \item date/times object (classes \code{POSIXct} and \code{POSIXlt})
#' }
#'
#' @dev
bin_count <- function(x, dates = get_dates_vec(x)) {
    groups <- as.Date(x) |>
        cut(breaks = c(dates, max(dates) + 31L)) |>
        table() |>
        as.integer()
    return(groups)
}

#' @title Add Years to a Date
#'
#' @description
#' Adds a specified number of years to a given date.
#'
#' @param x A date object
#' @param n An integer specifying the number of years to add. Can be positive
#'   or negative.
#'
#'
#' @returns
#' A `Date` object representing the input date with `n` years added.
#' If the input date is invalid or cannot be coerced to `POSIXlt`, an error is
#' raised.
#'
#' @examples
#' # Add years to a Date object
#' date <- as.Date("2023-01-15")
#' IssueTrackeR:::add_n_years(date, n = 2)
#'
#' # Subtract years
#' IssueTrackeR:::add_n_years(date, n = -1)
#'
#' # Handle leap years
#' leap_day <- as.Date("2020-02-29")
#' IssueTrackeR:::add_n_years(leap_day, n = 1)
#' IssueTrackeR:::add_n_years(leap_day, n = 4)
#'
#' # Add years to a character date
#' IssueTrackeR:::add_n_years("2023-12-31", n = 3)  # Returns "2026-12-31"
#'
#' @details
#' It is possible to subtract years with negative values of `n`.
#' Leap day is saved each 4 years.
#'
#' The accepted formats for the argument \code{x} are:
#'
#' \itemize{
#' \item \code{character} objects;
#' \item \code{Date} objects;
#' \item numeric (\code{integer} or \code{double});
#' \item date/times object (classes \code{POSIXct} and \code{POSIXlt})
#' }
#'
#' @dev
add_n_years <- function(x, n) {
    lt <- as.POSIXlt(x)
    lt$year <- lt$year + n
    return(as.Date(lt))
}

#' @title Count Still Open Issues Over Time
#'
#' @param x An object of class \code{IssuesTB}.
#' @param lag Numeric. Number of years to look back for "still open" issues.
#'   Default is 0.
#' @param \dots Currently not used.
#'
#' @returns `ts` object with still open issues counts per month.
#'
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' open_issues <- IssueTrackeR:::get_still_open(issues, lag = 1L)
#'
#' @dev
get_still_open <- function(x, ...) {
    UseMethod("get_still_open", x)
}

#' @rdname get_still_open
#' @export
#' @exportS3Method get_still_open IssuesTB
#' @method get_still_open IssuesTB
#' @importFrom stats ts
get_still_open.IssuesTB <- function(x, lag = 0L, ...) {
    dates <- get_dates_vec(x$created_at)

    closed <- as.Date(x$closed_at)
    closed[is.na(closed)] <- max(dates) + 32L
    created <- add_n_years(x$created_at, lag)

    keep <- closed > created

    new_created <- bin_count(created[keep], dates)
    new_closed <- bin_count(closed[keep], dates)
    still_open <- cumsum(new_created) - cumsum(new_closed)

    start_date <- as.integer(format(min(dates), format = c("%Y", "%m")))
    still_open <- stats::ts(still_open, start = start_date, frequency = 12L)

    return(still_open)
}

#' @rdname get_still_open
#' @export
#' @exportS3Method get_still_open default
#' @method get_still_open default
get_still_open.default <- function(...) {
    stop(
        "The function requires a IssuesTB object!",
        call. = FALSE
    )
}

#' @title Generate Age Matrix of Open Issues
#'
#' @param x An object of class \code{IssuesTB}.
#' @param n Number of age categories to create. Default: `3`.
#' @param \dots Currently not used.
#'
#' @returns `ts` matrix of open issue counts by age category.
#'
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' age_matrix <- IssueTrackeR:::generate_age_mat(issues, n = 2)
#'
#' @dev
generate_age_mat <- function(x, ...) {
    UseMethod("generate_age_mat", x)
}

#' @rdname generate_age_mat
#' @export
#' @exportS3Method generate_age_mat IssuesTB
#' @method generate_age_mat IssuesTB
generate_age_mat.IssuesTB <- function(x, n = 3L, ...) {
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

#' @rdname generate_age_mat
#' @export
#' @exportS3Method generate_age_mat default
#' @method generate_age_mat default
generate_age_mat.default <- function(...) {
    stop(
        "The function requires a IssuesTB object!",
        call. = FALSE
    )
}

#' @title Generate Matrix of Open Issues by Author
#'
#' @param x An object of class \code{IssuesTB}.
#' @param n Number of author to create. Default: `5`.
#' @param \dots Currently not used.
#'
#' @returns `ts` matrix of open issue counts by author.
#'
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' author_matrix <- IssueTrackeR:::generate_author_mat(issues, n = 2)
#'
#' @dev
#' @name generate_author_mat
generate_author_mat <- function(x, ...) {
    UseMethod("generate_author_mat", x)
}

#' @rdname generate_author_mat
#' @export
#' @exportS3Method generate_author_mat IssuesTB
#' @method generate_author_mat IssuesTB
generate_author_mat.IssuesTB <- function(x, n = 5L, ...) {
    authors <- unique(x$creator)

    if (n > length(authors)) {
        n <- length(authors)
    }
    issues_by_author <- lapply(
        X = authors,
        FUN = \(author) subset(x, x$creator == author)
    ) |>
        lapply(FUN = count_issues) |>
        as.numeric()

    cond_author <- issues_by_author >=
        sort(issues_by_author, decreasing = TRUE)[n]
    sub_authors <- authors[which(cond_author)]

    authors_mat <- lapply(
        X = sub_authors,
        FUN = \(author) subset(x, x$creator == author)
    ) |>
        lapply(
            FUN = get_still_open
        ) |>
        do.call(what = cbind)

    if (n < length(authors)) {
        authors_mat <- cbind(
            authors_mat,
            get_still_open(subset(x, !x$creator %in% sub_authors))
        )
        colnames(authors_mat) <- c(sub_authors, "Others")
    } else {
        colnames(authors_mat) <- sub_authors
    }

    authors_mat[is.na(authors_mat)] <- 0L
    return(authors_mat)
}

#' @rdname generate_author_mat
#' @export
#' @exportS3Method generate_author_mat default
#' @method generate_author_mat default
generate_author_mat.default <- function(...) {
    stop(
        "The function requires a IssuesTB object!",
        call. = FALSE
    )
}

#' @title Plot Evolution of Open Issues by Categories
#'
#' @param categorised_mat `mts` object with number of issues by categories.
#'
#' @returns Invisibly returns NULL.
#'
#' @details
#' The function generates a plot directly. The plot shows a stacked area chart
#' where each coloured area represents a category of open issues over time.
#'
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' age_mat <- IssueTrackeR:::generate_age_mat(issues, 3L)
#'
#' IssueTrackeR:::plot_barplot(age_mat)
#'
#' @importFrom graphics polygon legend
#' @importFrom grDevices hcl.colors
#' @importFrom zoo as.Date
#' @importFrom stats time
#'
#' @dev
plot_barplot <- function(categorised_mat, by = "Age") {
    dates <- zoo::as.Date(stats::time(categorised_mat))
    cols <- grDevices::hcl.colors(
        ncol(categorised_mat),
        palette = "Viridis",
        rev = TRUE
    )

    plot(
        range(dates),
        c(0L, max(rowSums(categorised_mat))),
        type = "n",
        xlab = "Date",
        ylab = "Open issues",
        main = paste("Open Issues by", by)
    )

    cum <- rep(0L, nrow(categorised_mat))

    for (j in seq_len(ncol(categorised_mat))) {
        y1 <- cum
        y2 <- cum + categorised_mat[, j]

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
        legend = colnames(categorised_mat),
        fill = cols,
        bty = "n"
    )

    return(invisible(NULL))
}

#' @title Plot Issue Creation, Closed, and Backlog Over Time
#'
#' @param x An object of class \code{IssuesTB}.
#'
#' @returns Invisibly returns NULL.
#'
#' @details
#' Generates a composite plot showing:
#'
#' - Green bars: New issues created per month (above x-axis)
#' - Purple bars: Issues closed per month (below x-axis)
#' - Black line: Cumulative backlog of open issues
#'
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#'
#' IssueTrackeR:::plot_created_closed(issues)
#' @dev
#'
#' @importFrom graphics abline
#' @importFrom graphics rect
#' @importFrom graphics lines
#' @importFrom graphics legend
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
        col = col_open_issues,
        border = NA
    )

    # fermetures
    graphics::rect(
        xleft = dates - 10L,
        ybottom = -new_closed,
        xright = dates + 10L,
        ytop = 0L,
        col = col_closed_issues,
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
        col = c("black", col_open_issues, col_closed_issues),
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
#' @param \dots Currently not used.
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
#' When \code{type = "author"}, the same graph as \code{type = "historic"} but
#' this time with the number of open issues by author over time.
#'
#' When \code{type = "created-closed"}, the total number of open issues is
#' displayed together with the monthly numbers of newly created and newly
#' closed issues. This visualization helps assess whether issue creation
#' and resolution rates are balanced over time.
#'
#' All statistics are aggregated monthly, from the month of the first issue
#' creation to the current date.
#'
#' When \code{type = "resolution-time"}, the resolution times are computed and
#' displayed in two forms:
#' - bar plot with categories from time
#' - ECDF to show the cumulative distribution of issues resolution times on a
#'   log scale
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
#' plot(all_issues, type = "author")
#' plot(all_issues, type = "created-closed")
#' plot(all_issues, type = "resolution-time")
#'
#' @name plot-issues
#'
#' @method plot IssuesTB
#' @exportS3Method base::plot
#' @export
#'
#' @importFrom withr with_par
#'
plot.IssuesTB <- function(
    x,
    type = c("historic", "author", "created-closed", "resolution-time"),
    n = 3L,
    ...
) {
    type <- match.arg(type)
    switch(
        type,
        historic = {
            age_mat <- generate_age_mat(x, n)
            plot_barplot(age_mat, by = "age")
        },
        author = {
            age_mat <- generate_author_mat(x, n)
            plot_barplot(age_mat, by = "author")
        },
        "created-closed" = plot_created_closed(x),
        "resolution-time" = withr::with_par(
            new = list(mfrow = c(1L, 2L)),
            code = {
                plot_resolution_bars(x)
                plot_resolution_ecdf(x)
            }
        )
    )
    return(invisible(x))
}
