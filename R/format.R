#' @title Round a timestamp to the inferior integer
#'
#' @description
#' This function round a timestamp ()
#'
#' @param x The timestamp. See detail section for more information.
#'
#' @details
#' The accepted formats for the argument \code{x} are:
#'
#' \itemize{
#' \item \code{character} objects;
#' \item \code{Date} objects;
#' \item numeric (\code{integer} or \code{double});
#' \item date/times object (classes \code{POSIXct} and \code{POSIXlt})
#' }
#'
#' @returns a \code{POSIXct} object with rounded \code{double} value.
#'
#' @dev
#'
#' @examples
#' IssueTrackeR:::format_timestamp(1743694674.9)
#' IssueTrackeR:::format_timestamp(Sys.Date())
#'
format_timestamp <- function(x) {
    output <- x |>
        as.POSIXct(origin = "1970-01-01", tz = "UTC") |>
        as.integer() |>
        as.POSIXct(origin = "1970-01-01", tz = "UTC")
    return(output)
}

#' @title GitHub Data Formatting Functions
#'
#' @description
#' A collection of functions to format GitHub API responses into simpler,
#' more usable R structures. These functions handle labels, comments,
#' issues, and milestones from the GitHub API.
#'
#' @param raw_issues a \code{gh_response} object output from the function
#' \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#' issues.
#' @param raw_comments a \code{gh_response} object output from the function
#' \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#' comments.
#' @param raw_labels a \code{gh_response} object output from the function
#' \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#' labels.
#' @param raw_milestone Raw milestone. Subset of a \code{gh_response} object
#' output from the function \code{\link[gh]{gh}} which contains all the data
#' and metadata for a GitHub milestone.
#' @param raw_milestones a \code{gh_response} object output from the function
#' \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#' milestones.
#' @param urls A character vector of issue URLs for which comments should be
#'   formatted.
#' @inheritParams get
#'
#' @returns
#' - `format_labels`: A data frame with columns: `name`, `description`, `color`.
#' - `format_comments`: A list of data frames with columns: `text`, `author`.
#' - `format_github_issues`: A list of IssuesTB objects with complete issue data.
#' - `format_milestone`: A data frame with milestone information.
#' - `format_milestones`: A list representing milestones with `title`,
#'   `description` and `due_on` date)
#'
#' @examplesIf gh::gh_token_exists() && gh::gh_rate_limit()$remaining > 0
#' \donttest{
#' # Formatting labels
#' raw_labels <- gh::gh(
#'    repo = "rjdemetra",
#'    owner = "rjdverse",
#'    endpoint = "/repos/:owner/:repo/labels",
#'    .limit = Inf,
#'    .progress = FALSE
#' )
#' IssueTrackeR:::format_labels(raw_labels)
#'
#' # Formatting milestone
#' raw_milestones <- gh::gh(
#'     repo = "jdplus-main",
#'     owner = "jdemetra",
#'     endpoint = "/repos/:owner/:repo/milestones",
#'     state = "all",
#'     .limit = Inf,
#'     .progress = FALSE
#' )
#' raw_milestone <- raw_milestones[[5L]]
#' IssueTrackeR:::format_milestone(raw_milestone)
#'
#' # Formatting milestones
#' milestones_jdplus_main <- gh::gh(
#'     repo = "jdplus-main",
#'     owner = "jdemetra",
#'     endpoint = "/repos/:owner/:repo/milestones",
#'     state = "all",
#'     .limit = Inf,
#'     .progress = FALSE
#'  )
#' IssueTrackeR:::format_milestones(milestones_jdplus_main)
#'
#' # Formatting issues
#' raw_issues <- gh::gh(
#'     repo = "rjdemetra",
#'     owner = "rjdverse",
#'     endpoint = "/repos/:owner/:repo/issues",
#'     .limit = Inf,
#'     .progress = FALSE
#' )
#' urls <- vapply(X = raw_issues, FUN = `[[`, "url", FUN.VALUE = character(1L))
#' raw_comments <- gh::gh(
#'     repo = "rjdemetra",
#'     owner = "rjdverse",
#'     endpoint = "/repos/:owner/:repo/issues/comments",
#'     .limit = Inf,
#'     .progress = FALSE
#' )
#' formatted_comments <- IssueTrackeR:::format_comments(raw_comments, urls)
#'
#' formatted_issues <- IssueTrackeR:::format_github_issues(raw_issues = raw_issues,
#'                             raw_comments = raw_comments,
#'                             verbose = FALSE)
#' }
#'
#' @name format
#' @noRd
#'
NULL


#' @rdname format
#' @noRd
format_comments <- function(
    raw_comments,
    urls,
    verbose = TRUE
) {
    comments_urls <- vapply(
        X = raw_comments,
        FUN = `[[`,
        "issue_url",
        FUN.VALUE = character(1L)
    )
    comments_author <- vapply(
        X = raw_comments,
        FUN = Reduce,
        f = `[[`,
        x = c("user", "login"),
        FUN.VALUE = character(1L)
    )
    comments_bodies <- vapply(
        X = raw_comments,
        FUN = `[[`,
        "body",
        FUN.VALUE = character(1L)
    )
    comments_list <- split(
        x = data.frame(text = comments_bodies, author = comments_author),
        f = comments_urls
    ) |>
        lapply(FUN = `rownames<-`, NULL)
    no_comment <- setdiff(urls, comments_urls)
    comments_list <- c(
        comments_list,
        stats::setNames(
            object = rep(
                x = list(data.frame(
                    text = character(0L),
                    author = character(0L),
                    stringsAsFactors = FALSE
                )),
                times = length(no_comment)
            ),
            nm = no_comment
        )
    )

    output <- comments_list[urls]
    names(output) <- NULL

    return(output)
}

#' @rdname format
#' @noRd
format_github_issues <- function(
    raw_issues,
    raw_comments,
    verbose = TRUE
) {
    urls <- vapply(X = raw_issues, FUN = `[[`, "url", FUN.VALUE = character(1L))
    structurel <- utils::strcapture(
        "^https://api.github.com/repos/([^/]+)/([^/]+)/issues/\\d+$",
        urls,
        proto = data.frame(
            owner = character(),
            repo = character(),
            stringsAsFactors = FALSE
        )
    )
    labels_list <- raw_issues |>
        lapply(FUN = `[[`, "labels") |>
        lapply(FUN = function(lbls) {
            if (length(lbls) == 0L) {
                data.frame(
                    name = character(0L),
                    color = character(0L),
                    stringsAsFactors = FALSE
                )
            } else {
                data.frame(
                    name = vapply(
                        X = lbls,
                        FUN = "[[",
                        "name",
                        FUN.VALUE = character(1L)
                    ),
                    color = paste0(
                        "#",
                        vapply(
                            X = lbls,
                            FUN = "[[",
                            "color",
                            FUN.VALUE = character(1L)
                        )
                    ),
                    stringsAsFactors = FALSE
                )
            }
        })

    issues <- new_issues.default(
        url = urls,
        html_url = vapply(
            X = raw_issues,
            FUN = `[[`,
            "html_url",
            FUN.VALUE = character(1L)
        ),
        title = vapply(
            X = raw_issues,
            FUN = `[[`,
            "title",
            FUN.VALUE = character(1L)
        ),
        state = vapply(
            X = raw_issues,
            FUN = `[[`,
            "state",
            FUN.VALUE = character(1L)
        ),
        body = vapply(
            X = raw_issues,
            FUN = function(x) {
                null_to_default(x$body, default = "")
            },
            FUN.VALUE = character(1L)
        ),
        number = vapply(
            X = raw_issues,
            FUN = `[[`,
            "number",
            FUN.VALUE = integer(1L)
        ),
        labels = labels_list,
        milestone = vapply(
            X = raw_issues,
            FUN = function(x) {
                null_to_default(x$milestone$title, default = NA_character_)
            },
            FUN.VALUE = character(1L)
        ),
        comments = format_comments(raw_comments = raw_comments, urls = urls),
        created_at = vapply(
            X = raw_issues,
            FUN = function(.x) {
                .x$created_at |>
                    null_to_default(default = NA_real_) |>
                    strptime(format = "%Y-%m-%dT%H:%M:%S") |>
                    format_timestamp()
            },
            FUN.VALUE = double(1L)
        ),
        closed_at = vapply(
            X = raw_issues,
            FUN = function(.x) {
                .x$closed_at |>
                    null_to_default(default = NA_real_) |>
                    strptime(format = "%Y-%m-%dT%H:%M:%S") |>
                    format_timestamp()
            },
            FUN.VALUE = double(1L)
        ),
        closed_by = vapply(
            X = raw_issues,
            FUN = function(.x) {
                null_to_default(.x$closed_by$login, default = NA_character_)
            },
            FUN.VALUE = character(1L)
        ),
        creator = vapply(
            X = raw_issues,
            FUN = Reduce,
            f = `[[`,
            x = c("user", "login"),
            FUN.VALUE = character(1L)
        ),
        assignee = vapply(
            X = raw_issues,
            FUN = function(x) {
                null_to_default(x$assignee$login, default = NA_character_)
            },
            FUN.VALUE = character(1L)
        ),
        state_reason = vapply(
            X = raw_issues,
            FUN = function(x) {
                null_to_default(x$state_reason, default = "open")
            },
            FUN.VALUE = character(1L)
        ),
        owner = structurel$owner,
        repo = structurel$repo
    )

    return(issues)
}

format_gitlab_issues <- function(
    raw_issues,
    verbose = TRUE
) {
    if (nrow(raw_issues) == 0) {
        return(new_issues())
    }
    structurel <- strsplit(raw_issues[["references.full"]], split = "/|#") |>
        lapply(\(x) {
            data.frame(
                owner = paste0(x[seq_len(length(x) - 2L)], collapse = "/"),
                repo = x[length(x) - 1L]
            )
        }) |>
        do.call(what = rbind)

    if (any(startsWith(colnames(raw_issues), "labels"))) {
        labels_list <- raw_issues[, startsWith(
            colnames(raw_issues),
            "labels"
        )] |>
            t() |>
            as.data.frame() |>
            lapply(FUN = function(x) {
                data.frame(
                    name = x[!is.na(x)],
                    color = rep(NA_character_, length(x[!is.na(x)]))
                )
            }) |>
            unname()
    } else {
        labels_list <- rep(
            x = list(data.frame(
                name = character(0L),
                color = character(0L),
                stringsAsFactors = FALSE
            )),
            times = nrow(raw_issues)
        )
    }
    issues <- new_issues(
        url = raw_issues[["_links.self"]],
        html_url = raw_issues[["web_url"]],
        title = raw_issues[["title"]],
        state = raw_issues[["state"]],
        body = raw_issues[["description"]],
        number = as.integer(raw_issues[["iid"]]),
        labels = labels_list,
        milestone = null_to_default(
            raw_issues[["milestone.title"]],
            default = NA_character_
        ),
        comments = format_comments(
            raw_comments = list(),
            urls = raw_issues[["_links.self"]]
        ),
        created_at = raw_issues[["created_at"]] |>
            strptime(format = "%Y-%m-%dT%H:%M:%S") |>
            format_timestamp(),
        closed_at = raw_issues[["closed_at"]] |>
            null_to_default(default = NA_character_) |>
            strptime(format = "%Y-%m-%dT%H:%M:%S") |>
            format_timestamp(),
        closed_by = null_to_default(
            raw_issues[["closed_by.username"]],
            default = NA_character_
        ),
        creator = raw_issues[["author.username"]],
        assignee = null_to_default(
            raw_issues[["assignee.username"]],
            default = NA_character_
        ),
        state_reason = NA_character_,
        owner = structurel[["owner"]],
        repo = structurel[["repo"]]
    )

    return(issues)
}

#' @rdname format
#' @noRd
format_labels <- function(raw_labels, verbose = TRUE) {
    if (verbose) {
        cat("Reading labels... ")
    }
    new_labels_structure <- lapply(
        X = raw_labels,
        FUN = base::`[`,
        c("name", "description", "color")
    ) |>
        lapply(FUN = \(label) {
            label$color <- paste0("#", label$color)
            label$description <- null_to_default(
                x = label$description,
                default = ""
            )
            return(as.data.frame(label))
        }) |>
        do.call(what = rbind)
    if (verbose) {
        cat("Done!\n", nrow(new_labels_structure), " labels found.\n", sep = "")
    }
    return(new_labels_structure)
}

#' @rdname format
#' @noRd
format_milestone <- function(raw_milestone, verbose = TRUE) {
    if (verbose) {
        cat("\t- ", raw_milestone[["title"]], "... Done!\n")
    }
    description <- null_to_default(
        x = raw_milestone[["description"]],
        default = ""
    )
    due_on <- format_timestamp(null_to_default(
        x = raw_milestone[["due_on"]],
        default = NA_real_
    ))
    closed_at <- format_timestamp(null_to_default(
        x = raw_milestone[["closed_at"]],
        default = NA_real_
    ))
    creator <- null_to_default(
        x = raw_milestone[["creator"]][["login"]],
        default = NA_character_
    )

    output <- data.frame(
        title = raw_milestone[["title"]],
        description = description,
        due_on = due_on,
        closed_at = closed_at,
        creator = creator,
        state = raw_milestone[["state"]],
        nb_issues_open = raw_milestone[["open_issues"]],
        nb_issues_closed = raw_milestone[["closed_issues"]]
    )
    return(output)
}

#' @rdname format
#' @noRd
format_milestones <- function(raw_milestones, verbose = TRUE) {
    if (verbose) {
        cat("Reading milestones... \n")
    }
    new_mlst_structure <- raw_milestones |>
        lapply(FUN = format_milestone, verbose = verbose) |>
        do.call(what = rbind) |>
        as.data.frame()
    if (verbose) {
        cat("Done!", nrow(new_mlst_structure), "milestones found.\n", sep = " ")
    }
    return(new_mlst_structure)
}
