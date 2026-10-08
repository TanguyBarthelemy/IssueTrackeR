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
#'   \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#'   issues.
#' @param raw_comments a \code{gh_response} object output from the function
#'   \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#'   comments.
#' @param raw_labels a \code{gh_response} object output from the function
#'   \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#'   labels.
#' @param raw_milestone Raw milestone. Subset of a \code{gh_response} object
#'   output from the function \code{\link[gh]{gh}} which contains all the data
#'   and metadata for a GitHub milestone.
#' @param raw_milestones a \code{gh_response} object output from the function
#'   \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#'   milestones.
#' @param urls A character vector of issue HTML URLs for which comments should
#'   be formatted.
#'
#' @returns
#' - `format_labels_github`: A data frame with columns: `name`, `description`,
#'   `color`.
#' - `format_comments_github`: A list of data frames with columns: `text`,
#'   `author`.
#' - `format_issues_github`: A list of IssuesTB objects with complete issue
#'   data.
#' - `format_milestone_github`: A data frame with milestone information.
#' - `format_milestones_github`: A list representing milestones with `title`,
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
#' IssueTrackeR:::format_labels_github(raw_labels)
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
#' IssueTrackeR:::format_milestone_github(raw_milestone)
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
#' IssueTrackeR:::format_milestones_github(milestones_jdplus_main)
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
#' formatted_comments <- IssueTrackeR:::format_comments_github(
#'     raw_comments,
#'     urls
#' )
#'
#' formatted_issues <- IssueTrackeR:::format_issues_github(
#'     raw_issues = raw_issues,
#'     raw_comments = raw_comments,
#'     verbose = FALSE
#' )
#' }
#'
#' @name format
#' @noRd
#'
NULL

#' @rdname format
#' @noRd
format_comments_github <- function(
    raw_comments,
    verbose = TRUE
) {
    comments_number <- vapply(
        X = raw_comments,
        FUN = `[[`,
        "issue_url",
        FUN.VALUE = character(1L)
    ) |>
        sub(pattern = ".*/", replacement = "") |>
        as.integer()
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
    comments_list <- data.frame(
        number = comments_number,
        text = comments_bodies,
        author = comments_author
    )

    return(comments_list)
}

generate_empty_comments_list <- function(issues_number) {
    comments_list <- rep(
        x = list(data.frame(
            text = character(0L),
            author = character(0L),
            stringsAsFactors = FALSE
        )),
        times = length(issues_number)
    )
    names(comments_list) <- issues_number
    return(comments_list)
}

extract_labels_from_raw_issue_github <- function(raw_issue) {
    raw_labels <- raw_issue[["labels"]]

    if (length(raw_labels) == 0L) {
        list_labels <- data.frame(
            name = character(0L),
            color = character(0L),
            stringsAsFactors = FALSE
        )
    } else {
        list_labels <- data.frame(
            name = vapply(
                X = raw_labels,
                FUN = "[[",
                "name",
                FUN.VALUE = character(1L)
            ),
            color = paste0(
                "#",
                vapply(
                    X = raw_labels,
                    FUN = "[[",
                    "color",
                    FUN.VALUE = character(1L)
                )
            ),
            stringsAsFactors = FALSE
        )
    }
    return(list_labels)
}

extract_labels_from_raw_issues_gitlab <- function(raw_issues) {
    if (any(startsWith(colnames(raw_issues), "labels"))) {
        list_labels <- raw_issues[, startsWith(
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
        list_labels <- rep(
            x = list(data.frame(
                name = character(0L),
                color = character(0L),
                stringsAsFactors = FALSE
            )),
            times = nrow(raw_issues)
        )
    }
    return(list_labels)
}

extract_url_from_raw_issue_github <- function(raw_issue) {
    output <- null_to_default(
        x = raw_issue[["url"]],
        default = NA_character_
    )
    return(output)
}

extract_state_from_raw_issue_github <- function(raw_issue) {
    output <- null_to_default(
        x = raw_issue[["state"]],
        default = NA_character_
    )
    return(output)
}

extract_title_from_raw_issue_github <- function(raw_issue) {
    output <- null_to_default(
        x = raw_issue[["title"]],
        default = NA_character_
    )
    return(output)
}

extract_body_from_raw_issue_github <- function(raw_issue) {
    output <- null_to_default(
        x = raw_issue[["body"]],
        default = NA_character_
    )
    return(output)
}

extract_number_from_raw_issue_github <- function(raw_issue) {
    output <- null_to_default(
        x = raw_issue[["number"]],
        default = NA_integer_
    )
    return(output)
}

extract_html_url_from_raw_issue_github <- function(raw_issue) {
    output <- null_to_default(
        x = raw_issue[["html_url"]],
        default = NA_character_
    )
    return(output)
}

extract_milestone_from_raw_issue_github <- function(raw_issue) {
    output <- null_to_default(
        x = raw_issue[["milestone"]][["title"]],
        default = NA_character_
    )
    return(output)
}

extract_state_reason_from_raw_issue_github <- function(raw_issue) {
    output <- null_to_default(
        x = raw_issue[["state_reason"]],
        default = NA_character_
    )
    return(output)
}

extract_created_at_from_raw_issue_github <- function(raw_issue) {
    output <- raw_issue[["created_at"]] |>
        null_to_default(default = NA_real_) |>
        strptime(format = "%Y-%m-%dT%H:%M:%S") |>
        format_timestamp()
    return(output)
}

extract_closed_at_from_raw_issue_github <- function(raw_issue) {
    output <- raw_issue[["closed_at"]] |>
        null_to_default(default = NA_real_) |>
        strptime(format = "%Y-%m-%dT%H:%M:%S") |>
        format_timestamp()
    return(output)
}

extract_closed_by_from_raw_issue_github <- function(raw_issue) {
    output <- null_to_default(
        x = raw_issue[["closed_by"]][["login"]],
        default = NA_character_
    )
    return(output)
}

extract_creator_from_raw_issue_github <- function(raw_issue) {
    output <- null_to_default(
        x = raw_issue[["creator"]][["login"]],
        default = NA_character_
    )
    return(output)
}

extract_assignee_from_raw_issue_github <- function(raw_issue) {
    output <- null_to_default(
        x = raw_issue[["assignee"]][["login"]],
        default = NA_character_
    )
    return(output)
}

#' @rdname format
#' @noRd
format_issue_github <- function(
    raw_issue,
    comments,
    verbose = TRUE
) {
    structurel <- utils::strcapture(
        "^https://api.github.com/repos/([^/]+)/([^/]+)/issues/\\d+$",
        raw_issue[["url"]],
        proto = data.frame(
            owner = character(),
            repo = character(),
            stringsAsFactors = FALSE
        )
    )
    issue_number <- extract_number_from_raw_issue_github(raw_issue)
    comments_n <- comments[comments$number == issue_number, ]
    comments_n$number <- NULL

    issues <- new_issue(
        url = extract_url_from_raw_issue_github(raw_issue),
        html_url = extract_html_url_from_raw_issue_github(raw_issue),
        title = extract_title_from_raw_issue_github(raw_issue),
        state = extract_state_from_raw_issue_github(raw_issue),
        body = extract_body_from_raw_issue_github(raw_issue),
        number = issue_number,
        labels = extract_labels_from_raw_issue_github(raw_issue),
        milestone = extract_milestone_from_raw_issue_github(raw_issue),
        comments = comments_n,
        created_at = extract_created_at_from_raw_issue_github(raw_issue),
        closed_at = extract_closed_at_from_raw_issue_github(raw_issue),
        closed_by = extract_closed_by_from_raw_issue_github(raw_issue),
        creator = extract_creator_from_raw_issue_github(raw_issue),
        assignee = extract_assignee_from_raw_issue_github(raw_issue),
        state_reason = extract_state_reason_from_raw_issue_github(raw_issue),
        owner = structurel$owner,
        repo = structurel$repo
    )

    return(issues)
}

#' @rdname format
#' @noRd
format_issues_github <- function(
    raw_issues,
    raw_comments,
    verbose = TRUE
) {
    comments <- format_comments_github(
        raw_comments = raw_comments,
        verbose = verbose
    )

    issues <- lapply(
        X = raw_issues,
        FUN = format_issue_github,
        comments = comments
    ) |>
        do.call(what = rbind)

    return(issues)
}

format_issues_gitlab <- function(
    raw_issues,
    verbose = TRUE
) {
    if (nrow(raw_issues) == 0L) {
        return(new_issues())
    }
    structurel <- strsplit(raw_issues[["references.full"]], split = "/|#") |>
        lapply(function(x) {
            data.frame(
                owner = paste(x[seq_len(length(x) - 2L)], collapse = "/"),
                repo = x[length(x) - 1L],
                stringsAsFactors = FALSE
            )
        }) |>
        do.call(what = rbind)

    issues_number <- as.integer(raw_issues[["iid"]])
    comments_list <- generate_empty_comments_list(issues_number = issues_number)
    list_labels <- extract_labels_from_raw_issues_gitlab(raw_issues)
    created_at <- raw_issues[["created_at"]] |>
        strptime(format = "%Y-%m-%dT%H:%M:%S") |>
        format_timestamp()
    closed_at <- raw_issues[["closed_at"]] |>
        null_to_default(default = NA_character_) |>
        strptime(format = "%Y-%m-%dT%H:%M:%S") |>
        format_timestamp()

    issues <- new_issues(
        url = raw_issues[["_links.self"]],
        html_url = raw_issues[["web_url"]],
        title = raw_issues[["title"]],
        state = raw_issues[["state"]],
        body = raw_issues[["description"]],
        number = issues_number,
        labels = list_labels,
        milestone = null_to_default(
            raw_issues[["milestone.title"]],
            default = NA_character_
        ),
        comments = comments_list,
        created_at = created_at,
        closed_at = closed_at,
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
format_labels_github <- function(raw_labels, verbose = TRUE) {
    if (verbose) {
        cat("Reading labels... ")
    }
    new_labels_structure <- lapply(
        X = raw_labels,
        FUN = base::`[`,
        c("name", "description", "color")
    ) |>
        lapply(FUN = function(label) {
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
format_milestone_github <- function(raw_milestone, verbose = TRUE) {
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
        source = "GitHub",
        title = raw_milestone[["title"]],
        description = description,
        due_on = due_on,
        closed_at = closed_at,
        creator = creator,
        state = raw_milestone[["state"]],
        nb_issues_open = raw_milestone[["open_issues"]],
        nb_issues_closed = raw_milestone[["closed_issues"]],
        url = raw_milestone[["html_url"]],
        stringsAsFactors = FALSE
    )

    return(output)
}

#' @rdname format
#' @noRd
format_milestones_github <- function(raw_milestones, verbose = TRUE) {
    if (verbose) {
        cat("Reading milestones... \n")
    }
    milestones <- raw_milestones |>
        lapply(FUN = format_milestone_github, verbose = verbose) |>
        do.call(what = rbind) |>
        as.data.frame()

    return(milestones)
}
