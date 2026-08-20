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
#' - `format_issues`: A list of IssuesTB objects with complete issue data.
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
#' formatted_issues <- IssueTrackeR:::format_issues(raw_issues = raw_issues,
#'                             raw_comments = raw_comments,
#'                             verbose = FALSE)
#' }
#'
#' @name format
#' @noRd
#'
NULL
