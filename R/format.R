#' @title Round a timestamp to the inferior integer
#'
#' @description
#' This function round a timestamp ()
#'
#' @param x The timestamp. See detail section for more information.
#'
#' @details
#' The accepted formats for the argument \code{x} are: color
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
#' @returns a \code{POSIXct} object with rounded \code{double} value.
#'
#' @keywords internal
#' @dev
#'
#' @examples
#'
#' format_timestamp(1743694674.9)
#' format_timestamp(Sys.Date())
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
#' @param urls A character vector of issue URLs for which comments should be
#'   formatted.
#' @inheritParams get_issues
#'
#' @returns
#' - `format_labels`: A data frame with columns: name, description, color.
#' - `format_comments`: A list of data frames with columns: text, author.
#' - `format_issues`: A list of IssuesTB objects with complete issue data.
#' - `format_milestone`: A data frame with milestone information.
#'
#' @examples
#' \dontrun{
#' # Get data from GitHub API
#' raw_labels <- gh::gh("/repos/owner/repo/labels")
#' raw_issues <- gh::gh("/repos/owner/repo/issues")
#' raw_comments <- gh::gh("/repos/owner/repo/issues/comments")
#' raw_milestone <- gh::gh("/repos/owner/repo/milestones/1")
#'
#' # Format the data
#' formatted_labels <- format_labels(raw_labels)
#' formatted_comments <- format_comments(raw_comments, urls)
#' formatted_issues <- format_issues(raw_issues, raw_comments)
#' formatted_milestone <- format_milestone(raw_milestone)
#' }
#'
#' @name format
#' @noRd
#'
NULL
