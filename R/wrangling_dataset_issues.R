
#' @title Retrieve the issues from github
#'
#' @param type a character string that is either \code{"online"} if you want to
#' fetch information from github or \code{"local"} if you want to fetch
#' information locally.
#' @param ... Additional arguments for the function \code{format_issues}
#'
#' @returns
#' The function returns an object of class \code{IssuesTB}. It is a list
#' composed by object of class \code{IssueTB}. An object of class \code{IssueTB}
#' represents an issue with simpler structure (with number, title, body and
#' labels).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_issues()
#' get_issues(type = "local")
#' }
#' get_issues(type = "online")
#'
get_issues <- function(type = c("local", "online"), ...) {
    type <- match.arg(type)

    if (type == "online") {
        raw_issues <- gh::gh(
            repo = "TODO",
            endpoint = "/repos/TanguyBarthelemy/:repo/issues",
            .limit = Inf
        )
        raw_comments <- gh::gh(
            repo = "TODO",
            endpoint = "/repos/TanguyBarthelemy/:repo/issues/comments",
            .limit = Inf
        )
        issues <- format_issues(raw_issues = raw_issues, raw_comments = raw_comments, ...)
    } else if (type == "local") {
        if (file.exists(path_dataset_issues)) {
            issues <- yaml::read_yaml(file = path_dataset_issues)
            for (id_issue in seq_along(issues)) {
                issues[[id_issue]][["created_at"]]  <- as.POSIXct(issues[[id_issue]][["created_at"]])
                class(issues[[id_issue]]) <- "IssueTB"
            }
            class(issues) <- "IssuesTB"
        } else {
            stop("The file doesn't exist. Run `write_issues_to_dataset()`",
                 " to write a set of issues in the repo.")
        }
    } else {
        stop("wrong type")
    }

    return(issues)
}

#' @title Format the issue in a simpler format
#'
#' @param raw_issues a \code{gh_response} object output from the function
#' \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#' issues.
#' @param raw_comments a \code{gh_response} object output from the function
#' \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#' comments.
#' @param verbose A logical value indicating whether to print additional
#' information. Default is \code{TRUE}.
#'
#' @returns a list representing an issue with simpler structure (with number,
#' title, body and labels) of all issues.
#' @export
#'
#' @examples
#'
#' raw_issues <- gh::gh(
#'     repo = "TODO",
#'     endpoint = "/repos/TanguyBarthelemy/:repo/issues",
#'     .limit = Inf
#' )
#' raw_comments <- gh::gh(
#'     repo = "TODO",
#'     endpoint = "/repos/TanguyBarthelemy/:repo/issues/comments",
#'     .limit = Inf
#' )
#' all_issues <- format_issues(raw_issues = raw_issues,
#'                             raw_comments = raw_comments,
#'                             verbose = FALSE)
#'
format_issues <- function(raw_issues,
                          raw_comments,
                          verbose = TRUE) {

    if (!missing(raw_comments)) {
        comments_body <- vapply(
            X = raw_comments,
            FUN = base::`[[`,
            "body",
            FUN.VALUE = character(1L)
        )
        aux <- function(text) {
            numbers <- gregexpr("\\d+$", text)
            matches <- regmatches(text, numbers) |> unlist()
            as.integer(matches)
        }
        comments_issue_nbr <- vapply(
            X = raw_comments,
            FUN = base::`[[`,
            "issue_url",
            FUN.VALUE = character(1L)
        ) |> aux()
    }

    new_issues_structure <- list()
    for (index in seq_along(raw_issues)) {
        if (verbose) {
            cat("Issue n\u00B0", index, "\n")
        }
        issue <- raw_issues[[index]]

        body_comment <- ifelse(
            test = missing(raw_comments)
            || all(comments_issue_nbr != issue[["number"]]),
            yes = "",
            no = paste0(
                comments_body[which(comments_issue_nbr == issue[["number"]])],
                collapse = "\n\nComment:\n"
            )
        )
        body <- paste0(issue[["body"]],
                       body_comment,
                       collapse = "\n\nComment:\n")

        new_issue <- structure(
            list(
                title = issue[["title"]],
                body = body,
                number = as.integer(issue[["number"]]),
                created_at = issue[["created_at"]] |>
                    as.POSIXct() |>
                    as.integer() |>
                    as.POSIXct(),
                labels = vapply(
                    X = issue[["labels"]],
                    FUN = `[[`, ... = "name",
                    FUN.VALUE = character(1L)
                ),
                milestone = issue[["milestone"]][["title"]]
            ),
            class = "IssueTB"
        )
        new_issues_structure[[index]] <- new_issue
    }
    class(new_issues_structure) <- "IssuesTB"

    return(new_issues_structure)
}

#' @title Save issue dataset in a yaml format
#'
#' @param issues a \code{IssuesTB} object.
#' @param type a character string that is either \code{"online"} (by default) if
#' you want to fetch information from github or \code{"local"} if you want to
#' fetch information locally.
#' @param ... Additional arguments for the function \code{get_issues}
#'
#' @returns invisibly (with \code{invisible()}) \code{TRUE} if the export was
#' successful and an error otherwise.
#' @export
#'
#' @examples
#' # With issues
#' all_issues <- get_issues(type = "online", verbose = FALSE)
#' write_issues_to_dataset(all_issues)
#'
#' # Without issues
#' write_issues_to_dataset(type = "online")
#'
write_issues_to_dataset <- function(issues, type = c("local", "online"), ...) {
    if (!dir.exists("data")) {
        dir.create("data")
    }
    if (missing(issues) || is.null(issues)) {
        type <- match.arg(type)
        issues <- get_issues(type = type, ...)
        return(write_issues_to_dataset(issues = issues, ...))
    }
    UseMethod(generic = "write_issues_to_dataset", object = issues)
}

#' @exportS3Method write_issues_to_dataset IssuesTB
#' @method write_issues_to_dataset IssuesTB
#' @export
write_issues_to_dataset.IssuesTB <- function(issues, type = c("local", "online"), ...) {
    type <- match.arg(type)
    yaml::write_yaml(x = issues, file = path_dataset_issues)
    return(invisible(TRUE))
}

#' @exportS3Method write_issues_to_dataset default
#' @method write_issues_to_dataset default
#' @export
write_issues_to_dataset.default <- function(issues, type,...) {
    stop("This function requires a IssuesTB object.")
}
