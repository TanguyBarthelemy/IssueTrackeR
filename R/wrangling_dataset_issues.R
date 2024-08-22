
#' @title Retrieve the issues from github
#'
#' @param source a character string that is either \code{"online"} if you want
#' to fetch information from github or \code{"local"} if you want to fetch
#' information locally.
#' @param path_dataset A character string specifying the path which contains the
#' datasets (only used if source is \code{"local"}). Defaults to the package
#' option \code{IssueTrackeR.dataset.path}.
#' @param repo A character string specifying the GitHub repository name (only
#' used if source is \code{"online"}). Defaults to the package option
#' \code{IssueTrackeR.repo}.
#' @param username A character string specifying the GitHub username (only used
#' if source is \code{"online"}). Defaults to the package option
#' \code{IssueTrackeR.username}.
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
#' get_issues(source = "local")
#' }
#' get_issues(source = "online")
#'
get_issues <- function(source = c("local", "online"),
                       path_dataset = getOption("IssueTrackeR.dataset.path"),
                       repo = getOption("IssueTrackeR.repo"),
                       username = getOption("IssueTrackeR.username"),
                       ...) {
    source <- match.arg(source)

    if (source == "online") {
        raw_issues <- gh::gh(
            repo = repo,
            username = username,
            endpoint = "/repos/:username/:repo/issues",
            .limit = Inf
        )
        raw_comments <- gh::gh(
            repo = repo,
            username = username,
            endpoint = "/repos/:username/:repo/issues/comments",
            .limit = Inf
        )
        issues <- format_issues(raw_issues = raw_issues,
                                raw_comments = raw_comments, ...)
    } else if (source == "local") {
        path_dataset_issues <- file.path(path_dataset, "list_issues.yaml")
        if (file.exists(path_dataset_issues)) {
            issues <- yaml::read_yaml(file = path_dataset_issues)
            for (id_issue in seq_along(issues)) {
                issues[[id_issue]] <- new_issue(issue = issues[[id_issue]])
            }
            issues <- new_issues(issues)
        } else {
            stop("The file doesn't exist. Run `write_issues_to_dataset()`",
                 " to write a set of issues in the repo.")
        }
    } else {
        stop("wrong source")
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
#'     repo = "rjdemetra",
#'     username = "rjdverse",
#'     endpoint = "/repos/:username/:repo/issues",
#'     .limit = Inf
#' )
#' raw_comments <- gh::gh(
#'     repo = "rjdemetra",
#'     username = "rjdverse",
#'     endpoint = "/repos/:username/:repo/issues/comments",
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
        comments_nbr <- vapply(
            X = raw_comments,
            FUN = base::`[[`,
            "issue_url",
            FUN.VALUE = character(1L)
        ) |> aux()
    }

    if (verbose) {
        cat("Reading issues...\n")
    }
    issues <- new_issues()
    for (index in seq_along(raw_issues)) {
        if (verbose) {
            cat("Issue n\u00B0 ", index, "... Done!\n", sep = "")
        }
        raw_issue <- raw_issues[[index]]

        body_comment <- ifelse(
            test = missing(raw_comments)
            || all(comments_nbr != raw_issue[["number"]]),
            yes = "",
            no = paste0(
                comments_body[which(comments_nbr == raw_issue[["number"]])],
                collapse = "\n\nComment:\n"
            )
        )
        body <- paste0(raw_issue[["body"]],
                       body_comment,
                       collapse = "\n\nComment:\n")

        issue <- new_issue(
            title = raw_issue[["title"]],
            body = body,
            number = raw_issue[["number"]],
            created_at = raw_issue[["created_at"]],
            labels = vapply(
                X = raw_issue[["labels"]],
                FUN = `[[`, ... = "name",
                FUN.VALUE = character(1L)
            ),
            milestone = raw_issue[["milestone"]][["title"]]
        )
        issues[[index]] <- issue
    }
    if (verbose) {
        cat(length(issue), " issues found.\n", sep = "")
    }
    return(issues)
}

#' @title Save issue dataset in a yaml format
#'
#' @param issues a \code{IssuesTB} object.
#' @param source a character string that is either \code{"online"} (by default)
#' if you want to fetch information from github or \code{"local"} if you want to
#' fetch information locally.
#' @param path_dataset A character string specifying the path which will contain
#' the datasets. Defaults to the package option
#' \code{IssueTrackeR.dataset.path}.
#' @param ... Additional arguments for the function \code{get_issues}
#'
#' @returns invisibly (with \code{invisible()}) \code{TRUE} if the export was
#' successful and an error otherwise.
#' @export
#'
#' @examples
#' # With issues
#' all_issues <- get_issues(source = "online", verbose = FALSE)
#' write_issues_to_dataset(all_issues)
#'
#' # Without issues
#' write_issues_to_dataset(source = "online")
#'
#' @rdname write_issues_to_dataset
#'
write_issues_to_dataset <- function(
        issues,
        source = c("local", "online"),
        ...) {
    if (missing(issues) || is.null(issues)) {
        source <- match.arg(source)
        issues <- get_issues(source = source, ...)
        return(write_issues_to_dataset(issues = issues, ...))
    }
    UseMethod(generic = "write_issues_to_dataset", object = issues)
}

#' @rdname write_issues_to_dataset
#' @exportS3Method write_issues_to_dataset IssuesTB
#' @method write_issues_to_dataset IssuesTB
#' @export
write_issues_to_dataset.IssuesTB <- function(
        issues,
        source,
        path_dataset = getOption("IssueTrackeR.dataset.path"),
        ...) {
    if (!dir.exists(path_dataset)) {
        dir.create(path_dataset)
    }
    path_dataset_issues <- file.path(path_dataset, "list_issues.yaml")
    yaml::write_yaml(x = issues, file = path_dataset_issues)
    return(invisible(TRUE))
}

#' @rdname write_issues_to_dataset
#' @exportS3Method write_issues_to_dataset default
#' @method write_issues_to_dataset default
#' @export
write_issues_to_dataset.default <- function(issues, source, ...) {
    stop("This function requires a IssuesTB object.")
}
