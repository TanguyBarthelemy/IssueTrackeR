
#' @title Retrieve the issues from GitHub
#'
#' @param source a character string that is either \code{"online"} if you want
#' to fetch information from GitHub or \code{"local"} (by default) if you want
#' to fetch information locally.
#' @param dataset_dir A character string specifying the path which contains the
#' datasets (only taken into account if \code{source} is set to \code{"local"}).
#' Defaults to the package option \code{IssueTrackeR.dataset.dir}.
#' @param dataset_name A character string specifying the name of the datasets
#' which will be written (only taken into account if \code{source} is set to
#' \code{"local"}).
#' Defaults to \code{"list_issues.yaml"}.
#' @param repo A character string specifying the GitHub repository name (only
#' taken into account if \code{source} is set to \code{"online"}).
#' Defaults to the package option \code{IssueTrackeR.repo}.
#' @param username A character string specifying the GitHub username (only taken
#' into account if \code{source} is set to \code{"online"}).
#' Defaults to the package option \code{IssueTrackeR.username}.
#' @param state a character string that is either \code{"open"} (by default) if
#' you want to fetch only open issues from GitHub, \code{"closed"} if you want
#' to fetch only closed issues from GitHub or \code{"all"} if you want to fetch
#' all issues from GitHub (closed and open).
#' Only taken into account if \code{source} is set to \code{"online"}.
#' @param verbose A logical value indicating whether to print additional
#' information. Default is \code{TRUE}.
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
                       dataset_dir = getOption("IssueTrackeR.dataset.dir"),
                       dataset_name = "list_issues.yaml",
                       repo = getOption("IssueTrackeR.repo"),
                       username = getOption("IssueTrackeR.username"),
                       state = c("open", "closed", "all"),
                       verbose = TRUE) {

    source <- match.arg(source)
    state <- match.arg(state)

    if (source == "online") {
        raw_issues <- gh::gh(
            repo = repo,
            username = username,
            endpoint = "/repos/:username/:repo/issues",
            state = state,
            .limit = Inf
        )
        raw_comments <- gh::gh(
            repo = repo,
            username = username,
            endpoint = "/repos/:username/:repo/issues/comments",
            .limit = Inf
        )
        issues <- format_issues(raw_issues = raw_issues,
                                raw_comments = raw_comments,
                                repo = repo,
                                username = username,
                                verbose = verbose)
    } else if (source == "local") {

        input_file <- tools::file_path_sans_ext(dataset_name)
        input_path <- file.path(dataset_dir, input_file) |>
            normalizePath(mustWork = FALSE) |>
            paste0(... = _, ".yaml")

        if (file.exists(input_path)) {
            if (verbose) {
                message("The issues will be read from ", input_path, ".")
            }
            issues <- yaml::read_yaml(file = input_path)
            for (id_issue in seq_along(issues)) {
                issues[[id_issue]] <- new_issue(issue = issues[[id_issue]])
            }
            issues <- new_issues(issues)
        } else {
            stop(
                "The file doesn't exist. Run `write_issues_to_dataset()`",
                " to write a set of issues in the repo.\n",
                "Or call get_issues() with the argument `source` to \"online\"."
            )
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
#' @param repo A character string specifying the GitHub repository name (only
#' used if source is \code{"online"}). Defaults to the package option
#' \code{IssueTrackeR.repo}.
#' @param username A character string specifying the GitHub username (only used
#' if source is \code{"online"}). Defaults to the package option
#' \code{IssueTrackeR.username}.
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
                          repo = getOption("IssueTrackeR.repo"),
                          username = getOption("IssueTrackeR.username"),
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
                "\n\nComment:\n",
                comments_body[which(comments_nbr == raw_issue[["number"]])],
                collapse = ""
            )
        )
        body <- paste(raw_issue[["body"]],
                      body_comment)

        issue <- new_issue(
            title = raw_issue[["title"]],
            state = raw_issue[["state"]],
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
#' @param dataset_dir A character string specifying the path to the directory
#' which will contain the datasets. Defaults to the package option
#' \code{IssueTrackeR.dataset.dir}.
#' @param dataset_name A named character string specifying the name of the
#' datasets which will be written. Defaults to \code{"list_issues.yaml"}.
#' @param \dots Unused argument.
#'
#' @returns invisibly (with \code{invisible()}) \code{TRUE} if the export was
#' successful and an error otherwise.
#' @export
#'
#' @examples
#'
#' all_issues <- get_issues(source = "online", verbose = FALSE)
#' write_issues_to_dataset(all_issues)
#'
#' @rdname write_issues_to_dataset
#'
write_issues_to_dataset <- function(issues, ...) {
    UseMethod(generic = "write_issues_to_dataset", object = issues)
}

#' @rdname write_issues_to_dataset
#' @exportS3Method write_issues_to_dataset IssuesTB
#' @method write_issues_to_dataset IssuesTB
#' @export
write_issues_to_dataset.IssuesTB <- function(
        issues,
        dataset_dir = getOption("IssueTrackeR.dataset.dir"),
        dataset_name = "list_issues.yaml",
        verbose = TRUE,
        ...) {

    output_file <- tools::file_path_sans_ext(dataset_name)
    output_path <- file.path(dataset_dir, output_file) |>
        normalizePath(mustWork = FALSE) |>
        paste0(... = _, ".yaml")

    if (verbose) {
        message("The datasets will be exported to ", output_path, ".")
        if (file.exists(output_path)) {
            message("The file already exists and will be overwritten.")
        }
    }

    if (!dir.exists(dataset_dir)) {
        dir.create(dataset_dir)
    }
    yaml::write_yaml(x = issues, file = output_path)
    return(invisible(TRUE))
}

#' @rdname write_issues_to_dataset
#' @exportS3Method write_issues_to_dataset default
#' @method write_issues_to_dataset default
#' @export
write_issues_to_dataset.default <- function(issues, ...) {
    stop("This function requires a IssuesTB object.")
}
