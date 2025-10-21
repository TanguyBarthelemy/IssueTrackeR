#' @title Retrieve information from the issues of GitHub
#'
#' @description
#' use \code{\link[gh]{gh}} to ask the API of GitHub and et a list of issues
#' with their labels and milestones.
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
#' Defaults to \code{"open_issues.yaml"}.
#' @param repo A character string specifying the GitHub repository name (only
#' taken into account if \code{source} is set to \code{"online"}).
#' Defaults to the package option \code{IssueTrackeR.repo}.
#' @param owner A character string specifying the GitHub owner (only taken
#' into account if \code{source} is set to \code{"online"}).
#' Defaults to the package option \code{IssueTrackeR.owner}.
#' @param state a character string that is either \code{"open"} (by default) if
#' you want to fetch only open issues from GitHub, \code{"closed"} if you want
#' to fetch only closed issues from GitHub or \code{"all"} if you want to fetch
#' all issues from GitHub (closed and open).
#' Only taken into account if \code{source} is set to \code{"online"}.
#' @param verbose A logical value indicating whether to print additional
#' information. Default is \code{TRUE}.
#'
#' @details
#' The functions of get type are useful to retrieve object related to issues
#' from GitHub. So it's possible to retrieve issues, labels and milestones.
#'
#' The defaults value for the argument \code{dataset_name} depends on the
#' function:
#' * defaults is \code{"list_issues.yaml"} for \code{get_issues()}
#' * defaults is \code{"list_milestones.yaml"} for \code{get_milestones()}
#' * defaults is \code{"list_labels.yaml"} for \code{get_labels()}
#'
#' @returns
#' The function \code{get_issues} returns an object of class \code{IssuesTB}. It
#' is a list composed by object of class \code{IssueTB}. An object of class
#' \code{IssueTB} represents an issue with simpler structure (with number,
#' title, body and labels).
#'
#' The function \code{get_labels} returns a list representing labels with
#' simpler structure (with name, description, colour).
#'
#' The function \code{get_milestones} returns a list representing milestones
#' with simpler structure (with title, description and due_on).
#'
#' @export
#'
#' @rdname get
#'
#' @examples
#'
#' \donttest{
#' # From online
#'
#' issues <- get_issues(source = "online", owner = "rjdverse", repo = NULL)
#' issues <- get_issues(source = "online")
#' print(issues)
#'
#' labels <- get_labels(source = "online")
#' print(labels)
#'
#' milestones <- get_milestones(source = "online")
#' print(milestones)
#' }
#'
#' # From local
#'
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' milestones <- get_milestones(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "list_milestones.yaml"
#' )
#' labels <- get_labels(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "list_labels.yaml"
#' )
get_issues <- function(
    source = c("local", "online"),
    dataset_dir = getOption("IssueTrackeR.dataset.dir"),
    dataset_name = "open_issues.yaml",
    repo = getOption("IssueTrackeR.repo"),
    owner = getOption("IssueTrackeR.owner"),
    state = c("open", "closed", "all"),
    verbose = TRUE
) {
    source <- match.arg(source)
    state <- match.arg(state)

    if (source == "online") {
        if (is.null(repo)) {
            if (verbose) {
                cat("Try to find all repositories...")
            }
            list_repo <- get_all_repos(owner)
            if (verbose) {
                cat(" Done!\n")
            }

            issues <- lapply(
                X = list_repo,
                FUN = get_issues,
                source = "online",
                owner = owner,
                state = state,
                verbose = verbose,
                dataset_dir = NULL,
                dataset_name = NULL
            ) |>
                do.call(what = rbind)

            return(issues)
        }

        if (verbose) {
            cat("Repo:", repo, " owner:", owner, "\n")
        }
        raw_issues <- try(expr = {
            gh::gh(
                repo = repo,
                owner = owner,
                endpoint = "/repos/:owner/:repo/issues",
                state = state,
                .limit = Inf
            )
        })
        check_response(raw_issues)

        raw_issues <- raw_issues |>
            Filter(f = function(.x) is.null(.x$pull_request))

        raw_comments <- try(expr = {
            gh::gh(
                repo = repo,
                owner = owner,
                endpoint = "/repos/:owner/:repo/issues/comments",
                .limit = Inf
            )
        })
        check_response(raw_comments)

        issues <- format_issues(
            raw_issues = raw_issues,
            raw_comments = raw_comments,
            verbose = verbose
        )
    } else if (source == "local") {
        if (verbose) {
            cat("Looking into", dataset_name, "...\n")
        }
        if (tools::file_ext(dataset_name) == "yaml") {
            input_file <- tools::file_path_sans_ext(dataset_name)
        }
        input_path <- file.path(dataset_dir, input_file) |>
            paste0(".yaml") |>
            normalizePath(mustWork = TRUE)

        if (verbose) {
            message("The issues will be read from ", input_path, ".")
        }

        raw_yaml <- readLines(con = input_path, encoding = "UTF-8")
        raw_yaml <- yaml::yaml.load(raw_yaml)

        raw_yaml$comments <- lapply(
            X = raw_yaml$comments,
            FUN = function(comments) {
                if (length(comments$text) == 0L) {
                    return(data.frame(
                        text = character(0L),
                        author = character(0L),
                        stringsAsFactors = FALSE
                    ))
                }
                return(data.frame(comments))
            }
        )

        raw_yaml$labels <- lapply(
            X = raw_yaml$labels,
            FUN = function(lbls) {
                if (length(lbls$name) == 0L) {
                    return(data.frame(
                        name = character(0L),
                        color = character(0L),
                        stringsAsFactors = FALSE
                    ))
                }
                return(data.frame(lbls))
            }
        )

        issues <- do.call(
            args = raw_yaml,
            what = new_issues
        )
    } else {
        stop("wrong source", call. = FALSE)
    }

    return(issues)
}

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

#' @title Format the issue in a simpler format
#'
#' @param raw_issues a \code{gh_response} object output from the function
#' \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#' issues.
#' @param raw_comments a \code{gh_response} object output from the function
#' \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#' comments.
#' @inheritParams get_issues
#'
#' @returns a list representing an issue with simpler structure (with number,
#' title, body and labels) of all issues.
#' @export
#'
#' @examples
#'
#' \donttest{
#' raw_issues <- gh::gh(
#'     repo = "rjdemetra",
#'     owner = "rjdverse",
#'     endpoint = "/repos/:owner/:repo/issues",
#'     .limit = Inf
#' )
#' raw_comments <- gh::gh(
#'     repo = "rjdemetra",
#'     owner = "rjdverse",
#'     endpoint = "/repos/:owner/:repo/issues/comments",
#'     .limit = Inf
#' )
#' all_issues <- format_issues(raw_issues = raw_issues,
#'                             raw_comments = raw_comments,
#'                             verbose = FALSE)
#' }
#'
format_issues <- function(
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
            FUN = function(x) if (is.null(x$body)) "" else x$body,
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
                if (is.null(x$milestone)) NA_character_ else x$milestone$title
            },
            FUN.VALUE = character(1L)
        ),
        comments = format_comments(raw_comments = raw_comments, urls = urls),
        created_at = vapply(
            X = raw_issues,
            FUN = `[[`,
            "created_at",
            FUN.VALUE = character(1L)
        ),
        closed_at = vapply(
            X = raw_issues,
            FUN = function(x) {
                if (is.null(x$closed_at)) NA_character_ else x$closed_at
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
                if (is.null(x$assignee)) NA_character_ else x$assignee$login
            },
            FUN.VALUE = character(1L)
        ),
        state_reason = vapply(
            X = raw_issues,
            FUN = function(x) {
                ifelse(
                    test = is.null(x$state_reason),
                    yes = "open",
                    no = x$state_reason
                )
            },
            FUN.VALUE = character(1L)
        ),
        owner = structurel$owner,
        repo = structurel$repo
    )

    return(issues)
}

#' @title Save datasets in a yaml file
#'
#' @param issues a \code{IssuesTB} object.
#' @param labels a list representing all labels with simpler structure (with
#' name, description, colour)
#' @param milestones a list representing milestones with simpler structure (with
#' title, description and due_on).
#' @inheritParams get_issues
#' @param \dots Unused parameter.
#'
#' @details
#' Depending on the object, the defaults value of the argument
#' \code{dataset_name} is:
#'
#' \itemize{
#' \item \code{"list_issues.yaml"} for issues;
#' \item \code{"list_labels.yaml"} for labels;
#' \item \code{"list_milestones.yaml"} for milestones.
#' }
#'
#' @returns invisibly (with \code{invisible()}) \code{TRUE} if the export was
#' successful and an error otherwise.
#' @export
#'
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' milestones <- get_milestones(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "list_milestones.yaml"
#' )
#' labels <- get_labels(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "list_labels.yaml"
#' )
#'
#' write_issues_to_dataset(issues)
#' write_labels_to_dataset(labels)
#' write_milestones_to_dataset(milestones)
#'
#' @rdname write
#'
write_issues_to_dataset <- function(issues, ...) {
    UseMethod(generic = "write_issues_to_dataset", object = issues)
}

#' @rdname write
#' @exportS3Method write_issues_to_dataset IssuesTB
#' @method write_issues_to_dataset IssuesTB
#' @export
write_issues_to_dataset.IssuesTB <- function(
    issues,
    dataset_dir = getOption("IssueTrackeR.dataset.dir"),
    dataset_name = "list_issues.yaml",
    verbose = TRUE,
    ...
) {
    if (tools::file_ext(dataset_name) == "yaml") {
        output_file <- tools::file_path_sans_ext(dataset_name)
    }
    output_path <- file.path(dataset_dir, output_file) |>
        paste0(".yaml") |>
        normalizePath(mustWork = FALSE)

    if (verbose) {
        message("The datasets will be exported to ", output_path, ".")
        if (file.exists(output_path)) {
            message("The file already exists and will be overwritten.")
        }
    }

    if (!dir.exists(dataset_dir)) {
        dir.create(dataset_dir)
    }
    issues_yaml <- yaml::as.yaml(issues)
    writeLines(text = enc2utf8(issues_yaml), con = output_path, useBytes = TRUE)
    return(invisible(TRUE))
}

#' @rdname write
#' @exportS3Method write_issues_to_dataset default
#' @method write_issues_to_dataset default
#' @export
write_issues_to_dataset.default <- function(issues, ...) {
    stop("This function requires a IssuesTB object.", call. = FALSE)
}
