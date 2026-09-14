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
#' @param verbose A boolean indicating whether to print additional
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
#' @name get
#'
#' @examplesIf gh::gh_token_exists() && gh::gh_rate_limit()$remaining > 0
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
#' }
#'
NULL

#' @export
get_issues <- function(selector, verbose = TRUE, ...) {
    if (is_empty(selector)) {
        if (verbose) {
            message("The selector is empty.")
        }
        return(new_issues())
    } else if (length(selector) == 1L) {
        args <- selector[[1L]]
        args <- args[names(args) != "source"]
        issues <- do.call(
            what = switch(
                selector[[1L]][["source"]],
                GitHub = get_issues_github,
                GitLab = get_issues_gitlab,
                local = get_issues_local
            ),
            args = c(args, list(...))
        )
        return(issues)
    }

    issues <- selector |>
        seq_along() |>
        lapply(FUN = extract_nth, x = selector, verbose = FALSE) |>
        lapply(FUN = get_issues, verbose = verbose) |>
        do.call(what = rbind)
    return(issues)
}

#' @title Get Issues from GitHub Repository
#'
#' @description
#' Fetches issues from a GitHub repository using the GitHub API.
#' Filters out pull requests and formats the results.
#'
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
#' @param verbose A boolean indicating whether to print additional
#' information. Default is \code{TRUE}.
#'
#' @returns An `IssuesTB` object containing the issues from the repository.
#' @dev
#'
#' @examplesIf gh::gh_token_exists() && gh::gh_rate_limit()$remaining > 0
#' \donttest{
#' my_issues <- IssueTrackeR:::get_issues_github(owner = "rjdverse", repo = "rjd3toolkit")
#' my_issues <- IssueTrackeR:::get_issues_github(source = "jdemetra", state = "all")
#' }
#'
#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_character
get_issues_github <- function(
        repo = NULL,
        owner = NULL,
        state = c("open", "opened", "closed", "all"),
        verbose = TRUE
) {
    state <- match.arg(state)
    if (state == "opened") {
        state <- "open"
    }
    checkmate::assert_flag(verbose)
    checkmate::assert_character(repo, len = 1L)
    checkmate::assert_character(owner, len = 1L)

    if (verbose) {
        cat("Repo:", repo, " owner:", owner, "\n")
    }
    raw_issues <- try(expr = {
        gh::gh(
            repo = repo,
            owner = owner,
            endpoint = "/repos/:owner/:repo/issues",
            state = state,
            .limit = Inf,
            .progress = FALSE
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
            .limit = Inf,
            .progress = FALSE
        )
    })
    check_response(raw_comments)

    issues <- format_github_issues(
        raw_issues = raw_issues,
        raw_comments = raw_comments,
        verbose = verbose
    )

    return(issues)
}

#' @title Get Issues from GitLab
#'
#' @description
#' Internal function to fetch issues from GitLab API for a specific project.
#'
#' @param project_id Integer. GitLab project ID.
#' @param state a character string that is either \code{"open"} if
#' you want to fetch only open issues from GitHub, \code{"closed"} if you want
#' to fetch only closed issues from GitHub or \code{"all"} (by default) if you
#' want to fetch all issues from GitHub (closed and open).
#' @param verbose A boolean indicating whether to print additional
#' information. Default is \code{TRUE}.
#'
#' @returns An `IssuesTB` object containing the issues from the specified project.
#'
#' @examples
#' # Get issues from a GitLab project
#' my_issues <- IssueTrackeR:::get_issues_gitlab(project_id = 4578)
#'
#' @dev
#' @importFrom gitlabr gl_list_issues
#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_count
get_issues_gitlab <- function(
        project_id,
        state = c("open", "opened", "closed", "all"),
        verbose = TRUE,
        ...
) {
    state <- match.arg(state)
    if (state == "open") {
        state <- "opened"
    }
    checkmate::assert_flag(verbose)
    checkmate::assert_count(project_id)

    if (verbose) {
        cat("Project id:", project_id, "\n")
    }

    raw_issues <- gitlabr::gl_list_issues(project = project_id, state = state, ...)
    issues <- format_gitlab_issues(raw_issues)

    return(issues)
}

#' @title Get Issues from Local YAML File
#'
#' @description
#' Reads issues from a local YAML file and formats them into an IssuesTB object.
#'
#' @param file Character. Path to the YAML file containing issues.
#' @param verbose A boolean indicating whether to print additional
#' information. Default is \code{TRUE}.
#'
#' @returns An IssuesTB object containing the issues from the YAML file.
#'
#' @examples
#' my_issues <- IssueTrackeR:::get_issues_local(
#'     file = system.file(
#'         "data_issues",
#'         "open_issues.yaml",
#'         package = "IssueTrackeR"
#'     )
#' )
#' @dev
#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_character
get_issues_local <- function(
        file = NULL,
        verbose = TRUE
) {
    file <- normalizePath(file, mustWork = TRUE)
    checkmate::assert_character(file, len = 1L)
    checkmate::assert_flag(verbose)

    if (dir.exists(file)) {
        stop("The path corresponds to a directory.")
    }
    if (tools::file_ext(file) != "yaml") {
        stop("The path should lead to a yaml file.")
    }

    if (verbose) {
        message("The issues will be read from ", file, ".")
    }

    raw_yaml <- readLines(con = file, encoding = "UTF-8")
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
    return(issues)
}


#' @export
#' @rdname get
get_labels <- function(selector, verbose = TRUE, ...) {
    if (is_empty(selector)) {
        if (verbose) {
            message("The selector is empty.")
        }
        return(NULL)
    } else if (length(selector) == 1L) {
        args <- selector[[1L]]
        args <- args[names(args) != "source"]
        labels <- do.call(
            what = switch(
                selector[[1L]][["source"]],
                GitHub = get_labels_github,
                GitLab = get_labels_gitlab,
                local = get_labels_local
            ),
            args = c(args, list(...))
        )
        return(labels)
    }

    labels <- selector |>
        seq_along() |>
        lapply(FUN = extract_nth, x = selector, verbose = FALSE) |>
        lapply(FUN = get_labels, verbose = verbose) |>
        do.call(what = rbind)
    return(labels)
}

#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_character
get_labels_github <- function(
        repo = NULL,
        owner = NULL,
        verbose = TRUE
) {
    checkmate::assert_flag(verbose)
    checkmate::assert_character(repo, len = 1L)
    checkmate::assert_character(owner, len = 1L)

    raw_labels <- try(expr = {
        gh::gh(
            repo = repo,
            owner = owner,
            endpoint = "/repos/:owner/:repo/labels",
            .limit = Inf,
            .progress = FALSE
        )
    })
    check_response(raw_labels)

    if (verbose) {
        cat("Repo:", repo, " owner:", owner, "\n")
    }
    list_labels <- format_labels(raw_labels = raw_labels, verbose = verbose)

    if (!is.null(list_labels)) {
        list_labels <- cbind(list_labels, repo = repo, owner = owner)
    }

    class(list_labels) <- c("LabelsTB", "data.frame")
    return(list_labels)
}

#' @importFrom gitlabr gl_get_project
#' @importFrom gitlabr gitlab
#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_count
get_labels_gitlab <- function(
        project_id,
        verbose = TRUE,
        ...
) {
    checkmate::assert_flag(verbose)
    checkmate::assert_count(project_id)

    if (verbose) {
        cat("Repo:", repo, " owner:", owner, "\n")
    }
    structurel <- gitlabr::gl_get_project(project = project_id, ...)
    raw_labels <- gitlabr::gitlab(
        req = c("projects", project_id, "labels"),
        ...
    )
    if (nrow(raw_labels) == 0L) {
        return(NULL)
    }
    list_labels <- data.frame(
        name = null_to_default(raw_labels[["name"]], default = NA_character_),
        description = null_to_default(raw_labels[["description"]], default = NA_character_),
        color = null_to_default(raw_labels[["color"]], default = NA_character_),
        repo = structurel$path,
        owner = structurel$namespace.full_path
    )

    class(list_labels) <- c("LabelsTB", "data.frame")
    return(list_labels)
}

get_labels_local <- function(
        file = NULL,
        verbose = TRUE
) {
    file <- normalizePath(file, mustWork = TRUE)
    checkmate::assert_character(file, len = 1L)
    checkmate::assert_flag(verbose)

    if (dir.exists(file)) {
        stop("The path corresponds to a directory.")
    }
    if (tools::file_ext(file) != "yaml") {
        stop("The path should lead to a yaml file.")
    }

    if (verbose) {
        message("The labels will be read from ", file, ".")
    }

    list_labels <- readLines(con = file, encoding = "UTF-8") |>
        yaml::yaml.load() |>
        as.data.frame()

    class(list_labels) <- c("LabelsTB", "data.frame")
    return(list_labels)
}

#' @rdname get
#' @export
get_milestones <- function(
        source = c("local", "online"),
        dataset_dir = getOption("IssueTrackeR.dataset.dir"),
        dataset_name = "list_milestones.yaml",
        repo = getOption("IssueTrackeR.repo"),
        owner = getOption("IssueTrackeR.owner"),
        state = c("open", "closed", "all"),
        verbose = TRUE
) {
    source <- match.arg(source)
    state <- match.arg(state)

    if (source == "online") {
        if (is.null(repo)) {
            if (length(owner) > 1L) {
                milestones <- lapply(
                    X = owner,
                    FUN = get_milestones,
                    source = "online",
                    repo = NULL,
                    state = state,
                    verbose = verbose,
                    dataset_dir = NULL,
                    dataset_name = NULL
                ) |>
                    do.call(what = rbind)

                return(milestones)
            }
            list_repo <- get_all_repos(owner, verbose = verbose)

            milestones <- lapply(
                X = list_repo,
                FUN = get_milestones,
                source = "online",
                owner = owner,
                state = state,
                verbose = verbose,
                dataset_dir = NULL,
                dataset_name = NULL
            ) |>
                do.call(what = rbind)

            return(milestones)
        }

        if (verbose) {
            cat("Repo:", repo, " owner:", owner, "\n")
        }
        raw_milestones <- try(expr = {
            gh::gh(
                repo = repo,
                owner = owner,
                endpoint = "/repos/:owner/:repo/milestones",
                state = state,
                .limit = Inf,
                .progress = FALSE
            )
        })
        check_response(raw_milestones)
        milestones <- format_milestones(raw_milestones, verbose = verbose)

        if (nrow(milestones) > 0L) {
            milestones <- cbind(milestones, repo = repo, owner = owner)
        }
    } else if (source == "local") {
        if (tools::file_ext(dataset_name) == "yaml") {
            input_file <- tools::file_path_sans_ext(dataset_name)
        }
        input_path <- file.path(dataset_dir, input_file) |>
            paste0(".yaml") |>
            normalizePath(mustWork = TRUE)

        if (verbose) {
            message("The milestones will be read from ", input_path, ".")
        }
        milestones <- readLines(con = input_path, encoding = "UTF-8") |>
            yaml::yaml.load() |>
            as.data.frame()
        if (nrow(milestones) > 0L) {
            milestones[["due_on"]] <- format_timestamp(
                x = milestones[["due_on"]]
            )
        }
    } else {
        stop("wrong argument source", call. = FALSE)
    }

    class(milestones) <- c("MilestonesTB", "data.frame")
    return(milestones)
}
