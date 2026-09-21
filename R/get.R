#' @title Retrieve information from the issues of GitHub
#'
#' @description
#' use \code{\link[gh]{gh}} to ask the API of GitHub and et a list of issues
#' with their labels and milestones.
#'
#' @param selector A `SelectorTB` object (to define the range of the repo /
#'   source used). Created with [`init_selector()`].
#' @param verbose A boolean indicating whether to print additional
#' information. Default is \code{TRUE}.
#' @param \dots Other parameters for GitLab as `gitlab_url` and `private_token`.
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
#'
#' \donttest{
#' # From online
#'
#' online_selector <- init_selector(
#'     source = "GitHub",
#'     owner = "TanguyBarthelemy",
#'     repo = "IssueTrackeR"
#' )
#' issues <- get_issues(selector = online_selector)
#' print(issues)
#'
#' labels <- get_labels(selector = online_selector)
#' print(labels)
#'
#' milestones <- get_milestones(selector = online_selector)
#' print(milestones)
#' }
#'
#' # From local
#'
#' local_issues_selector <- init_selector(
#'     source = "Local",
#'     file = file.path(
#'         system.file("data_issues", package = "IssueTrackeR"),
#'         "list_issues.yaml"
#'     )
#' )
#' local_labels_selector <- init_selector(
#'     source = "Local",
#'     file = file.path(
#'         system.file("data_issues", package = "IssueTrackeR"),
#'         "list_labels.yaml"
#'     )
#' )
#' local_milestones_selector <- init_selector(
#'     source = "Local",
#'     file = file.path(
#'         system.file("data_issues", package = "IssueTrackeR"),
#'         "list_milestones.yaml"
#'     )
#' )
#'
#' issues <- get_issues(selector = local_issues_selector)
#' labels <- get_labels(selector = local_labels_selector)
#' milestones <- get_milestones(selector = local_milestones_selector)
#'
get_issues <- function(selector, verbose = TRUE, ...) {
    if (is_empty(selector)) {
        if (verbose) {
            message("The selector is empty.")
        }
        return(new_issues())
    } else if (length(selector) == 1L) {
        sel_args <- selector[[1L]]
        sel_args <- sel_args[names(sel_args) != "source"]
        issues <- do.call(
            what = switch(
                selector[[1L]][["source"]],
                GitHub = get_issues_github,
                GitLab = get_issues_gitlab,
                local = get_issues_local
            ),
            args = c(sel_args, list(...))
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
#' my_issues1 <- IssueTrackeR:::get_issues_github(
#'     owner = "rjdverse",
#'     repo = "rjd3toolkit"
#' )
#' my_issues2 <- IssueTrackeR:::get_issues_github(
#'     owner = "TanguyBarthelemy",
#'     repo = "IssueTrackeR",
#'     state = "all"
#' )
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

    issues <- format_issues_github(
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
#' @returns An `IssuesTB` object containing the issues from the specified
#' project.
#'
#' @examples
#' \dontrun{
#' # Get issues from a GitLab project
#' my_issues <- IssueTrackeR:::get_issues_gitlab(project_id = 4578)
#' }
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

    raw_issues <- gitlabr::gl_list_issues(
        project = project_id,
        state = state,
        ...
    )
    issues <- format_issues_gitlab(raw_issues)

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
#'         "list_issues.yaml",
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
        stop("The path corresponds to a directory.", call. = FALSE)
    }
    if (tools::file_ext(file) != "yaml") {
        stop("The path should lead to a yaml file.", call. = FALSE)
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
        sel_args <- selector[[1L]]
        sel_args <- sel_args[names(sel_args) != "source"]
        list_labels <- do.call(
            what = switch(
                selector[[1L]][["source"]],
                GitHub = get_labels_github,
                GitLab = get_labels_gitlab,
                local = get_labels_local
            ),
            args = c(sel_args, list(...))
        )
        return(list_labels)
    }

    list_labels <- selector |>
        seq_along() |>
        lapply(FUN = extract_nth, x = selector, verbose = FALSE) |>
        lapply(FUN = get_labels, verbose = verbose) |>
        do.call(what = rbind)
    return(list_labels)
}

#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_character
get_labels_github <- function(
    repo = NULL,
    owner = NULL,
    verbose = TRUE,
    ...
) {
    checkmate::assert_flag(verbose)
    checkmate::assert_character(repo, len = 1L)
    checkmate::assert_character(owner, len = 1L)

    if (verbose) {
        cat("Repo:", repo, " owner:", owner, "\n")
    }
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

    list_labels <- format_labels_github(
        raw_labels = raw_labels,
        verbose = verbose
    )

    if (!is.null(list_labels)) {
        list_labels <- cbind(
            source = "https://github.com/",
            list_labels,
            repo = repo,
            owner = owner,
            url = file.path(
                "https://github.com",
                owner,
                repo,
                "labels",
                utils::URLencode(list_labels$name),
                fsep = "/"
            )
        )
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
        cat("Project id:", project_id, "\n")
    }
    structurel <- gitlabr::gl_get_project(project = project_id, ...)
    raw_labels <- gitlabr::gitlab(
        req = c("projects", project_id, "labels"),
        ...
    )
    if (nrow(raw_labels) == 0L) {
        return(NULL)
    }
    labels_name <- null_to_default(
        raw_labels[["name"]],
        default = NA_character_
    )
    list_labels <- data.frame(
        source = gsub(
            x = structurel$web_url,
            pattern = paste0(structurel$path_with_namespace, "$"),
            replacement = ""
        ),
        name = labels_name,
        description = null_to_default(
            raw_labels[["description"]],
            default = NA_character_
        ),
        color = null_to_default(raw_labels[["color"]], default = NA_character_),
        repo = structurel$path,
        owner = structurel$namespace.full_path,
        url = file.path(
            structurel$web_url,
            "-",
            paste0("work_items?label_name%5B%5D=", labels_name),
            fsep = "/"
        )
    )

    class(list_labels) <- c("LabelsTB", "data.frame")
    return(list_labels)
}

#' @importFrom tools file_ext
#' @importFrom checkmate assert_character
#' @importFrom checkmate assert_flag
#' @importFrom yaml yaml.load
get_labels_local <- function(
    file = NULL,
    verbose = TRUE
) {
    file <- normalizePath(file, mustWork = TRUE)
    checkmate::assert_character(file, len = 1L)
    checkmate::assert_flag(verbose)

    if (dir.exists(file)) {
        stop("The path corresponds to a directory.", call. = FALSE)
    }
    if (tools::file_ext(file) != "yaml") {
        stop("The path should lead to a yaml file.", call. = FALSE)
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
get_milestones <- function(selector, verbose = TRUE, ...) {
    if (is_empty(selector)) {
        if (verbose) {
            message("The selector is empty.")
        }
        return(NULL)
    } else if (length(selector) == 1L) {
        sel_args <- selector[[1L]]
        sel_args <- sel_args[names(sel_args) != "source"]
        milestones <- do.call(
            what = switch(
                selector[[1L]][["source"]],
                GitHub = get_milestones_github,
                GitLab = get_milestones_gitlab,
                local = get_milestones_local
            ),
            args = c(sel_args, list(...))
        )
        return(milestones)
    }

    milestones <- selector |>
        seq_along() |>
        lapply(FUN = extract_nth, x = selector, verbose = FALSE) |>
        lapply(FUN = get_milestones, verbose = verbose) |>
        do.call(what = rbind)
    return(milestones)
}

#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_character
get_milestones_github <- function(
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

    milestones <- format_milestones_github(raw_milestones, verbose = verbose)
    if (nrow(milestones) > 0L) {
        milestones <- cbind(milestones, repo = repo, owner = owner)
    }

    class(milestones) <- c("MilestonesTB", "data.frame")
    return(milestones)
}


#' @importFrom gitlabr gl_get_project
#' @importFrom gitlabr gitlab
#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_count
get_milestones_gitlab <- function(
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

    structurel <- gitlabr::gl_get_project(project = project_id, ...)
    raw_milestones <- gitlabr::gitlab(
        req = c("projects", project_id, "milestones"),
        state = "all",
        ...
    )

    if (nrow(raw_milestones) == 0L) {
        milestones <- data.frame(
            source = character(0L),
            title = character(0L),
            description = character(0L),
            due_on = format_timestamp(character(0L)),
            closed_at = format_timestamp(character(0L)),
            creator = character(0L),
            state = character(0L),
            nb_issues_open = integer(0L),
            nb_issues_closed = integer(0L),
            repo = character(0L),
            owner = character(0L),
            url = character(0L),
            stringsAsFactors = FALSE
        )
    } else {
        milestones <- data.frame(
            source = gsub(
                x = structurel$web_url,
                pattern = paste0(structurel$path_with_namespace, "$"),
                replacement = ""
            ),
            title = null_to_default(
                raw_milestones[["title"]],
                default = NA_character_
            ),
            description = null_to_default(
                raw_milestones[["description"]],
                default = NA_character_
            ),
            due_on = format_timestamp(null_to_default(
                raw_milestones[["due_date"]],
                default = NA_character_
            )),
            closed_at = format_timestamp(NA_character_),
            creator = NA_character_,
            state = null_to_default(
                raw_milestones[["state"]],
                default = NA_character_
            ),
            nb_issues_open = NA_integer_,
            nb_issues_closed = NA_integer_,
            repo = structurel$path,
            owner = structurel$namespace.full_path,
            url = raw_milestones$web_url
        )
    }

    class(milestones) <- c("MilestonesTB", "data.frame")
    return(milestones)
}

#' @importFrom tools file_ext
#' @importFrom checkmate assert_character
#' @importFrom checkmate assert_flag
#' @importFrom yaml yaml.load
get_milestones_local <- function(
    file = NULL,
    verbose = TRUE
) {
    file <- normalizePath(file, mustWork = TRUE)
    checkmate::assert_character(file, len = 1L)
    checkmate::assert_flag(verbose)

    if (dir.exists(file)) {
        stop("The path corresponds to a directory.", call. = FALSE)
    }
    if (tools::file_ext(file) != "yaml") {
        stop("The path should lead to a yaml file.", call. = FALSE)
    }

    if (verbose) {
        message("The milestones will be read from ", file, ".")
    }

    list_milestones <- readLines(con = file, encoding = "UTF-8") |>
        yaml::yaml.load() |>
        as.data.frame()

    class(list_milestones) <- c("MilestonesTB", "data.frame")
    return(list_milestones)
}
