#' @title Check if a selector is Empty
#'
#' @description
#' Checks whether a SetectorTB object contains any elements.
#'
#' @param selector A `SelectorTB` object to check.
#'
#' @returns `TRUE` if the selector is empty, `FALSE` otherwise.
#'
#' @examples
#' empty_sel <- IssueTrackeR:::empty_selector()
#' IssueTrackeR:::is_empty(empty_sel)  # Returns TRUE
#'
#' full_sel <- init_selector(source = "GitHub", owner = "TanguyBarthelemy", repo = "IssueTrackeR")
#' IssueTrackeR:::is_empty(full_sel)  # Returns FALSE
#' @dev
#' @importFrom checkmate assert_class
is_empty <- function(selector) {
    checkmate::assert_class(selector, "SelectorTB")
    return(length(selector) == 0L)
}

#' @title Create an Empty Selector Object
#'
#' @description
#' Creates an empty selector object of class `SelectorTB`.
#'
#' @returns An empty `SelectorTB` object.
#'
#' @examples
#' empty_sel <- IssueTrackeR:::empty_selector()
#' @dev
empty_selector <- function() {
    selector <- list()
    class(selector) <- "SelectorTB"
    return(selector)
}

#' @title Initialise a SelectorTB object
#'
#' @description
#' Creates a SelectorTB object for specifying data sources (GitHub, GitLab, or local).
#' The selector can be used to fetch issues from different sources.
#'
#' @param source a character string that is either:
#'   - \code{"GitHub"} if you want to fetch information from GitHub
#'   - \code{"GitLab"} if you want to fetch information from GitLab
#'   - or \code{"local"} if you want to fetch information locally.
#' @param ... Additional arguments specific to the source type:
#'          - For GitHub: `owner`, `repo`
#'          - For GitLab: `project_id`
#'          - For local: `file`
#'
#' @returns A `SelectorTB` object configured for the specified source.
#'
#' @details
#' `SelectorTB` objects define data sources for retrieving issues from
#' GitHub, GitLab, or local YAML files. They allow combining multiple sources
#' into a single query using \code{merge_selector()}.
#'
#' @examples
#' # GitHub selector for a specific repository
#' gh_sel <- init_selector(source = "GitHub", owner = "TanguyBarthelemy", repo = "IssueTrackeR")
#'
#' # GitLab selector for multiple projects
#' gl_sel <- init_selector(source = "GitLab", project_id = c(4578, 2384))
#'
#' # Local selector for a YAML file
#' local_sel <- init_selector(source = "local", file = "path/to/issues.yaml")
#' @export
#' @importFrom checkmate assert_character
init_selector <- function(source, ...) {
    args <- list(...)
    if (length(args) == 0L) {
        return(empty_selector())
    }

    checkmate::assert_character(source, len = 1L)
    source <- tolower(source)
    checkmate::assert_character(source, len = 1L)

    if (source == "github") {
        return(init_selector_github(...))
    } else if (source == "gitlab") {
        return(init_selector_gitlab(...))
    } else if (source == "local") {
        return(init_selector_local(...))
    }

    stop("Wrong source. Source should be \"GitHub\", \"GitLab\" or \"local\".")
}

#' @title Initialize GitHub Selector
#'
#' @description
#' Creates a selector for GitHub repositories. If no repository is specified,
#' it automatically fetches all repositories for the given owner.
#'
#' @param owner Character. GitHub owner (user or organization).
#' @param repo Character. GitHub repository name(s). If NULL, all repositories
#'        for the owner will be included.
#' @param ... Additional arguments to pass to the selector.
#'
#' @returns A `SelectorTB` object configured for GitHub repositories.
#'          Returns an empty selector if `owner` is `NULL`.
#'
#' @examples
#' # Select a specific repository
#' gh_sel <- IssueTrackeR:::init_selector_github(owner = "TanguyBarthelemy", repo = "IssueTrackeR")
#'
#' # Select all repositories for an owner
#' all_repos_sel <- IssueTrackeR:::init_selector_github(owner = "TanguyBarthelemy")
#'
#' # Select multiple repositories
#' multi_repo_sel <- IssueTrackeR:::init_selector_github(
#'     owner = "TanguyBarthelemy",
#'     repo = c("IssueTrackeR", "AnotherRepo")
#' )
#' @importFrom checkmate assert_character
#' @dev
init_selector_github <- function(owner = NULL, repo = NULL, ...) {
    checkmate::assert_character(owner, null.ok = TRUE)
    checkmate::assert_character(repo, null.ok = TRUE)

    if (is.null(owner)) {
        warning("There is no owner Please provide a owner.")
        return(empty_selector())
    } else if (length(owner) > 1) {
        selector <- lapply(owner, init_selector_github, repo = repo, ...) |>
            do.call(what = merge_selector)
        return(selector)
    } else if (is.null(repo)) {
        return(init_selector_github(
            owner = owner,
            repo = get_all_repos(owner),
            ...
        ))
    }

    selector <- lapply(repo, \(r) {
        list(source = "GitHub", repo = r, owner = owner, ...)
    })
    class(selector) <- "SelectorTB"
    return(selector)
}

#' @title Initialize GitLab Selector
#'
#' @description
#' Creates a selector for GitLab projects.
#'
#' @param project_id Integer. GitLab project ID(s). Can be a vector to select
#'        multiple projects.
#' @param ... Additional arguments to pass to the selector.
#'
#' @returns A `SelectorTB` object configured for GitLab projects.
#'          Returns an empty selector if `project_id` is `NULL`.
#'
#' @dev
#' @examples
#' # Select a single project
#' gl_sel <- IssueTrackeR:::init_selector_gitlab(project_id = 8425)
#'
#' # Select multiple projects
#' multi_project_sel <- IssueTrackeR:::init_selector_gitlab(
#'     project_id = c(2153, 7865, 6542)
#' )
#' @importFrom checkmate assert_integerish
init_selector_gitlab <- function(project_id = NULL, ...) {
    checkmate::assert_integerish(project_id, null.ok = TRUE)
    if (is.null(project_id)) {
        warning("There is no project_id. Please provide a project_id.")
        return(empty_selector())
    }
    selector <- lapply(project_id, \(id) {
        list(source = "GitLab", project_id = id, ...)
    })
    class(selector) <- "SelectorTB"
    return(selector)
}

#' @title Initialize Local Selector
#'
#' @description
#' Creates a selector for local YAML files containing issue data.
#'
#' @param file Character. Path(s) to YAML file(s) containing issue data.
#'   Can be a vector to select multiple files.
#'
#' @returns A `SelectorTB` object configured for local YAML files.
#'
#' @examples
#' # Select a single YAML file
#' local_sel <- IssueTrackeR:::init_selector_local(
#'     file = system.file(
#'         "data_issues",
#'         "open_issues.yaml",
#'         package = "IssueTrackeR"
#'     )
#' )
#'
#' # Select multiple YAML files
#' multi_file_sel <- IssueTrackeR:::init_selector_local(
#'     file = c(
#'         "path/to/open_issues.yaml",
#'         "path/to/closed_issues.yaml"
#'     )
#' )
#' @dev
init_selector_local <- function(file) {
    file <- normalizePath(file, mustWork = TRUE)
    selector <- lapply(file, \(f) list(source = "local", file = f))
    class(selector) <- "SelectorTB"
    return(selector)
}

#' @title Merge Multiple Selectors
#'
#' @description
#' Combines multiple selector objects into a single selector.
#' Useful for fetching issues from multiple sources in one operation.
#'
#' @param \dots `SelectorTB` objects to merge.
#'
#' @returns A merged `SelectorTB` object containing all elements from the input selectors.
#'
#' @examples
#' s1 <- init_selector(source = "GitHub", owner = "TanguyBarthelemy", repo = "IssueTrackeR")
#' s2 <- init_selector(source = "GitHub", owner = "TractorTom", repo = NULL)
#' merged <- merge_selector(s1, s2)
#' @export
merge_selector <- function(...) {
    selectors <- list(...)
    if (length(selectors) == 0L) {
        return(empty_selector())
    }
    selector <- do.call(selectors, what = c)
    class(selector) <- "SelectorTB"
    return(selector)
}
