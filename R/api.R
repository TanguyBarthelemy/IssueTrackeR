# Thin wrappers around the GitHub and GitLab APIs.
#
# Every HTTP call in the package goes through one of these functions so
# that the API layer can be mocked in tests with
# `testthat::local_mocked_bindings()`. Do not call `gh::` or `gitlabr::`
# functions anywhere else in the package.

#' @noRd
#' @name api
#' @importFrom gh gh
#' @importFrom gh gh_token_exists
#' @importFrom gh gh_rate_limit
#' @importFrom gitlabr gitlab
#' @importFrom gitlabr gl_get_project
#' @importFrom gitlabr gl_list_issues
NULL

#' @noRd
.api_issues_gh <- function(owner, repo, state) {
    return(gh::gh(
        repo = repo,
        owner = owner,
        endpoint = "/repos/:owner/:repo/issues",
        state = state,
        .limit = Inf,
        .progress = FALSE
    ))
}

#' @noRd
.api_comments_gh <- function(owner, repo) {
    return(gh::gh(
        repo = repo,
        owner = owner,
        endpoint = "/repos/:owner/:repo/issues/comments",
        .limit = Inf,
        .progress = FALSE
    ))
}

#' @noRd
.api_labels_gh <- function(owner, repo) {
    return(gh::gh(
        repo = repo,
        owner = owner,
        endpoint = "/repos/:owner/:repo/labels",
        .limit = Inf,
        .progress = FALSE
    ))
}

#' @noRd
.api_milestones_gh <- function(owner, repo, state) {
    return(gh::gh(
        repo = repo,
        owner = owner,
        endpoint = "/repos/:owner/:repo/milestones",
        state = state,
        .limit = Inf,
        .progress = FALSE
    ))
}

#' @noRd
.api_owner_gh <- function(owner) {
    return(gh::gh(
        endpoint = "/users/:owner",
        owner = owner,
        .limit = Inf,
        .progress = FALSE
    ))
}

#' @noRd
.api_repos_gh <- function(endpoint, owner) {
    return(gh::gh(
        endpoint = endpoint,
        owner = owner,
        .limit = Inf,
        .progress = FALSE
    ))
}

#' @noRd
.api_private_repos_gh <- function() {
    return(gh::gh(
        endpoint = "/user/repos",
        .limit = Inf,
        visibility = "private",
        .progress = FALSE
    ))
}

#' @noRd
.api_token_gh <- function() {
    return(gh::gh_token_exists())
}

#' @noRd
.api_rate_limit_gh <- function() {
    return(gh::gh_rate_limit())
}

#' @noRd
.api_issues_gl <- function(project_id, state, ...) {
    return(gitlabr::gl_list_issues(
        project = project_id,
        state = state,
        ...
    ))
}

#' @noRd
.api_project_gl <- function(project_id, ...) {
    return(gitlabr::gl_get_project(project = project_id, ...))
}

#' @noRd
.api_labels_gl <- function(project_id, ...) {
    return(gitlabr::gitlab(
        req = c("projects", project_id, "labels"),
        ...
    ))
}

#' @noRd
.api_milestones_gl <- function(project_id, ...) {
    return(gitlabr::gitlab(
        req = c("projects", project_id, "milestones"),
        state = "all",
        ...
    ))
}
