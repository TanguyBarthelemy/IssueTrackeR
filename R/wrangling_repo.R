#' @title Retrieve all the visible repos from a user / an organisation
#'
#' @description
#' Returns a list of repos.
#'
#' @param owner Character. GitHub repository owner . GitHub owner (user or
#'   organization).
#' @param public Boolean. Should we include public repos?
#' (Default \code{TRUE})
#' @param private Boolean. Should we include private repos?
#' (Default \code{TRUE})
#' @inheritParams reset_options verbose
#'
#' @returns A string with the list of repo of a user or an organisation.
#'
#' @examplesIf gh::gh_token_exists() && gh::gh_rate_limit()$remaining > 0
#' \donttest{
#' get_all_repos("rjdverse")
#' }
#' @export
get_all_repos <- function(
    owner,
    public = TRUE,
    private = TRUE,
    verbose = TRUE
) {
    if (verbose) {
        cat("Try to find all repositories from ", owner, "...", sep = "")
    }

    if (isFALSE(public | private)) {
        if (verbose) {
            cat(" Done!\n")
        }
        return(NULL)
    }

    info_owner <- try(expr = {
        gh::gh(
            endpoint = "/users/:owner",
            owner = owner,
            .limit = Inf,
            .progress = FALSE
        )
    })
    check_response(info_owner)

    owner_type <- info_owner$type
    if (owner_type == "User") {
        endpoint <- "/users/:owner/repos"
    } else if (owner_type == "Organization") {
        endpoint <- "/orgs/:owner/repos"
    } else {
        stop("owner type not taken into account", call. = FALSE)
    }
    list_repo <- NULL

    if (public) {
        list_repo <- c(list_repo, get_public_repos(endpoint, owner))
    }

    if (private) {
        list_repo <- c(list_repo, get_private_repos(owner))
    }

    list_repo <- unique(list_repo)

    if (verbose) {
        cat(" Done!\n")
    }
    return(list_repo)
}

get_public_repos <- function(endpoint, owner) {
    raw_list_public_repo <- try({
        gh::gh(
            endpoint = endpoint,
            owner = owner,
            .limit = Inf,
            .progress = FALSE
        )
    })
    check_response(raw_list_public_repo)
    list_public_repo <- vapply(
        X = raw_list_public_repo,
        FUN = "[[",
        "name",
        FUN.VALUE = character(1L)
    )
    return(list_public_repo)
}

get_private_repos <- function(owner) {
    raw_list_private_repo <- try({
        gh::gh(
            endpoint = "/user/repos",
            .limit = Inf,
            visibility = "private",
            .progress = FALSE
        )
    })
    check_response(raw_list_private_repo)
    list_private_repo <- raw_list_private_repo |>
        Filter(f = \(.x) .x$owner$login == owner) |>
        vapply(FUN = "[[", "name", FUN.VALUE = character(1L))
    return(list_private_repo)
}
