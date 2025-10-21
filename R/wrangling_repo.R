#' @title Retrieve all the visible repos from a user / an organisation
#'
#' @description
#' Returns a list of repos.
#'
#' @inheritParams get_issues
#' @param public Boolean. Should we include public repos?
#' (Default \code{TRUE})
#' @param private Boolean. Should we include private repos?
#' (Default \code{TRUE})
#'
#' @returns A string with the list of repo of a user or an organisation.
#'
#' @examples
#'
#' \donttest{
#' get_all_repos("rjdverse")
#' }
#'
#' @export
get_all_repos <- function(owner, public = TRUE, private = TRUE) {
    if (isFALSE(public | private)) {
        return(NULL)
    }

    info_owner <- try(expr = {
        gh::gh(
            endpoint = "/users/:owner",
            owner = owner,
            .limit = Inf
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
        list_public_repo <- try({
            gh::gh(
                endpoint = endpoint,
                owner = owner,
                .limit = Inf
            )
        })
        check_response(list_public_repo)
        list_public_repo <- vapply(
            X = list_public_repo,
            FUN = "[[",
            "name",
            FUN.VALUE = character(1L)
        )

        list_repo <- c(list_repo, list_public_repo)
    }

    if (private) {
        list_private_repo <- try({
            gh::gh(
                endpoint = "/user/repos",
                .limit = Inf,
                visibility = "private"
            )
        })
        check_response(list_private_repo)
        list_private_repo <- list_private_repo |>
            Filter(f = \(.x) .x$owner$login == owner) |>
            vapply(FUN = "[[", "name", FUN.VALUE = character(1L))

        list_repo <- c(list_repo, list_private_repo)
    }

    list_repo <- unique(list_repo)

    return(list_repo)
}
