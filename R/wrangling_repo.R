
#' @export
get_all_repos <- function(owner) {
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
        FUN = "[[", "name",
        FUN.VALUE = character(1L)
    )

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

    list_repo <- unique(c(list_private_repo, list_public_repo))

    return(list_repo)
}
