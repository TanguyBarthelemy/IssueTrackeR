
#' @export
get_all_repos <- function(owner) {
    info_owner <- try(expr = {
        gh::gh(
            endpoint = "/users/:owner",
            owner = owner,
            .limit = Inf
        )
    })
    if (inherits(info_owner, "try-error")) {
        message_response <- attr(
            info_owner,
            "condition"
        )$response_content$message
        if (message_response == "Not Found") {
            stop(
                owner,
                " is not a valid GitHub user.\n",
                "The argument owner must be a valid GitHub user.",
                call. = FALSE
            )
        } else if (grepl("API rate limit exceeded", message_response,
                         fixed = TRUE)) {
            warning(
                message_response,
                "\n",
                attr(info_owner, "condition")$body,
                call. = FALSE
            )
            return(new_issues())
        } else {
            stop("Weird message... Contact the maintainer of the package.",
                 call. = FALSE)
        }
    }

    owner_type <- info_owner$type
    if (owner_type == "User") {
        endpoint <- "/users/:owner/repos"
    } else if (owner_type == "Organization") {
        endpoint <- "/orgs/:owner/repos"
    } else {
        stop("owner type not taken into account", call. = FALSE)
    }

    list_public_repo <- gh::gh(
        endpoint = endpoint,
        owner = owner,
        .limit = Inf
    ) |>
        vapply(FUN = "[[", "name", FUN.VALUE = character(1L))

    list_private_repo <- gh::gh(
        endpoint = "/user/repos",
        .limit = Inf,
        visibility = "private"
    ) |>
        Filter(f = \(.x) .x$owner$login == owner) |>
        vapply(FUN = "[[", "name", FUN.VALUE = character(1L))

    list_repo <- unique(c(list_private_repo, list_public_repo))

    return(list_repo)
}
