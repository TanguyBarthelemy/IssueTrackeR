has_timeout <- function(msg) {
    return(grepl("Timeout was reached", msg, ignore.case = TRUE))
}

need_auth <- function(msg) {
    return(grepl(
        pattern = "Requires authentication",
        x = msg,
        ignore.case = TRUE,
        perl = FALSE
    ))
}

api_rate_reached <- function(msg) {
    return(
        grepl(
            pattern = "API rate limit exceeded",
            x = msg,
            ignore.case = TRUE,
            perl = FALSE
        )
    )
}

has_no_http <- function(msg) {
    return(grepl(
        pattern = "Failed to perform HTTP request",
        x = msg,
        ignore.case = TRUE
    ))
}

is_not_found <- function(msg) {
    return(grepl(
        pattern = "URL not found",
        x = msg,
        ignore.case = TRUE,
        perl = FALSE
    ))
}

is_orgs_call <- function(msg) {
    return(grepl(pattern = "/orgs/", x = msg, fixed = TRUE))
}

is_user_call <- function(msg) {
    return(grepl(pattern = "/users/", x = msg, fixed = TRUE))
}

is_repo_call <- function(msg) {
    return(grepl(pattern = "/repos/", x = msg, fixed = TRUE))
}

timeout_msg <- c(
    " The GitHub API request timed out. \U1F553\n",
    "\u2192 Check your network connection\n",
    "\u2192 Or increase timeout options.\n",
    "\u2192 Or wait a few seconds and try again."
)
auth_msg <- c(
    " The GitHub token used does not have sufficient permissions",
    " \U1F512.\n",
    "\u2192 Try using a Personal Access Token (PAT) with 'repo' scope."
)
api_rate_msg <- c(
    " GitHub API rate limit exceeded \U23F3\n",
    "\u2192 Wait a few minutes\n",
    "\u2192 Or authenticate with a PAT to increase your limit."
)
no_resource_msg <- c(
    "The requested resource was not found on GitHub \U274C.\n",
    "\u2192 Check the API endpoint and parameters."
)

no_http_msg <- c(
    "Unable to reach GitHub servers \u1f310\n",
    "\u2192 Check your internet connection.\n",
    "\u2192 If you recently changed network (Wi-Fi/Ethernet/VPN), ",
    "wait a few seconds and try again.\n",
    "\u2192 If the problem persists, ",
    "verify your proxy or firewall settings."
)

wrong_repo_msg <- function(owner, repo) {
    return(c(
        "The repository '",
        owner,
        "/",
        repo,
        "' does not exist\n",
        "\u2192 Or is not accessible on GitHub \U274C.\n",
        "\u2192 Verify that both owner and repo names are correct, ",
        "and that you have access rights."
    ))
}

wrong_username_msg <- function(owner) {
    return(c(
        "The user '",
        owner,
        "' does not exist\n",
        "\u2192 Or is not accessible on GitHub \U274C.\n",
        "\u2192 Check that the username is correct."
    ))
}

wrong_org_name_msg <- function(owner) {
    return(c(
        "The organization '",
        owner,
        "' does not exist\n",
        "\u2192 Or is not accessible on GitHub \U274C.\n",
        "\u2192 Check that the organization name is correct."
    ))
}

weird_msg <- function(msg) {
    return(c(
        " Weird message...\n",
        "\u2192 Please contact the maintainer of the package with the ",
        "error message:\n",
        msg
    ))
}

check_response <- function(x) {
    if (!inherits(x, "try-error")) {
        return(invisible(NULL))
    }

    cond <- attr(x, "condition")
    msg <- conditionMessage(cond)
    if (is.null(msg)) {
        msg <- as.character(x)
    }

    if (has_timeout(msg)) {
        stop(timeout_msg, call. = FALSE)
    } else if (need_auth(msg)) {
        stop(auth_msg, call. = FALSE)
    } else if (api_rate_reached(msg)) {
        stop(api_rate_msg, call. = FALSE)
    } else if (
        inherits(x = cond, what = "http_error_404") || is_not_found(msg)
    ) {
        url_repo <- msg |>
            sub(
                pattern = ".*api.github.com/",
                replacement = "api.github.com/"
            ) |>
            sub(pattern = "\033\\]8.*$", replacement = "")

        if (is_repo_call(url_repo)) {
            repo_path <- sub(
                pattern = "^.*/repos/",
                replacement = "",
                x = url_repo,
                fixed = FALSE
            )
            parts <- strsplit(x = repo_path, split = "/", fixed = TRUE)[[1L]]
            owner <- parts[1L]
            repo <- parts[2L]
            stop(wrong_repo_msg(owner, repo), call. = FALSE)
        } else if (is_user_call(url_repo)) {
            owner <- sub("^.*/users/", "", url_repo)
            owner <- sub("\\?.*$", "", owner)
            stop(wrong_username_msg(owner), call. = FALSE)
        } else if (is_orgs_call(url_repo)) {
            owner <- sub(pattern = "^.*/orgs/", "", url_repo)
            owner <- sub(pattern = "/.*$", "", owner)
            owner <- sub(pattern = "\\?.*$", "", owner)
            stop(wrong_org_name_msg(owner), call. = FALSE)
        } else {
            stop(no_resource_msg, call. = FALSE)
        }
    } else if (has_no_http(msg)) {
        stop(no_http_msg, call. = FALSE)
    } else {
        stop(weird_msg(msg), call. = FALSE)
    }
}
