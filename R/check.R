check_response <- function(x, context = "GitHub API call") {
    if (!inherits(x, "try-error")) {
        return(invisible(NULL))
    }

    cond <- attr(x, "condition")
    msg <- conditionMessage(cond)
    if (is.null(msg)) {
        msg <- as.character(x)
    }

    if (grepl("Timeout was reached", msg, ignore.case = TRUE)) {
        stop(
            "[",
            context,
            "]",
            " The GitHub API request timed out. \U1F553\n",
            "\u2192 Check your network connection\n",
            "\u2192 Or increase timeout options.\n",
            "\u2192 Or wait a few seconds and try again.",
            call. = FALSE
        )
    } else if (
        grepl(
            pattern = "Requires authentication",
            x = msg,
            ignore.case = TRUE,
            fixed = TRUE
        )
    ) {
        stop(
            "[",
            context,
            "]",
            " The GitHub token used does not have sufficient permissions",
            " \U1F512.\n",
            "\u2192 Try using a Personal Access Token (PAT) with 'repo' scope.",
            call. = FALSE
        )
    } else if (grepl("API rate limit exceeded", msg, ignore.case = TRUE)) {
        stop(
            "[",
            context,
            "]",
            " GitHub API rate limit exceeded \U23F3\n",
            "\u2192 Wait a few minutes\n",
            "\u2192 Or authenticate with a PAT to increase your limit.",
            call. = FALSE
        )
    } else if (
        inherits(x = cond, what = "http_error_404") ||
            grepl(
                pattern = "URL not found",
                x = msg,
                ignore.case = TRUE,
                fixed = TRUE
            )
    ) {
        url_repo <- cond$body["x"] |>
            sub(pattern = ".*<8;;", replacement = "") |>
            sub(pattern = "\\a.*", replacement = "") |>
            trimws()

        if (grepl(pattern = "/repos/", x = url_repo, fixed = TRUE)) {
            repo_path <- sub(
                pattern = "^.*/repos/",
                replacement = "",
                x = url_repo,
                fixed = TRUE
            )
            parts <- strsplit(x = repo_path, split = "/", fixed = TRUE)[[1L]]
            owner <- parts[1L]
            repo <- parts[2L]

            stop(
                "[",
                context,
                "] ",
                "The repository '",
                owner,
                "/",
                repo,
                "' does not exist\n",
                "\u2192 Or is not accessible on GitHub \U274C.\n",
                "\u2192 Verify that both owner and repo names are correct, ",
                "and that you have access rights.",
                call. = FALSE
            )
        } else if (grepl(pattern = "/users/", x = url_repo, fixed = TRUE)) {
            owner <- sub("^.*/users/", "", url_repo)
            owner <- sub("\\?.*$", "", owner)

            stop(
                "[",
                context,
                "] ",
                "The user '",
                owner,
                "' does not exist\n",
                "\u2192 Or is not accessible on GitHub \U274C.\n",
                "\u2192 Check that the username is correct.",
                call. = FALSE
            )
        } else if (grepl(pattern = "/orgs/", x = url_repo, fixed = TRUE)) {
            owner <- sub("^.*/orgs/", "", url_repo)
            owner <- sub("/.*$", "", owner)
            owner <- sub("\\?.*$", "", owner)

            stop(
                "[",
                context,
                "] ",
                "The organization '",
                owner,
                "' does not exist\n",
                "\u2192 Or is not accessible on GitHub \U274C.\n",
                "\u2192 Check that the organization name is correct.",
                call. = FALSE
            )
        } else {
            stop(
                "[",
                context,
                "] ",
                "The requested resource was not found on GitHub \U274C.\n",
                "\u2192 Check the API endpoint and parameters.",
                call. = FALSE
            )
        }
    } else {
        stop(
            "[",
            context,
            "]",
            " Weird message...\n",
            "\u2192 Please contact the maintainer of the package with the ",
            "error message:\n",
            msg,
            call. = FALSE
        )
    }
}
