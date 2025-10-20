check_response <- function(x, context = "GitHub API call") {
    if (!inherits(x, "try-error")) {
        return(invisible(NULL))
    }

    cond <- attr(x, "condition")
    msg <- conditionMessage(cond)
    if (is.null(msg)) msg <- as.character(x)

    if (grepl("Timeout was reached", msg, ignore.case = TRUE)) {
        stop(
            "[", context, "]", " The GitHub API request timed out. \U1F553\n",
            "\u2192 Check your network connection or increase timeout options.\n",
            "\u2192 Or wait a few seconds and try again.",
            call. = FALSE
        )
    } else if (grepl("Resource not accessible by integration", msg, ignore.case = TRUE)) {
        stop(
            "[", context, "]", " The GitHub token used does not have sufficient permissions \U1F512.\n",
            "\u2192 Try using a Personal Access Token (PAT) with 'repo' scope.",
            call. = FALSE
        )
    } else if (grepl("API rate limit exceeded", msg, ignore.case = TRUE)) {
        stop(
            "[", context, "]", " GitHub API rate limit exceeded \U23F3\n",
            "\u2192 Wait a few minutes or authenticate with a PAT to increase your limit.",
            call. = FALSE
        )
    } else if (inherits(cond, "http_error_404") || grepl("URL not found", msg, ignore.case = TRUE)) {
        url_line <- cond$body["x"]
        url <- sub(".*<8;;", "", url_line)
        url <- sub("\\a.*", "", url)
        url <- trimws(url)

        if (grepl("/repos/", url)) {
            repo_path <- sub("^.*/repos/", "", url)
            parts <- strsplit(repo_path, "/")[[1]]
            owner <- parts[1]
            repo  <- parts[2]

            stop(
                "[", context, "] ", "The repository '", owner, "/", repo,
                "' does not exist or is not accessible on GitHub \U274C.\n",
                "\u2192 Verify that both owner and repo names are correct, and that you have access rights.",
                call. = FALSE
            )

        } else if (grepl("/users/", url)) {
            owner <- sub("^.*/users/", "", url)
            owner <- sub("\\?.*$", "", owner)

            stop(
                "[", context, "] ", "The user '", owner,
                "' does not exist or is not accessible on GitHub \U274C.\n",
                "\u2192 Check that the username is correct.",
                call. = FALSE
            )

        } else if (grepl("/orgs/", url)) {
            owner <- sub("^.*/orgs/", "", url)
            owner <- sub("/.*$", "", owner)
            owner <- sub("\\?.*$", "", owner)

            stop(
                "[", context, "] ", "The organization '", owner,
                "' does not exist or is not accessible on GitHub \U274C.\n",
                "\u2192 Check that the organization name is correct.",
                call. = FALSE
            )

        } else {
            stop(
                "[", context, "] ", "The requested resource was not found on GitHub \U274C.\n",
                "\u2192 Check the API endpoint and parameters.",
                call. = FALSE
            )
        }
    } else {
        stop(
            "[", context, "]",
            " Weird message...\n",
            "\u2192 Please contact the maintainer of the package with the error message:\n",
            msg,
            call. = FALSE
        )
    }
}
