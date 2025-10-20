check_response <- function(x, context = "GitHub API call") {
    if (!inherits(x, "try-error")) {
        return(invisible(NULL))
    }

    cond <- attr(x, "condition")
    msg <- conditionMessage(cond)
    if (is.null(msg)) msg <- as.character(x)

    if (grepl("Timeout was reached", msg, ignore.case = TRUE)) {
        stop(
            "[", context, "]", " The GitHub API request timed out. 🕓\n",
            "→ Check your network connection or increase timeout options.\n",
            "→ Or wait a few seconds and try again.",
            call. = FALSE
        )
    } else if (grepl("Resource not accessible by integration", msg, ignore.case = TRUE)) {
        stop(
            "[", context, "]", " The GitHub token used does not have sufficient permissions 🔒.\n",
            "→ Try using a Personal Access Token (PAT) with 'repo' scope.",
            call. = FALSE
        )
    } else if (grepl("API rate limit exceeded", msg, ignore.case = TRUE)) {
        stop(
            "[", context, "]", " GitHub API rate limit exceeded ⏳.\n",
            "→ Wait a few minutes or authenticate with a PAT to increase your limit.",
            call. = FALSE
        )
    } else if (inherits(cond, "http_error_404") || grepl("URL not found", msg, ignore.case = TRUE)) {
        # Extraire l'URL depuis le message d'erreur
        url_line <- cond$body["x"]
        url <- sub(".*<8;;", "", url_line)
        url <- sub("\\a.*", "", url)
        url <- trimws(url)

        # Cas 1 : owner/repo non trouvé
        if (grepl("/repos/", url)) {
            repo_path <- sub("^.*/repos/", "", url)
            parts <- strsplit(repo_path, "/")[[1]]
            owner <- parts[1]
            repo  <- parts[2]

            stop(
                "[", context, "] ", "The repository '", owner, "/", repo,
                "' does not exist or is not accessible on GitHub ❌.\n",
                "→ Verify that both owner and repo names are correct, and that you have access rights.",
                call. = FALSE
            )

            # Cas 2 : utilisateur inexistant
        } else if (grepl("/users/", url)) {
            owner <- sub("^.*/users/", "", url)
            owner <- sub("\\?.*$", "", owner)

            stop(
                "[", context, "] ", "The user '", owner,
                "' does not exist or is not accessible on GitHub ❌.\n",
                "→ Check that the username is correct.",
                call. = FALSE
            )

            # Cas 3 : organisation inexistante
        } else if (grepl("/orgs/", url)) {
            owner <- sub("^.*/orgs/", "", url)
            owner <- sub("/.*$", "", owner)   # ✅ coupe tout après le nom de l’organisation
            owner <- sub("\\?.*$", "", owner)

            stop(
                "[", context, "] ", "The organization '", owner,
                "' does not exist or is not accessible on GitHub ❌.\n",
                "→ Check that the organization name is correct.",
                call. = FALSE
            )

            # Cas 4 : 404 générique
        } else {
            stop(
                "[", context, "] ", "The requested resource was not found on GitHub ❌.\n",
                "→ Check the API endpoint and parameters.",
                call. = FALSE
            )
        }
    } else if (grepl("URL not found", msg, ignore.case = TRUE)) {
        stop(
            "[", context, "]",
            # repo, " is not a repository from ", owner, ".\n",
            "The argument repo must be a valid GitHub repo name.",
            call. = FALSE
        )
    } else {
        stop(
            "[", context, "]",
            " Weird message...\n",
            "→ Please contact the maintainer of the package with the error message:\n",
            msg,
            call. = FALSE
        )
    }
}
