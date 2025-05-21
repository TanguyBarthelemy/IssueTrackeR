skip_if_no_github <- function(has_scope = NULL) {
    testthat::skip_if_offline("github.com")
    testthat::skip_on_cran()

    if (gh::gh_token() == "") {
        testthat::skip("No GitHub token")
    }

    if (!is.null(has_scope) && !has_scope %in% test_scopes()) {
        msg <- cli::format_inline("Current token lacks '{has_scope}' scope")
        testthat::skip(msg)
    }

    if (gh::gh_rate_limit()$remaining == 0L) {
        testthat::skip("API rate limit exceeded")
    }
}

test_scopes <- function() {
    # whoami fails on GHA
    whoami <- rlang::env_cache(
        cache,
        "whoami",
        tryCatch(
            gh::gh_whoami(),
            error = function(err) list(scopes = "")
        )
    )
    strsplit(whoami$scopes, ", ", fixed = TRUE)[[1L]]
}

cache <- rlang::new_environment()
