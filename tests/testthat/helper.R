skip_if_no_github <- function(has_scope = NULL) {
    testthat::skip_if_offline("github.com")
    testthat::skip_on_cran()

    if (gh::gh_token() == "") {
        testthat::skip("No GitHub token")
    }

    if (!is.null(has_scope) && !has_scope %in% test_scopes()) {
        testthat::skip(cli::format_inline("Current token lacks '{has_scope}' scope"))
    }
}

test_scopes <- function() {
    # whoami fails on GHA
    whoami <- env_cache(cache, "whoami", tryCatch(
        gh_whoami(),
        error = function(err) list(scopes = "")
    ))
    strsplit(whoami$scopes, ", ")[[1]]
}

cache <- rlang::new_environment()
