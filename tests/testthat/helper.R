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

    try_rate_limit <- try(gh::gh_rate_limit(), silent = TRUE)
    if (inherits(try_rate_limit, "try-error")) {
        testthat::skip("API cannot connect to GitHub")
    }

    if (try_rate_limit$remaining == 0L) {
        testthat::skip("API rate limit exceeded")
    }
}

skip_if_no_gitlab <- function(token) {
    testthat::skip_if_offline("gitlab.com")
    testthat::skip_on_cran()

    if (!nzchar(token)) {
        testthat::skip("No GitLab token")
    }

    # Test the API connection and authentication
    response <- tryCatch(
        httr2::request("https://gitlab.com/api/v4/user") |>
            httr2::req_headers(`PRIVATE-TOKEN` = token) |>
            httr2::req_error(is_error = function(resp) FALSE) |>
            httr2::req_perform(),
        error = function(e) NULL
    )

    if (is.null(response)) {
        testthat::skip("Cannot connect to GitLab API")
    }

    status <- httr2::resp_status(response)

    if (status == 401 || status == 403) {
        testthat::skip("GitLab token is invalid or lacks permissions")
    }

    if (status == 429L) {
        testthat::skip("GitLab API rate limit exceeded")
    }

    if (status >= 500L) {
        testthat::skip("GitLab API is temporarily unavailable")
    }

    if (status >= 400L) {
        testthat::skip("GitLab API request failed")
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

expect_issues <- function(x) {
    testthat::expect_type(x, "list")
    testthat::expect_s3_class(x, "IssuesTB")
    expect_identical(ncol(x), 17L)
    expect_in(x[["state"]], c("open", "closed"))
    testthat::expect_s3_class(x[["created_at"]], "POSIXct")
    testthat::expect_s3_class(x[["closed_at"]], "POSIXct")
    expect_in(
        x[["state_reason"]],
        c(
            "open",
            "reopened",
            "completed",
            "not_planned",
            "duplicated",
            "duplicate"
        )
    )
    expect_identical(
        names(x),
        c(
            "title",
            "body",
            "number",
            "state",
            "created_at",
            "closed_at",
            "closed_by",
            "labels",
            "comments",
            "milestone",
            "repo",
            "owner",
            "url",
            "html_url",
            "creator",
            "assignee",
            "state_reason"
        )
    )
}

expect_issue <- function(x) {
    testthat::expect_type(x, "list")
    testthat::expect_s3_class(x, "IssueTB")
    expect_length(x, 17L)
    expect_in(x[["state"]], c("open", "closed"))
    testthat::expect_s3_class(x[["created_at"]], "POSIXct")
    testthat::expect_s3_class(x[["closed_at"]], "POSIXct")
    expect_in(
        x[["state_reason"]],
        c(
            "open",
            "reopened",
            "completed",
            "not_planned",
            "duplicated",
            "duplicate"
        )
    )
    expect_identical(
        names(x),
        c(
            "title",
            "body",
            "number",
            "state",
            "created_at",
            "closed_at",
            "closed_by",
            "labels",
            "comments",
            "milestone",
            "repo",
            "owner",
            "url",
            "html_url",
            "creator",
            "assignee",
            "state_reason"
        )
    )
}

# selector <- init_selector(
#     source = "GitHub",
#     repo = "jdplus-main",
#     owner = "jdemetra",
#     state = "closed"
# )
# tested_issues <- get_issues(selector = selector, verbose = FALSE) |>
#     subset(number %in% c(963, 958, 347, 323, 311, 154))
# write(
#     x = tested_issues,
#     dataset_dir = testthat::test_path("data"),
#     dataset_name = NULL
# )

local_selector <- init_selector(
    source = "Local",
    dataset_dir = testthat::test_path("data"),
    dataset_name = NULL
)

tested_issues <- get_issues(local_selector, verbose = FALSE)
tested_labels <- get_labels(local_selector, verbose = FALSE)
tested_milestones <- get_milestones(local_selector, verbose = FALSE)
