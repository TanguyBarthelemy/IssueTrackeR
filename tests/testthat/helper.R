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
            "number",
            "title",
            "body",
            "state",
            "url",
            "html_url",
            "milestone",
            "created_at",
            "closed_at",
            "closed_by",
            "creator",
            "assignee",
            "state_reason",
            "owner",
            "repo",
            "labels",
            "comments"
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
            "number",
            "title",
            "body",
            "state",
            "url",
            "html_url",
            "milestone",
            "created_at",
            "closed_at",
            "closed_by",
            "creator",
            "assignee",
            "state_reason",
            "owner",
            "repo",
            "labels",
            "comments"
        )
    )
}

# my_issues <- get_issues(
#     source = "online",
#     repo = "jdplus-main",
#     owner = "jdemetra",
#     state = "all"
# ) |>
#     subset(number %in% c(963, 958, 347, 323, 311, 154))
# write_to_dataset(
#     x = my_issues,
#     dataset_dir = testthat::test_path("data"),
#     dataset_name = "closed_issues.yaml"
# )

my_issues <- get_issues(
    source = "local",
    dataset_dir = testthat::test_path("data"),
    dataset_name = "closed_issues.yaml"
)
my_labels <- get_labels(
    source = "local",
    dataset_dir = testthat::test_path("data"),
    dataset_name = "list_labels.yaml"
)
my_milestones <- get_milestones(
    source = "local",
    dataset_dir = testthat::test_path("data"),
    dataset_name = "list_milestones.yaml"
)
