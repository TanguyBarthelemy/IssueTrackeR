# ---------------------------------------------------------------------------
# Live API integration tests. These exercise the .api_* layer directly
# against the real GitHub and GitLab APIs. There is no mocking, no get_* call
# and no init_selector here. Tests are skipped when there is no connection or
# no valid token.
# ---------------------------------------------------------------------------

test_that("GitHub API layer is reachable (live)", {
    skip_if_no_github()

    issues <- .api_issues_gh(
        owner = "rjdverse",
        repo = "rjd3toolkit",
        state = "open"
    )
    testthat::expect_type(issues, "list")

    labels <- .api_labels_gh(
        owner = "rjdverse",
        repo = "rjd3toolkit"
    )
    testthat::expect_type(labels, "list")

    milestones <- .api_milestones_gh(
        owner = "rjdverse",
        repo = "rjd3toolkit",
        state = "all"
    )
    testthat::expect_type(milestones, "list")
})

test_that("GitLab public API layer is reachable (live, no token)", {
    # Public project (Tractortom/onyxia-setup) on gitlab.com, accessed
    # without a token. GitLab only exposes the project and issues endpoints
    # unauthenticated: the labels and milestones endpoints return HTTP 401
    # without a token, even for public projects.
    skip_if_no_gitlab()
    gitlabr::set_gitlab_connection(
        gitlab_url = "https://gitlab.com",
        private_token = NULL
    )

    project <- .api_project_gl(project_id = 76570225L)
    testthat::expect_s3_class(project, "data.frame")
    testthat::expect_identical(project$name, "onyxia-setup")

    issues <- .api_issues_gl(project_id = 76570225L, state = "opened")
    testthat::expect_s3_class(issues, "data.frame")
})

# Two independent blocks: a skip on one instance must not cascade to the
# other. With a valid token, all endpoints (project, issues, labels,
# milestones) are accessible.

test_that("GitLab API layer is reachable (SSP Cloud, live)", {
    skip_if_not(nzchar(Sys.getenv("GITLAB_SSPCLOUD_API")))
    skip_if_no_gitlab(
        Sys.getenv("GITLAB_SSPCLOUD_API"),
        gitlab_url = "https://git.lab.sspcloud.fr"
    )
    gitlabr::set_gitlab_connection(
        gitlab_url = "https://git.lab.sspcloud.fr",
        private_token = Sys.getenv("GITLAB_SSPCLOUD_API")
    )

    testthat::expect_s3_class(
        .api_project_gl(project_id = 2075L),
        "data.frame"
    )
    testthat::expect_s3_class(
        .api_issues_gl(project_id = 2075L, state = "opened"),
        "data.frame"
    )
    testthat::expect_s3_class(
        .api_labels_gl(project_id = 2075L),
        "data.frame"
    )
    testthat::expect_s3_class(
        .api_milestones_gl(project_id = 2075L),
        "data.frame"
    )
})

test_that("GitLab API layer is reachable (gitlab.com, live)", {
    skip_if_not(nzchar(Sys.getenv("GITLAB_TRACTORTOM_API")))
    skip_if_no_gitlab(
        Sys.getenv("GITLAB_TRACTORTOM_API"),
        gitlab_url = "https://gitlab.com"
    )
    gitlabr::set_gitlab_connection(
        gitlab_url = "https://gitlab.com",
        private_token = Sys.getenv("GITLAB_TRACTORTOM_API")
    )

    testthat::expect_s3_class(
        .api_project_gl(project_id = 51699988L),
        "data.frame"
    )
    testthat::expect_s3_class(
        .api_issues_gl(project_id = 51699988L, state = "opened"),
        "data.frame"
    )
    testthat::expect_s3_class(
        .api_labels_gl(project_id = 51699988L),
        "data.frame"
    )
    testthat::expect_s3_class(
        .api_milestones_gl(project_id = 51699988L),
        "data.frame"
    )
})
