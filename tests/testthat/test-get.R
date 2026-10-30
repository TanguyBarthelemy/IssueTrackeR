test_that("get_issues works for GitHub", {
    local_mocked_bindings(
        .api_issues_gh = function(...) load_raw("raw_issues_gh"),
        .api_comments_gh = function(...) load_raw("raw_comments_gh")
    )

    selector <- init_selector(
        source = "GitHub",
        owner = "TanguyBarthelemy",
        repo = "IssueTrackeR"
    )
    issues <- get_issues(selector = selector, verbose = FALSE)
    expect_issues(issues)
    testthat::expect_identical(nrow(issues), 2L)
})

test_that("get_labels works for GitHub", {
    local_mocked_bindings(
        .api_labels_gh = function(...) load_raw("raw_labels_gh")
    )

    selector <- init_selector(
        source = "GitHub",
        owner = "TanguyBarthelemy",
        repo = "IssueTrackeR"
    )
    labels <- get_labels(selector = selector, verbose = FALSE)
    expect_labels(labels)
    testthat::expect_identical(nrow(labels), 2L)
})

test_that("get_milestones works for GitHub", {
    local_mocked_bindings(
        .api_milestones_gh = function(...) load_raw("raw_milestones_gh")
    )

    selector <- init_selector(
        source = "GitHub",
        owner = "TanguyBarthelemy",
        repo = "IssueTrackeR"
    )
    milestones <- get_milestones(selector = selector, verbose = FALSE)
    expect_milestones(milestones)
    testthat::expect_identical(nrow(milestones), 2L)
})

test_that("get_issues works for GitLab", {
    local_mocked_bindings(
        .api_issues_gl = function(...) load_raw("raw_issues_gl")
    )

    selector <- init_selector(source = "GitLab", project_id = 278964L)
    issues <- get_issues(selector = selector, verbose = FALSE)
    expect_issues(issues)
    testthat::expect_identical(nrow(issues), 2L)
})

test_that("get_labels works for GitLab", {
    local_mocked_bindings(
        .api_project_gl = function(...) load_raw("raw_project_gl"),
        .api_labels_gl = function(...) load_raw("raw_labels_gl")
    )

    selector <- init_selector(source = "GitLab", project_id = 278964L)
    labels <- get_labels(selector = selector, verbose = FALSE)
    expect_labels(labels)
    testthat::expect_identical(nrow(labels), 2L)
})

test_that("get_milestones works for GitLab", {
    local_mocked_bindings(
        .api_project_gl = function(...) load_raw("raw_project_gl"),
        .api_milestones_gl = function(...) load_raw("raw_milestones_gl")
    )

    selector <- init_selector(source = "GitLab", project_id = 278964L)
    milestones <- get_milestones(selector = selector, verbose = FALSE)
    expect_milestones(milestones)
    testthat::expect_identical(nrow(milestones), 2L)
})

test_that("get_issues with multiple repos works", {
    local_mocked_bindings(
        .api_owner_gh = function(...) {
            list(
                type = "Organization",
                login = "rjdverse"
            )
        },
        .api_repos_gh = function(...) {
            list(
                list(name = "repo-a"),
                list(name = "repo-b")
            )
        },
        .api_private_repos_gh = function(...) list(),
        .api_issues_gh = function(...) load_raw("raw_issues_gh"),
        .api_comments_gh = function(...) load_raw("raw_comments_gh")
    )

    selector <- init_selector(
        source = "GitHub",
        owner = "rjdverse",
        repo = NULL
    )
    issues <- get_issues(selector = selector, verbose = FALSE)
    expect_issues(issues)
    # 2 repositories x 2 issues
    testthat::expect_identical(nrow(issues), 4L)
})

test_that("get_issues_gh formats the GitHub API response", {
    local_mocked_bindings(
        .api_issues_gh = function(...) load_raw("raw_issues_gh"),
        .api_comments_gh = function(...) load_raw("raw_comments_gh")
    )

    issues <- get_issues_gh(
        owner = "TanguyBarthelemy",
        repo = "IssueTrackeR",
        verbose = FALSE
    )

    expect_issues(issues)
    expect_identical(nrow(issues), 2L)
    expect_identical(issues$title, c("First issue", "Second issue"))
    expect_identical(issues$number, c(1L, 2L))
    expect_identical(issues$state, c("open", "closed"))
    expect_identical(issues$owner, rep("TanguyBarthelemy", 2L))
    expect_identical(issues$repo, rep("IssueTrackeR", 2L))
    expect_identical(issues$creator, c("opener1", "opener2"))
    expect_identical(issues$assignee, c(NA_character_, "assignee1"))
    expect_identical(issues$closed_by, c(NA_character_, "closer1"))
    expect_identical(issues$milestone, c(NA_character_, "v1.0.0"))
    expect_identical(issues$state_reason, c(NA_character_, "completed"))
    expect_s3_class(issues$created_at, "POSIXct")
    expect_true(is.na(issues$closed_at[1L]))
    expect_false(is.na(issues$closed_at[2L]))
    expect_true(issues$created_at[2L] < issues$closed_at[2L])

    expect_identical(issues$labels[[1L]]$name, "bug")
    expect_identical(issues$labels[[1L]]$color, "#d73a4a")
    expect_identical(
        issues$labels[[2L]]$name,
        c("enhancement", "priority")
    )
    expect_identical(
        issues$labels[[2L]]$color,
        c("#a2eeef", "#0075ca")
    )

    expect_identical(
        issues$comments[[1L]]$text,
        "A comment on issue 1"
    )
    expect_identical(issues$comments[[1L]]$author, "commenter1")
    expect_identical(
        issues$comments[[2L]]$text,
        "A comment on issue 2"
    )
    expect_identical(issues$comments[[2L]]$author, "commenter2")
})

test_that("get_issues_gh returns empty issues on an empty API response", {
    local_mocked_bindings(
        .api_issues_gh = function(...) list()
    )

    issues <- get_issues_gh(
        owner = "TanguyBarthelemy",
        repo = "IssueTrackeR",
        verbose = FALSE
    )

    expect_issues(issues)
    expect_identical(nrow(issues), 0L)
})

test_that("get_issues_gl formats the GitLab API response", {
    local_mocked_bindings(
        .api_issues_gl = function(...) load_raw("raw_issues_gl")
    )

    issues <- get_issues_gl(project_id = 278964L, verbose = FALSE)

    expect_issues(issues)
    expect_identical(nrow(issues), 2L)
    expect_identical(issues$title, c("Gl issue one", "Gl issue two"))
    expect_identical(issues$number, c(1L, 2L))
    expect_identical(issues$state, c("open", "closed"))
    expect_identical(issues$creator, c("gl_opener1", "gl_opener2"))
    expect_identical(issues$assignee, c(NA_character_, "gl_assignee1"))
    expect_identical(issues$closed_by, c(NA_character_, "gl_closer1"))
    expect_identical(
        issues$milestone,
        c(NA_character_, "Gl milestone")
    )
    expect_identical(issues$owner, rep("gitlab-org", 2L))
    expect_identical(issues$repo, rep("gitlab", 2L))
    expect_s3_class(issues$created_at, "POSIXct")
    expect_true(is.na(issues$closed_at[1L]))
    expect_false(is.na(issues$closed_at[2L]))

    expect_identical(issues$labels[[1L]]$name, "bug")
    expect_identical(
        issues$labels[[2L]]$name,
        c("enhancement", "priority")
    )
})

test_that("get_labels_gh formats the GitHub API response", {
    local_mocked_bindings(
        .api_labels_gh = function(...) load_raw("raw_labels_gh")
    )

    labels <- get_labels_gh(
        owner = "TanguyBarthelemy",
        repo = "IssueTrackeR",
        verbose = FALSE
    )

    expect_labels(labels)
    expect_identical(nrow(labels), 2L)
    expect_identical(labels$name, c("bug", "enhancement"))
    expect_identical(
        labels$description,
        c("Something is not working", "")
    )
    expect_identical(labels$color, c("#d73a4a", "#a2eeef"))
    expect_identical(labels$source, rep("https://github.com/", 2L))
    expect_identical(labels$owner, rep("TanguyBarthelemy", 2L))
    expect_identical(labels$repo, rep("IssueTrackeR", 2L))
})

test_that("get_labels_gl formats the GitLab API response", {
    local_mocked_bindings(
        .api_project_gl = function(...) load_raw("raw_project_gl"),
        .api_labels_gl = function(...) load_raw("raw_labels_gl")
    )

    labels <- get_labels_gl(project_id = 278964L, verbose = FALSE)

    expect_labels(labels)
    expect_identical(nrow(labels), 2L)
    expect_identical(labels$name, c("bug", "enhancement"))
    expect_identical(
        labels$description,
        c("Something is not working", NA_character_)
    )
    expect_identical(labels$color, c("#d73a4a", "#a2eeef"))
    expect_identical(labels$source, rep("https://gitlab.com/", 2L))
    expect_identical(labels$owner, rep("gitlab-org", 2L))
    expect_identical(labels$repo, rep("gitlab", 2L))
})

test_that("get_milestones_gh formats the GitHub API response", {
    local_mocked_bindings(
        .api_milestones_gh = function(...) load_raw("raw_milestones_gh")
    )

    milestones <- get_milestones_gh(
        owner = "TanguyBarthelemy",
        repo = "IssueTrackeR",
        state = "all",
        verbose = FALSE
    )

    expect_milestones(milestones)
    expect_identical(nrow(milestones), 2L)
    expect_identical(milestones$title, c("v1.0.0", "v0.1.0"))
    expect_identical(milestones$state, c("open", "closed"))
    expect_identical(milestones$creator, rep("creator1", 2L))
    expect_identical(milestones$nb_issues_open, c(2L, 0L))
    expect_identical(milestones$nb_issues_closed, c(1L, 3L))
    expect_identical(milestones$source, rep("GitHub", 2L))
    expect_identical(milestones$owner, rep("TanguyBarthelemy", 2L))
    expect_identical(milestones$repo, rep("IssueTrackeR", 2L))
    expect_true(is.na(milestones$due_on[2L]))
    expect_true(is.na(milestones$closed_at[1L]))
})

test_that("get_milestones_gl formats the GitLab API response", {
    local_mocked_bindings(
        .api_project_gl = function(...) load_raw("raw_project_gl"),
        .api_milestones_gl = function(...) load_raw("raw_milestones_gl")
    )

    milestones <- get_milestones_gl(project_id = 278964L, verbose = FALSE)

    expect_milestones(milestones)
    expect_identical(nrow(milestones), 2L)
    expect_identical(milestones$title, c("v1.0.0", "v0.1.0"))
    expect_identical(milestones$state, c("active", "closed"))
    expect_identical(milestones$owner, rep("gitlab-org", 2L))
    expect_identical(milestones$repo, rep("gitlab", 2L))
    expect_identical(
        milestones$source,
        rep("https://gitlab.com/", 2L)
    )
    expect_true(is.na(milestones$due_on[2L]))
})

test_that("get_issues dispatches over a mixed selector without network", {
    local_mocked_bindings(
        .api_issues_gh = function(...) load_raw("raw_issues_gh"),
        .api_comments_gh = function(...) load_raw("raw_comments_gh"),
        .api_issues_gl = function(...) load_raw("raw_issues_gl")
    )

    selector <- merge_selector(
        init_selector(
            source = "GitHub",
            owner = "TanguyBarthelemy",
            repo = "IssueTrackeR"
        ),
        init_selector(source = "GitLab", project_id = 278964L),
        local_selector
    )

    issues <- get_issues(selector = selector, verbose = FALSE)

    expect_issues(issues)
    # 2 GitHub + 2 GitLab + 6 local issues
    expect_identical(nrow(issues), 10L)
    expect_identical(
        sort(issues$number),
        c(1L, 1L, 2L, 2L, 154L, 311L, 323L, 347L, 958L, 963L)
    )
})

test_that("get_issues generates error", {
    testthat::expect_error(
        object = {
            init_selector(
                source = "online",
                owner = "rjdverse",
                repo = "rjd3toolkito"
            )
        }
    )
    testthat::expect_error(
        object = {
            init_selector(
                source = "en-ligne",
                owner = "rjdverse",
                repo = "rjd3toolkit"
            )
        }
    )
    testthat::expect_error(
        object = init_selector(
            source = "local",
            state = "closed",
            owner = "rjdverse",
            repo = "rjd3toolkit"
        )
    )
    testthat::expect_error(
        object = init_selector(
            source = "local",
            state = "open",
            owner = "rjdverse",
            repo = "rjd3toolkit"
        )
    )
    testthat::expect_error(
        object = init_selector(
            source = "autre",
            owner = "rjdverse",
            repo = "rjd3toolkit"
        )
    )

    selector <- init_selector(
        source = "GitHub",
        state = "pas fraîche",
        owner = "rjdverse",
        repo = "rjd3toolkit"
    )
    testthat::expect_error(
        object = get_issues(selector = selector)
    )
})
