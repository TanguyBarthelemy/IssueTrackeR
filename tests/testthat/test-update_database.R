test_that("update_database works for GitHub without network", {
    local_mocked_bindings(
        .api_issues_gh = function(...) load_raw("raw_issues_gh"),
        .api_comments_gh = function(...) load_raw("raw_comments_gh"),
        .api_labels_gh = function(...) load_raw("raw_labels_gh"),
        .api_milestones_gh = function(...) load_raw("raw_milestones_gh")
    )

    tested_dir <- tempfile(pattern = "test-update_database")

    selector <- init_selector(
        source = "GitHub",
        owner = "TanguyBarthelemy",
        repo = "IssueTrackeR",
        state = "all"
    )

    expect_true(object = {
        update_database(
            selector = selector,
            dataset_dir = tested_dir,
            dataset_name = NULL,
            verbose = FALSE
        )
    })
    tmp_content <- list.files(
        path = tested_dir,
        pattern = "*.yaml",
        recursive = FALSE,
        full.names = FALSE
    )
    expect_true(all(
        c(
            "list_issues.yaml",
            "list_labels.yaml",
            "list_milestones.yaml"
        ) %in%
            tmp_content
    ))
})

test_that("update_database works for GitLab without network", {
    local_mocked_bindings(
        .api_issues_gl = function(...) load_raw("raw_issues_gl"),
        .api_project_gl = function(...) load_raw("raw_project_gl"),
        .api_labels_gl = function(...) load_raw("raw_labels_gl"),
        .api_milestones_gl = function(...) load_raw("raw_milestones_gl")
    )

    tested_dir <- tempfile(pattern = "test-update_database-gl")

    selector <- init_selector(source = "GitLab", project_id = 278964L)

    expect_true(object = {
        update_database(
            selector = selector,
            dataset_dir = tested_dir,
            dataset_name = NULL,
            verbose = FALSE
        )
    })
    tmp_content <- list.files(
        path = tested_dir,
        pattern = "*.yaml",
        recursive = FALSE,
        full.names = FALSE
    )
    expect_true(all(
        c(
            "list_issues.yaml",
            "list_labels.yaml",
            "list_milestones.yaml"
        ) %in%
            tmp_content
    ))
})
