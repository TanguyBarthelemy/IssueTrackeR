my_dir <- tempfile(pattern = "test-update_database")

test_that("test update_database", {
    skip_if_no_github()
    selector <- init_selector(
        source = "GitHub",
        owner = "TanguyBarthelemy",
        repo = "IssueTrackeR",
        state = "all"
    )

    expect_true(object = {
        update_database(
            selector = selector,
            dataset_dir = my_dir,
            dataset_name = NULL
        )
    })
    tmp_content <- list.files(
        path = my_dir,
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
