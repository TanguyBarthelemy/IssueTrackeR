my_dir <- tempdir()

test_that("test update_database", {
    skip_if_no_github()
    selector <- init_selector(
        source = "GitHub",
        owner = "TanguyBarthelemy",
        repo = "IssueTrackeR",
        state = "all"
    )

    expect_true(update_database(selector = selector, dataset_dir = my_dir))
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
