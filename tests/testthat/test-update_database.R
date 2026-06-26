my_dir <- tempdir()

test_that("test update_database", {
    skip_if_no_github()
    expect_true(update_database(dataset_dir = my_dir))
    tmp_content <- list.files(
        path = my_dir,
        pattern = "*.yaml",
        recursive = FALSE,
        full.names = FALSE
    )
    expect_true(all(
        c(
            "closed_issues.yaml",
            "open_issues.yaml",
            "list_labels.yaml",
            "list_milestones.yaml"
        ) %in%
            tmp_content
    ))
})
