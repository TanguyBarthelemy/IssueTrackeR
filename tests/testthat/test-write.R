my_dir <- tempdir()

test_that("writing works", {
    expect_true(write_to_dataset(issues, dataset_dir = my_dir))
    expect_true(write_to_dataset(labels, dataset_dir = my_dir))
    expect_true(write_to_dataset(milestones, dataset_dir = my_dir))
    tmp_content <- list.files(
        path = my_dir,
        pattern = "*.yaml",
        recursive = FALSE,
        full.names = FALSE
    )
    expect_true(all(
        c("list_issues.yaml", "list_labels.yaml", "list_milestones.yaml") %in%
            tmp_content
    ))
})
