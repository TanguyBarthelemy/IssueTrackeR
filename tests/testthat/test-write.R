issues <- get_issues(
    source = "local",
    dataset_dir = testthat::test_path("data"),
    dataset_name = "closed_issues.yaml"
)
labels <- get_labels(
    source = "local",
    dataset_dir = testthat::test_path("data"),
    dataset_name = "list_labels.yaml"
)
milestones <- get_milestones(
    source = "local",
    dataset_dir = testthat::test_path("data"),
    dataset_name = "list_milestones.yaml"
)

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
