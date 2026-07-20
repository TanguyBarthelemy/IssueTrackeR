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

testthat::test_that(".write works correctly", {
    test_obj <- list(a = 1, b = "test", c = TRUE)
    test_path <- tempfile(fileext = ".yaml")
    result <- .write(
        x = test_obj,
        dataset_dir = dirname(test_path),
        dataset_name = basename(test_path),
        overwrite = TRUE
    )
    expect_true(file.exists(test_path))
    expect_equal(result, normalizePath(test_path))

    result_no_overwrite <- .write(
        x = test_obj,
        dataset_dir = dirname(test_path),
        dataset_name = basename(test_path),
        overwrite = FALSE
    )
    expect_false(result_no_overwrite)
})
