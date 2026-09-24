my_dir <- tempfile(pattern = "test-write")

test_that("writing works", {
    expect_true(object = {
        write(my_issues, dataset_dir = my_dir, dataset_name = NULL)
    })
    expect_true(object = {
        write(my_labels, dataset_dir = my_dir, dataset_name = NULL)
    })
    expect_true(object = {
        write(my_milestones, dataset_dir = my_dir, dataset_name = NULL)
    })
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
    yaml_tmp <- tempfile(fileext = ".yaml")

    result <- .write(
        x = test_obj,
        dataset_dir = dirname(yaml_tmp),
        dataset_name = basename(yaml_tmp),
        overwrite = TRUE
    )
    expect_true(file.exists(yaml_tmp))
    expect_equal(result, normalizePath(yaml_tmp))

    data_tmp_dir <- tempfile("data")
    result2 <- .write(
        x = test_obj,
        dataset_dir = data_tmp_dir,
        overwrite = TRUE
    )
    expect_true(dir.exists(data_tmp_dir))
    expect_equal(result2, normalizePath(file.path(data_tmp_dir, "object.yaml")))

    result_no_overwrite <- .write(
        x = test_obj,
        dataset_dir = dirname(yaml_tmp),
        dataset_name = basename(yaml_tmp),
        overwrite = FALSE
    )
    expect_false(result_no_overwrite)
})
