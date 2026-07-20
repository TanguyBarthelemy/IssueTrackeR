my_dir <- tempdir()

test_that("writing works", {
    expect_true(write_to_dataset(my_issues, dataset_dir = my_dir))
    expect_true(write_to_dataset(my_labels, dataset_dir = my_dir))
    expect_true(write_to_dataset(my_milestones, dataset_dir = my_dir))
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
    print(test_path)
    result <- .write(
        x = test_obj,
        dataset_dir = dirname(test_path),
        dataset_name = basename(test_path),
        overwrite = TRUE
    )
    expect_true(file.exists(test_path))
    expect_equal(result, normalizePath(test_path))

    test_path2 <- normalizePath(tempfile(), mustWork = FALSE)
    print(test_path2)
    result2 <- .write(
        x = test_obj,
        dataset_dir = test_path2,
        overwrite = TRUE
    )
    expect_true(dir.exists(test_path2))
    print("On teste les path !")
    print("results :")
    print(result2)
    print(dput(result2))
    print("results (normalised):")
    print(normalizePath(result2))
    print(dput(normalizePath(result2)))
    print("le mien")
    print(dput(normalizePath(file.path(test_path2, "object.yaml"))))
    expect_equal(result2, normalizePath(file.path(test_path2, "object.yaml")))

    result_no_overwrite <- .write(
        x = test_obj,
        dataset_dir = dirname(test_path),
        dataset_name = basename(test_path),
        overwrite = FALSE
    )
    expect_false(result_no_overwrite)
})
