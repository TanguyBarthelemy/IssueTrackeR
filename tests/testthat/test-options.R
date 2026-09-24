test_that("options are set", {
    dataset_dir <- file.path(tempdir(), "data") |>
        normalizePath(mustWork = FALSE)

    # nolint start undesirable_function_linter
    options(IssueTrackeR.dataset.dir = "my_dir")
    options(IssueTrackeR.dataset.name = "new_name")
    # nolint end

    expect_identical(getOption("IssueTrackeR.dataset.dir"), "my_dir")
    expect_identical(getOption("IssueTrackeR.dataset.name"), "new_name")

    set.seed(5L)
    expect_null(reset_options())
    expect_identical(getOption("IssueTrackeR.dataset.dir"), dataset_dir)
    expect_identical(getOption("IssueTrackeR.dataset.name"), "bkyok")
})
