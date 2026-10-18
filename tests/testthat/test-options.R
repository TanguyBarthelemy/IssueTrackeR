test_that("set_default_dataset_dir works", {
    path <- "/tmp/test_dir"
    set_default_dataset_dir(path)
    expect_identical(get_default_dataset_dir(), path)
    expect_identical(getOption("IssueTrackeR.dataset.dir"), path)
})

test_that("set_default_dataset_name works", {
    set_default_dataset_name("my_name")
    expect_identical(get_default_dataset_name(), "my_name")
    expect_identical(getOption("IssueTrackeR.dataset.name"), "my_name")
})

test_that("set_default_selector works", {
    sel <- list(source = "GitHub", owner = "test", repo = "test")
    class(sel) <- c("SelectorTB", "list")

    set_default_selector(sel)
    expect_identical(get_default_selector(), sel)
    expect_identical(getOption("IssueTrackeR.selector"), sel)
})

