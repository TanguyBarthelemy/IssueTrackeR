test_that("format_timestamp works", {

    output_expected <- 1743694674.0
    class(output_expected) <- c("POSIXct", "POSIXt")
    attr(x = output_expected, which = "tzone") <- ""

    testthat::expect_identical(
        object = IssueTrackeR:::format_timestamp(1743694674.9),
        expected = output_expected
    )
    testthat::expect_identical(
        object = IssueTrackeR:::format_timestamp(1743694674L),
        expected = output_expected
    )
    IssueTrackeR:::format_timestamp(Sys.time())
})
