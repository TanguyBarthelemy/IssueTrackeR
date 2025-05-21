test_that("format_timestamp works", {
    output_expected <- as.POSIXct("2025-04-03 17:37:54")
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
