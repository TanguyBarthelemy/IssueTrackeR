test_that("format_timestamp basic example", {
    output_expected <- as.POSIXct(
        1743694674.0,
        origin = "1970-01-01",
        tz = "UTC"
    )
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

test_that("Changing tz with format_timestamp", {
    output_expected <- as.POSIXct(
        1767311641.0,
        origin = "1970-01-01",
        tz = "UTC"
    )

    withr::with_timezone(tz = "Pacific/Johnston", code = {
        testthat::expect_no_error(object = {
            out1 <- IssueTrackeR:::format_timestamp(1767311641.0)
            out2 <- IssueTrackeR:::format_timestamp("2026-01-01 23:54:01")
        })
    })

    testthat::expect_identical(
        object = out1,
        expected = output_expected
    )
    testthat::expect_identical(
        object = out2,
        expected = output_expected
    )
})
