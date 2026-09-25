test_that("plot return list of issues", {
    testthat::expect_identical(
        object = plot(tested_issues, type = "historic"),
        expected = tested_issues
    )
    testthat::expect_identical(
        object = plot(tested_issues, type = "created-closed"),
        expected = tested_issues
    )
    testthat::expect_identical(
        object = plot(tested_issues, type = "resolution-time"),
        expected = tested_issues
    )
    testthat::expect_identical(
        object = plot(tested_issues, type = "area-chart", by = "creator"),
        expected = tested_issues
    )
})

test_that("plot fails if wrong type", {
    testthat::expect_error(
        plot(tested_issues, type = "NULL")
    )
    testthat::expect_error(
        plot(tested_issues, type = "wrong type")
    )
    testthat::expect_error(
        plot(tested_issues, type = NA)
    )
})
