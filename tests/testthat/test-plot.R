test_that("plot return list of issues", {
    testthat::expect_identical(
        object = plot(issues, type = "historic"),
        expected = issues
    )
    testthat::expect_identical(
        object = plot(issues, type = "created-closed"),
        expected = issues
    )
})

test_that("plot fails if wrong type", {
    testthat::expect_error(
        plot(issues, type = "a")
    )
    testthat::expect_error(
        plot(issues, type = "wrong type")
    )
    testthat::expect_error(
        plot(issues, type = NA)
    )
})
