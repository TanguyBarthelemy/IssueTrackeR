test_that("plot return list of issues", {
    testthat::expect_identical(
        object = plot(my_issues, type = "historic"),
        expected = my_issues
    )
    testthat::expect_identical(
        object = plot(my_issues, type = "created-closed"),
        expected = my_issues
    )
    testthat::expect_identical(
        object = plot(my_issues, type = "resolution-time"),
        expected = my_issues
    )
    testthat::expect_identical(
        object = plot(my_issues, type = "author"),
        expected = my_issues
    )
})

test_that("plot fails if wrong type", {
    testthat::expect_error(
        plot(my_issues, type = "NULL")
    )
    testthat::expect_error(
        plot(my_issues, type = "wrong type")
    )
    testthat::expect_error(
        plot(my_issues, type = NA)
    )
})
