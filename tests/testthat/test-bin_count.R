testthat::test_that("bin_count correctly counts observations in date bins", {
    # Test 1: Basic binning with predefined dates
    dates <- as.Date(c("2023-01-01", "2023-02-01", "2023-03-01"))
    x <- as.Date(c(
        "2023-01-15",
        "2023-01-20",
        "2023-02-10",
        "2023-02-28",
        "2023-03-15",
        "2023-03-20"
    ))
    result <- bin_count(x, dates)
    expected <- c(2L, 2L, 2L)
    expect_identical(result, expected)

    # Test 2: Empty input
    result <- bin_count(as.Date(character()), dates)
    expect_identical(result, rep(0L, length(dates)))

    # Test 3: Observations in the last bin (extended by 31 days)
    dates <- as.Date(c("2023-03-01", "2023-04-01"))
    x <- as.Date(c("2023-03-15", "2023-04-10", "2023-04-20"))
    result <- bin_count(x, dates)
    expected <- c(1L, 2L)
    expect_identical(result, expected)

    # Test 4: NA values in input
    x_with_na <- c(x, NA)
    result <- bin_count(x_with_na, dates)
    expect_identical(result, expected)

    # Test 5: Error for invalid dates
    expect_error(bin_count("invalid_date", dates))
})

testthat::test_that("bin_count works with different format", {
    obj_chr <- c("2023-03-15", "2023-04-10", "2023-04-20")
    obj_date <- as.Date(obj_chr)
    obj_POSIXct <- as.POSIXct(obj_chr)
    obj_POSIXlt <- as.POSIXlt(obj_chr)

    dates <- seq.Date(
        from = as.Date("2023-03-01"),
        length.out = 41L,
        by = "month"
    )

    expect_identical(
        bin_count(obj_chr, dates = dates),
        rep(c(1L, 2L, 0L), c(1L, 1L, 39L))
    )
    expect_identical(
        bin_count(obj_date, dates = dates),
        rep(c(1L, 2L, 0L), c(1L, 1L, 39L))
    )
    expect_identical(
        bin_count(obj_POSIXct, dates = dates),
        rep(c(1L, 2L, 0L), c(1L, 1L, 39L))
    )
    expect_identical(
        bin_count(obj_POSIXlt, dates = dates),
        rep(c(1L, 2L, 0L), c(1L, 1L, 39L))
    )
})

testthat::test_that("bin_count generates an error with invalid x", {
    expect_error(bin_count("invalid_date", dates = as.Date("2023-04-20")))
})

testthat::test_that("bin_count generates an error with invalid dates", {
    expect_error(bin_count("2023-12-31", as.POSIXlt(c("2023-04-01"))))
    expect_error(bin_count("2023-12-31", as.POSIXct(c("2023-04-01"))))
    expect_error(bin_count("2023-12-31", dates = "2023-04-01"))
    expect_error(bin_count("2023-12-31", dates = "bla"))
})
