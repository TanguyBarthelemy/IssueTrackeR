testthat::test_that("add_n_years correctly adds years to dates", {
    # Test 1: Add positive years
    date <- as.Date("2023-01-15")
    result <- add_n_years(date, n = 2)
    expected <- as.Date("2025-01-15")
    expect_identical(result, expected)

    # Test 2: Subtract years
    result <- add_n_years(date, n = -1)
    expected <- as.Date("2022-01-15")
    expect_identical(result, expected)

    # Test 3: Handle leap years (Feb 29)
    leap_day <- as.Date("2020-02-29")
    result <- add_n_years(leap_day, n = 1)
    expected <- as.Date("2021-03-01") # Non-leap year
    expect_identical(result, expected)

    # Test 4: Add multiple years to a leap day
    result <- add_n_years(leap_day, n = 4)
    expected <- as.Date("2024-02-29") # Leap year
    expect_identical(result, expected)

    # Test 5: Add years to a character date
    result <- add_n_years("2023-12-31", n = 3)
    expected <- as.Date("2026-12-31")
    expect_identical(result, expected)
})

testthat::test_that("add_n_years works with different format", {
    obj_chr <- "2023-01-15"
    obj_date <- as.Date(obj_chr)
    obj_POSIXct <- as.POSIXct(obj_chr)
    obj_POSIXlt <- as.POSIXlt(obj_chr)

    expect_identical(add_n_years(obj_chr, n = 2), as.Date("2025-01-15"))
    expect_identical(add_n_years(obj_date, n = 2), as.Date("2025-01-15"))
    expect_identical(add_n_years(obj_POSIXct, n = 2), as.Date("2025-01-15"))
    expect_identical(add_n_years(obj_POSIXlt, n = 2), as.Date("2025-01-15"))
})

testthat::test_that("add_n_years generates an error with invalid date", {
    expect_error(add_n_years("invalid_date", n = 1))
})

testthat::test_that("add_n_years generates an error with invalid n", {
    expect_error(add_n_years("2023-12-31", n = "bla"))
})
