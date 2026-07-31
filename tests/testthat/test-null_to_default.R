test_that("null_to_default works with scalar", {
    expect_identical(null_to_default(NULL, default = 4L), 4L)
    expect_identical(null_to_default(4L, default = NULL), 4L)
    expect_identical(null_to_default(NA, default = 4L), NA)
    expect_identical(null_to_default(NULL, default = NA), NA)
    expect_null(null_to_default(NULL, default = NULL))
})
test_that("null_to_default works as expected with vectors", {
    expect_identical(null_to_default(1:3, default = 4L), 1:3)
    expect_identical(null_to_default(NULL, default = 1:3), 1:3)
})
test_that("null_to_default works as expected with lists", {
    expect_identical(null_to_default(list(1, 10), default = 4L), list(1, 10))
    expect_identical(
        null_to_default(list(1, 10, NULL), default = 4L),
        list(1, 10, 4L)
    )
    expect_identical(null_to_default(list(1:10), default = 4L), list(1:10))
})
