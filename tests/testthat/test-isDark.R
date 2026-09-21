test_that("is_dark works for bright colours", {
    expect_false(is_dark("white"))
    expect_false(is_dark("#00AEB7"))
})

test_that("is_dark works for dark colour", {
    expect_true(is_dark("black"))
    expect_true(is_dark("#0800D6"))
})
