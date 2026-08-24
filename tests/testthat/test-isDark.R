test_that("isDark works for bright colours", {
    expect_false(isDark("white"))
    expect_false(isDark("#00AEB7"))
})

test_that("isDark works for dark colour", {
    expect_true(isDark("black"))
    expect_true(isDark("#0800D6"))
})
