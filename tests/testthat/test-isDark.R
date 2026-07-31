test_that("isDark works for bright colors", {
    expect_false(isDark("white"))
    expect_false(isDark("#00AEB7"))
})

test_that("isDark works for dark color", {
    expect_true(isDark("black"))
    expect_true(isDark("#0800D6"))
})
