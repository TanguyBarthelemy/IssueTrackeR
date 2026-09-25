test_that("count_issues works for IssuesTB object", {
    expect_identical(count_issues(tested_issues[1, , drop = FALSE]), 1L)
    expect_identical(count_issues(tested_issues), 6L)
})
