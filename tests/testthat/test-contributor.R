test_that("compute_contribution works for IssuesTB object", {
    # Use the global tested_issues from helper.R
    expect_issues(tested_issues)

    contributions <- compute_contribution(tested_issues)

    expect_s3_class(contributions, "ContributionsTB")
    expect_equal(nrow(contributions), 4)

    # Basic check to ensure it's not all zeros and contains expected row names
    expect_true(any(contributions > 0))
    expect_true(all(
        c("opener", "commenter", "closer", "assignee") %in%
            rownames(contributions)
    ))
})

test_that("compute_contribution with n parameter works", {
    # Use the global tested_issues from helper.R
    expect_issues(tested_issues)

    # Take a small subset to make 'n' meaningful
    subset_issues <- tested_issues[1:5, ]

    # Top 1 user
    contributions_top1 <- compute_contribution(subset_issues, n = 1)

    expect_true("other" %in% colnames(contributions_top1))
})

test_that("compute_contribution fails for non-IssuesTB object", {
    expect_error(
        compute_contribution(data.frame(a = 1:5)),
        "This function requires a IssuesTB object."
    )
})

test_that("regroup_vect works correctly", {
    x <- c(10, 8, 5, 3, 2, 1)

    # n = 2: top 2 kept, rest summed
    expect_equal(regroup_vect(x, n = 2), c(10, 8, other = 11))

    # n = Inf: no regrouping
    expect_equal(regroup_vect(x, n = Inf), x)

    # n > length: no regrouping
    expect_equal(regroup_vect(x, n = 10), x)
})
