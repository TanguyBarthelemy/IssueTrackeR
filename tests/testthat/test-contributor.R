test_that("compute_contribution works for IssuesTB object", {
    # Use the global tested_issues from helper.R
    expect_issues(tested_issues)

    contributions <- compute_contribution(tested_issues)

    # Testing class
    expect_s3_class(contributions, "ContributionsTB")

    # Testing type
    expect_type(contributions, type = "integer")
    # Testing column values
    expect_identical(
        contributions[, 1L],
        c(opener = 1L, commenter = 0L, closer = 0L, assignee = 0L)
    )
    expect_identical(
        contributions[, 2L],
        c(opener = 4L, commenter = 0L, closer = 4L, assignee = 3L)
    )
    expect_identical(
        contributions[, 3L],
        c(opener = 1L, commenter = 3L, closer = 2L, assignee = 1L)
    )
    expect_identical(
        contributions[, 4L],
        c(opener = 0L, commenter = 1L, closer = 0L, assignee = 0L)
    )
    # Testing column names
    expect_identical(
        colnames(contributions),
        c("AQLT", "charphi", "palatej", "ChristianeHofer")
    )
    # Testing row names
    expect_identical(
        rownames(contributions),
        c("opener", "commenter", "closer", "assignee")
    )
    # Testing dimensions
    expect_identical(
        dim(contributions),
        c(4L, 4L)
    )
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
    x <- c(10L, 8L, 5L, 3L, 2L, 1L)

    # n = 2: top 2 kept, rest summed
    expect_identical(regroup_vect(x, n = 2L), c(10L, 8L, other = 11L))

    # n = Inf: no regrouping
    expect_identical(regroup_vect(x, n = Inf), x)

    # n > length: no regrouping
    expect_identical(regroup_vect(x, n = 10L), x)

    # n > length: no regrouping
    expect_identical(regroup_vect(x, n = 6L), x)
})
