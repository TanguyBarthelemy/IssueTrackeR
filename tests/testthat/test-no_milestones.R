testthat::test_that("no_milestones works", {
    skip_if_no_github()

    all_issues <- get_issues(source = "online",
                             state = "all",
                             owner = "rjdverse",
                             repo = "rjd3toolkit")
    issues <- IssueTrackeR:::no_milestones(all_issues)

    testthat::expect_type(issues, "list")
    testthat::expect_s3_class(issues, "IssuesTB")
})
