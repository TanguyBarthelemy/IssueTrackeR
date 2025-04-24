test_that("simple_sort works", {
    skip_if_no_github()

    all_issues <- get_issues(source = "online",
                             state = "all",
                             owner = "jdemetra",
                             repo = "jdplus-main")
    all_milestones <- get_milestones(source = "online",
                                     owner = "jdemetra",
                                     repo = "jdplus-main")

    # with milestones
    issues_m <- IssueTrackeR:::simple_sort(
        issues = all_issues,
        milestones = all_milestones,
        sorting_variables = list(
            c(object = "milestones", field = "due_on"),
            c(object = "issues", field = "created_at")
        )
    )

    testthat::expect_type(issues_m, "list")
    testthat::expect_s3_class(issues_m, "IssuesTB")

    # without milestones
    issues_wm <- IssueTrackeR:::simple_sort(
        issues = all_issues,
        sorting_variables = list(
            c(object = "milestones", field = "due_on"),
            c(object = "issues", field = "created_at")
        ),
        source = "online",
        owner = "jdemetra",
        repo = "jdplus-main"
    )

    testthat::expect_type(issues_wm, "list")
    testthat::expect_s3_class(issues_wm, "IssuesTB")

    testthat::expect_identical(issues_m, issues_wm)
})
