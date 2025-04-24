test_that("multiplication works", {
    skip_if_no_github()

    all_issues <- get_issues(source = "online",
                             state = "all",
                             owner = "jdemetra",
                             repo = "jdplus-main")
    all_milestones <- get_milestones(owner = "jdemetra",
                                     repo = "jdplus-main")

    # with milestones
    issues <- IssueTrackeR:::simple_sort(
        issues = all_issues,
        milestones = all_milestones,
        sorting_variables = list(
            c(object = "milestones", field = "due_on"),
            c(object = "issues", field = "created_at")
        )
    )

    testthat::expect_type(issues, "list")
    testthat::expect_s3_class(issues, "IssuesTB")

    # without milestones
    issues <- IssueTrackeR:::simple_sort(
        issues = all_issues,
        sorting_variables = list(
            c(object = "milestones", field = "due_on"),
            c(object = "issues", field = "created_at")
        ),
        source = "local"
    )

    testthat::expect_type(issues, "list")
    testthat::expect_s3_class(issues, "IssuesTB")
})
