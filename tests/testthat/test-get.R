testthat::test_that("get works", {
    skip_if_no_github()

    issues <- get_issues(
        source = "online",
        state = "all",
        owner = "rjdverse",
        repo = "rjd3toolkit"
    )
    testthat::expect_type(issues, "list")
    testthat::expect_s3_class(issues, "IssuesTB")

    labels <- get_labels(
        source = "online",
        owner = "rjdverse",
        repo = "rjd3toolkit"
    )
    testthat::expect_type(labels, "list")
    testthat::expect_s3_class(labels, "LabelsTB")

    milestones <- get_milestones(
        source = "online",
        state = "all",
        owner = "rjdverse",
        repo = "rjd3toolkit"
    )
    testthat::expect_type(milestones, "list")
    testthat::expect_s3_class(milestones, "MilestonesTB")
})

testthat::test_that("get_issues with multiple repos works", {
    skip_if_no_github()

    issues <- get_issues(
        source = "online",
        state = "all",
        owner = "TractorTom",
        repo = NULL
    )
    testthat::expect_type(issues, "list")
    testthat::expect_s3_class(issues, "IssuesTB")
})

testthat::test_that("get_issues generates error", {
    skip_if_no_github()

    testthat::expect_error(
        object = get_issues(
            source = "online",
            state = "all",
            owner = "rjdverse",
            repo = "rjd3toolkito"
        )
    )
    testthat::expect_error(
        object = get_issues(
            source = "en-ligne",
            state = "all",
            owner = "rjdverse",
            repo = "rjd3toolkit"
        )
    )
    testthat::expect_error(
        object = get_issues(
            source = "local",
            state = "closed",
            owner = "rjdverse",
            repo = "rjd3toolkit"
        )
    )
    testthat::expect_error(
        object = get_issues(
            source = "local",
            state = "open",
            owner = "rjdverse",
            repo = "rjd3toolkit"
        )
    )
    testthat::expect_error(
        object = get_issues(
            source = "online",
            state = "pas fraîche",
            owner = "rjdverse",
            repo = "rjd3toolkit"
        )
    )
    testthat::expect_error(
        object = get_issues(
            source = "autre",
            state = "pas fraîche",
            owner = "rjdverse",
            repo = "rjd3toolkit"
        )
    )
})
