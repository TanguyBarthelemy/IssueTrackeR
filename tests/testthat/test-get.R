testthat::test_that("get works", {
    skip_if_no_github()

    selector <- init_selector(
        source = "GitHub",
        owner = "rjdverse",
        repo = "rjd3toolkit"
    )
    issues <- get_issues(selector = selector)
    testthat::expect_type(issues, "list")
    testthat::expect_s3_class(issues, "IssuesTB")

    labels <- get_labels(selector = selector)
    testthat::expect_type(labels, "list")
    testthat::expect_s3_class(labels, "LabelsTB")

    milestones <- get_milestones(selector = selector)
    testthat::expect_type(milestones, "list")
    testthat::expect_s3_class(milestones, "MilestonesTB")
})

testthat::test_that("get_issues with multiple repos works", {
    skip_if_no_github()

    selector <- init_selector(
        source = "GitHub",
        owner = "rjdverse",
        repo = NULL
    )
    issues <- get_issues(selector = selector)
    testthat::expect_type(issues, "list")
    testthat::expect_s3_class(issues, "IssuesTB")
})

testthat::test_that("get_issues generates error", {
    skip_if_no_github()

    testthat::expect_error(
        object = {
            init_selector(
                source = "online",
                owner = "rjdverse",
                repo = "rjd3toolkito"
            )
        }
    )
    testthat::expect_error(
        object = {
            init_selector(
                source = "en-ligne",
                owner = "rjdverse",
                repo = "rjd3toolkit"
            )
        }
    )
    testthat::expect_error(
        object = init_selector(
            source = "local",
            state = "closed",
            owner = "rjdverse",
            repo = "rjd3toolkit"
        )
    )
    testthat::expect_error(
        object = init_selector(
            source = "local",
            state = "open",
            owner = "rjdverse",
            repo = "rjd3toolkit"
        )
    )
    testthat::expect_error(
        object = init_selector(
            source = "autre",
            owner = "rjdverse",
            repo = "rjd3toolkit"
        )
    )

    selector <- init_selector(
        source = "GitHub",
        state = "pas fraîche",
        owner = "rjdverse",
        repo = "rjd3toolkit"
    )
    testthat::expect_error(
        object = get_issues(selector = selector)
    )
})
