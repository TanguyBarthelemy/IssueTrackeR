test_that("Checks for fail API call", {
        x1 <- try(
            {
                gh::gh(
                    repo = "aa",
                    owner = "bb",
                    endpoint = "/repos/:owner/:repo/issues",
                    state = "all",
                    .limit = Inf,
                    .progress = FALSE
                )
            },
            silent = TRUE
        )
    x2 <- try(
        {
            gh::gh(
                endpoint = "/users/:owner",
                owner = "Tanguyyyyyyyy",
                .limit = Inf,
                .progress = FALSE
            )
        },
        silent = TRUE
    )
    x3 <- try(
        {
            gh::gh(
                repo = "aa",
                owner = "bb",
                endpoint = "/repos/:owner/:repo/milestones",
                state = "all",
                .limit = Inf,
                .progress = FALSE
            )
        },
        silent = TRUE
    )
    x4 <- try(
        {
            gh::gh(
                repo = "aa",
                owner = "bb",
                endpoint = "/repos/:owner/:repo/labels",
                .limit = Inf,
                .progress = FALSE
            )
        },
        silent = TRUE
    )
    x5 <- try(
        {
            gh::gh(
                endpoint = "/orgs/:owner/repos",
                owner = "Tanguyyyyyyyy",
                .limit = Inf,
                .progress = FALSE
            )
        },
        silent = TRUE
    )

    expect_error(check_response(x1))
    expect_error(check_response(x2))
    expect_error(check_response(x3))
    expect_error(check_response(x4))
    expect_error(check_response(x5))
})

test_that("Checks for good call", {
    x6 <- try(
        {
            gh::gh(
                repo = "IssueTrackeR",
                owner = "TanguyBarthelemy",
                endpoint = "/repos/:owner/:repo/issues",
                state = "all",
                .limit = Inf,
                .progress = FALSE
            )
        },
        silent = TRUE
    )

    expect_null(check_response(x6))
})


test_that("Checks for missing info", {
    x <- structure(
        "URL not found",
        class = "try-error",
        condition = structure(
            list(body = c(x = "https://github.com/repos/bb/aa/")),
            class = "condition"
        )
    )

    expect_no_error(expect_error(check_response(x), regexp = "The repository .* does not exist"))
})
