test_that("Checks for fail API call", {
    expect_error({
        check_response(try(
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
        ))
    })

    expect_error({
        check_response(try(
            {
                gh::gh(
                    endpoint = "/users/:owner",
                    owner = "Tanguyyyyyyyy",
                    .limit = Inf,
                    .progress = FALSE
                )
            },
            silent = TRUE
        ))
    })

    expect_error({
        check_response(try(
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
        ))
    })

    expect_error({
        check_response(try(
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
        ))
    })

    expect_error({
        check_response(try(
            {
                gh::gh(
                    endpoint = "/orgs/:owner/repos",
                    owner = "Tanguyyyyyyyy",
                    .limit = Inf,
                    .progress = FALSE
                )
            },
            silent = TRUE
        ))
    })
})

test_that("Checks for good call", {
    expect_null(
        object = check_response(
            gh::gh(
                repo = "IssueTrackeR",
                owner = "TanguyBarthelemy",
                endpoint = "/repos/:owner/:repo/issues",
                state = "all",
                .limit = Inf,
                .progress = FALSE
            )
        )
    )
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
