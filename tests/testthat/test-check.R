test_that("Checks for fail API call", {
    expect_error({
        check_response(try(
            {
                gh::gh(
                    repo = "aa",
                    owner = "bb",
                    endpoint = "/repos/:owner/:repo/issues",
                    state = "all",
                    .limit = Inf
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
                    .limit = Inf
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
                    .limit = Inf
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
                    .limit = Inf
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
                    .limit = Inf
                )
            },
            silent = TRUE
        ))
    })
})

test_that("Checks for good call", {
    gh::gh(
        repo = "IssueTrackeR",
        owner = "TanguyBarthelemy",
        endpoint = "/repos/:owner/:repo/issues",
        state = "all",
        .limit = Inf
    )
})
