check_result_path <- file.path(
    testthat::test_path("data"),
    "check-results.rds"
)
# check_results <- list(
#     x1 = try(
#         {
#             gh::gh(
#                 repo = "aa",
#                 owner = "bb",
#                 endpoint = "/repos/:owner/:repo/issues",
#                 state = "all",
#                 .limit = Inf,
#                 .progress = FALSE
#             )
#         },
#         silent = TRUE
#     ),
#     x2 = try(
#         {
#             gh::gh(
#                 repo = "aa",
#                 owner = "rjdverse",
#                 endpoint = "/repos/:owner/:repo/issues",
#                 state = "all",
#                 .limit = Inf,
#                 .progress = FALSE
#             )
#         },
#         silent = TRUE
#     ),
#     x3 = try(
#         {
#             gh::gh(
#                 endpoint = "/users/:owner",
#                 owner = "Tanguyyyyyyyy",
#                 .limit = Inf,
#                 .progress = FALSE
#             )
#         },
#         silent = TRUE
#     ),
#     x4 = try(
#         {
#             gh::gh(
#                 endpoint = "/orgs/:owner/repos",
#                 owner = "Tanguyyyyyyyy",
#                 .limit = Inf,
#                 .progress = FALSE
#             )
#         },
#         silent = TRUE
#     ),
#     x5 = try(
#         {
#             gh::gh(
#                 repo = "IssueTrackeR",
#                 owner = "TanguyBarthelemy",
#                 endpoint = "/repos/:owner/:repo/issues",
#                 state = "all",
#                 .limit = Inf,
#                 .progress = FALSE
#             )
#         },
#         silent = TRUE
#     )
# )
# saveRDS(check_results, check_result_path)
check_results <- readRDS(check_result_path)

test_that("Checks for fail API call", {
    expect_error(
        check_response(check_results[[1L]]),
        regexp = "The repository 'bb/aa' does not exist"
    )
    expect_error(
        check_response(check_results[[2L]]),
        regexp = "The repository 'rjdverse/aa' does not exist"
    )
    expect_error(
        check_response(check_results[[3L]]),
        regexp = "The user 'Tanguyyyyyyyy' does not exist"
    )
    expect_error(
        check_response(check_results[[4L]]),
        regexp = "The organization 'Tanguyyyyyyyy' does not exist"
    )
})

test_that("Checks for good call", {
    expect_null(check_response(check_results[[5L]]))
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

    expect_no_error(expect_error(
        check_response(x),
        regexp = "The requested resource was not found on GitHub"
    ))
})

test_that("check-helpers works", {
    # is_orgs_call
    expect_false(is_orgs_call("https://api.github.com/users/Tanguyyyyyyyy"))
    expect_true(is_orgs_call("https://api.github.com/orgs/Tanguyyyyyyyy"))

    # is_user_call
    expect_false(is_user_call("https://api.github.com/repos/rjdverse/aa/"))
    expect_true(is_user_call("https://api.github.com/users/Tanguyyyyyyyy"))

    # is_repo_call
    expect_false(is_repo_call("https://api.github.com/users/Tanguyyyyyyyy"))
    expect_true(is_repo_call("https://api.github.com/repos/rjdverse/aa/"))

    # Time out
    expect_false(has_timeout("No Timeout"))
    expect_true(has_timeout("Timeout was reached"))

    # Authentication
    expect_false(need_auth("Authentication ok"))
    expect_true(need_auth("Requires authentication"))

    # API
    expect_false(api_rate_reached("API rate ok"))
    expect_true(api_rate_reached("API rate limit exceeded"))

    # HTTP
    expect_false(has_no_http("HTTP request succeed"))
    expect_true(has_no_http("Failed to perform HTTP request"))

    # URL not found
    expect_false(is_not_found("URL found"))
    expect_true(is_not_found("URL not found"))

    # wrong_repo_msg
    expect_identical(
        object = wrong_repo_msg("a", "b"),
        expected = c(
            "The repository '",
            "a",
            "/",
            "b",
            "' does not exist\n",
            "\u2192 Or is not accessible on GitHub \U274C.\n",
            "\u2192 Verify that both owner and repo names are correct, ",
            "and that you have access rights."
        )
    )

    # wrong_username_msg
    expect_identical(
        object = wrong_username_msg("a"),
        expected = c(
            "The user '",
            "a",
            "' does not exist\n",
            "\u2192 Or is not accessible on GitHub \U274C.\n",
            "\u2192 Check that the username is correct."
        )
    )

    # wrong_org_name_msg
    expect_identical(
        object = wrong_org_name_msg("a"),
        expected = c(
            "The organization '",
            "a",
            "' does not exist\n",
            "\u2192 Or is not accessible on GitHub \U274C.\n",
            "\u2192 Check that the organization name is correct."
        )
    )
})
