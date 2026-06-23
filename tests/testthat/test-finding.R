issues <- get_issues(
    source = "local",
    dataset_dir = testthat::test_path("data"),
    dataset_name = "closed_issues.yaml"
)

test_that("with_text works", {
    fix_issue <- with_text(issues, "fix")
    expect_issues(fix_issue)
    expect_identical(nrow(fix_issue), 1L)
    expect_identical(fix_issue[["title"]], "seasonal filter not shown in X-11")

    typo_issue <- with_text(issues, "typo")
    expect_issues(typo_issue)
    expect_identical(nrow(typo_issue), 0L)

    fix_issue2 <- with_text(issues, "fix", ignore.case = TRUE)
    expect_issues(fix_issue2)
    expect_identical(nrow(fix_issue2), 5L)

    fix_issue3 <- with_text(
        issues,
        "fix",
        ignore.case = TRUE,
        in_body = FALSE,
        in_comments = FALSE
    )
    expect_issues(fix_issue3)
    expect_identical(nrow(fix_issue3), 4L)

    awful_issue <- with_text(issues, "awful", in_body = FALSE)
    expect_issues(awful_issue)
    expect_identical(nrow(awful_issue), 1L)
})
