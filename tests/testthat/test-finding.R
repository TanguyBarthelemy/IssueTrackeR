test_that("with_text works", {
    fix_issue <- with_text(my_issues, "fix")
    expect_issues(fix_issue)
    expect_identical(nrow(fix_issue), 1L)
    expect_identical(fix_issue[["title"]], "seasonal filter not shown in X-11")

    typo_issue <- with_text(my_issues, "typo")
    expect_issues(typo_issue)
    expect_identical(nrow(typo_issue), 0L)

    fix_issue2 <- with_text(my_issues, "fix", ignore.case = TRUE)
    expect_issues(fix_issue2)
    expect_identical(nrow(fix_issue2), 5L)

    fix_issue3 <- with_text(
        my_issues,
        "fix",
        ignore.case = TRUE,
        in_body = FALSE,
        in_comments = FALSE
    )
    expect_issues(fix_issue3)
    expect_identical(nrow(fix_issue3), 4L)

    awful_issue <- with_text(my_issues, "awful", in_body = FALSE)
    expect_issues(awful_issue)
    expect_identical(nrow(awful_issue), 1L)
})

test_that("with_labels works", {
    bug_issues <- with_labels(my_issues, "bug")
    expect_issues(bug_issues)
    expect_identical(nrow(bug_issues), 5L)
    expect_identical(bug_issues[["number"]], c(963L, 958L, 347L, 323L, 311L))
})

test_that("with_comments works", {
    commented_issues <- with_comments(my_issues)
    expect_issues(commented_issues)
    expect_identical(nrow(commented_issues), 2L)
    expect_identical(commented_issues[["number"]], c(323L, 154L))

    not_commented_issues <- with_comments(my_issues, negate = TRUE)
    expect_issues(not_commented_issues)
    expect_identical(nrow(not_commented_issues), 4L)
    expect_identical(
        not_commented_issues[["number"]],
        c(963L, 958L, 347L, 311L)
    )
})

test_that("get_nbr_comments works", {
    expect_identical(
        object = get_nbr_comments(my_issues),
        expected = c(0L, 0L, 0L, 1L, 0L, 3L)
    )
})

test_that("author_last_comment works", {
    expect_identical(
        object = author_last_comment(my_issues),
        expected = c("", "", "", "palatej", "", "palatej")
    )
    expect_identical(
        object = author_last_comment(my_issues[6L, ]),
        expected = "palatej"
    )
    expect_message(expect_identical(
        object = author_last_comment(my_issues[1, ]),
        expected = ""
    ))
})

test_that("extract_nth works", {
    expect_message(first_issue <- extract_nth(my_issues, 1L))
    expect_issue(first_issue)
    expect_identical(first_issue$number, 963L)
})
