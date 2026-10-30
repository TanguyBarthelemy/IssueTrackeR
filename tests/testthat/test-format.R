test_that("format_timestamp basic example", {
    output_expected <- as.POSIXct(
        1743694674.0,
        origin = "1970-01-01",
        tz = "UTC"
    )
    testthat::expect_identical(
        object = IssueTrackeR:::format_timestamp(1743694674.9),
        expected = output_expected
    )
    testthat::expect_identical(
        object = IssueTrackeR:::format_timestamp(1743694674L),
        expected = output_expected
    )
    IssueTrackeR:::format_timestamp(Sys.time())
})

test_that("Changing tz with format_timestamp", {
    output_expected <- as.POSIXct(
        1767311641.0,
        origin = "1970-01-01",
        tz = "UTC"
    )

    withr::with_timezone(tz = "Pacific/Johnston", code = {
        testthat::expect_no_error(object = {
            out1 <- IssueTrackeR:::format_timestamp(1767311641.0)
            out2 <- IssueTrackeR:::format_timestamp("2026-01-01 23:54:01")
        })
    })

    testthat::expect_identical(
        object = out1,
        expected = output_expected
    )
    testthat::expect_identical(
        object = out2,
        expected = output_expected
    )
})

test_that("format_comments_gh dispatches on gh_response", {
    raw <- load_raw("raw_comments_gh")
    expect_s3_class(raw, "gh_response")
    result <- format_comments_gh(raw)
    expect_s3_class(result, "data.frame")
    expect_named(result, c("number", "text", "author"))
})

test_that("format_labels_gh dispatches on gh_response", {
    raw <- load_raw("raw_labels_gh")
    expect_s3_class(raw, "gh_response")
    result <- format_labels_gh(raw, verbose = FALSE)
    expect_s3_class(result, "data.frame")
    expect_named(result, c("name", "description", "color"))
})

test_that("format_milestones_gh dispatches on gh_response", {
    raw <- load_raw("raw_milestones_gh")
    expect_s3_class(raw, "gh_response")
    result <- format_milestones_gh(raw, verbose = FALSE)
    expect_s3_class(result, "data.frame")
    expect_true("title" %in% names(result))
})

test_that("format_issues_gh dispatches on gh_response", {
    raw_issues <- load_raw("raw_issues_gh")
    raw_comments <- load_raw("raw_comments_gh")
    expect_s3_class(raw_issues, "gh_response")
    expect_s3_class(raw_comments, "gh_response")
    result <- format_issues_gh(
        x = raw_issues,
        raw_comments = raw_comments,
        verbose = FALSE
    )
    expect_s3_class(result, "IssuesTB")
})

test_that("format_issues_gh errors on non-gh_response class", {
    plain_list <- list(
        list(number = 1L, title = "test", state = "open")
    )
    expect_error(
        format_issues_gh(plain_list),
        regexp = "`x` should be a `gh_response` object"
    )
})

test_that("format_labels_gh errors on non-gh_response class", {
    plain_df <- data.frame(name = "test", stringsAsFactors = FALSE)
    expect_error(
        format_labels_gh(plain_df),
        regexp = "`x` should be a `gh_response` object"
    )
})

test_that("format_comments_gh errors on non-gh_response class", {
    expect_error(
        format_comments_gh(list(a = 1L)),
        regexp = "`x` should be a `gh_response` object"
    )
})

test_that("format_milestones_gh errors on non-gh_response class", {
    expect_error(
        format_milestones_gh(list(a = 1L)),
        regexp = "`x` should be a `gh_response` object"
    )
})
