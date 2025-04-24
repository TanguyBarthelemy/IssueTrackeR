test_that("format_timestamp works", {
    IssueTrackeR:::format_timestamp(1743694674.9)
    IssueTrackeR:::format_timestamp(1743694674L)
    IssueTrackeR:::format_timestamp(Sys.time())
})
