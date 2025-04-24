test_that("logical_reducer works", {

    output <- IssueTrackeR:::logical_reducer(
        c(TRUE, FALSE, TRUE, FALSE), c(FALSE, TRUE, FALSE, FALSE),
        logic_gate = "AND",
        orientation = "vector-wise"
    )
    expect_identical(output, rep(FALSE, 4L))

    output <- IssueTrackeR:::logical_reducer(
        FALSE, c(TRUE, FALSE, TRUE), c(FALSE, TRUE, FALSE),
        logic_gate = "OR",
        orientation = "overall"
    )
    expect_true(object = output)

})
