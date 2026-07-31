test_that("print works for IssueTB object", {
    expect_output(
        object = print(my_issues[1, ]),
        regexp = "Fix config import in ProvidersTopComponent"
    )
})

test_that("print works for IssuesTB object", {
    expect_output(
        object = print(my_issues[1:2, ]),
        regexp = "There are 2 issues."
    )
    expect_output(
        object = print(my_issues[1:2, ]),
        regexp = "Fix config import in ProvidersTopComponent"
    )
    expect_output(
        object = print(my_issues[1:2, ]),
        regexp = "Fix missing ARM-based packages in releases"
    )
})

test_that("print works for summary.IssueTB object", {
    expect_output(
        object = print(summary(my_issues[1, ])),
        regexp = "Fix config import in ProvidersTopComponent"
    )
    expect_output(
        object = print(summary(my_issues[1, ])),
        regexp = "Labels: bug"
    )
    expect_output(
        object = print(summary(my_issues[1, ])),
        regexp = "State: ✔ Completed"
    )
})

test_that("print works for summary.IssuesTB object", {
    expect_output(
        object = print(summary(my_issues[1:2, ])),
        regexp = "There are 2 issues."
    )
    expect_output(
        object = print(summary(my_issues[1:2, ])),
        regexp = "- jdemetra/jdplus-main#963 ✔ Completed"
    )
    expect_output(
        object = print(summary(my_issues[1:2, ])),
        regexp = "- jdemetra/jdplus-main#963 ✔ Completed"
    )
})

test_that("print works for summary.IssuesTB object with labels", {
    expect_output(
        object = print(summary(my_issues[1:2, ], with_labels = TRUE)),
        regexp = "bug"
    )
})

test_that("print works for summary.IssuesTB object without labels", {
    expect_error(
        expect_output(
            object = print(summary(my_issues[1:2, ], with_labels = FALSE)),
            regexp = "bug"
        )
    )
})
