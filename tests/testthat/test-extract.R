test_that("issues are issues", {
    expect_issues(my_issues)
    expect_issues(my_issues[0, ])
})

my_issues2 <- my_issues[c(1L, 4L, 6L), ]

l <- list(
    data.frame(number = c(963L, 323L, 154L), row.names = c(1L, 4L, 6L)),
    list(
        data.frame(name = "bug", color = "#d73a4a"),
        data.frame(name = "bug", color = "#d73a4a"),
        data.frame(name = character(0), color = character(0))
    ),
    new_issue(
        number = 963L,
        title = "Fix config import in ProvidersTopComponent",
        body = "Config import in ProvidersTopComponent doesn't work anymore. Export is OK.",
        state = "closed",
        url = "https://api.github.com/repos/jdemetra/jdplus-main/issues/963",
        html_url = "https://github.com/jdemetra/jdplus-main/issues/963",
        milestone = "3.8.0",
        created_at = as.POSIXct("2026-04-24", tz = "UTC"),
        closed_at = as.POSIXct("2026-04-24", tz = "UTC"),
        creator = "charphi",
        assignee = "charphi",
        state_reason = "completed",
        owner = "jdemetra",
        repo = "jdplus-main",
        labels = data.frame(name = "bug", color = "#d73a4a"),
        comments = data.frame(text = character(0), author = character(0))
    ),
    list(data.frame(name = "bug", color = "#d73a4a")),
    new_issue(
        number = 323L,
        title = "User-defined variables with lags",
        body = "User-defined variables with lags are not included in the model",
        state = "closed",
        url = "https://api.github.com/repos/jdemetra/jdplus-main/issues/323",
        html_url = "https://github.com/jdemetra/jdplus-main/issues/323",
        milestone = "3.2.3",
        created_at = as.POSIXct("2024-06-13", tz = "UTC"),
        closed_at = as.POSIXct("2024-06-13", tz = "UTC"),
        creator = "palatej",
        assignee = "palatej",
        state_reason = "completed",
        owner = "jdemetra",
        repo = "jdplus-main",
        labels = data.frame(name = "bug", color = "#d73a4a"),
        comments = data.frame(
            text = "Solved in #324 ",
            author = "palatej"
        )
    ),
    list(data.frame(name = character(0), color = character(0)))
)

test_that("[ function is good", {
    testthat::expect_identical(my_issues2[], my_issues2)
    testthat::expect_identical(my_issues2[1], l[[1L]])
    testthat::expect_identical(
        my_issues2["labels"],
        list(labels = l[[2L]]) |>
            structure(row.names = c(1L, 4L, 6L), class = "data.frame")
    )
    testthat::expect_identical(my_issues2[1, ], l[[3L]])
    testthat::expect_identical(my_issues2[1, , drop = TRUE], l[[3L]])
    testthat::expect_identical(
        my_issues2[1, , drop = FALSE],
        new_issues(l[[3]])
    )

    testthat::expect_warning(testthat::expect_identical(
        my_issues2[1, drop = TRUE],
        l[[1L]]
    ))
    testthat::expect_warning(testthat::expect_identical(
        my_issues2[1, drop = FALSE],
        l[[1L]]
    ))
    testthat::expect_identical(my_issues2[, "labels"], l[[2L]])
    testthat::expect_identical(my_issues2[, 1], l[[1]][[1]])
    testthat::expect_identical(my_issues2[, 1, drop = TRUE], l[[1]][[1]])
    testthat::expect_identical(my_issues2[, 1, drop = FALSE], l[[1]])

    testthat::expect_identical(my_issues2[1, "labels"], l[[4L]])
    testthat::expect_identical(my_issues2[1, "labels", drop = TRUE], l[[4L]])
    testthat::expect_identical(
        my_issues2[1, "labels", drop = FALSE],
        list(labels = l[[4L]]) |>
            structure(row.names = 1L, class = "data.frame")
    )

    testthat::expect_identical(my_issues2[2, ], l[[5]])
    testthat::expect_identical(my_issues2[3, "labels"], l[[6]])
    testthat::expect_identical(my_issues2[3, "labels", drop = TRUE], l[[6]])
    testthat::expect_identical(
        my_issues2[3, "labels", drop = FALSE],
        list(labels = l[[6]]) |>
            structure(row.names = 6L, class = "data.frame")
    )
    testthat::expect_identical(my_issues2[3, 1], 154L)
    testthat::expect_identical(my_issues2[3, 1, drop = TRUE], 154L)
    testthat::expect_identical(
        my_issues2[3, 1, drop = FALSE],
        data.frame(number = 154L, row.names = 6L)
    )
})
