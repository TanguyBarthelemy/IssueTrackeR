closed_issues <- get_issues(
    source = "local",
    dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
    dataset_name = "closed_issues.yaml"
)
my_issues <- closed_issues[c(2L, 1L, 22L), ]
l <- list(
    data.frame(number = c(827L, 836L, 639L), row.names = c(2L, 1L, 22L)),
    list(
        data.frame(name = character(0), color = character(0)),
        data.frame(name = "bug", color = "#d73a4a"),
        data.frame(
            name = c("bug", "enhancement"),
            color = c("#d73a4a", "#a2eeef")
        )
    ),
    new_issue(
        number = 827L,
        title = "build process not including all v2 jar files",
        body = "Dear jdemetra team, \nAs jar file (demetra-tstoolkit-2.2.3 ) user available in v2, I would like a build process for the version 3 of the same file, or build instructions if we have to build it ourselves.\nThank you ",
        state = "closed",
        url = "https://api.github.com/repos/jdemetra/jdplus-main/issues/827",
        html_url = "https://github.com/jdemetra/jdplus-main/issues/827",
        milestone = NA_character_,
        created_at = as.POSIXct("2025-12-04"),
        closed_at = as.POSIXct("2025-12-08"),
        creator = "TakacsP",
        assignee = NA_character_,
        state_reason = "completed",
        owner = "jdemetra",
        repo = "jdplus-main",
        labels = data.frame(name = character(0), color = character(0)),
        comments = data.frame(
            text = 'Hi TakacsP. \n\nEvery jars are available on Maven Central: https://search.maven.org/artifact/eu.europa.ec.joinup.sat/jdplus-toolkit-base-api\nSee "structure" and "naming" at https://github.com/jdemetra/jdplus-main#structure to know which jar you need.\n\nTo build it youself, see the instructions at https://github.com/jdemetra/jdplus-main#developing',
            author = "charphi"
        )
    ),
    list(data.frame(name = character(0), color = character(0))),
    new_issue(
        number = 836L,
        title = "Action split-into-yearly-components fails on daily data",
        body = "Exception:\n\n```\njdplus.toolkit.base.api.timeseries.TsException: Incompatible frequencies\n\tat jdplus.toolkit.base.api.timeseries.TsDomain.aggregate(TsDomain.java:240)\n\tat jdplus.toolkit.desktop.plugin.components.parts.HasTsCollectionSupport$SplitCommand.yearsOf(HasTsCollectionSupport.java:581)\n\tat jdplus.toolkit.desktop.plugin.components.parts.HasTsCollectionSupport$SplitCommand.split(HasTsCollectionSupport.java:599)\n\tat jdplus.toolkit.desktop.plugin.components.parts.HasTsCollectionSupport$SplitCommand.execute(HasTsCollectionSupport.java:574)\n\tat jdplus.toolkit.desktop.plugin.components.parts.HasTsCollectionSupport$SplitCommand.execute(HasTsCollectionSupport.java:554)\n[catch] at ec.util.various.swing.JCommand.executeSafely(JCommand.java:80)\n```",
        state = "closed",
        url = "https://api.github.com/repos/jdemetra/jdplus-main/issues/836",
        html_url = "https://github.com/jdemetra/jdplus-main/issues/836",
        milestone = "3.7.0",
        created_at = as.POSIXct("2025-12-15"),
        closed_at = as.POSIXct("2025-12-16"),
        creator = "charphi",
        assignee = "charphi",
        state_reason = "completed",
        owner = "jdemetra",
        repo = "jdplus-main",
        labels = data.frame(name = "bug", color = "#d73a4a"),
        comments = data.frame(text = character(0), author = character(0))
    ),
    list(data.frame(
        name = c("bug", "enhancement"),
        color = c("#d73a4a", "#a2eeef")
    ))
)

test_that("[ function is good", {
    testthat::expect_identical(my_issues[], my_issues)
    testthat::expect_identical(my_issues[1], l[[1L]])
    testthat::expect_identical(
        my_issues["labels"],
        list(labels = l[[2L]]) |>
            structure(row.names = c(2L, 1L, 22L), class = "data.frame")
    )
    testthat::expect_identical(my_issues[1, ], l[[3L]])
    testthat::expect_identical(my_issues[1, , drop = TRUE], l[[3L]])
    testthat::expect_identical(
        my_issues[1, , drop = FALSE],
        new_issues(l[[3]]) |> structure(row.names = 2L)
    )

    testthat::expect_warning(testthat::expect_identical(
        my_issues[1, drop = TRUE],
        l[[1L]]
    ))
    testthat::expect_warning(testthat::expect_identical(
        my_issues[1, drop = FALSE],
        l[[1L]]
    ))
    testthat::expect_identical(my_issues[, "labels"], l[[2L]])
    testthat::expect_identical(my_issues[, 1], l[[1]][[1]])
    testthat::expect_identical(my_issues[, 1, drop = TRUE], l[[1]][[1]])
    testthat::expect_identical(my_issues[, 1, drop = FALSE], l[[1]])

    testthat::expect_identical(my_issues[1, "labels"], l[[4L]])
    testthat::expect_identical(my_issues[1, "labels", drop = TRUE], l[[4L]])
    testthat::expect_identical(
        my_issues[1, "labels", drop = FALSE],
        list(labels = l[[4L]]) |>
            structure(row.names = 2L, class = "data.frame")
    )

    testthat::expect_identical(my_issues[2, ], l[[5]])
    testthat::expect_identical(my_issues[3, "labels"], l[[6]])
    testthat::expect_identical(my_issues[3, "labels", drop = TRUE], l[[6]])
    testthat::expect_identical(
        my_issues[3, "labels", drop = FALSE],
        list(labels = l[[6]]) |>
            structure(row.names = 22L, class = "data.frame")
    )
    testthat::expect_identical(my_issues[3, 1], 639L)
    testthat::expect_identical(my_issues[3, 1, drop = TRUE], 639L)
    testthat::expect_identical(
        my_issues[3, 1, drop = FALSE],
        data.frame(number = 639L, row.names = 22L)
    )
})
