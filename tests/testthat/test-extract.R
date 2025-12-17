
all_issues <- get_issues(
    source = "local",
    dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
    dataset_name = "open_issues.yaml"
)
my_issues <- all_issues[c(3L, 2L, 40L), ]
l <- list(
    data.frame(number = c(758L, 761L, 562L), row.names = c(3L, 2L, 40L)),
    list(
        data.frame(name = character(0), color = character(0)),
        data.frame(name = "bug", color = "#d73a4a"),
        data.frame(name = c("documentation", "enhancement"), color = c("#0075ca", "#a2eeef"))
    ),
    new_issue(
        number = 758L,
        title = "Unify period display in GUI (2)",
        body = 'Issue #489 is unresolved. \nThe problem remains in version 3.5.1. \n\nAlso, the SI ratio display is still different and appears with Q1, Q2, Q3, Q4 :\n\n<img width="605" height="255" alt="Image" src="https://github.com/user-attachments/assets/49948a60-a027-4bed-8947-9e9eedc49537" />',
        state = "open",
        url = "https://api.github.com/repos/jdemetra/jdplus-main/issues/758",
        html_url = "https://github.com/jdemetra/jdplus-main/issues/758",
        milestone = NA_character_,
        created_at = as.POSIXct("2025-10-14 02:00:00"),
        closed_at = as.POSIXct(NA_character_),
        creator = "TanguyBarthelemy",
        assignee = NA_character_,
        state_reason = "open",
        owner = "jdemetra",
        repo = "jdplus-main",
        labels = data.frame(name = character(0), color = character(0)),
        comments = data.frame(
            text = c(
                "It was changed after the release of 3.5.1 so you have to check on the development branch.",
                "Ok, I will try the new dev version! Thank you\n\nSince the issue is still valid until a new release is made I leave this issue open."
            ),
            author = c("Immurb", "TanguyBarthelemy")
        )
    ),
    list(data.frame(name = character(0), color = character(0))),
    new_issue(
        number = 761L,
        title = "Reloading an xlsx files in Providers create new SA-Item",
        body = "We observe what appears to be a bug.\n\n### Description\n\nWhen reloading a data file in the Providers tab, new series are added to the open SA-Processing.\n\n### Procedure\n\n1) Open JDemetra+ v3.5.1\n2) Create a new WS\n3) Create a new SAP\n4) Add data from Providers to SAP\n5) Add new points to data file\n6) Re-add the data file under Providers (even if already present and with star)\n7) Click on \"Reload\"\n8) Series are added again to SAP\n\nHere's a short video describing the procedure:\n\nhttps://github.com/user-attachments/assets/323a11ee-9949-467d-8f43-79b4cdf8ab3b",
        state = "open",
        url = "https://api.github.com/repos/jdemetra/jdplus-main/issues/761",
        html_url = "https://github.com/jdemetra/jdplus-main/issues/761",
        milestone = NA_character_,
        created_at = as.POSIXct("2025-10-15 02:00:00"),
        closed_at = as.POSIXct(NA_character_),
        creator = "TanguyBarthelemy",
        assignee = NA_character_,
        state_reason = "open",
        owner = "jdemetra",
        repo = "jdplus-main",
        labels = data.frame(name = "bug", color = "#d73a4a"),
        comments = data.frame(
            text = "Just to add some additional information:\n\n1. Still happens in the develop branch\n2. Seems to duplicate only the last added items (last transfer) when the corresponding data file is reloaded\n3. Only happens once, so if you reload the same data file multiple times only one set of duplicates is added\n4. No new data is needed in the data file and it doesn't matter if you add any data files to the provider (Step 5 and 6)\n5. Seems to be related to the events fired in InternalTsProvider (so not only xlsx shows this behaviour)",
            author = "Immurb"
        )
    ),
    list(data.frame(name = c("documentation", "enhancement"), color = c("#0075ca", "#a2eeef")))
)

test_that("[ function is good", {

    testthat::expect_identical(my_issues[], my_issues)
    testthat::expect_identical(my_issues[1], l[[1L]])
    testthat::expect_identical(my_issues["labels"], list(labels = l[[2L]]) |> structure(row.names = c(3L, 2L, 40L), class = "data.frame"))
    testthat::expect_identical(my_issues[1, ], l[[3L]])
    testthat::expect_identical(my_issues[1,, drop = TRUE], l[[3L]])
    testthat::expect_identical(my_issues[1,, drop = FALSE], new_issues(l[[3]]) |> structure(row.names = 3L))

    testthat::expect_warning(testthat::expect_identical(my_issues[1, drop = TRUE], l[[1L]]))
    testthat::expect_warning(testthat::expect_identical(my_issues[1, drop = FALSE], l[[1L]]))
    testthat::expect_identical(my_issues[, "labels"], l[[2L]])
    testthat::expect_identical(my_issues[, 1], l[[1]][[1]])
    testthat::expect_identical(my_issues[, 1, drop = TRUE], l[[1]][[1]])
    testthat::expect_identical(my_issues[, 1, drop = FALSE], l[[1]])

    testthat::expect_identical(my_issues[1, "labels"], l[[4L]])
    testthat::expect_identical(my_issues[1, "labels", drop = TRUE], l[[4L]])
    testthat::expect_identical(my_issues[1, "labels", drop = FALSE], list(labels = l[[4L]]) |> structure(row.names = 3L, class = "data.frame"))

    testthat::expect_identical(my_issues[2,], l[[5]])
    testthat::expect_identical(my_issues[3, "labels"], l[[6]])
    testthat::expect_identical(my_issues[3, "labels", drop = TRUE], l[[6]])
    testthat::expect_identical(my_issues[3, "labels", drop = FALSE], list(labels = l[[6]]) |> structure(row.names = 40L, class = "data.frame"))
    testthat::expect_identical(my_issues[3, 1], 562L)
    testthat::expect_identical(my_issues[3, 1, drop = TRUE], 562L)
    testthat::expect_identical(my_issues[3, 1, drop = FALSE], data.frame(number = 562L, row.names = 40L))
})
