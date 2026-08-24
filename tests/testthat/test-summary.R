test_that("summary has good structure", {
    s1 <- summary(my_issues, with_labels = FALSE)
    s2 <- summary(my_issues, with_labels = TRUE)
    s3 <- summary(my_labels)

    testthat::expect_type(s1, "list")
    testthat::expect_s3_class(s1, "summary.IssuesTB")
    expect_identical(object = length(s1), 4L)
    expect_identical(object = s1$nbr_issues, 6L)
    expect_null(s1$label_display)

    testthat::expect_type(s2, "list")
    testthat::expect_s3_class(s2, "summary.IssuesTB")
    expect_identical(object = length(s2), 5L)
    expect_identical(object = s2$nbr_issues, 6L)
    expect_identical(object = length(s2$label_display), 6L)

    testthat::expect_type(s3, "list")
    testthat::expect_s3_class(s3, "summary.LabelsTB")
    # Testing column values
    expect_identical(
        s3[["name"]],
        c(
            "bug",
            "dependencies",
            "documentation",
            "duplicate",
            "enhancement",
            "github_actions",
            "good first issue",
            "help wanted",
            "invalid",
            "java",
            "question",
            "wontfix"
        )
    )
    expect_identical(
        s3[["description"]],
        c(
            "Something isn't working",
            "Pull requests that update a dependency file",
            "Improvements or additions to documentation",
            "This issue or pull request already exists",
            "New feature or request",
            "Pull requests that update GitHub Actions code",
            "Good for newcomers",
            "Extra attention is needed",
            "This doesn't seem right",
            "Pull requests that update Java code",
            "Further information is requested",
            "This will not be worked on"
        )
    )
    expect_identical(
        s3[["color"]],
        c(
            "#d73a4a",
            "#0366d6",
            "#0075ca",
            "#cfd3d7",
            "#a2eeef",
            "#000000",
            "#7057ff",
            "#008672",
            "#e4e669",
            "#ffa221",
            "#d876e3",
            "#ffffff"
        )
    )
    expect_identical(
        s3[["repo"]],
        c(
            "jdplus-main",
            "jdplus-main",
            "jdplus-main",
            "jdplus-main",
            "jdplus-main",
            "jdplus-main",
            "jdplus-main",
            "jdplus-main",
            "jdplus-main",
            "jdplus-main",
            "jdplus-main",
            "jdplus-main"
        )
    )
    expect_identical(
        s3[["owner"]],
        c(
            "jdemetra",
            "jdemetra",
            "jdemetra",
            "jdemetra",
            "jdemetra",
            "jdemetra",
            "jdemetra",
            "jdemetra",
            "jdemetra",
            "jdemetra",
            "jdemetra",
            "jdemetra"
        )
    )
    expect_identical(
        s3[["labels_bgcolor"]],
        c(
            "#d73a4a",
            "#0366d6",
            "#0075ca",
            "#cfd3d7",
            "#a2eeef",
            "#000000",
            "#7057ff",
            "#008672",
            "#e4e669",
            "#ffa221",
            "#d876e3",
            "#ffffff"
        )
    )
    expect_identical(
        s3[["labels_color"]],
        c(
            "ivory",
            "ivory",
            "ivory",
            "grey8",
            "grey8",
            "ivory",
            "ivory",
            "ivory",
            "grey8",
            "grey8",
            "grey8",
            "grey8"
        )
    )
    expect_identical(
        s3[["labels_url"]],
        c(
            "https://github.com/jdemetra/jdplus-main/labels/bug",
            "https://github.com/jdemetra/jdplus-main/labels/dependencies",
            "https://github.com/jdemetra/jdplus-main/labels/documentation",
            "https://github.com/jdemetra/jdplus-main/labels/duplicate",
            "https://github.com/jdemetra/jdplus-main/labels/enhancement",
            "https://github.com/jdemetra/jdplus-main/labels/github_actions",
            "https://github.com/jdemetra/jdplus-main/labels/good%20first%20issue",
            "https://github.com/jdemetra/jdplus-main/labels/help%20wanted",
            "https://github.com/jdemetra/jdplus-main/labels/invalid",
            "https://github.com/jdemetra/jdplus-main/labels/java",
            "https://github.com/jdemetra/jdplus-main/labels/question",
            "https://github.com/jdemetra/jdplus-main/labels/wontfix"
        )
    )
    expect_identical(
        s3[["formated_label"]],
        c(
            "bug",
            "dependencies",
            "documentation",
            "duplicate",
            "enhancement",
            "github_actions",
            "good first issue",
            "help wanted",
            "invalid",
            "java",
            "question",
            "wontfix"
        )
    )
    # Testing column names
    expect_identical(
        names(s3),
        c(
            "name",
            "description",
            "color",
            "repo",
            "owner",
            "labels_bgcolor",
            "labels_color",
            "labels_url",
            "formated_label"
        )
    )
    # Testing dimensions
    expect_identical(
        dim(s3),
        c(12L, 9L)
    )
})
