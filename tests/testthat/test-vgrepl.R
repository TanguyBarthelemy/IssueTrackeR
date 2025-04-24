test_that("vgrepl works", {

    # Same result with one pattern
    output <- IssueTrackeR:::vgrepl(x = c("Bonne nuit", "Au revoir", "Bonjour"),
                                    pattern = "Bon")[, 1L]
    output1 <- grepl(x = c("Bonne nuit", "Au revoir", "Bonjour"),
                     pattern = "Bon", fixed = TRUE)
    expect_identical(object = output, expected = c(TRUE, FALSE, TRUE))
    expect_identical(object = output, expected = output1)

    # With multiple patterns
    output <- IssueTrackeR:::vgrepl(
        x = c("Bonne nuit", "Au revoir", "Bonjour"),
        pattern = c("Bon", "voir")
    )
    output_expected <- matrix(
        c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
        nrow = 3L,
        dimnames = list(NULL, c("Bon", "voir"))
    )
    expect_identical(object = output, expected = output_expected)

})
