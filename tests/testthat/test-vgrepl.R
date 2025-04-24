test_that("multiplication works", {

    # Same result with one pattern
    output <- IssueTrackeR:::vgrepl(x = c("Bonne nuit", "Au revoir", "Bonjour"),
                          pattern = "Bon")[, 1]
    output1 <- grepl(x = c("Bonne nuit", "Au revoir", "Bonjour"), pattern = "Bon")
    expect_identical(object = output, expected = c(TRUE, FALSE, TRUE))
    expect_identical(object = output, expected = output1)

    # With multiple patterns
    output <- IssueTrackeR:::vgrepl(
        x = c("Bonne nuit", "Au revoir", "Bonjour"),
        pattern = c("Bon", "voir")
    )
    output_expected <- structure(
        c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
        dim = 3:2,
        dimnames = list(NULL, c("Bon", "voir"))
    )
    expect_identical(object = output, expected = output_expected)

})
