d <- function(x) {
    s <- summary(x) |> do.call(what = cbind)
    v <- x |> dplyr::select(html_url, creator)
    x <- merge(s, x, by = "html_url") |> dplyr::arrange(creator)

    cat(paste0("\n- ", cli::style_hyperlink(
        text = x[["issue_desc"]],
        url = x[["html_url"]]), " by ",
        x[["creator"]]),
        "\n")
    return(invisible(x))
}

with_comment <- function(x) {
    nbr_comment <- x$comments |> lapply(nrow) |> as.numeric()
    return(x[nbr_comment > 0, ])
}

author_last_comment <- function(x) {
    authors <- x$comments |>
        lapply(FUN = \(.x) {.x[nrow(.x), ]$author}) |>
        do.call(what = c)
    return(authors)
}

issue_with_comments <- all_issues |> with_comment()
issue_without_answer <- issue_with_comments |>
    dplyr::filter(author_last_comment(issue_with_comments) == creator)

d(issue_without_answer)
