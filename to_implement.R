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
