# Who contributes?

report_contribution <- function(x) {
    opener <- table(x$creator)
    commenter <- x$comments |>
        lapply(FUN = `[[`, "author") |>
        do.call(what = c) |>
        table()
    closer <- table(x$closed_by)

    all_id <- unique(c(names(opener), names(commenter), names(closer)))
    opener[setdiff(all_id, names(opener))] <- 0L
    opener <- opener[all_id]
    commenter[setdiff(all_id, names(commenter))] <- 0L
    commenter <- commenter[all_id]
    closer[setdiff(all_id, names(closer))] <- 0L
    closer <- closer[all_id]
    return(rbind(opener, commenter, closer))
}
