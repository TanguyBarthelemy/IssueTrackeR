with_labels <- function (x, ...)
{
    UseMethod("with_labels", x)
}

with_labels.IssueTB <- function (x, ...)
{
    return(grepl(x = sapply(x$labels, `[[`, "name"), pattern = ...))
}

with_labels.IssuesTB <- function (x, ...)
{
    return(grepl(
        x = sapply(X = x$labels, FUN = \(.x) sapply(.x, `[[`, "name")),
        pattern = ...
    ))
}
