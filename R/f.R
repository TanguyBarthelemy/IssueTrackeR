#' @title New fun g
#'
#' @param x a numeric
#'
#' @export
g <- function(x) {
    return(f(x) / 2)
}

f <- function(x) {
    UseMethod("f", x)
}

#' @export
f.a <- function(x) {
    2 * x
}

#' @export
f.default <- function(x) {
    3 * x
}
