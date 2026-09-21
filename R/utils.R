#' @title Check if a Colour is Dark
#'
#' @description
#' Determines whether a given colour is "dark" based on its luminance contrast.
#'
#' @param colr A colour.  specification. The colour must be valid and recognized
#'   by `grDevices::col2rgb()`. It can be a character string (e.g.,
#'   `"#RRGGBB"`, `"red"`, `"transparent"`) or an integer vector representing
#'   RGB values.
#'
#' @returns
#' A boolean:
#' - `TRUE` if the colour is dark.
#' - `FALSE` if the colour is light.
#'
#' @details
#' The function uses the **relative luminance** formula derived from the
#' [WCAG](https://www.w3.org/WAI/WCAG22/quickref/?versions=2.1) (Web Content
#' Accessibility Guidelines) to calculate the *perceived brightness* of the
#' colour.
#' If the luminance is below a 123 (on a scale of 0-255), the colour is
#' considered dark.
#'
#' @importFrom grDevices col2rgb
#'
#' @examples
#' # Check a hexadecimal color
#' IssueTrackeR:::is_dark("#000000")  # black is dark
#' IssueTrackeR:::is_dark("#FFFFFF")  # white is light
#'
#' # Check a named color
#' IssueTrackeR:::is_dark("navy")
#' IssueTrackeR:::is_dark("yellow")
#'
#' # Check an RGB vector
#' IssueTrackeR:::is_dark(grDevices::rgb(0, 0, 0))
#' IssueTrackeR:::is_dark(grDevices::rgb(255, 255, 255, maxColorValue = 255))
#' @dev
is_dark <- function(colr) {
    col1 <- grDevices::col2rgb(colr) * c(299L, 587L, 114L)
    contrast <- colSums(col1) / 1000L < 123L
    return(contrast)
}

#' @title Replace NULL Values with a Default Value
#'
#' @description
#' Recursively replaces every `NULL` values in an object with a specified
#' default value.
#'
#' @param x An R object
#' @param default The default value to replace `NULL` with.
#'
#' @returns
#' The input object `x` with all `NULL` values replaced by `default`.
#'
#' @examples
#' # Replace NULL with a numeric default
#' x <- list(a = 1, b = NULL, c = 3)
#' IssueTrackeR:::null_to_default(x, default = 0)
#'
#' # Replace NULL with a character default
#' y <- list(name = "Alice", age = NULL, city = NULL)
#' IssueTrackeR:::null_to_default(y, default = "unknown")
#'
#' # Nested list with NULL values
#' z <- list(
#'   id = 1,
#'   details = list(
#'     address = NULL,
#'     phone = "123-456-7890"
#'   )
#' )
#' IssueTrackeR:::null_to_default(z, default = NA)
#'
#' # Atomic NULL value
#' IssueTrackeR:::null_to_default(NULL, default = FALSE)
#'
#' @dev
#' @details
#' If the input is a list, the function applies the replacement to each element
#' of the list (recursively).
#'
null_to_default <- function(x, default) {
    if (is.null(x)) {
        return(default)
    }
    if (is.list(x)) {
        return(lapply(x, null_to_default, default = default))
    }
    return(x)
}
