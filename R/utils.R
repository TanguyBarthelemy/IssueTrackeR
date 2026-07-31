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
#' A logical value:
#' - `TRUE` if the colour is dark.
#' - `FALSE` if the colour is light.
#'
#' @details
#' The function uses the **relative luminance** formula derived from the
#' [WCAG](https://www.w3.org/WAI/WCAG21/quickref/) (Web Content Accessibility
#' Guidelines) to calculate the *perceived brightness* of the colour.
#' If the luminance is below a 123 (on a scale of 0-255), the colour is
#' considered dark.
#'
#' @importFrom grDevices col2rgb
#'
#' @examples
#' # Check a hexadecimal color
#' IssueTrackeR:::isDark("#000000")  # black is dark
#' IssueTrackeR:::isDark("#FFFFFF")  # white is light
#'
#' # Check a named color
#' IssueTrackeR:::isDark("navy")
#' IssueTrackeR:::isDark("yellow")
#'
#' # Check an RGB vector
#' IssueTrackeR:::isDark(grDevices::rgb(0, 0, 0))
#' IssueTrackeR:::isDark(grDevices::rgb(255, 255, 255, maxColorValue = 255))
#' @dev
isDark <- function(colr) {
    col1 <- grDevices::col2rgb(colr) * c(299L, 587L, 114L)
    contrast <- colSums(col1) / 1000L < 123L
    return(contrast)
}
