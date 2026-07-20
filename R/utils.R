#' @title Check if a Color is Dark
#'
#' @description
#' Determines whether a given color is "dark" based on its luminance contrast.
#'
#' @param colr A color.  specification. The color must be valid and recognized
#'   by `grDevices::col2rgb()`. It can be a character string (e.g.,
#'   `"#RRGGBB"`, `"red"`, `"transparent"`) or an integer vector representing
#'   RGB values.
#'
#' @returns
#' A logical value:
#' - `TRUE` if the color is dark.
#' - `FALSE` if the color is light.
#'
#' @details
#' The function uses the **relative luminance** formula derived from the
#' [WCAG](https://www.w3.org/WAI/WCAG21/quickref/) (Web Content Accessibility
#' Guidelines) to calculate the *perceived brightness* of the color.
#' If the luminance is below a 123 (on a scale of 0-255), the color is
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
#' IssueTrackeR:::isDark(c(0, 0, 0))
#' IssueTrackeR:::isDark(c(255, 255, 255))
#' @dev
isDark <- function(colr) {
    col1 <- grDevices::col2rgb(colr) * c(299L, 587L, 114L)
    contrast <- colSums(col1) / 1000L < 123L
    return(contrast)
}
