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
#' @param x A scalar, a vector or a list.
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

#' @title Check GitHub Connection
#'
#' @description
#' A helper function to verify if a GitHub connection is active and has
#' sufficient rate limit remaining. This is intended for internal use
#' or testing.
#'
#' @param timeout Numeric. Timeout in seconds for the API call. Default is 10.
#' @param verbose Boolean. Should messages be printed?
#'
#' @return Boolean. `TRUE` if the connection is valid and rate limit
#'   is above a threshold, `FALSE` otherwise.
#'
#' @examples
#' # Check if we can connect to GitHub
#' check_github_connection()
#'
#' @export
#'
#' @importFrom gh gh_token_exists gh
#' @importFrom checkmate assert_flag assert_number
check_github_connection <- function(timeout = 60, verbose = TRUE) {
    checkmate::assert_number(timeout, lower = 1)
    checkmate::assert_flag(verbose)

    if (!gh::gh_token_exists()) {
        if (verbose) message("No GitHub token found.")
        return(FALSE)
    }

    # We use tryCatch to handle potential network timeouts/errors
    res <- tryCatch({
        # We call the rate_limit endpoint explicitly to apply the timeout
        limit <- gh::gh(
            endpoint = "rate_limit",
            config = list(timeout = timeout)
        )
        # Accessing the core resource remaining count
        limit$resources$core$remaining > 0
    }, error = function(e) {
        if (verbose) message("Error checking GitHub rate limit: ", e$message)
        return(FALSE)
    })

    return(res)
}
