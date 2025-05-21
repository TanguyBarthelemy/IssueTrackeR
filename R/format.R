#' @title Round a timestamp to the inferior integer
#'
#' @description
#' This function round a timestamp ()
#'
#' @param x The timestamp. See detail section for more information.
#'
#' @details
#' The accepted formats for the argument \code{x} are:
#'
#' \itemize{
#' \item \code{Date} objects;
#' \item numeric (\code{integer} or \code{double});
#' \item date/times object (classes \code{POSIXct} and \code{POSIXlt})
#' }
#'
#' @returns a \code{POSIXct} object with rounded \code{double} value.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#'
#' format_timestamp(1743694674.9)
#' format_timestamp(Sys.Date())
#'
format_timestamp <- function(x) {
    output <- x |>
        as.POSIXct(origin = "1970-01-01") |>
        as.integer() |>
        as.POSIXct(origin = "1970-01-01")
    return(output)
}
