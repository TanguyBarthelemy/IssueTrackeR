
#' @keywords internal
format_timestamp <- function(x) {
    output <- x |>
        as.POSIXct(origin = 0L) |> #"1970-01-01") |>
        as.integer() |>
        as.POSIXct(origin = 0L) #"1970-01-01")
    return(output)
}
