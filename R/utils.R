isDark <- function(colr) {
    col1 <- grDevices::col2rgb(colr) * c(299L, 587L, 114L)
    contrast <- colSums(col1) / 1000L < 123L
    return(contrast)
}
