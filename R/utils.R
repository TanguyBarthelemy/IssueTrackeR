isDark <- function(colr) {
    apply(
        X = grDevices::col2rgb(colr) * c(299L, 587L, 114L),
        FUN = sum,
        MARGIN = 2L
    ) /
        1000L <
        123L
}
