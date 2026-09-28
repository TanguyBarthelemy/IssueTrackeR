#' @keywords internal
#' @noRd
.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        "Currently, the default options are:",
        "\n- location for datasets: ",
        getOption("IssueTrackeR.dataset.dir"),
        "\n- name for the datasets: ",
        getOption("IssueTrackeR.dataset.name"),
        "\n- selector: ",
        getOption("IssueTrackeR.selector")
    )
}

#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
    reset_options(verbose = FALSE)
}
