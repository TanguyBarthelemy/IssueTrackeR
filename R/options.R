#' @title Set, get and reset options
#'
#' @description
#' Options to manage the default configuration of datasets and selectors.
#'
#' @param name Character. Representing the name of the option.
#' @param path Character. Representing a directory path.
#' @param selector An object of class \code{SelectorTB}.
#' @param verbose Boolean indicating whether to print additional information.
#'
#' @returns
#' \itemize{
#'   \item \code{get_...}: The value of the requested option.
#'   \item \code{set_...}: The value assigned, invisibly.
#'   \item \code{reset_options}: \code{NULL} invisibly.
#' }
#'
#' @examples
#' # Resetting options
#' reset_options()
#'
#' # Working with default dataset directory
#' get_default_dataset_dir()
#' set_default_dataset_dir("/tmp/my_data")
#'
#' # Working with default dataset name
#' get_default_dataset_name()
#' set_default_dataset_name("my_dataset")
#'
#' # Working with default selector
#' sel <- init_selector(source = "GitHub", owner = "test", repo = "test")
#' set_default_selector(sel)
#' get_default_selector()
#'
#' @name IssueTrackeR-options
#'
NULL

#' @export
#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_character
#' @importFrom checkmate assert_list
#' @rdname IssueTrackeR-options
reset_options <- function(verbose = TRUE) {
    checkmate::assert_flag(verbose)

    dataset_dir <- file.path(tempdir(), "data") |>
        normalizePath(mustWork = FALSE)

    # nolint start undesirable_function_linter
    options(list(IssueTrackeR.dataset.dir = dataset_dir))
    new_name <- letters |>
        sample(size = 5L, replace = TRUE) |>
        paste(collapse = "")
    options(list(IssueTrackeR.dataset.name = new_name))
    options(list(IssueTrackeR.selector = init_selector()))
    # nolint end

    if (verbose) {
        cat(
            "Reset the default options to:",
            paste("\n- location for datasets: ", dataset_dir),
            paste("\n- name for the datasets: ", new_name),
            paste("\n- selector: NULL")
        )
        cat("\n")
    }

    return(invisible(NULL))
}

#' @export
#' @rdname IssueTrackeR-options
set_default_dataset_dir <- function(path) {
    checkmate::assert_character(path, pattern = "^/")
    options(IssueTrackeR.dataset.dir = path)
}

#' @export
#' @rdname IssueTrackeR-options
get_default_dataset_dir <- function() {
    getOption("IssueTrackeR.dataset.dir")
}

#' @export
#' @rdname IssueTrackeR-options
set_default_dataset_name <- function(name) {
    checkmate::assert_character(name, min.chars = 1)
    options(IssueTrackeR.dataset.name = name)
}

#' @export
#' @rdname IssueTrackeR-options
get_default_dataset_name <- function() {
    getOption("IssueTrackeR.dataset.name")
}

#' @export
#' @rdname IssueTrackeR-options
set_default_selector <- function(selector) {
    checkmate::assert_class(selector, "SelectorTB")
    options(IssueTrackeR.selector = selector)
}

#' @export
#' @rdname IssueTrackeR-options
get_default_selector <- function() {
    getOption("IssueTrackeR.selector")
}
