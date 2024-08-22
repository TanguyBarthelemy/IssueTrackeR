
#' @title Retrieve the labels from github
#'
#' @param source a character string that is either \code{"online"} if you want
#' to fetch information from github or \code{"local"} if you want to fetch
#' information locally.
#' @param path_dataset A character string specifying the path which contains the
#' datasets (only used if source is \code{"local"}). Defaults to the package
#' option \code{IssueTrackeR.dataset.path}.
#' @param repo A character string specifying the GitHub repository name (only
#' used if source is \code{"online"}). Defaults to the package option
#' \code{IssueTrackeR.repo}.
#' @param username A character string specifying the GitHub username (only used
#' if source is \code{"online"}). Defaults to the package option
#' \code{IssueTrackeR.username}.
#' @param ... Additional arguments for the function \code{format_labels}
#'
#' @returns
#' a list representing labels with simpler structure (with name,
#' description, color)
#' @export
#'
#' @examples
#' \dontrun{
#' get_labels()
#' get_labels(source = "local")
#' }
#' get_labels(source = "online")
#'
get_labels <- function(source = c("local", "online"),
                       path_dataset = getOption("IssueTrackeR.dataset.path"),
                       repo = getOption("IssueTrackeR.repo"),
                       username = getOption("IssueTrackeR.username"),
                       ...) {
    source <- match.arg(source)

    if (source == "online") {
        labels <- gh::gh(
            repo = repo,
            username = username,
            endpoint = "/repos/:username/:repo/labels",
            .limit = Inf
        ) |>
            format_labels(...)
    } else if (source == "local") {
        path_dataset_labels <- file.path(path_dataset, "list_labels.yaml")
        if (file.exists(path_dataset_labels)) {
            labels <- yaml::read_yaml(file = path_dataset_labels) |>
                as.data.frame()
        } else {
            stop("The file doesn't exist. Run `write_labels_to_dataset()`",
                 " to write a set of issues in the repo.")
        }
    } else {
        stop("wrong source")
    }

    return(labels)
}

#' @title Format the label in a simpler format
#'
#' @param raw_labels a \code{gh_response} object output from the function
#' \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#' labels.
#' @param verbose A logical value indicating whether to print additional
#' information. Default is \code{TRUE}.
#'
#' @returns a list representing labels with simpler structure (with name,
#' description, color)
#' @export
#'
#' @examples
#' # With labels
#' raw_labels <- gh::gh(
#'    repo = "rjdemetra",
#'    username = "rjdverse",
#'    endpoint = "/repos/:username/:repo/labels",
#'    .limit = Inf
#' )
#' format_labels(raw_labels)
#'
format_labels <- function(raw_labels, verbose = TRUE) {

    if (verbose) {
        cat("Reading labels... ")
    }
    new_labels_structure <- lapply(
        X = raw_labels,
        FUN = base::`[`,
        c("name", "description", "color")
    ) |>
        do.call(what = rbind) |>
        as.data.frame()
    if (verbose) {
        cat("Done!\n",
            length(new_labels_structure), " labels found.\n", sep = "")
    }
    return(new_labels_structure)
}

#' @title Save label dataset in a yaml format
#'
#' @param labels a list representing all labels with simpler structure (with
#' name, description, color)
#' @param source a character string that is either \code{"online"} (by default)
#' if you want to fetch information from github or \code{"local"} if you want to
#' fetch information locally.
#' @param path_dataset A character string specifying the path which will contain
#' the datasets. Defaults to the package option
#' \code{IssueTrackeR.dataset.path}.
#'
#' @returns invisibly (with \code{invisible()}) \code{TRUE} if the export was
#' successful and an error otherwise.
#' @export
#'
#' @examples
#' # With labels
#' labels <- get_labels()
#' write_labels_to_dataset(labels)
#'
#' # Without labels
#' write_labels_to_dataset()
#'
write_labels_to_dataset <- function(
        labels,
        source = "online",
        path_dataset = getOption("IssueTrackeR.dataset.path")) {
    if (!dir.exists(path_dataset)) {
        dir.create(path_dataset)
    }
    source <- match.arg(source)
    path_dataset_labels <- file.path(path_dataset, "list_labels.yaml")
    if (missing(labels)) {
        labels <- get_labels(source = source)
    }
    yaml::write_yaml(x = labels, file = path_dataset_labels)
    return(invisible(TRUE))
}
