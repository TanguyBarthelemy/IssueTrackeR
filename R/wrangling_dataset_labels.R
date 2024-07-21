
#' @title Retrieve the labels from github
#'
#' @param type a character string that is either \code{"online"} if you want to
#' fetch information from github or \code{"local"} if you want to fetch
#' information locally.
#'
#' @returns
#' a list representing labels with simpler structure (with name,
#' description, color)
#' @export
#'
#' @examples
#' \dontrun{
#' get_labels()
#' get_labels(type = "local")
#' }
#' get_labels(type = "online")
#'
get_labels <- function(type = c("local", "online")) {
    type <- match.arg(type)

    if (type == "online") {
        labels <- gh::gh(
            repo = "TODO",
            username = "TanguyBarthelemy",
            endpoint = "/repos/:username/:repo/labels",
            .limit = Inf
        ) |>
            format_labels()
    } else if (type == "local") {
        if (file.exists(path_dataset_labels)) {
            labels <- yaml::read_yaml(file = path_dataset_labels)
        } else {
            stop("The file doesn't exist. Run `write_labels_to_dataset()`",
                 " to write a set of issues in the repo.")
        }
    } else {
        stop("wrong type")
    }

    return(labels)
}

#' @title Format the label in a simpler format
#'
#' @param raw_labels a \code{gh_response} object output from the function
#' \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#' labels.
#'
#' @returns a list representing labels with simpler structure (with name,
#' description, color)
#' @export
#'
#' @examples
#' # With labels
#' raw_labels <- gh::gh(
#'    repo = "TODO",
#'    username = "TanguyBarthelemy",
#'    endpoint = "/repos/:username/:repo/labels",
#'    .limit = Inf
#' )
#' format_labels(raw_labels)
#'
format_labels <- function(raw_labels) {
    new_labels_structure <- lapply(
        X = raw_labels,
        FUN = base::`[`,
        c("name", "description", "color")
    )
    return(new_labels_structure)
}

#' @title Save label dataset in a yaml format
#'
#' @param labels a list representing all labels with simpler structure (with
#' name, description, color)
#' @param type a character string that is either \code{"online"} (by default) if
#' you want to fetch information from github or \code{"local"} if you want to
#' fetch information locally.
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
write_labels_to_dataset <- function(labels, type = "online") {
    if (!dir.exists("data")) {
        dir.create("data")
    }
    type <- match.arg(type)
    if (missing(labels)) {
        labels <- get_labels(type = type)
    }
    yaml::write_yaml(x = labels, file = path_dataset_labels)
    return(invisible(TRUE))
}
