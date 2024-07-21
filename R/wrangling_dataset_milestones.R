
#' @title Retrieve the milestones from github
#'
#' @param type a character string that is either \code{"online"} if you want to
#' fetch information from github or \code{"local"} if you want to fetch
#' information locally.
#' @param path_dataset A character string specifying the path which contains the
#' datasets (only used if type is \code{"local"}). Defaults to the package
#' option \code{IssueTrackeR.dataset.path}.
#' @param repo A character string specifying the GitHub repository name (only
#' used if type is \code{"online"}). Defaults to the package option
#' \code{IssueTrackeR.repo}.
#' @param username A character string specifying the GitHub username (only used
#' if type is \code{"online"}). Defaults to the package option
#' \code{IssueTrackeR.username}.
#'
#' @returns
#' a list representing milestones with simpler structure (with title,
#' description and due_on)
#' @export
#'
#' @examples
#' \dontrun{
#' get_milestones()
#' get_milestones(type = "local")
#' }
#' get_milestones(type = "online")
#'
get_milestones <- function(
        type = c("local", "online"),
        path_dataset = getOption("IssueTrackeR.dataset.path"),
        repo = getOption("IssueTrackeR.repo"),
        username = getOption("IssueTrackeR.username")) {
    type <- match.arg(type)

    if (type == "online") {
        milestones <- gh::gh(
            repo = repo,
            username = username,
            endpoint = "/repos/:username/:repo/milestones",
            .limit = Inf
        ) |>
            format_milestones()
    } else if (type == "local") {
        path_dataset_milestones <- file.path(path_dataset,
                                             "list_milestones.yaml")
        if (file.exists(path_dataset_milestones)) {
            milestones <- yaml::read_yaml(file = path_dataset_milestones)
            milestones[["due_on"]]  <- as.POSIXct(milestones[["due_on"]])
        } else {
            stop("The file doesn't exist. Run `write_milestones_to_dataset()`",
                 " to write a set of issues in the repo.")
        }
    } else {
        stop("wrong type")
    }

    return(milestones)
}

#' @title Format the milestone in a simpler format
#'
#' @param raw_milestones a \code{gh_response} object output from the function
#' \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#' milestones.
#'
#' @returns a list representing milestones with simpler structure (with title,
#' description and due_on)
#' @export
#'
#' @examples
#' # With milestones
#' raw_milestones <- gh::gh(
#'     repo = "rjdemetra",
#'     username = "rjdverse",
#'     endpoint = "/repos/:username/:repo/milestones",
#'     .limit = Inf
#' )
#' format_milestones(raw_milestones)
#'
format_milestones <- function(raw_milestones) {
    new_mlst_structure <- raw_milestones |>
        lapply(FUN = \(x) data.frame(
            title = x$title,
            description = x$description,
            due_on = ifelse(test = is.null(x$due_on),
                            yes = as.POSIXct(NA_integer_),
                            no = x$due_on |>
                                as.POSIXct() |>
                                as.integer() |>
                                as.POSIXct())
        )) |>
        do.call(what = rbind)
    return(new_mlst_structure)
}

#' @title Save milestone dataset in a yaml format
#'
#' @param milestones a list representing milestones with simpler structure (with
#' title, description and due_on).
#' @param type a character string that is either \code{"online"} (by default) if
#' you want to fetch information from github or \code{"local"} if you want to
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
#' # With milestones
#' milestones <- get_milestones()
#' write_milestones_to_dataset(milestones)
#'
#' # Without milestones
#' write_milestones_to_dataset(type = "online")
#'
write_milestones_to_dataset <- function(
        milestones,
        type = "online",
        path_dataset = getOption("IssueTrackeR.dataset.path")) {

    if (!dir.exists(path_dataset)) {
        dir.create(path_dataset)
    }
    type <- match.arg(type)
    path_dataset_milestones <- file.path(path_dataset, "list_milestones.yaml")
    if (missing(milestones)) {
        milestones <- get_milestones(type = type)
    }
    yaml::write_yaml(
        x = milestones,
        file = path_dataset_milestones
    )
    return(invisible(TRUE))
}
