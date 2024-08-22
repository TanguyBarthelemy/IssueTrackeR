
#' @title Retrieve the milestones from github
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
#' @param ... Additional arguments for the function \code{format_milestones}
#'
#' @returns
#' a list representing milestones with simpler structure (with title,
#' description and due_on)
#' @export
#'
#' @examples
#' \dontrun{
#' get_milestones()
#' get_milestones(source = "local")
#' }
#' get_milestones(source = "online")
#'
get_milestones <- function(
        source = c("local", "online"),
        path_dataset = getOption("IssueTrackeR.dataset.path"),
        repo = getOption("IssueTrackeR.repo"),
        username = getOption("IssueTrackeR.username"),
        ...) {
    source <- match.arg(source)

    if (source == "online") {
        milestones <- gh::gh(
            repo = repo,
            username = username,
            endpoint = "/repos/:username/:repo/milestones",
            .limit = Inf
        ) |>
            format_milestones(...)
    } else if (source == "local") {
        path_dataset_milestones <- file.path(path_dataset,
                                             "list_milestones.yaml")
        if (file.exists(path_dataset_milestones)) {
            milestones <- yaml::read_yaml(file = path_dataset_milestones) |>
                as.data.frame()
            milestones[["due_on"]]  <- as.POSIXct(milestones[["due_on"]])
        } else {
            stop("The file doesn't exist. Run `write_milestones_to_dataset()`",
                 " to write a set of issues in the repo.")
        }
    } else {
        stop("wrong source")
    }

    return(milestones)
}

#' @title Format the milestone in a simpler format
#'
#' @param raw_milestones a \code{gh_response} object output from the function
#' \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#' milestones.
#' @param verbose A logical value indicating whether to print additional
#' information. Default is \code{TRUE}.
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
format_milestones <- function(raw_milestones, verbose = TRUE) {

    if (verbose) {
        cat("Reading milestones... ")
    }
    new_mlst_structure <- raw_milestones |>
        lapply(FUN = function(x) {
            if (verbose) {
                cat("Milestone ", x[["title"]], "... Done!\n")
            }
            data.frame(
                title = x[["title"]],
                description = x[["description"]],
                due_on = ifelse(test = is.null(x[["due_on"]]),
                                yes = as.POSIXct(NA_integer_),
                                no = x[["due_on"]] |>
                                    as.POSIXct() |>
                                    as.integer() |>
                                    as.POSIXct())
            )
        }) |>
        do.call(what = rbind)
    if (verbose) {
        cat("Done!\n",
            ifelse(
                test = is.null(new_mlst_structure),
                yes = 0L,
                no = nrow(new_mlst_structure)
            ), " milestones found.\n", sep = "")
    }
    return(new_mlst_structure)
}

#' @title Save milestone dataset in a yaml format
#'
#' @param milestones a list representing milestones with simpler structure (with
#' title, description and due_on).
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
#' # With milestones
#' milestones <- get_milestones()
#' write_milestones_to_dataset(milestones)
#'
#' # Without milestones
#' write_milestones_to_dataset(source = "online")
#'
write_milestones_to_dataset <- function(
        milestones,
        source = "online",
        path_dataset = getOption("IssueTrackeR.dataset.path")) {

    if (!dir.exists(path_dataset)) {
        dir.create(path_dataset)
    }
    source <- match.arg(source)
    path_dataset_milestones <- file.path(path_dataset, "list_milestones.yaml")
    if (missing(milestones)) {
        milestones <- get_milestones(source = source)
    }
    yaml::write_yaml(
        x = milestones,
        file = path_dataset_milestones
    )
    return(invisible(TRUE))
}
