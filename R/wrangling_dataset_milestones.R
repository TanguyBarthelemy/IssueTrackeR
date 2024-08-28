
#' @title Retrieve the milestones from github
#'
#' @param source a character string that is either \code{"online"} if you want
#' to fetch information from github or \code{"local"} if you want to fetch
#' information locally.
#' @param dataset_dir A character string specifying the path which contains the
#' datasets (only used if source is \code{"local"}). Defaults to the package
#' option \code{IssueTrackeR.dataset.dir}.
#' @param repo A character string specifying the GitHub repository name (only
#' used if source is \code{"online"}). Defaults to the package option
#' \code{IssueTrackeR.repo}.
#' @param username A character string specifying the GitHub username (only used
#' if source is \code{"online"}). Defaults to the package option
#' \code{IssueTrackeR.username}.
#' @param \dots Additional arguments for the function
#' \code{\link[IssueTrackeR]{format_milestones}}.
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
        dataset_dir = getOption("IssueTrackeR.dataset.dir"),
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

        if (nrow(milestones) > 0L) {
            milestones <- cbind(milestones, repo = repo,
                                username = username)
        }

    } else if (source == "local") {
        dataset_dir_milestones <- file.path(dataset_dir,
                                             "list_milestones.yaml")
        if (file.exists(dataset_dir_milestones)) {
            milestones <- yaml::read_yaml(file = dataset_dir_milestones) |>
                as.data.frame()
            if (nrow(milestones) > 0L) {
                milestones[["due_on"]]  <- as.POSIXct(milestones[["due_on"]])
            }
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
        cat("Reading milestones... \n")
    }
    new_mlst_structure <- raw_milestones |>
        lapply(FUN = function(x) {
            if (verbose) {
                cat("\t- ", x[["title"]], "... Done!\n")
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
        do.call(what = rbind) |>
        as.data.frame()
    if (verbose) {
        cat("Done!", nrow(new_mlst_structure),
            "milestones found.\n", sep = " ")
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
#' @param dataset_dir A character string specifying the path which will contain
#' the datasets. Defaults to the package option
#' \code{IssueTrackeR.dataset.dir}.
#' @param \dots Additional arguments for the function
#' \code{\link[IssueTrackeR]{get_milestones}}.
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
        source = c("online", "local"),
        dataset_dir = getOption("IssueTrackeR.dataset.dir"),
        ...) {

    if (!dir.exists(dataset_dir)) {
        dir.create(dataset_dir)
    }
    source <- match.arg(source)
    dataset_dir_milestones <- file.path(dataset_dir, "list_milestones.yaml")
    if (missing(milestones)) {
        milestones <- get_milestones(source = source, ...)
    }
    yaml::write_yaml(
        x = milestones,
        file = dataset_dir_milestones
    )
    return(invisible(TRUE))
}
