
#' @title Retrieve the milestones from github
#'
#' @param type a character string that is either \code{"online"} if you want to
#' fetch information from github or \code{"local"} if you want to fetch
#' information locally.
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
get_milestones <- function(type = c("local", "online")) {
    type <- match.arg(type)

    if (type == "online") {

        milestones <- gh::gh(
            repo = "TODO",
            username = "TanguyBarthelemy",
            endpoint = "/repos/:username/:repo/milestones",
            .limit = Inf
        ) |>
            format_milestones()
    } else if (type == "local") {
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
#'     repo = "TODO",
#'     username = "TanguyBarthelemy",
#'     endpoint = "/repos/:username/:repo/milestones",
#'     .limit = Inf
#' )
#' format_milestones(raw_milestones)
#'
format_milestones <- function(raw_milestones) {
    new_mlst_structure <- raw_milestones |>
        lapply(
            FUN = base::`[`,
            c("title", "description", "due_on")
        ) |>
        lapply(FUN = as.data.frame) |>
        do.call(what = rbind)

    new_mlst_structure[["due_on"]]  <- new_mlst_structure[["due_on"]] |>
        as.POSIXct() |>
        as.integer() |>
        as.POSIXct()

    return(new_mlst_structure)
}

#' @title Save milestone dataset in a yaml format
#'
#' @param milestones a list representing milestones with simpler structure (with
#' title, description and due_on).
#' @param type a character string that is either \code{"online"} (by default) if
#' you want to fetch information from github or \code{"local"} if you want to
#' fetch information locally.
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
write_milestones_to_dataset <- function(milestones, type = "online") {
    if (!dir.exists("data")) {
        dir.create("data")
    }
    type <- match.arg(type)
    if (missing(milestones)) {
        milestones <- get_milestones(type = type)
    }
    yaml::write_yaml(
        x = milestones,
        file = path_dataset_milestones
    )
    return(invisible(TRUE))
}
