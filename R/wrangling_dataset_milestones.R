
#' @keywords internal
format_milestone <- function(raw_milestone, verbose = TRUE) {
    if (verbose) {
        cat("\t- ", raw_milestone[["title"]], "... Done!\n")
    }
    description <- ifelse(
        test = is.null(raw_milestone[["description"]]),
        yes = NA_character_,
        no = raw_milestone[["description"]]
    )
    due_on <- ifelse(
        test = is.null(raw_milestone[["due_on"]]),
        yes = format_timestamp(NA_integer_),
        no = format_timestamp(raw_milestone[["due_on"]])
    )
    output <- data.frame(
        title = raw_milestone[["title"]],
        description = description,
        due_on = due_on
    )
    return(output)
}

#' @rdname get
#' @export
get_milestones <- function(
        source = c("local", "online"),
        dataset_dir = getOption("IssueTrackeR.dataset.dir"),
        dataset_name = "list_milestones.yaml",
        repo = getOption("IssueTrackeR.repo"),
        owner = getOption("IssueTrackeR.owner"),
        verbose = TRUE) {
    source <- match.arg(source)

    if (source == "online") {
        milestones <- gh::gh(
            repo = repo,
            owner = owner,
            endpoint = "/repos/:owner/:repo/milestones",
            state = "all",
            .limit = Inf
        ) |>
            format_milestones(verbose = verbose)

        if (nrow(milestones) > 0L) {
            milestones <- cbind(milestones, repo = repo,
                                owner = owner)
        }

    } else if (source == "local") {

        input_file <- tools::file_path_sans_ext(dataset_name)
        input_path <- file.path(dataset_dir, input_file) |>
            normalizePath(mustWork = FALSE) |>
            paste0(".yaml")

        if (file.exists(input_path)) {
            if (verbose) {
                message("The milestones will be read from ", input_path, ".")
            }
            milestones <- yaml::read_yaml(file = input_path) |>
                as.data.frame()
            if (nrow(milestones) > 0L) {
                milestones[["due_on"]]  <- format_timestamp(
                    x = milestones[["due_on"]]
                )
            }
        } else {
            stop(
                "The file ", input_path, " doesn't exist.\n",
                "Run `write_milestones_to_dataset()`",
                " to write a set of milestones in the directory\n",
                "Or call get_milestones() with the argument",
                " `source` to \"online\".",
                call. = FALSE
            )
        }
    } else {
        stop("wrong argument source", call. = FALSE)
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
#'     owner = "rjdverse",
#'     endpoint = "/repos/:owner/:repo/milestones",
#'     .limit = Inf
#' )
#' format_milestones(raw_milestones)
#'
format_milestones <- function(raw_milestones, verbose = TRUE) {

    if (verbose) {
        cat("Reading milestones... \n")
    }
    new_mlst_structure <- raw_milestones |>
        lapply(FUN = format_milestone, verbose = verbose) |>
        do.call(what = rbind) |>
        as.data.frame()
    if (verbose) {
        cat("Done!", nrow(new_mlst_structure),
            "milestones found.\n", sep = " ")
    }
    return(new_mlst_structure)
}

#' @title Save milestone dataset in a yaml file
#'
#' @param milestones a list representing milestones with simpler structure (with
#' title, description and due_on).
#' @inheritParams get_issues
#'
#' @details
#' The defaults value of the argument \code{dataset_name} is
#' \code{"list_milestones.yaml"}.
#'
#' @returns invisibly (with \code{invisible()}) \code{TRUE} if the export was
#' successful and an error otherwise.
#' @export
#'
#' @examples
#' milestones <- get_milestones(source = "online")
#' write_milestones_to_dataset(milestones)
#'
write_milestones_to_dataset <- function(
        milestones,
        dataset_dir = getOption("IssueTrackeR.dataset.dir"),
        dataset_name = "list_milestones.yaml",
        verbose = TRUE) {

    output_file <- tools::file_path_sans_ext(dataset_name)
    output_path <- file.path(dataset_dir, output_file) |>
        normalizePath(mustWork = FALSE) |>
        paste0(".yaml")

    if (!dir.exists(dataset_dir)) {
        dir.create(dataset_dir)
    }
    if (verbose) {
        message("The datasets will be exported to ", output_path, ".")
        if (file.exists(output_path)) {
            message("The file already exists and will be overwritten.")
        }
    }

    yaml::write_yaml(
        x = milestones,
        file = output_path
    )
    return(invisible(TRUE))
}
