#' @export
#' @rdname get
get_labels <- function(
    source = c("local", "online"),
    dataset_dir = getOption("IssueTrackeR.dataset.dir"),
    dataset_name = "list_labels.yaml",
    repo = getOption("IssueTrackeR.repo"),
    owner = getOption("IssueTrackeR.owner"),
    verbose = TRUE
) {
    source <- match.arg(source)

    if (source == "online") {
        list_labels <- gh::gh(
            repo = repo,
            owner = owner,
            endpoint = "/repos/:owner/:repo/labels",
            .limit = Inf
        ) |>
            format_labels(verbose = verbose)

        if (!is.null(list_labels)) {
            list_labels <- cbind(list_labels, repo = repo, owner = owner)
        }
    } else if (source == "local") {
        input_file <- tools::file_path_sans_ext(dataset_name)
        input_path <- file.path(dataset_dir, input_file) |>
            normalizePath(mustWork = FALSE) |>
            paste0(".yaml")

        if (!file.exists(input_path)) {
            stop(
                "The file doesn't exist. Run `write_labels_to_dataset()`",
                " to write a set of labels in the directory\n",
                "Or call get_labels() with ",
                "the argument `source` to \"online\".",
                call. = FALSE
            )
        }
        if (verbose) {
            message("The labels will be read from ", input_path, ".")
        }
        list_labels <- yaml::read_yaml(file = input_path) |>
            as.data.frame()
    } else {
        stop("wrong argument source", call. = FALSE)
    }

    return(list_labels)
}

#' @title Format the label in a simpler format
#'
#' @param raw_labels a \code{gh_response} object output from the function
#' \code{\link[gh]{gh}} which contains all the data and metadata for GitHub
#' labels.
#' @inheritParams get_issues
#'
#' @returns a list representing labels with simpler structure (with name,
#' description, colour)
#' @export
#'
#' @examples
#'
#' \donttest{
#' # With labels
#' raw_labels <- gh::gh(
#'    repo = "rjdemetra",
#'    owner = "rjdverse",
#'    endpoint = "/repos/:owner/:repo/labels",
#'    .limit = Inf
#' )
#' format_labels(raw_labels)
#' }
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
        cat("Done!\n", nrow(new_labels_structure), " labels found.\n", sep = "")
    }
    return(new_labels_structure)
}

#' @rdname write
#' @export
write_labels_to_dataset <- function(
    labels,
    dataset_dir = getOption("IssueTrackeR.dataset.dir"),
    dataset_name = "list_labels.yaml",
    verbose = TRUE
) {
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
        x = labels,
        file = output_path
    )
    return(invisible(TRUE))
}
