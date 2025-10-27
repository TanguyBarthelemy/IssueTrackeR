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
        if (is.null(repo)) {
            if (length(owner) > 1L) {
                list_labels <- lapply(
                    X = owner,
                    FUN = get_labels,
                    source = "online",
                    repo = NULL,
                    verbose = verbose,
                    dataset_dir = NULL,
                    dataset_name = NULL
                ) |>
                    do.call(what = rbind)

                return(list_labels)
            }
            list_repo <- get_all_repos(owner, verbose = verbose)

            list_labels <- lapply(
                X = list_repo,
                FUN = get_labels,
                source = "online",
                owner = owner,
                verbose = verbose,
                dataset_dir = NULL,
                dataset_name = NULL
            ) |>
                do.call(what = rbind)

            return(list_labels)
        }

        raw_labels <- try(expr = {
            gh::gh(
                repo = repo,
                owner = owner,
                endpoint = "/repos/:owner/:repo/labels",
                .limit = Inf
            )
        })
        check_response(raw_labels)

        if (verbose) {
            cat("Repo:", repo, " owner:", owner, "\n")
        }
        list_labels <- format_labels(raw_labels = raw_labels, verbose = verbose)

        if (!is.null(list_labels)) {
            list_labels <- cbind(list_labels, repo = repo, owner = owner)
        }
    } else if (source == "local") {
        if (tools::file_ext(dataset_name) == "yaml") {
            input_file <- tools::file_path_sans_ext(dataset_name)
        }
        input_path <- file.path(dataset_dir, input_file) |>
            paste0(".yaml") |>
            normalizePath(mustWork = TRUE)

        if (verbose) {
            message("The labels will be read from ", input_path, ".")
        }

        list_labels <- readLines(con = input_path, encoding = "UTF-8") |>
            yaml::yaml.load() |>
            as.data.frame()
    } else {
        stop("wrong argument source", call. = FALSE)
    }

    class(list_labels) <- c("LabelsTB", "data.frame")
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
#' \dontrun{
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
        lapply(FUN = \(label) {
            label$color <- paste0("#", label$color)
            label$description <- null_to_default(
                x = label$description,
                default = ""
            )
            return(as.data.frame(label))
        }) |>
        do.call(what = rbind)
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
    if (tools::file_ext(dataset_name) == "yaml") {
        output_file <- tools::file_path_sans_ext(dataset_name)
    }
    output_path <- file.path(dataset_dir, output_file) |>
        paste0(".yaml") |>
        normalizePath(mustWork = FALSE)

    if (!dir.exists(dataset_dir)) {
        dir.create(dataset_dir)
    }
    if (verbose) {
        message("The datasets will be exported to ", output_path, ".")
        if (file.exists(output_path)) {
            message("The file already exists and will be overwritten.")
        }
    }

    labels_yaml <- yaml::as.yaml(labels)
    writeLines(text = enc2utf8(labels_yaml), con = output_path, useBytes = TRUE)
    return(invisible(TRUE))
}
