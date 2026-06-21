.write <- function(
    x,
    dataset_dir = tempdir(),
    dataset_name = "object.yaml",
    verbose = TRUE,
    ...
) {
    output_file <- dataset_name
    if (tools::file_ext(output_file) == "yaml") {
        output_file <- tools::file_path_sans_ext(output_file)
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
    x_yaml <- yaml::as.yaml(x, precision = 22L, indent = 2L)
    writeLines(
        text = enc2utf8(x_yaml),
        con = output_path,
        useBytes = TRUE
    )
    return(invisible(TRUE))
}

#' @title Save datasets in a yaml file
#'
#' @param x an object of class \code{IssuesTB}, \code{LabelsTB} or
#' \code{MilestonesTB}.
#' @inheritParams get_issues
#' @param \dots Unused parameter.
#'
#' @details
#' Depending on the object, the defaults value of the argument
#' \code{dataset_name} (by default) is:
#'
#' \itemize{
#' \item \code{"list_issues.yaml"} for issues;
#' \item \code{"list_labels.yaml"} for labels;
#' \item \code{"list_milestones.yaml"} for milestones.
#' }
#'
#' @returns invisibly (with \code{invisible()}) \code{TRUE} if the export was
#' successful and an error otherwise.
#' @export
#'
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' milestones <- get_milestones(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "list_milestones.yaml"
#' )
#' labels <- get_labels(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "list_labels.yaml"
#' )
#'
#' write_to_dataset(x = issues, dataset_dir = tempdir())
#' write_to_dataset(x = labels, dataset_dir = tempdir())
#' write_to_dataset(x = milestones, dataset_dir = tempdir())
#'
#' write_to_dataset(x = issues, dataset_dir = tempdir(),
#'                  dataset_name = "my_issues")
#' write_to_dataset(x = labels, dataset_dir = tempdir(),
#'                  dataset_name = "my_labels")
#' write_to_dataset(x = milestones, dataset_dir = tempdir(),
#'                  dataset_name = "my_milestones")
#'
#' @rdname write
#'
write_to_dataset <- function(x, ...) {
    UseMethod(generic = "write_to_dataset", object = x)
}

#' @rdname write
#' @exportS3Method write_to_dataset IssuesTB
#' @method write_to_dataset IssuesTB
#' @export
write_to_dataset.IssuesTB <- function(
    x,
    dataset_dir = getOption("IssueTrackeR.dataset.dir"),
    dataset_name = "list_issues.yaml",
    verbose = TRUE,
    ...
) {
    .write(x, dataset_dir, dataset_name, verbose)
    return(invisible(TRUE))
}


#' @rdname write
#' @exportS3Method write_to_dataset LabelsTB
#' @method write_to_dataset LabelsTB
#' @export
write_to_dataset.LabelsTB <- function(
    x,
    dataset_dir = getOption("IssueTrackeR.dataset.dir"),
    dataset_name = "list_labels.yaml",
    verbose = TRUE,
    ...
) {
    .write(x, dataset_dir, dataset_name, verbose)
    return(invisible(TRUE))
}

#' @rdname write
#' @exportS3Method write_to_dataset MilestonesTB
#' @method write_to_dataset MilestonesTB
#' @export
write_to_dataset.MilestonesTB <- function(
    x,
    dataset_dir = getOption("IssueTrackeR.dataset.dir"),
    dataset_name = "list_milestones.yaml",
    verbose = TRUE,
    ...
) {
    .write(x, dataset_dir, dataset_name, verbose)
    return(invisible(TRUE))
}

#' @rdname write
#' @exportS3Method write_to_dataset default
#' @method write_to_dataset default
#' @export
write_to_dataset.default <- function(x, ...) {
    stop(
        "This function requires a IssuesTB, LabelsTB or MilestonesTB object.",
        call. = FALSE
    )
}
