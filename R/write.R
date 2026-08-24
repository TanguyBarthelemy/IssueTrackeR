#' @title Export an R Object to YAML
#'
#' @description
#' This function exports an R object (such as a list, vector, data.frame, etc.)
#' to a YAML file.
#'
#' @param x An R object to export.
#' @param dataset_dir The destination directory where the YAML file will be
#'   saved. By default, the system's temporary directory is used (`tempdir()`).
#' @param dataset_name The name of the output file (without extension).
#'   By default, the name is `"object.yaml"`.
#' @param overwrite Logical indicating whether to overwrite the file if it
#'   already exists. Defaults to `TRUE`.
#' @param verbose A logical value indicating whether to print additional
#' information. Default is \code{TRUE}.
#' @param \dots Currently not used.
#'
#' @returns
#' The function returns **invisibly** the full path of the written YAML file.
#' If the file already exists and `overwrite = FALSE`, it returns `FALSE`
#' without writing.
#'
#' @details
#' The function automatically handles directory creation when the path doesn't
#' exist.
#'
#' @dev
#' @importFrom yaml as.yaml
#' @importFrom tools file_ext
#' @importFrom tools file_path_sans_ext
#'
#' @examples
#' my_list <- list(name = "John", age = 30, city = "Paris")
#' IssueTrackeR:::.write(my_list, dataset_name = "example_list")
#'
#' my_df <- data.frame(id = 1:3, value = c("A", "B", "C"))
#' my_data_dir <- tempfile("data")
#' IssueTrackeR:::.write(
#'     x = my_df,
#'     dataset_dir = my_data_dir,
#'     dataset_name = "my_dataframe"
#' )
#'
#' IssueTrackeR:::.write(
#'     x = my_list,
#'     dataset_name = "example_list",
#'     overwrite = FALSE
#' )
.write <- function(
    x,
    dataset_dir = tempdir(),
    dataset_name = "object.yaml",
    overwrite = TRUE,
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

    if (file.exists(output_path) && !overwrite) {
        if (verbose) {
            message(
                "The file already exists and won't be overwritten. ",
                "To overwrite this file, please set `overwrite = TRUE`."
            )
        }
        return(invisible(FALSE))
    }

    if (file.exists(output_path) && verbose) {
        message("The file already exists and will be overwritten.")
    }

    if (!dir.exists(dataset_dir)) {
        dir.create(dataset_dir)
    }
    if (verbose) {
        message("The datasets will be exported to ", output_path, ".")
    }
    x_yaml <- yaml::as.yaml(x, precision = 22L, indent = 2L)
    writeLines(
        text = enc2utf8(x_yaml),
        con = output_path,
        useBytes = TRUE
    )
    output_path <- normalizePath(output_path, mustWork = TRUE)
    return(invisible(output_path))
}

#' @title Save datasets in a yaml file
#'
#' @param x an object of class \code{IssuesTB}, \code{LabelsTB} or
#' \code{MilestonesTB}.
#' @inheritParams get
#' @param overwrite Boolean. If the dataset file already exists,
#'   should it be overwrite? Default is TRUE.
#' @param \dots Currently not used.
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
write_to_dataset <- function(
    x,
    ...
) {
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
    overwrite = TRUE,
    verbose = TRUE,
    ...
) {
    .write(x, dataset_dir, dataset_name, overwrite, verbose)
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
    overwrite = TRUE,
    verbose = TRUE,
    ...
) {
    .write(x, dataset_dir, dataset_name, overwrite, verbose)
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
    overwrite = TRUE,
    verbose = TRUE,
    ...
) {
    .write(x, dataset_dir, dataset_name, overwrite, verbose)
    return(invisible(TRUE))
}

#' @rdname write
#' @exportS3Method write_to_dataset default
#' @method write_to_dataset default
#' @export
write_to_dataset.default <- function(...) {
    stop(
        "This function requires a IssuesTB, LabelsTB or MilestonesTB object.",
        call. = FALSE
    )
}
