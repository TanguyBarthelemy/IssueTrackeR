#' @title Export an R Object to YAML
#'
#' @description
#' This function exports an R object (such as a list, vector, data.frame, etc.)
#' to a YAML file.
#'
#' @param x An R object to export.
#' @inheritParams write
#' @param overwrite A boolean indicating whether to overwrite the file if it
#'   already exists. Defaults to `TRUE`.
#' @param verbose A boolean indicating whether to print additional
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
#' @importFrom checkmate assert_character
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
    checkmate::assert_character(dataset_name, len = 1L)

    output_file <- basename(dataset_name)
    ext_file <- tools::file_ext(output_file)
    if (ext_file %in% c("yaml", "yml")) {
        output_file <- output_file |>
            basename() |>
            tools::file_path_sans_ext()
    } else if (nzchar(ext_file)) {
        stop(
            "The `dataset_name` argument must be a name",
            " or a file with a YAML extension (.yml or .yaml).",
            call. = FALSE
        )
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
#' @param dataset_dir The destination directory where the YAML file will be
#'   saved. By default, the system's temporary directory is used (`tempdir()`).
#' @param dataset_name The name of the output file (without extension).
#'   By default, the name is `"object.yaml"`.
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
#' issues_selector <- init_selector(
#'     source = "Local",
#'     file = file.path(
#'         system.file("data_issues", package = "IssueTrackeR"),
#'         "list_issues.yaml"
#'     )
#' )
#' labels_selector <- init_selector(
#'     source = "Local",
#'     file = file.path(
#'         system.file("data_issues", package = "IssueTrackeR"),
#'         "list_labels.yaml"
#'     )
#' )
#' milestones_selector <- init_selector(
#'     source = "Local",
#'     file = file.path(
#'         system.file("data_issues", package = "IssueTrackeR"),
#'         "list_milestones.yaml"
#'     )
#' )
#'
#' issues <- get_issues(selector = issues_selector)
#' labels <- get_labels(selector = labels_selector)
#' milestones <- get_milestones(selector = milestones_selector)
#'
#' write(x = issues, dataset_dir = tempdir())
#' write(x = labels, dataset_dir = tempdir())
#' write(x = milestones, dataset_dir = tempdir())
#'
#' write(x = issues, dataset_dir = tempdir(),
#'                  dataset_name = "my_issues")
#' write(x = labels, dataset_dir = tempdir(),
#'                  dataset_name = "my_labels")
#' write(x = milestones, dataset_dir = tempdir(),
#'                  dataset_name = "my_milestones")
#'
#' @name write
#'
write <- function(
    x,
    ...
) {
    UseMethod(generic = "write", object = x)
}

#' @rdname write
#' @exportS3Method write IssuesTB
#' @method write IssuesTB
#' @export
write.IssuesTB <- function(
    x,
    dataset_dir = getOption("IssueTrackeR.dataset.dir"),
    dataset_name = getOption("IssueTrackeR.dataset.name"),
    overwrite = TRUE,
    verbose = TRUE,
    ...
) {
    if (is.null(dataset_name)) {
        dataset_name <- "list_issues.yaml"
    } else {
        dataset_name <- paste0("list_issues_", dataset_name, ".yaml")
    }
    .write(x, dataset_dir, dataset_name, overwrite, verbose)
    return(invisible(TRUE))
}


#' @rdname write
#' @exportS3Method write LabelsTB
#' @method write LabelsTB
#' @export
write.LabelsTB <- function(
    x,
    dataset_dir = getOption("IssueTrackeR.dataset.dir"),
    dataset_name = getOption("IssueTrackeR.dataset.name"),
    overwrite = TRUE,
    verbose = TRUE,
    ...
) {
    if (is.null(dataset_name)) {
        dataset_name <- "list_labels.yaml"
    } else {
        dataset_name <- paste0("list_labels_", dataset_name, ".yaml")
    }
    .write(x, dataset_dir, dataset_name, overwrite, verbose)
    return(invisible(TRUE))
}

#' @rdname write
#' @exportS3Method write MilestonesTB
#' @method write MilestonesTB
#' @export
write.MilestonesTB <- function(
    x,
    dataset_dir = getOption("IssueTrackeR.dataset.dir"),
    dataset_name = getOption("IssueTrackeR.dataset.name"),
    overwrite = TRUE,
    verbose = TRUE,
    ...
) {
    if (is.null(dataset_name)) {
        dataset_name <- "list_milestones.yaml"
    } else {
        dataset_name <- paste0("list_milestones_", dataset_name, ".yaml")
    }
    .write(x, dataset_dir, dataset_name, overwrite, verbose)
    return(invisible(TRUE))
}

#' @rdname write
#' @exportS3Method write default
#' @method write default
#' @export
write.default <- function(...) {
    stop(
        "This function requires a IssuesTB, LabelsTB or MilestonesTB object.",
        call. = FALSE
    )
}
