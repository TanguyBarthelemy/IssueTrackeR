null_to_default <- function(x, default) {
    if (is.list(x)) {
        return(lapply(x, null_to_default, default = default))
    }
    return(ifelse(
        test = is.null(x),
        yes = default,
        no = x
    ))
}

#' @title Format the milestone in a simpler format
#'
#' @param raw_milestone Milestone. Subset of a \code{gh_response} object output
#' from the function \code{\link[gh]{gh}} which contains all the data and
#' metadata for a GitHub milestone.
#' @param verbose A logical value indicating whether to print additional
#' information. Default is \code{TRUE}.
#'
#' @returns a data.frame with 3 entries:
#'
#' \itemize{
#' \item Title: name of the milestone
#' \item description: Description of the milestone
#' \item due_on: date to with the issue is due
#' }
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#'
#' # With milestones
#'
#' \dontrun{
#' raw_milestones <- gh::gh(
#'     repo = "jdplus-main",
#'     owner = "jdemetra",
#'     endpoint = "/repos/:owner/:repo/milestones",
#'     state = "all",
#'     .limit = Inf,
#'     .progress = FALSE
#' )
#' raw_milestone <- raw_milestones[[5L]]
#' format_milestone(raw_milestone)
#' }
#'
format_milestone <- function(raw_milestone, verbose = TRUE) {
    if (verbose) {
        cat("\t- ", raw_milestone[["title"]], "... Done!\n")
    }
    description <- null_to_default(
        x = raw_milestone[["description"]],
        default = ""
    )
    due_on <- format_timestamp(null_to_default(
        x = raw_milestone[["due_on"]],
        default = NA_real_
    ))
    closed_at <- format_timestamp(null_to_default(
        x = raw_milestone[["closed_at"]],
        default = NA_real_
    ))
    creator <- null_to_default(
        x = raw_milestone[["creator"]][["login"]],
        default = NA_character_
    )

    output <- data.frame(
        title = raw_milestone[["title"]],
        description = description,
        due_on = due_on,
        closed_at = closed_at,
        creator = creator,
        state = raw_milestone[["state"]],
        nb_issues_open = raw_milestone[["open_issues"]],
        nb_issues_closed = raw_milestone[["closed_issues"]]
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
    state = c("open", "closed", "all"),
    verbose = TRUE
) {
    source <- match.arg(source)
    state <- match.arg(state)

    if (source == "online") {
        if (is.null(repo)) {
            if (length(owner) > 1L) {
                milestones <- lapply(
                    X = owner,
                    FUN = get_milestones,
                    source = "online",
                    repo = NULL,
                    state = state,
                    verbose = verbose,
                    dataset_dir = NULL,
                    dataset_name = NULL
                ) |>
                    do.call(what = rbind)

                return(milestones)
            }
            list_repo <- get_all_repos(owner, verbose = verbose)

            milestones <- lapply(
                X = list_repo,
                FUN = get_milestones,
                source = "online",
                owner = owner,
                state = state,
                verbose = verbose,
                dataset_dir = NULL,
                dataset_name = NULL
            ) |>
                do.call(what = rbind)

            return(milestones)
        }

        if (verbose) {
            cat("Repo:", repo, " owner:", owner, "\n")
        }
        raw_milestones <- try(expr = {
            gh::gh(
                repo = repo,
                owner = owner,
                endpoint = "/repos/:owner/:repo/milestones",
                state = state,
                .limit = Inf,
                .progress = FALSE
            )
        })
        check_response(raw_milestones)
        milestones <- format_milestones(raw_milestones, verbose = verbose)

        if (nrow(milestones) > 0L) {
            milestones <- cbind(milestones, repo = repo, owner = owner)
        }
    } else if (source == "local") {
        if (tools::file_ext(dataset_name) == "yaml") {
            input_file <- tools::file_path_sans_ext(dataset_name)
        }
        input_path <- file.path(dataset_dir, input_file) |>
            paste0(".yaml") |>
            normalizePath(mustWork = TRUE)

        if (verbose) {
            message("The milestones will be read from ", input_path, ".")
        }
        milestones <- readLines(con = input_path, encoding = "UTF-8") |>
            yaml::yaml.load() |>
            as.data.frame()
        if (nrow(milestones) > 0L) {
            milestones[["due_on"]] <- format_timestamp(
                x = milestones[["due_on"]]
            )
        }
    } else {
        stop("wrong argument source", call. = FALSE)
    }

    class(milestones) <- c("MilestonesTB", "data.frame")
    return(milestones)
}

#' @title Format the milestones in a simpler format
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
#'
#' \dontrun{
#' # With milestones
#' milestones_jdplus_main <- gh::gh(
#'     repo = "jdplus-main",
#'     owner = "jdemetra",
#'     endpoint = "/repos/:owner/:repo/milestones",
#'     state = "all",
#'     .limit = Inf,
#'     .progress = FALSE
#'  )
#' format_milestones(milestones_jdplus_main)
#' }
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
        cat("Done!", nrow(new_mlst_structure), "milestones found.\n", sep = " ")
    }
    return(new_mlst_structure)
}
