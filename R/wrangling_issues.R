#' @title Create a new \code{IssueTB} object
#'
#' @param x a object representing an issue (\code{IssueTB} object, a \code{list}
#' or a \code{data.frame})
#' @param title a string. The title of the issue.
#' @param state a string that is either \code{"open"} (by default) if
#' the issue is still open or \code{"closed"} if the issue is now closed.
#' @param body a string. The body (text) of the issue.
#' @param number a string. The number of the issue.
#' @param created_at a date (or timestamp). The creation date of the issue.
#' @param closed_at a date (or timestamp). The closing date of the issue.
#' @param closed_by a string. The GitHub username of the person who closed the
#' issue.
#' @param labels a vector string (or missing). The labels of the issue.
#' @param milestone a string (or missing). The milestone of the issue.
#' @inheritParams get
#' @param url a string. The URL of the API to the GitHub issue.
#' @param html_url a string. The URL to the GitHub issue.
#' @param comments vector of string (the comments of the issue)
#' @param creator a string. The GitHub username of the creator of the issue.
#' @param assignee a string. The GitHub username of the assignee of the issue.
#' @param state_reason a string. \code{"open"}, \code{"completed"},
#' \code{"reopened"}, \code{"not_planned"} or \code{"duplicate"}.
#' @param \dots Other information we would like to add to the issue.
#'
#' @returns a \code{IssueTB} object.
#' @export
#'
#' @examples
#' # Empty issue
#' issue1 <- new_issue()
#'
#' # Custom issue
#' issue1 <- new_issue(
#'     title = "Nouvelle issue",
#'     body = "Un nouveau bug pour la fonction...",
#'     number = 47L,
#'     created_at = Sys.Date()
#' )
#'
#' issue2 <- new_issue(x = issue1)
new_issue <- function(x = NULL, ...) {
    UseMethod("new_issue", x)
}

#' @rdname new_issue
#' @exportS3Method new_issue IssueTB
#' @method new_issue IssueTB
#' @export
new_issue.IssueTB <- function(x, ...) {
    return(x)
}

#' @rdname new_issue
#' @exportS3Method new_issue data.frame
#' @method new_issue data.frame
#' @export
new_issue.data.frame <- function(x, ...) {
    if (nrow(x) != 1L) {
        stop("There are several issues in the object `x`.", call. = FALSE)
    }
    issue <- unclass(x)
    issue$labels <- issue$labels[[1L]]
    issue$comments <- issue$comments[[1L]]
    return(new_issue(issue))
}

#' @rdname new_issue
#' @exportS3Method new_issue list
#' @method new_issue list
#' @export
new_issue.list <- function(x, ...) {
    issue <- do.call(args = x, what = new_issue)
    return(issue)
}

#' @rdname new_issue
#' @exportS3Method new_issue IssuesTB
#' @method new_issue IssuesTB
#' @export
new_issue.IssuesTB <- function(x, ...) {
    if (nrow(x) != 1L) {
        stop("There are several issues in the object `x`.", call. = FALSE)
    }
    return(NextMethod())
}

#' @rdname new_issue
#' @exportS3Method new_issue default
#' @method new_issue default
#' @export
#' @importFrom checkmate assert_character
#' @importFrom checkmate assert_integer
#' @importFrom checkmate assert_numeric
#' @importFrom checkmate assert_scalar
#' @importFrom checkmate assert_data_frame
new_issue.default <- function(
    x,
    title = NA_character_,
    body = NA_character_,
    number = NA_integer_,
    state = NA_character_,
    created_at = as.Date(NA_integer_),
    closed_at = as.Date(NA_integer_),
    closed_by = NA_character_,
    labels = NULL,
    milestone = NA_character_,
    repo = NA_character_,
    owner = NA_character_,
    url = NA_character_,
    html_url = NA_character_,
    comments = NULL,
    creator = NA_character_,
    assignee = NA_character_,
    state_reason = NA_character_,
    ...
) {

    checkmate::assert_character(title)
    checkmate::assert_character(body)
    checkmate::assert_integer(number)
    checkmate::assert_character(state)
    checkmate::assert_numeric(created_at)
    checkmate::assert_numeric(closed_at)
    checkmate::assert_character(closed_by)
    checkmate::assert_character(milestone)
    checkmate::assert_character(repo)
    checkmate::assert_character(owner)
    checkmate::assert_character(url)
    checkmate::assert_character(html_url)
    checkmate::assert_character(creator)
    checkmate::assert_character(assignee)
    checkmate::assert_character(state_reason)

    checkmate::assert_scalar(title, na.ok = TRUE)
    checkmate::assert_scalar(body, na.ok = TRUE)
    checkmate::assert_scalar(number, na.ok = TRUE)
    checkmate::assert_scalar(state, na.ok = TRUE)
    checkmate::assert_scalar(created_at, na.ok = TRUE)
    checkmate::assert_scalar(closed_at, na.ok = TRUE)
    checkmate::assert_scalar(closed_by, na.ok = TRUE)
    checkmate::assert_scalar(milestone, na.ok = TRUE)
    checkmate::assert_scalar(repo, na.ok = TRUE)
    checkmate::assert_scalar(owner, na.ok = TRUE)
    checkmate::assert_scalar(url, na.ok = TRUE)
    checkmate::assert_scalar(html_url, na.ok = TRUE)
    checkmate::assert_scalar(creator, na.ok = TRUE)
    checkmate::assert_scalar(assignee, na.ok = TRUE)
    checkmate::assert_scalar(state_reason, na.ok = TRUE)

    checkmate::assert_data_frame(labels, null.ok = TRUE)
    checkmate::assert_data_frame(comments, null.ok = TRUE)

    issue <- list(
        number = as.integer(number),
        title = title,
        body = body,
        state = state,
        url = url,
        html_url = html_url,
        milestone = milestone,
        created_at = format_timestamp(created_at),
        closed_at = format_timestamp(closed_at),
        closed_by = closed_by,
        creator = creator,
        assignee = assignee,
        state_reason = state_reason,
        owner = owner,
        repo = repo,
        labels = labels,
        comments = comments
    )

    class(issue) <- "IssueTB"
    return(issue)
}

#' @title Create a new \code{IssuesTB} object
#'
#' @param x a object representing a list of issues (\code{IssuesTB} object, a
#' \code{list} or a \code{data.frame})
#' @param title a vector of string. The titles of the issues.
#' @param state a vector of string that is either \code{"open"} (by default) if
#' the issues are still open or \code{"closed"} if the issues are now closed.
#' @param body a vector of string. The bodies (text) of the issues.
#' @param number a vector of string. The numbers of the issues.
#' @param created_at a vector of date (or timestamp). The creation date of the
#' issues.
#' @param closed_at a vector of date (or timestamp). The closing date of the
#' issues.
#' @param closed_by a vector of string. The GitHub usernames of the person who
#' closed the issues.
#' @param labels a list of vector string (or missing). The labels of the issues.
#' @param milestone a vector of string (or missing). The milestones of the
#' issues.
#' @inheritParams get
#' @param url a vector of string. The URLs of the API to the GitHub issues.
#' @param html_url a vector of string. The URLs to the GitHub issues.
#' @param comments a list of vector string. The comments of the issues.
#' @param creator a vector of string. The GitHub usernames of the creator of the
#'  issues.
#' @param assignee a vector of string. The GitHub usernames of the assignee of
#' the issues.
#' @param state_reason a vector of string. \code{"open"}, \code{"completed"},
#' \code{"reopened"}, \code{"not_planned"} or \code{"duplicate"}.
#' @param \dots Other information we would like to add to the issue.
#'
#' @returns a \code{IssuesTB} object.
#' @export
#'
#' @examples
#' # Empty list of issues
#' issues1 <- new_issues()
#'
#' # List of issues from issue
#' issue1 <- new_issue(
#'     title = "Une autre issue",
#'     state = "open",
#'     body = "J'ai une question au sujet de...",
#'     number = 2L,
#'     created_at = Sys.Date()
#' )
#' issues2 <- new_issues(x = issue1)
#'
#' # Custom issues
#' issues3 <- new_issues(
#'     title = "Une autre issue",
#'     state = "open",
#'     body = "J'ai une question au sujet de...",
#'     number = 2L,
#'     created_at = Sys.Date()
#' )
#'
#' issues4 <- new_issues(
#'     title = c("Nouvelle issue", "Une autre issue"),
#'     body = c("Un nouveau bug pour la fonction...",
#'              "J'ai une question au sujet de..."),
#'     state = c("open", "closed"),
#'     number = 1:2,
#'     created_at = c(Sys.Date() - 30, Sys.Date())
#' )
#' @rdname new_issues
#'
new_issues <- function(x = NULL, ...) {
    UseMethod("new_issues", x)
}

#' @rdname new_issues
#' @exportS3Method new_issues IssueTB
#' @method new_issues IssueTB
#' @export
new_issues.IssueTB <- function(x, ...) {
    x$comments <- list(x$comments)
    x$labels <- list(x$labels)
    issues <- do.call(args = x, what = new_issues)
    return(issues)
}

#' @rdname new_issues
#' @exportS3Method new_issues IssuesTB
#' @method new_issues IssuesTB
#' @export
new_issues.IssuesTB <- function(x, ...) {
    return(x)
}

#' @rdname new_issues
#' @exportS3Method new_issues data.frame
#' @method new_issues data.frame
#' @export
new_issues.data.frame <- function(x, ...) {
    issues <- do.call(args = x, what = new_issues)
    return(issues)
}

#' @rdname new_issues
#' @exportS3Method new_issues list
#' @method new_issues list
#' @export
new_issues.list <- function(x, ...) {
    issues <- do.call(args = x, what = new_issues)
    return(issues)
}

#' @rdname new_issues
#' @exportS3Method new_issues default
#' @method new_issues default
#' @export
#' @importFrom checkmate assert_character
#' @importFrom checkmate assert_integer
#' @importFrom checkmate assert_numeric
#' @importFrom checkmate assert_list
new_issues.default <- function(
    x,
    title,
    body,
    number,
    state,
    created_at = as.Date(NA_integer_),
    closed_at = as.Date(NA_integer_),
    closed_by = NA_character_,
    labels = list(),
    comments = list(),
    milestone = NA_character_,
    repo = NA_character_,
    owner = NA_character_,
    url = NA_character_,
    html_url = NA_character_,
    creator = NA_character_,
    assignee = NA_character_,
    state_reason = NA_character_,
    ...
) {
    if (missing(title) && missing(body) && missing(number) && missing(state)) {
        title <- character(0L)
        body <- character(0L)
        number <- integer(0L)
        state <- character(0L)
        created_at <- format_timestamp(as.Date(character(0L)))
        closed_at <- format_timestamp(as.Date(character(0L)))
        closed_by <- character(0L)
        milestone <- character(0L)
        repo <- character(0L)
        owner <- character(0L)
        url <- character(0L)
        html_url <- character(0L)
        creator <- character(0L)
        assignee <- character(0L)
        state_reason <- character(0L)
    }

    checkmate::assert_character(title)
    checkmate::assert_character(body)
    checkmate::assert_integer(number)
    checkmate::assert_character(state)
    checkmate::assert_numeric(created_at)
    checkmate::assert_numeric(closed_at)
    checkmate::assert_character(closed_by)
    checkmate::assert_character(milestone)
    checkmate::assert_character(repo)
    checkmate::assert_character(owner)
    checkmate::assert_character(url)
    checkmate::assert_character(html_url)
    checkmate::assert_character(creator)
    checkmate::assert_character(assignee)
    checkmate::assert_character(state_reason)

    if (missing(labels)) {
        labels <- rep(
            x = list(data.frame(
                name = character(0L),
                color = character(0L),
                stringsAsFactors = FALSE
            )),
            times = length(title)
        )
    }
    if (missing(comments)) {
        comments <- rep(
            x = list(data.frame(
                text = character(0L),
                author = character(0L),
                stringsAsFactors = FALSE
            )),
            times = length(title)
        )
    }

    checkmate::assert_list(labels)
    checkmate::assert_list(comments)

    issues <- data.frame(
        number = as.integer(number),
        title = title,
        body = body,
        state = state,
        url = url,
        html_url = html_url,
        milestone = milestone,
        created_at = format_timestamp(created_at),
        closed_at = format_timestamp(closed_at),
        closed_by = closed_by,
        creator = creator,
        assignee = assignee,
        state_reason = state_reason,
        owner = owner,
        repo = repo,
        stringsAsFactors = FALSE
    )
    issues$labels <- labels
    issues$comments <- comments

    class(issues) <- c("IssuesTB", "data.frame")

    return(issues)
}

#' @title Extraction and replacement of information in issues
#'
#' @param x An object of class \code{IssuesTB}.
#' @inheritParams base::`[`
#'
#' @returns Information inside the `IssuesTB` object
#'
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' open_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' first_issue <- open_issues[1, ]
#' number <- open_issues[1, 1]
#' state <- open_issues[1, "state"]
#' open_issues[1, "state"] <- "closed"
#' open_issues[["number"]] <- seq_along(open_issues[["number"]])
#'
#' @exportS3Method `[` IssuesTB
#' @method `[` IssuesTB
#' @export
#'
#' @name extraction-issues
#' @noRd
`[.IssuesTB` <- function(x, i, j, drop = TRUE) {
    output <- NextMethod("[")
    Narg <- nargs() - !missing(drop)
    # Cas sélection de colonne
    if (!missing(j)) {
        if (length(j) > 1L || !drop) {
            return(as.data.frame(output))
        }
        return(output)
    } else if (Narg == 2L && !missing(i)) {
        return(as.data.frame(output))
    }

    output <- new_issues(output)
    if (drop && nrow(output) == 1L) {
        return(new_issue(output))
    }
    return(output)
}

#' @noRd
#' @rdname extraction-issues
#' @exportS3Method `[<-` IssuesTB
#' @method `[<-` IssuesTB
#' @export
`[<-.IssuesTB` <- function(x, ..., value) {
    return(new_issues(NextMethod()))
}

#' @noRd
#' @rdname extraction-issues
#' @exportS3Method `[[<-` IssuesTB
#' @method `[[<-` IssuesTB
#' @export
`[[<-.IssuesTB` <- function(x, ..., value) {
    return(new_issues(NextMethod()))
}

#' @rdname append
#' @export
#' @inherit base::append
append <- function(x, values, after = length(x)) {
    UseMethod("append")
}

#' @rdname append
#' @exportS3Method append IssuesTB
#' @param values a \code{IssueTB} or a \code{IssuesTB} object.
#' @method append IssuesTB
#' @export
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' open_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' closed_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "closed_issues.yaml"
#' )
#' new_issues <- append(open_issues, closed_issues)
append.IssuesTB <- function(x, values, after = nrow(x)) {
    if (after > nrow(x)) {
        after <- nrow(x)
    }
    if (after < 0L) {
        after <- 0L
    }

    if (inherits(values, "IssuesTB")) {
        return(rbind(
            x[seq_len(after), , drop = FALSE],
            values,
            x[-seq_len(after), , drop = FALSE]
        ))
    } else if (inherits(values, "IssueTB")) {
        return(append(x, values = new_issues(values), after = after))
    } else {
        stop(
            "This function requires a IssueTB or IssuesTB object ",
            "for `values` argument.",
            call. = FALSE
        )
    }
}

#' @exportS3Method append default
#' @method append default
#' @export
append.default <- function(x, values, after = length(x)) {
    base::append(x, values, after)
}

#' @title Combining Issues
#'
#' @description
#' S3 method for combining \code{IssueTB} objects by rows.
#'
#' @param \dots Objects of class \code{IssueTB} or \code{IssuesTB}.
#'
#' @returns An \code{IssueTB} object containing the combined rows of all input
#' objects.
#'
#' @seealso
#' \code{\link[base]{rbind}} for the generic function
#'
#' @name rbind-issues
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' open_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' new_issues <- rbind(open_issues[1, ], open_issues[-1, ])
#' @exportS3Method rbind IssueTB
#' @method rbind IssueTB
#' @export
rbind.IssueTB <- function(...) {
    list(...) |>
        lapply(FUN = new_issues) |>
        do.call(what = rbind.data.frame) |>
        new_issues()
}

#' @rdname rbind-issues
#' @exportS3Method rbind IssuesTB
#' @method rbind IssuesTB
#' @export
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' open_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' closed_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "closed_issues.yaml"
#' )
#' new_issues <- rbind(open_issues, closed_issues)
rbind.IssuesTB <- function(...) {
    list(...) |>
        lapply(FUN = new_issues) |>
        do.call(what = rbind.data.frame) |>
        new_issues()
}

#' @rdname subset
#' @inherit base::subset
#' @param x An object of class \code{IssuesTB}.
#' @exportS3Method subset IssuesTB
#' @method subset IssuesTB
#' @export
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' open_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' new_issues <- subset(open_issues, number < 150)
subset.IssuesTB <- function(x, ...) {
    output <- new_issues(NextMethod())
    return(output)
}

#' @title Random Sampling
#'
#' @description
#' Generic function for drawing a random sample from an object.
#'
#' For objects of class IssuesTB, this method returns a random subset
#' of the issues.
#'
#' @inheritParams base::sample
#'
#' @returns
#' - For `IssuesTB` objects, an object of the same class containing the
#' sampled issues.
#' - For all other objects, the result of [base::sample()].
#'
#' @details
#' The arguments and overall behavior are consistent with [base::sample()].
#' For details about the sampling algorithm, probability weights, and special
#' cases, refer to the original documentation:
#' https://stat.ethz.ch/R-manual/R-devel/library/base/html/sample.html
#'
#' @seealso [base::sample()]
#'
#' @name sample-issues
#' @export
sample <- function(x, size, replace = FALSE, prob = NULL) {
    UseMethod("sample")
}

#' @param x An object of class \code{IssuesTB}.
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' open_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' new_issues <- sample(open_issues, size = 5L)
#' @rdname sample-issues
#' @exportS3Method sample IssuesTB
#' @method sample IssuesTB
#' @export
sample.IssuesTB <- function(
    x,
    size = nrow(x),
    replace = FALSE,
    prob = NULL
) {
    selected_lines <- sample.int(
        n = nrow(x),
        size = size,
        replace = replace,
        prob = prob
    )
    return(x[selected_lines, , drop = FALSE])
}

#' @rdname sample-issues
#' @exportS3Method sample default
#' @method sample default
#' @export
sample.default <- function(x, size, replace = FALSE, prob = NULL) {
    base::sample(x = x, size = size, replace = replace, prob = prob)
}

#' @title Unique issues of an IssuesTB Object
#'
#' @description
#' Keep only different issues from a IssuesTB Object
#'
#' @param x An object of class \code{IssuesTB}.
#' @inheritParams base::unique
#'
#' @returns
#' An `IssuesTB` object containing only unique issues.
#'
#' @details
#' This method is consistent with [base::unique()]. For details about the
#' generic function and its default methods, refer to the original
#' documentation:
#' https://stat.ethz.ch/R-manual/R-devel/library/base/html/unique.html
#'
#' @examples
#' path <- system.file("data_issues", package = "IssueTrackeR")
#' open_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = path,
#'     dataset_name = "open_issues.yaml"
#' )
#' new_issues <- unique(open_issues)
#'
#' @seealso [base::unique()], [base::duplicated()]
#'
#' @name unique-issues
#' @exportS3Method unique IssuesTB
#' @method unique IssuesTB
#' @export
unique.IssuesTB <- function(x, incomparables = FALSE, ...) {
    return(x[!duplicated(x), ])
}

#' @title Count the number of Issues
#'
#' @description
#' Generic function to count the number of issues in a list of issues.
#'
#' @param x An object of class \code{IssuesTB}.
#' @param verbose A logical value indicating whether to print additional
#' information. Default is \code{TRUE}.
#' @param \dots Currently not used.
#'
#' @returns Integer. The number of issues.
#'
#' @examples
#' all_issues <- get_issues(
#'     source = "local",
#'     dataset_dir = system.file("data_issues", package = "IssueTrackeR"),
#'     dataset_name = "open_issues.yaml"
#' )
#'
#' count_issues(all_issues)
#' @export
count_issues <- function(x, ...) {
    UseMethod("count_issues", x)
}

#' @rdname count_issues
#' @exportS3Method count_issues IssuesTB
#' @method count_issues IssuesTB
#' @export
count_issues.IssuesTB <- function(x, verbose = TRUE, ...) {
    return(nrow(x))
}

#' @rdname count_issues
#' @exportS3Method count_issues default
#' @method count_issues default
#' @export
count_issues.default <- function(...) {
    stop("`x` should be a `IssuesTB` object.", call. = FALSE)
}
