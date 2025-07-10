#' @title Create a new \code{IssueTB} object
#'
#' @param title a string. The title of the issue.
#' @param state a character string that is either \code{"open"} (by default) if
#' the issue is still open or \code{"closed"} if the issue is now closed.
#' @param body a string. The title of the issue.
#' @param number a string. The title of the issue.
#' @param created_at a date. The title of the issue.
#' @param labels a vector string (or missing). The labels of the issue.
#' @param milestone a string (or missing). The milestone of the issue.
#' @param issue a list representing the object.
#' @inheritParams get_issues
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
#'     number = 47,
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
    issue <- do.call(args = x, what = new_issue)
    return(issue)
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
new_issue.default <- function(
    x,
    title,
    body,
    number,
    state = c("open", "closed"),
    created_at = Sys.Date(),
    labels = NULL,
    milestone = NA_character_,
    repo = NA_character_,
    owner = NA_character_,
    url = NA_character_,
    html_url = NA_character_,
    comments = NULL,
    creator = NA_character_,
    assignee = NA_character_,
    ...
) {
    state <- match.arg(state)

    if (missing(title) && missing(body) && missing(number)) {
        title <- character(0L)
        body <- character(0L)
        number <- integer(0L)
        state <- character(0L)
        created_at <- format_timestamp(as.Date(character(0L)))
        labels <- list()
        milestone <- character(0L)
        repo <- character(0L)
        owner <- character(0L)
        url <- character(0L)
        html_url <- character(0L)
        comments <- list()
        creator <- character(0L)
        assignee <- character(0L)
    }

    issue <- list(
        number = as.integer(number),
        title = title,
        body = body,
        state = state,
        url = url,
        html_url = html_url,
        milestone = milestone,
        created_at = format_timestamp(created_at),
        creator = creator,
        assignee = assignee,
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
#' @param x a list containing \code{IssueTB} objects
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
#'     number = 2,
#'     created_at = Sys.Date()
#' )
#' issues2 <- new_issues(x = issue1)
#'
#' # Custom issues
#' issues3 <- new_issues(
#'     title = "Une autre issue",
#'     state = "open",
#'     body = "J'ai une question au sujet de...",
#'     number = 2,
#'     created_at = Sys.Date()
#' )
#'
#' issues4 <- new_issues(
#'     title = c("Nouvelle issue", "Une autre issue"),
#'     body = c("Un nouveau bug pour la fonction...",
#'              "J'ai une question au sujet de..."),
#'     state = c("open", "closed"),
#'     number = 1:2,
#'     created_at = Sys.Date()
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
new_issues.default <- function(
    x,
    title,
    body,
    number,
    state,
    created_at = Sys.Date(),
    labels = list(),
    milestone = NA_character_,
    repo = NA_character_,
    owner = NA_character_,
    url = NA_character_,
    html_url = NA_character_,
    comments = list(),
    creator = NA_character_,
    assignee = NA_character_,
    ...
) {
    if (missing(title) && missing(body) && missing(number) && missing(state)) {
        title <- character(0L)
        body <- character(0L)
        number <- integer(0L)
        state <- character(0L)
        created_at <- format_timestamp(as.Date(character(0L)))
        labels <- list()
        milestone <- character(0L)
        repo <- character(0L)
        owner <- character(0L)
        url <- character(0L)
        html_url <- character(0L)
        comments <- list()
        creator <- character(0L)
        assignee <- character(0L)
    }

    if (length(labels) == 0L) {
        labels <- rep(list(NULL), times = length(title))
    }
    if (length(comments) == 0L) {
        comments <- rep(list(NULL), times = length(title))
    }

    issues <- data.frame(
        number = as.integer(number),
        title = title,
        body = body,
        state = state,
        url = url,
        html_url = html_url,
        milestone = milestone,
        created_at = format_timestamp(created_at),
        creator = creator,
        assignee = assignee,
        owner = owner,
        repo = repo,
        stringsAsFactors = FALSE
    )
    issues$labels <- labels
    issues$comments <- comments

    class(issues) <- c("IssuesTB", "data.frame")

    return(issues)
}

#' @exportS3Method `[` IssuesTB
#' @method `[` IssuesTB
#' @export
`[.IssuesTB` <- function(x, ..., drop = TRUE) {
    output <- new_issues(NextMethod(object = x, generic = "IssuesTB"))
    if (drop && nrow(output) == 1L) {
        return(new_issue(output))
    }
    return(output)
}

#' @exportS3Method `[<-` IssuesTB
#' @method `[<-` IssuesTB
#' @export
`[<-.IssuesTB` <- function(x, ..., value) {
    return(new_issues(NextMethod()))
}

#' @exportS3Method `[[<-` IssuesTB
#' @method `[[<-` IssuesTB
#' @export
`[[<-.IssuesTB` <- function(x, ..., value) {
    return(new_issues(NextMethod()))
}

#' @exportS3Method c IssuesTB
#' @method c IssuesTB
#' @export
c.IssuesTB <- function(...) {
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
append.IssuesTB <- function(x, values, after = length(x)) {
    if (after > nrow(x)) after <- nrow(x)
    if (after < 0L) after <- 0L

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

#' @exportS3Method unique IssuesTB
#' @method unique IssuesTB
#' @export
unique.IssuesTB <- function(x, incomparables = FALSE, ...) {
    return(x[!duplicated(x), ])
}
