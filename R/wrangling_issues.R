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
#'
#' # Empty issue
#' issue1 <- new_issue()
#'
#' # Custom issue
#' issue2 <- new_issue(
#'     title = "Nouvelle issue",
#'     body = "Un nouveau bug pour la fonction...",
#'     number = 47,
#'     created_at = Sys.Date()
#' )
#'
#' issue3 <- new_issue(issue = issue2)
#'
new_issue <- function(title,
                      body,
                      number,
                      state = c("open", "closed"),
                      created_at = Sys.Date(),
                      labels = NULL,
                      milestone = NULL,
                      issue = list(),
                      repo = NULL,
                      owner = NULL,
                      ...) {

    state <- match.arg(state)

    if (!(missing(title)
          || missing(body)
          || missing(number))) {
        issue <- list(title = title,
                      body = body,
                      number = as.integer(number),
                      created_at = format_timestamp(created_at),
                      labels = labels,
                      milestone = milestone,
                      repo = repo,
                      owner = owner)
    } else if (!missing(issue)) {
        issue[["created_at"]] <- format_timestamp(issue[["created_at"]])
        issue[["number"]] <- as.integer(issue[["number"]])
    }
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
#'
#' # Empty issue
#' issues1 <- new_issues()
#'
#' # Custom issue
#' issues2 <- new_issues(
#'     x = new_issue(
#'         title = "Une autre issue",
#'         body = "J'ai une question au sujet de...",
#'         number = 2,
#'         created_at = Sys.Date()
#'     )
#' )
#'
#' issues3 <- new_issues(x = list(
#'     new_issue(
#'         title = "Nouvelle issue",
#'         body = "Un nouveau bug pour la fonction...",
#'         state = "open",
#'         number = 1,
#'         created_at = Sys.Date()
#'     ),
#'     new_issue(
#'         title = "Une autre issue",
#'         body = "J'ai une question au sujet de...",
#'         state = "closed",
#'         number = 2,
#'         created_at = Sys.Date()
#'     )
#' ))
#' @rdname new_issues
#'
new_issues <- function(x = list()) {
    UseMethod("new_issues", x)
}

#' @rdname new_issues
#' @exportS3Method new_issues IssueTB
#' @method new_issues IssueTB
#' @export
new_issues.IssueTB <- function(x) {
    issues <- list(x)
    return(new_issues(x = issues))
}

#' @rdname new_issues
#' @exportS3Method new_issues IssuesTB
#' @method new_issues IssuesTB
#' @export
new_issues.IssuesTB <- function(x) {
    return(x)
}

#' @rdname new_issues
#' @exportS3Method new_issues default
#' @method new_issues default
#' @export
new_issues.default <- function(x = list()) {
    class(x) <- "IssuesTB"
    return(x)
}

#' @exportS3Method `[[` IssuesTB
#' @method `[[` IssuesTB
#' @export
`[[.IssuesTB` <- function(x, ...) {
    return(new_issue(issue = NextMethod()))
}

#' @exportS3Method `[` IssuesTB
#' @method `[` IssuesTB
#' @export
`[.IssuesTB` <- function(x, ...) {
    return(new_issues(NextMethod()))
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
append.IssuesTB <- function(x, values, after) {
    if (inherits(values, "IssuesTB")) {
        return(new_issues(NextMethod()))
    } else if (inherits(values, "IssueTB")) {
        return(append(x, values = new_issues(values)))
    } else {
        stop("This function requires a IssueTB or IssuesTB object ",
             "for `values` argument.", call. = FALSE)
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
    saved_issues <- new_issues()
    for (issue in x) {
        is_in <- FALSE
        for (already in saved_issues) {
            if (identical(issue, already)) {
                is_in <- TRUE
            }
        }
        if (!is_in) {
            saved_issues <- append(saved_issues, issue)
        }
    }
    return(saved_issues)
}
