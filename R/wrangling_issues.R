#' @title Create a new \code{IssueTB} object
#'
#' @param title a string. The title of the issue.
#' @param body a string. The title of the issue.
#' @param number a string. The title of the issue.
#' @param created_at a date. The title of the issue.
#' @param labels a vector string (or missing). The labels of the issue.
#' @param milestone a string (or missing). The milestone of the issue.
#' @param issue a list representing the object.
#' @param repo a string. The name of the GitHub repository to which the issue
#' belongs.
#' @param username a string. The name of the owner of the GitHub repository to
#' which the issue belongs.
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
                      created_at = Sys.Date(),
                      labels = NULL,
                      milestone = NULL,
                      issue = list(),
                      repo = NULL,
                      username = NULL,
                      ...) {
    if (!(missing(title)
          || missing(body)
          || missing(number))) {
        issue <- list(title = title,
                      body = body,
                      number = as.integer(number),
                      created_at = created_at |>
                          as.POSIXct() |>
                          as.integer() |>
                          as.POSIXct(),
                      labels = labels,
                      milestone = milestone,
                      repo = repo,
                      username = username)
    } else if (!missing(issue)) {
        issue[["created_at"]] <- issue[["created_at"]] |>
            as.POSIXct() |>
            as.integer() |>
            as.POSIXct()
        issue[["number"]] <- as.integer(issue[["number"]])
    }
    class(issue) <- "IssueTB"
    return(issue)
}

#' @title Create a new \code{IssuesTB} object
#'
#' @param issues a list containing \code{IssueTB} objects
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
#' issues2 <- new_issues(list(
#'     new_issue(
#'         title = "Nouvelle issue",
#'         body = "Un nouveau bug pour la fonction...",
#'         number = 1,
#'         created_at = Sys.Date()
#'     ),
#'     new_issue(
#'         title = "Une autre issue",
#'         body = "J'ai une question au sujet de...",
#'         number = 2,
#'         created_at = Sys.Date()
#'     )
#' ))
#'
new_issues <- function(issues = list()) {
    class(issues) <- "IssuesTB"
    return(issues)
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

#' @exportS3Method append IssuesTB
#' @method append IssuesTB
#' @export
append.IssuesTB <- function(...) {
    return(new_issues(NextMethod()))
}
