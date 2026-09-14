
empty_selector <- function() {
    selector <- list()
    class(selector) <- "SelectorTB"
    return(selector)
}

init_selector <- function(source, ...) {
    args <- list(...)
    if (length(args) == 0L) {
        return(empty_selector())
    }

    checkmate::assert_character(source, len = 1L)
    source <- tolower(source)
    checkmate::assert_character(source, len = 1L)

    if (source == "GitHub") {
        return(init_selector_github(...))
    } else if (source =="GitLab") {
        return(init_selector_gitlab(...))
    } else if (source == "local") {
        return(init_selector_local(...))
    }

    stop("Wrong source. Source should be \"GitHub\", \"GitLab\" or \"local\".")
}

init_selector_github <- function(owner = NULL, repo = NULL) {
    checkmate::assert_character(owner, null.ok = TRUE)
    checkmate::assert_character(repo, null.ok = TRUE)

    if (is.null(owner)) {
        warning("There is no owner Please provide a owner.")
        return(empty_selector())
    } else if (length(owner) > 1) {
        selector <- lapply(owner, init_selector_github, repo = repo) |>
            do.call(what = merge_selector)
        return(selector)
    } else if (is.null(repo)) {
        return(init_selector_github(owner = owner, repo = get_all_repos(owner)))
    }

    selector <- lapply(repo, \(r) list(source = "GitHub", repo = r, owner = owner))
    return(selector)
}

init_selector_gitlab <- function(project_id = NULL) {
    checkmate::assert_integerish(project_id, null.ok = TRUE)
    if (is.null(project_id)) {
        warning("There is no project_id. Please provide a project_id.")
        return(empty_selector())
    }
    selector <- lapply(project_id, \(id) list(id = id, source = "GitLab"))
    class(selector) <- "SelectorTB"
    return(selector)
}

init_selector_local <- function(file) {
    file <- normalizePath(file, mustWork = TRUE)
    selector <- do.call(selectors, what = c)
    class(selector) <- "SelectorTB"
    return(selector)
    return(empty_selector())
}

merge_selector <- function(...) {
    selectors <- list(...)
    if (length(selectors) == 0L) {
        return(empty_selector())
    }
    selector <- do.call(selectors, what = c)
    class(selector) <- "SelectorTB"
    return(selector)
}

a <- init_selector_github(owner = c("jdemetra", "rjdverse"))
b <- init_selector_gitlab(45:48)
c <- init_selector_local(file = "bonjour")
get_all_repos
