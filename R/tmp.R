
devtools::load_all()
library("gitlabr")

format_gl_issues <- function(
        raw_issues,
        verbose = TRUE
) {
    if (nrow(raw_issues) == 0) {
        return(new_issues())
    }
    structurel <- strsplit(raw_issues[["references.full"]], split = "/|#") |>
        lapply(\(x) data.frame(owner = paste0(x[seq_len(length(x) - 2L)], collapse = "/"), repo = x[length(x) - 1L])) |>
        do.call(what = rbind)

    if (any(startsWith(colnames(raw_issues), "labels"))) {
        labels_list <- raw_issues[, startsWith(colnames(raw_issues), "labels")] |>
            t() |>
            as.data.frame() |>
            lapply(FUN = function(x) {
                data.frame(
                    name = x[!is.na(x)],
                    color = rep(NA_character_, length(x[!is.na(x)]))
                )
            }) |>
            unname()
    } else {
        labels_list <- rep(
            x = list(data.frame(
                name = character(0L),
                color = character(0L),
                stringsAsFactors = FALSE
            )),
            times = nrow(raw_issues)
        )
    }
    issues <- new_issues(
        url = raw_issues[["_links.self"]],
        html_url = raw_issues[["web_url"]],
        title = raw_issues[["title"]],
        state = raw_issues[["state"]],
        body = raw_issues[["description"]],
        number = as.integer(raw_issues[["iid"]]),
        labels = labels_list,
        milestone = null_to_default(raw_issues[["milestone.title"]], default = NA_character_),
        comments = format_comments(
            raw_comments = list(),
            urls = raw_issues[["_links.self"]]
        ),
        created_at = raw_issues[["created_at"]] |>
            strptime(format = "%Y-%m-%dT%H:%M:%S") |>
            format_timestamp(),
        closed_at = raw_issues[["closed_at"]] |>
            null_to_default(default = NA_character_) |>
            strptime(format = "%Y-%m-%dT%H:%M:%S") |>
            format_timestamp(),
        closed_by = null_to_default(raw_issues[["closed_by.username"]], default = NA_character_),
        creator = raw_issues[["author.username"]],
        assignee = null_to_default(raw_issues[["assignee.username"]], default = NA_character_),
        state_reason = NA_character_,
        owner = structurel[["owner"]],
        repo = structurel[["repo"]]
    )

    return(issues)
}


set_gitlab_connection(
    gitlab_url = "https://gitlab.com",
    private_token = "glpat-XXX"
)

# a <- gl_list_user_projects(user_id = 2868915)
#
# for (k in 1:nrow(a)) {
#     print(a[k, "name"])
#     id <- a$id[k]
#     print(id)
#     print(dim(gl_list_issues(project = id)))
# }

# 3 issues
id <- 51699988
raw_gl_issues <- gl_list_issues(project = id)
b1 <- format_gl_issues(raw_gl_issues)

# 1 issue avec info manquantes
id <- 29346974
raw_gl_issues <- gl_list_issues(project = id)
b2 <- format_gl_issues(raw_gl_issues)

# Pas d'issues
id <- 15028532
raw_gl_issues <- gl_list_issues(project = id)
b3 <- format_gl_issues(raw_gl_issues)


set_gitlab_connection(
    gitlab_url = "https://git.lab.sspcloud.fr",
    private_token = "glpat-XXX"
)

# Pas d'issues
id <- 2075
raw_gl_issues <- gl_list_issues(project = id)
b4 <- format_gl_issues(raw_gl_issues)

## Faire la différence entre les groups, user, repo, owner... un peu plus compliqué en gitlab j'ai l'impression !
