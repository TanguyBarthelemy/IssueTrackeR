
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {IssueTrackeR}

<!-- badges: start -->
<!-- [![R-CMD-check](https://github.com/TanguyBarthelemy/IssueTrackeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/TanguyBarthelemy/IssueTrackeR/actions/workflows/R-CMD-check.yaml) -->
<!-- [![lint](https://github.com/TanguyBarthelemy/IssueTrackeR/actions/workflows/lint.yaml/badge.svg)](https://github.com/TanguyBarthelemy/IssueTrackeR/actions/workflows/lint.yaml) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/TanguyBarthelemy/IssueTrackeR/graph/badge.svg)](https://app.codecov.io/gh/TanguyBarthelemy/IssueTrackeR) -->
<!-- badges: end -->

**{IssueTrackeR}** is an R package designed to retrieve and manage
GitHub issues directly within R. This package allows users to
efficiently track and handle issues from their GitHub repositories.

This package relies a lot on the package
[{gh}](https://github.com/r-lib/gh) to use the GitHub API and retrieve
data from GitHub.

## Installation

You can install the development version of {IssueTrackeR} from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("TanguyBarthelemy/IssueTrackeR")
```

## Features

- **Retrieve Issues**: Fetch issues from any (with sufficient rights)
  GitHub repository.
- **Issue Management**: Class S3 to manage the issues as a datasets
  within R.
- **Filtering**: Filter issues by labels, content and milestones.

## Usage

``` r
library("IssueTrackeR")
#> 
#> Attaching package: 'IssueTrackeR'
#> The following object is masked from 'package:base':
#> 
#>     append
```

### Retrieve information from GitHub

To get information from a repository, you can call the functions
`get_issues`, `get_labels` and `get_milestones`

``` r
# From online
my_issues <- get_issues(source = "online", owner = "jdemetra", repo = "jdplus-main", verbose = FALSE)
#> Running gh query ■■■■■■■■■■■■■■■■ 50% | ETA: 1sRunning gh query
#> ■■■■■■■■■■■■■■■■■■■■■■■ 75% | ETA: 1s
my_labels <- get_labels(source = "online", owner = "jdemetra", repo = "jdplus-main")
#> Reading labels... Done!
#> 12 labels found.
my_milestones <- get_milestones(source = "online", owner = "jdemetra", repo = "jdplus-main")
#> Reading milestones... 
#>  -  backlog ... Done!
#>  -  3.2.2 ... Done!
#>  -  3.2.3 ... Done!
#>  -  3.4.0 ... Done!
#>  -  3.5.0 ... Done!
#>  -  3.6.0 ... Done!
#> Done! 6 milestones found.
```

### Save issues in local

You can also write the datasets in local with
`write_issues_to_dataset()`, `write_labels_to_dataset()` and
`write_milestones_to_dataset()`:

``` r
write_issues_to_dataset(
    issues = my_issues, 
    dataset_dir = tempdir()
)
#> The datasets will be exported to C:\Users\UTZK0M\AppData\Local\Temp\RtmpMBnd1Q\list_issues.yaml.

write_labels_to_dataset(
    labels = my_labels, 
    dataset_dir = tempdir()
)
#> The datasets will be exported to C:\Users\UTZK0M\AppData\Local\Temp\RtmpMBnd1Q\list_labels.yaml.

write_milestones_to_dataset(
    milestones = my_milestones, 
    dataset_dir = tempdir()
)
#> The datasets will be exported to C:\Users\UTZK0M\AppData\Local\Temp\RtmpMBnd1Q\list_milestones.yaml.
```

### Options

It is also possible to set option for a R session:

``` r
# The directory containing the yaml files in local
options(IssueTrackeR.dataset.dir = tempdir())
# The default GitHub owner
options(IssueTrackeR.owner = "jdemetra")
# the default GitHub repository
options(IssueTrackeR.repo = "jdplus-main")
```

### Retrieve issues from local

Then it’s possible to read Issues from local yaml files:

``` r
# From local
my_issues <- get_issues(source = "local")
my_labels <- get_labels(source = "local")
my_milestones <- get_milestones(source = "local")
```

### Update full database

You can update your full database of issues, labels and milestones with
`update_database()`:

``` r
# From online
update_database(verbose = FALSE)
#> Running gh query ■■■■■■■■■■■ 33% | ETA: 2sRunning gh query ■■■■■■■■■■■■■■■■ 50%
#> | ETA: 2sRunning gh query ■■■■■■■■■■■■■■■■■■■■■ 67% | ETA: 2sRunning gh query
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■ 83% | ETA: 1s
```

### Filtering and sorting

There are plenty of different filtering ways.

``` r
# Condition: issues containing "README" in its body OR title
filtered_issues <- filter_issues(
    x = my_issues,
    fields = c("body", "title"),
    values = "README",
    fields_logic_gate = "OR"
)
```

See `?filter_issues` and `?contains` to explore more options.

### Sorting

Finally you can also sort your issues according to certain variable
(quantitative) and impose sole filtering factors (conditions as in
`filter_issues()`), which will be applied one after the other:

``` r
sorted_issues <- sort(
    x = my_issues,
    sorting_variables = list(list(object = "milestones", field = "due_on"),
                             list(object = "issues", field = "created_at")),
    filtering_factors = list(list(values = "bug",
                                  fields = "labels",
                                  values_logic_gate = "OR"),
                             list(values = "package", fields = "title")),
    milestones = my_milestones
)
```

## Contributing

Contributions are welcome! Please feel free to submit a pull request or
report any issues.

## License

This project is licensed under the MIT License. See the
[LICENSE](LICENSE) file for details.
