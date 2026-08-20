# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres
to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

* New methods to plot resolution time and author in `plot()` [#14](https://github.com/TanguyBarthelemy/IssueTrackeR/issues/14) and [#40](https://github.com/TanguyBarthelemy/IssueTrackeR/issues/40)
* `plot_resolution_bars` and `plot_resolution_ecdf` function to plot resolution time object.
* `extract_nth()` returns the nth issue of a list of issues [#42](https://github.com/TanguyBarthelemy/IssueTrackeR/issues/42)
* `count_issues()` returns the number of issues in a list of issues [#35](https://github.com/TanguyBarthelemy/IssueTrackeR/issues/35)
* New field `created_by` in issues

### Fixed

* `closed_at` column is `POSIXct` [#35](https://github.com/TanguyBarthelemy/IssueTrackeR/issues/35)
* `get_still_open()` and `generate_age_mat()` return `ts` and `mts` objects  [#41](https://github.com/TanguyBarthelemy/IssueTrackeR/issues/41)
* `author_last_comment()` works with a single issue.
* `closed_at` and `created_at` are now timestamps and no more dates.
* `summary()` has correct links and colors for `LabelsTB` objects.


## [1.4.1] - 2026-07-31

### Added

* `overwrite` argument to specify if we want to overwrite an existing file if it already exists. [#18](https://github.com/TanguyBarthelemy/IssueTrackeR/issues/18)

## [1.4.0] - 2026-06-21

### Added

* New class `MilestonesTB` for milestones objects
* Number of issues open or closed in each milestones
* New function to plot issues
* New checked case if internet connection is down
* Display of the default options at the start of the package [#7](https://github.com/TanguyBarthelemy/IssueTrackeR/issues/7)
* New function `reset_options` to reset the default options [#4](https://github.com/TanguyBarthelemy/IssueTrackeR/issues/4)
* Argument `with_labels` display the labels with the issue in a summary call of a list of issues

### Changed

* Removed progress bar in `gh::gh()` calls
* `update_database()` updates all the milestones (closed and open)
* `write_issues_to_dataset()`, `write_labels_to_dataset()` and `write_milestones_to_dataset()` are regrouped into `write_to_dataset()`
* `get_issues()` accept now several repos to look for issues. Argument `repo` can be a vector of several repos.

### Fixed

* Removed a warning due to the use of `ignore.case = TRUE` and `fixed = TRUE` simultaneously in `grepl()`

## [1.3.1] - 2025-10-27

### Added

* New function to convert NULL value to default value
* Add information in milestones datasets
* `get_nbr_comments()`: Function to compute the number of comments
* `with_comments()`: Function to filter the issue with or without comments
* `author_last_comment()`: Add new function to retrieve the name of the last commentator

## [1.3.0] - 2025-10-21

### Added

* `summary()` accept `duplicated` as a `state_reason`
* `get_issues()` with `repo = NULL` return all the issues of all the repositories of a user
* error and warning message if a username or a repository don't exist
* new entry `closed_at` with the closing date of an issue
* New function `with_text()` to select the issues that contains text in their title, body or comments
* New logo
* New function `with_labels()` to filter issues by label's name
* New function `get_all_repos()` to get all the repos' name from an owner
* New method `print()` and `summary()` for `LabelsTB` objects
* Not exported `isDark()` function to verify if a colour is dark or light (to display text in black or white above)
* New function to check the output of the API

### Changed

* Labels in issues have been lightened (just the name and the colour of the labels and no more information are stored)
* `get_labels()` now accepts `repo = NULL` and retrieve all the labels from all the repos from an owner.
* slightly change the writing and reading method of datasets

## [1.2.0] - 2025-07-16

### Changed

* Changed structure of issue --> data.frame

### Removed

* Remove sorting and filtering function (to use the tidyverse instead)

### Added

* Url link with Issues printed
* New `sample()`, `rbind()`, `summary()` method

## [1.1.1] - 2025-04-26

### Added

* New function to format Milestones
* New function to format timestamp
* New argument accepted in `filter_issues()` : `"b"` for `"body"`, `"t"` for `"title"`, `"l"` for `"labels"` and `"m"` for `"milestones"`

### Changed

* `ignore.case = FALSE` if `fixed = TRUE`
* lint condition_call (with `call. = FALSE`)

## [1.1.0] - 2025-01-09

### Added

* Additional argument ... to functions `filter_issues` and `contains` to custom `vgrepl` (and therefore to `grepl`)

### Changed

* internal function `vgrepl()` uses `fixed = TRUE` and `perl = FALSE` as default argument

### Fixed

* Bug fixed when milestones description is missing (commit 9b4832)
* Bug fixed :missing argument ... in `vgrepl` call (commit 490d00a)

### Removed

* `[.IssuesTB` was duplicated
* removed `... = _` in paste0 for R version before 4.2

## [1.0.0] - 2024-09-12

### Added

* First release
* New CHANGELOG (`NEWS.md`)
* Documentation for `logic_reducer()`, `no_milestones()`, `vgrepl()` and `simple_sort`

[Unreleased]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.4.1...HEAD
[1.4.1]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.4.0...v1.4.1
[1.4.0]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.3.1...v1.4.0
[1.3.1]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.3.0...v1.3.1
[1.3.0]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.2.0...v1.3.0
[1.2.0]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.1.1...v1.2.0
[1.1.1]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.1.0...v1.1.1
[1.1.0]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/TanguyBarthelemy/IssueTrackeR/releases/tag/v1.0.0
