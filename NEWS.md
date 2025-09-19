# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres
to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unreleased]

### Added

* `summary()` accept `duplicated` as a `state_reason`
* `get_issues()` with `repo = NULL` return all the issues of all the repositories of a user
* error and warning message if a username or a repository don't exist
* new entry `closed_at` with the closing date of an issue
* New function `with_text()` to select the issues that contains text in their title, body or comments
* New logo

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

[Unreleased]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.2.0...HEAD
[1.2.0]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.1.1...v1.2.0
[1.1.1]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.1.0...v1.1.1
[1.1.0]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/TanguyBarthelemy/IssueTrackeR/releases/tag/v1.0.0
