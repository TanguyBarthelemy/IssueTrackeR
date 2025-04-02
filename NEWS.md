# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres
to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unreleased]

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

[Unreleased]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.1.0...HEAD
[1.1.0]: https://github.com/TanguyBarthelemy/IssueTrackeR/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/TanguyBarthelemy/IssueTrackeR/releases/tag/v1.0.0
