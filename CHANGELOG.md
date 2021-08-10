# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- `downloadFcsFiles` function
- `downloadAttachment` function

### Changed
- fix for [confusing bulk entity retrieval](https://github.com/primitybio/cellengine-r-toolkit/issues/48)
- `getEvents()` now includes the header row by default.
- `getEvents()` now uses [`check.names=F`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/data.frame.html)
  when returning a `data.frame`. This means column names be identical to those
  found in the FCS file. For example, "CD4 (Ax647-A)" will no longer be
  converted to "CD4..Ax647.A.".

### Removed
