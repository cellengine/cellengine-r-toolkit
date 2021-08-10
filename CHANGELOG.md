# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- `downloadFcsFiles` function
- `downloadAttachment` function

### Changed
- Fix for [confusing bulk entity retrieval](https://github.com/primitybio/cellengine-r-toolkit/issues/48)
- For performance, `applyScale` now only accepts atomic vectors. Lists can no
  longer be used.

### Removed
