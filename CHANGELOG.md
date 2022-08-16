# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v1-dev Unreleased]

### Added
- `createCompensation()` function
- `getCompensation()` function
- `updateCompensation()` function
- `deleteCompensation()` function

### Changed
- **Breaking** `getEvents()` now uses `check.names=FALSE`, so data.frame column
  names will be identical to the parameter names found in the FCS file. For
  example, "CD4 (Ax647-A)" will no longer be converted to "CD4..Ax647.A.".
- **Breaking** `getEvents()` now defaults to `headerQ=TRUE`, so data.frames will
  have column names by default now.
- **Breaking** In `getEvents()`, the argument `clamp_q` was renamed to `clampQ`.
- Change the default for `createPopulation` arg in gate creation functions to `is.null(fcsFileId)`
- **Breaking** `getCompensations` now returns a list of lists, with the `spillMatrix` formatted as a matrix.

### Removed
- The `xVertices` and `yVertices` arguments to `createPolygonGate` are replaced
  by `vertices=list(c(), c(), c())`.

## [Unreleased]

### Added
- `downloadFcsFiles()` function
- `downloadAttachment()` function
- `updatePopulation()` function
- `applyScaleSet()` function
- The `authenticate()` function will now prompt for your username, password and
  OTP (if configured) when running in RStudio or when the `getPass` library is
  installed.
- The `authenticate` function now supports two-factor authentication.
- The `authenticate` function now supports credentials stored in environment
  variables .
- The `authenticate` function now supports [Personal Access Tokens](https://docs.cellengine.com/api/#authentication).
- Support `byName()` for the `gateId` argument in `updateGate()`
- Introduce `lintr` as a linter with an Actions workflow
- Add `fromFlowCore` to convert flowCore objects to CellEngine
- Add `toFlowCore` to convert CellEngine objects to flowCore
- Add `updateFcsFile`
- Add `getPlot` function
- Support `addEventNumber` as a parameter in `getEvents()`.

### Changed
- Fix for [confusing bulk entity retrieval](https://github.com/primitybio/cellengine-r-toolkit/issues/48)
- For performance, `applyScale()` now only accepts atomic vectors. Lists can no
  longer be used.
- Fix mangling of experiment properties in `updateExperiment()`.
- Fix mangling of experiment properties in `updateGateFamily()`.
- Fix mangling of experiment properties in `updateGate()`.
- Remove unused `params` argument from `getExperiment()`.
- Run `styler` on all files with default params

### Removed
