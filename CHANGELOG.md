# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
- Deprecate `createPolygonGate` args `xVertices=c()` and
  `yVertices=c()` in favor of `vertices=list(c(), c(), c())`

### Removed
