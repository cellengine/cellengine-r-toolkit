# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v1 Release Candidate]

v1 will be the first stable release of the CellEngine R toolkit and has several
breaking changes compared to the pre-release version.

### Added
- `createCompensation()` function
- `getCompensation()` function
- `updateCompensation()` function
- `deleteCompensation()` function
- All functions now consistently accept `byName()` expressions for any ID
  parameter.
- Support for transfers to S3 to `getEvents()`
- `PER_FILE` compensation constant
- Support for `geometricMean` as a statistics value in `getStatistics()`.
- Support for `layout` argument in `getStatistics()`.

### Changed
- **Breaking** Failed API requests will now stop instead of warn.
- **Breaking** `getEvents()` now uses `check.names=FALSE`, so data.frame column
  names will be identical to the parameter names found in the FCS file. For
  example, "CD4 (Ax647-A)" will no longer be converted to "CD4..Ax647.A.".
- **Breaking** `getEvents()` now defaults to `headerQ=TRUE`, so data.frames will
  have column names by default now.
- **Breaking** In `getEvents()`, the argument `clamp_q` was renamed to `clampQ`.
- **Breaking** The default for the `createPopulation` parameter of gate creation
  functions was changed to `is.null(fcsFileId)`.
- **Breaking** `getCompensations()` now returns a list of lists, with the
  `spillMatrix` formatted as a matrix.
- **Breaking** All functions now return lists instead of data.frames. (In the
  future, the lists may be changed to S3 classes.)
- **Breaking** The `xVertices` and `yVertices` parameters of `createPolygonGate`
  are replaced `vertices=list(c(x1, y1), c(x2, y2), c(x3, y3))`.
- **Breaking** The `create__Gate()` functions previously had two parameters for
  specifying the FCS file for tailoring (`fcsFileId` and `fcsFile`) and two
  parameters for specifying the parent population (`parentPopulationId` and
  `parentPopulation`), with the latter parameters performing a lookup by name.
  To be consistent with the rest of the toolkit, `fcsFile` and
  `parentPopulation` have been removed. Instead, use `byName()` expressions,
  e.g. `createRectangleGate(..., fcsFileId=byName("Sample1.fcs"), parentPopulationId=byName("CD3+"), ...)`.
- `createQuadrantGate()` and `createSplitGate()` accept either
  `createPopulation`, singular, for consistency with other `create__Gate`
  functions; or `createPopulations`, plural, to accurately reflect the behavior.
- `getStatistics(fcsFileIds=NULL, ...)` should return statistics for all
  non-control FCS files.
- `getStatistics()` should support `percentOf=c("PARENT")`.

### Removed
