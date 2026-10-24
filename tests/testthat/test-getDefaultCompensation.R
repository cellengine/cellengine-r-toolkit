context("getDefaultCompensation")

test_that("returns file-internal compensation when all files have it", {
  fcsFiles <- list(
    list(hasFileInternalComp = TRUE),
    list(hasFileInternalComp = TRUE)
  )
  local_mocked_bindings(
    getExperiment = function(experimentId) {
      list(`_id` = experimentId, perFileCompensationsEnabled = FALSE)
    },
    getFcsFiles = function(experimentId, params = list()) fcsFiles,
    .package = "cellengine"
  )

  expect_equal(getDefaultCompensation("experiment-id"), FILE_INTERNAL)
})

test_that("returns uncompensated when not all files have file-internal compensation", {
  fcsFiles <- list(
    list(hasFileInternalComp = TRUE),
    list(hasFileInternalComp = FALSE)
  )
  local_mocked_bindings(
    getExperiment = function(experimentId) {
      list(`_id` = experimentId, perFileCompensationsEnabled = FALSE)
    },
    getFcsFiles = function(experimentId, params = list()) fcsFiles,
    .package = "cellengine"
  )

  expect_equal(getDefaultCompensation("experiment-id"), UNCOMPENSATED)

  fcsFiles <- list()
  expect_equal(getDefaultCompensation("experiment-id"), UNCOMPENSATED)
})

test_that("returns per-file compensation when enabled", {
  local_mocked_bindings(
    getExperiment = function(experimentId) {
      list(`_id` = experimentId, perFileCompensationsEnabled = TRUE)
    },
    getFcsFiles = function(...) stop("FCS files should not be requested"),
    .package = "cellengine"
  )

  expect_equal(getDefaultCompensation("experiment-id"), PER_FILE)
})
