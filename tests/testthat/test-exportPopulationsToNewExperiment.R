context("exportPopulationsToNewExperiment")

test_that("Correct HTTP request is made", {
  local_mocked_bindings(
    request_perform = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(
        req$url,
        paste0(
          "https://my.server.com/api/v1/experiments/",
          "591a3b441d725115208a6fda/populationsToNew"
        )
      )
      body <- jsonlite::fromJSON(
        rawToChar(req$options$postfields),
        simplifyVector = FALSE
      )
      expect_equal(
        body,
        list(
          compensationId = "591a3b441d725115208a6fdb",
          name = "Exported populations",
          fcsFileIds = list("591a3b441d725115208a6fdc"),
          filenameTemplate = "{fcsfile.filename}-{population._id}",
          populationIds = list("591a3b441d725115208a6fdd"),
          preSubsampleN = 100,
          postSubsampleN = 50,
          preSubsampleP = 0.5,
          postSubsampleP = 0.25,
          seed = 42,
          addEventNumber = TRUE
        )
      )
      httptest::fake_response(
        req$url,
        req$method,
        content = '{"_id":"591a3b441d725115208a6fde","name":"Exported populations"}',
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
    },
    .package = "httr"
  )
  setServer("https://my.server.com")
  resp <- exportPopulationsToNewExperiment(
    "591a3b441d725115208a6fda",
    "591a3b441d725115208a6fdb",
    name = "Exported populations",
    fcsFileIds = "591a3b441d725115208a6fdc",
    filenameTemplate = "{fcsfile.filename}-{population._id}",
    populationIds = "591a3b441d725115208a6fdd",
    preSubsampleN = 100,
    postSubsampleN = 50,
    preSubsampleP = 0.5,
    postSubsampleP = 0.25,
    seed = 42,
    addEventNumber = TRUE
  )
  expect_equal(resp$`_id`, "591a3b441d725115208a6fde")
})

test_that("Optional arguments are omitted when not supplied", {
  local_mocked_bindings(
    request_perform = function(req, handle, refresh) {
      body <- jsonlite::fromJSON(
        rawToChar(req$options$postfields),
        simplifyVector = FALSE
      )
      expect_equal(body, list(
        compensationId = "591a3b441d725115208a6fdb",
        addEventNumber = FALSE
      ))
      httptest::fake_response(
        req$url,
        req$method,
        content = "{}",
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
    },
    .package = "httr"
  )
  setServer("https://my.server.com")
  exportPopulationsToNewExperiment(
    "591a3b441d725115208a6fda",
    "591a3b441d725115208a6fdb"
  )
})

test_that("byName values in ID lists are resolved before posting", {
  lookups <- list()
  request <- NULL
  local_mocked_bindings(
    baseGet = function(url, params = list()) {
      lookups[[length(lookups) + 1]] <<- list(url = url, query = params$query)
      if (grepl("Sample 1.fcs", params$query, fixed = TRUE)) {
        id <- "resolved-file-id"
      } else if (grepl("Singlets", params$query, fixed = TRUE)) {
        id <- "resolved-population-id"
      } else {
        stop("Unexpected resource lookup")
      }
      list(list(
        "_id" = id
      ))
    },
    basePost = function(url, body, params = list()) {
      request <<- list(url = url, body = body)
      list()
    },
    .package = "cellengine"
  )

  exportPopulationsToNewExperiment(
    "experiment-id",
    "compensation-id",
    fcsFileIds = list(byName("Sample 1.fcs")),
    populationIds = list(byName("Singlets"))
  )

  expect_equal(
    vapply(lookups, `[[`, character(1), "url"),
    c(
      "/api/v1/experiments/experiment-id/fcsfiles",
      "/api/v1/experiments/experiment-id/populations"
    )
  )
  expect_match(lookups[[1]]$query, "filename")
  expect_match(lookups[[2]]$query, "name")
  expect_equal(
    jsonlite::fromJSON(request$body, simplifyVector = FALSE),
    list(
      compensationId = "compensation-id",
      fcsFileIds = list("resolved-file-id"),
      populationIds = list("resolved-population-id"),
      addEventNumber = FALSE
    )
  )
})
