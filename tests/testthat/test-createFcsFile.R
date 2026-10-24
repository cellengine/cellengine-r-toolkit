context("createFcsFile")

test_that("Correct HTTP request is made", {
  local_mocked_bindings(
    request_perform = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(
        req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles"
      )
      body <- jsonlite::fromJSON(
        rawToChar(req$options$postfields),
        simplifyVector = FALSE
      )
      expect_equal(
        body,
        list(
          "fcsFiles" = list("591a3b441d725115208a6fdc", "591a3b441d725115208a6fdd"),
          "filename" = "combined.fcs",
          "preSubsampleN" = 100
        )
      )
      httptest::fake_response(
        req$url,
        req$method,
        content = '{"_id":"591a3b441d725115208a6fde","filename":"combined.fcs"}',
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
    },
    .package = "httr"
  )
  setServer("https://my.server.com")
  resp <- createFcsFile(
    "591a3b441d725115208a6fda",
    list("591a3b441d725115208a6fdc", "591a3b441d725115208a6fdd"),
    filename = "combined.fcs",
    preSubsampleN = 100
  )
  expect_equal(resp$`_id`, "591a3b441d725115208a6fde")
  expect_equal(resp$filename, "combined.fcs")
})

test_that("Source objects are sent as an array", {
  local_mocked_bindings(
    request_perform = function(req, handle, refresh) {
      body <- jsonlite::fromJSON(
        rawToChar(req$options$postfields),
        simplifyVector = FALSE
      )
      expect_equal(
        body$fcsFiles,
        list(list("_id" = "591a3b441d725115208a6fdf", experimentId = "591a3b441d725115208a6fdb"))
      )
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
  createFcsFile(
    "591a3b441d725115208a6fda",
    list(list("_id" = "591a3b441d725115208a6fdf", experimentId = "591a3b441d725115208a6fdb"))
  )
})
