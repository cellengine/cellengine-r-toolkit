context("parsePopulationArgs")

test_that("returns the ID of a single matching population", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?query=eq%28name%2C%20%22name%22%29") # nolint
      expect_equal(req$method, "GET")
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '[
          {
            "_id":"591a3b5f1d725115208a7088","experimentId":"591a3b441d725115208a6fda","name":"name",
            "gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33b\\"]}","parentId":null,
            "terminalGateGid":"591a3b5961a8a2302d15a33a","__v":0,"id":"591a3b5f1d725115208a7088"
          }
        ]',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      result <- parsePopulationArgs(NULL, "name", "591a3b441d725115208a6fda")
      expect_equal(result, "591a3b5f1d725115208a7088")
    }
  )
})

test_that("passes through the parentPopulationId if provided", {
  result <- parsePopulationArgs("591a3b5f1d725115208a7088", NULL, "591a3b441d725115208a6fda")
  expect_equal(result, "591a3b5f1d725115208a7088")
})

test_that("throws an error if multiple populations match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?query=eq%28name%2C%20%22name%22%29") # nolint
      expect_equal(req$method, "GET")
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '[
          {
            "_id":"591a3b5f1d725115208a7087","experimentId":"591a3b441d725115208a6fda","name":"name",
            "gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33a\\"]}","parentId":null,
            "terminalGateGid":"591a3b5961a8a2302d15a33a","__v":0,"id":"591a3b5f1d725115208a7087"
          },
          {
            "_id":"591a3b5f1d725115208a7088","experimentId":"591a3b441d725115208a6fda","name":"name",
            "gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33b\\"]}","parentId":null,
            "terminalGateGid":"591a3b5961a8a2302d15a33a","__v":0,"id":"591a3b5f1d725115208a7088"
          }
        ]',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      expect_error(parsePopulationArgs(NULL, "name", "591a3b441d725115208a6fda"), "More than one")
    }
  )
})

test_that("throws an error if both parentPopulationId and parentPopulation are provided", {
  setServer("https://my.server.com")
  expect_error(
    parsePopulationArgs(
      "591a3b441d725115208a6fdc",
      "name",
      "591a3b441d725115208a6fda"
    ),
    "Please specify only one"
  )
})

test_that("throws an error if no populations match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?query=eq%28name%2C%20%22name%22%29") # nolint
      expect_equal(req$method, "GET")
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = "[]",
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      expect_error(parsePopulationArgs(NULL, "name", "591a3b441d725115208a6fda"), "does not exist")
    }
  )
})

context("parseFcsFileArgs")

test_that("assigns the ID of a single matching fcsFile", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles?query=eq%28filename%2C%20%22name%22%29&fields=%2B_id") # nolint
      expect_equal(req$method, "GET")
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '[
          {"_id":"591a3b5f1d725115208a7088"}
        ]',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      result <- parseFcsFileArgs(list(), TRUE, NULL, "name", "591a3b441d725115208a6fda")
      expect_equal(result$fcsFileId, jsonlite::unbox("591a3b5f1d725115208a7088"))
    }
  )
})

test_that("passes through the fcsFileId if provided", {
  result <- parseFcsFileArgs(list(), TRUE, "591a3b5f1d725115208a7088", NULL, "591a3b441d725115208a6fda")
  expect_equal(result$fcsFileId, jsonlite::unbox("591a3b5f1d725115208a7088"))
})

test_that("throws an error if multiple fcsFiles match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles?query=eq%28filename%2C%20%22name%22%29&fields=%2B_id") # nolint
      expect_equal(req$method, "GET")
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '[
          {"_id":"591a3b5f1d725115208a7087"},
          {"_id":"591a3b5f1d725115208a7088"}
        ]',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      expect_error(parseFcsFileArgs(list(), TRUE, NULL, "name", "591a3b441d725115208a6fda"), "More than one")
    }
  )
})

test_that("throws an error if both fcsFileId and fcsFile are provided", {{ setServer("https://my.server.com")
  expect_error(
    parseFcsFileArgs(list(), TRUE, "591a3b441d725115208a6fdc", "name", "591a3b441d725115208a6fda"),
    "Please specify only one"
  ) }})

test_that("doesn't assign fcsFileId if not tailored", {{ setServer("https://my.server.com")
  result <- parseFcsFileArgs(list(), FALSE, NULL, "name", "591a3b441d725115208a6fda")
  expect_null(result$fcsFileId) }})

test_that("doesn't assign fcsFileId if global tailored gate", {{ setServer("https://my.server.com")
  result <- parseFcsFileArgs(list(), TRUE, NULL, NULL, "591a3b441d725115208a6fda")
  expect_null(result$fcsFileId) }})

test_that("throws an error if no fcsFiles match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles?query=eq%28filename%2C%20%22name%22%29&fields=%2B_id") # nolint
      expect_equal(req$method, "GET")
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = "[]",
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      expect_error(parseFcsFileArgs(list(), TRUE, NULL, "name", "591a3b441d725115208a6fda"), "does not exist")
    }
  )
})
