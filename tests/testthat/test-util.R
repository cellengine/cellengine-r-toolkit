context("utils")

# test_that("Sets the UA correctly", {})

test_that("baseGet requires baseURL to have been set", {{ pkg.env$baseURL <- ""
  expect_error(baseGet(), "setServer") }})

test_that("basePut requires baseURL to have been set", {{ pkg.env$baseURL <- ""
  expect_error(basePut(), "setServer") }})

test_that("basePost requires baseURL to have been set", {{ pkg.env$baseURL <- ""
  expect_error(basePost(), "setServer") }})

test_that("baseDelete requires baseURL to have been set", {{ pkg.env$baseURL <- ""
  expect_error(baseDelete(), "setServer") }})

test_that("lookupByName stops for 0 matches", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_match(req$url, "https://cellengine.com/api/v1/experiments")
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
      setServer("https://cellengine.com")
      expect_error(
        lookupByName("experiments", byName("My experiment")),
        "Resource with the name 'My experiment' does not exist."
      )
    }
  )
})

test_that("lookupByName stops for >1 match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_match(req$url, "https://cellengine.com/api/v1/experiments")
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '[{"name": "My experiment", "_id": "591a3b441d725115208a6fda"}, {"name": "My experiment", "_id": "591a3b441d725115208a6fdb"}]', # nolint
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://cellengine.com")
      expect_error(
        lookupByName("experiments", byName("My experiment")),
        "More than one resource with the name 'My experiment' exists."
      )
    }
  )
})

test_that("lookupByName returns for 1 match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_match(req$url, "https://cellengine.com/api/v1/experiments")
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '[{"name": "My experiment", "_id": "591a3b441d725115208a6fda"}]',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://cellengine.com")
      expect_equal(
        lookupByName("experiments", byName("My experiment")),
        "591a3b441d725115208a6fda"
      )
    }
  )
})
