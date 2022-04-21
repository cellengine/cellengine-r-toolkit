context("getFcsFiles")

fcsFiles <- function() {
  con <- file("../fcsfiles.json")
  on.exit(close(con))
  readLines(con)
}

test_that("makes expected HTTP request", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        status_code = 200,
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getFcsFiles("591a3b441d725115208a6fda")
    }
  )
})

test_that("returns a dataframe", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = fcsFiles(),
        status_code = 200,
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getFcsFiles("591a3b441d725115208a6fda")
      expect_equal(class(resp), "data.frame")
    }
  )
})


test_that("returns a dataframe when a query with no response is passed", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles?query=eq%28deleted%2C3%29") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = "[]",
        status_code = 200,
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getFcsFiles("591a3b441d725115208a6fda", params = list("query" = "eq(deleted,3)"))
      expect_equal(class(resp), "data.frame")
    }
  )
})
