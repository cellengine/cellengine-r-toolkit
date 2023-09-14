context("getEvents")

test_that("makes expected HTTP request", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/591a3b441d725115208a6fdc.FCS?compensatedQ=FALSE&headers=FALSE&addEventNumber=TRUE") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = "FCS3.1   00000000000000000000000000000000",
        status_code = 200,
        headers = list(`Content-Type` = "application/vnd.isac.fcs")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getEvents("591a3b441d725115208a6fda", "591a3b441d725115208a6fdc", addEventNumber = TRUE)
    }
  )
})

test_that("makes expected HTTP request with subsampling", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/591a3b441d725115208a6fdc.FCS?compensatedQ=FALSE&headers=FALSE&addEventNumber=FALSE&preSubsampleN=50&seed=2.25") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = "FCS3.1   00000000000000000000000000000000",
        status_code = 200,
        headers = list(`Content-Type` = "application/vnd.isac.fcs")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getEvents(
        "591a3b441d725115208a6fda",
        "591a3b441d725115208a6fdc",
        subsampling = list(preSubsampleN = 50, seed = 2.25)
      )
    }
  )
})
