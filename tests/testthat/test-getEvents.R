context("getEvents")

test_that("makes expected HTTP request", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/591a3b441d725115208a6fdc.FCS?compensatedQ=FALSE&headers=TRUE") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = readBin("../5k.fcs", raw(), file.info("../5k.fcs")$size),
        status_code = 200,
        headers = list(`Content-Type` = "application/vnd.isac.fcs")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getEvents("591a3b441d725115208a6fda", "591a3b441d725115208a6fdc")
    }
  )
})

test_that("makes expected HTTP request with subsampling", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/591a3b441d725115208a6fdc.FCS?compensatedQ=FALSE&headers=TRUE&preSubsampleN=50&seed=2.25") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = readBin("../5k.fcs", raw(), file.info("../5k.fcs")$size),
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

test_that("doesn't mangle column names", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/591a3b441d725115208a6fdc.TSV?compensatedQ=FALSE&headers=TRUE") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = "FSC-A\tCD3 (Ax647-A)\r\n1.23\t2.34\r\n3.45\t4.56\r\n",
        status_code = 200,
        headers = list(`Content-Type` = "application/vnd.isac.fcs")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getEvents("591a3b441d725115208a6fda", "591a3b441d725115208a6fdc", format = "TSV")
      expect_equal(colnames(resp), c("FSC-A", "CD3 (Ax647-A)"))
    }
  )
})
