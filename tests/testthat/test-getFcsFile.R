context("getFcsFiles")

fcsFile <- function() {
  con <- file("../fcsfiles.json")
  on.exit(close(con))
  readLines(con)
}

test_that("makes expected HTTP request", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/591a3b441d725115208a6fdc") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = fcsFile(),
        status_code = 200,
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getFcsFile("591a3b441d725115208a6fda", "591a3b441d725115208a6fdc")
    }
  )
})
