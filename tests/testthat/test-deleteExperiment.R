context("deleteExperiment")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "DELETE")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda")
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = "",
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- deleteExperiment("591a3b441d725115208a6fda")
    }
  )
})
