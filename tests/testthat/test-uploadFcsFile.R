context("uploadFcsFile")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fdb/fcsfiles")
      # Not sure the best way to assert on this.
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '{"__v":0,"_id":"591a3b441d725115208a6fda","filename":"5k.fcs"}',
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- uploadFcsFile("591a3b441d725115208a6fdb", "../5k.fcs")
      expect_equal(resp$`_id`, "591a3b441d725115208a6fda")
      expect_equal(resp$filename, "5k.fcs")
    }
  )
})
