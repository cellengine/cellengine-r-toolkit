context("downloadAttachment")

test_that("makes expected HTTP request", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/attachments/591a3b441d725115208a6fde")
      response = httptest::fake_response(
        req$url,
        req$method,
        content="text",
        status_code = 200,
        headers = list(`Content-Type` = "text/plain; charset=utf-8")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp = downloadAttachment("591a3b441d725115208a6fda", "591a3b441d725115208a6fde")
    }
  )
})
