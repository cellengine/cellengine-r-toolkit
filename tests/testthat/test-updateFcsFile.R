context("updateFcsFile")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "PATCH")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/592640aa298f1480900e10e4") # nolint
      body <- rawToChar(req$options$postfields)
      expect_equal(body, '{"filename":"new name"}')

      content <- jsonlite::fromJSON('../fcsfile.json')
      content <- modifyList(content, list("filename" = "new name"))
      content <- jsonlite::toJSON(content, null = "null", auto_unbox = TRUE)

      response <- httptest::fake_response(
        req$url,
        req$method,
        content = content,
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- updateFcsFile("591a3b441d725115208a6fda", "592640aa298f1480900e10e4", list("filename" = "new name"))
      expect_equal(resp$filename, "new name")
    }
  )
})
