context("createExperiment")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments")
      body <- rawToChar(req$options$postfields)
      expect_equal(body, '{"name":"my experiment"}')
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '{"__v":0,"_id":"591a3b441d725115208a6fda","name":"my experiment"}',
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- createExperiment(list("name" = "my experiment"))
      expect_equal(resp$`_id`, "591a3b441d725115208a6fda")
      expect_equal(resp$name, "my experiment")
    }
  )
})
