context("updateExperiment")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "PATCH")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda")
      body <- rawToChar(req$options$postfields)
      expect_equal(body, '{"name":"new name"}')

      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '{"name":"new name","created":"2017-06-10T01:00:23.638Z","__v":0,"_id":"593b44a7ff5925084dd96ed1","public":false,"uploader":{"_id":"57e497d9e3f1430e16805d17","lastName":"Bjornson","email":"zbjornson@primitybio.com","username":"zbjornson","firstName":"Zach","fullName":"Zach Bjornson"},"permissions":{},"primaryResearcher":{"_id":"57e497d9e3f1430e16805d17","lastName":"Bjornson","email":"zbjornson@primitybio.com","username":"zbjornson","firstName":"Zach","fullName":"Zach Bjornson"},"updated":"2017-06-10T01:00:25.632Z"}', # nolint
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- updateExperiment("591a3b441d725115208a6fda", list("name" = "new name"))
      expect_equal(resp$name, "new name")
    }
  )
})
