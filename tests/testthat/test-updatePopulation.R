context("updatePopulation")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "PATCH")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations/592640aa298f1480900e10e4") # nolint
      body <- rawToChar(req$options$postfields)
      expect_equal(body, '{"name":"new name"}')

      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '{"experimentId":"591a3b441d725115208a6fda","name":"new name","parentId":null,"gates":"{\\\"$and\\\":[\\\"591a3b441d725115203a6fda\\\"]}","terminalGateGid":"591a3b441d725115203a6fda"}', # nolint
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- updatePopulation("591a3b441d725115208a6fda", "592640aa298f1480900e10e4", list("name" = "new name"))
      expect_equal(resp$name, "new name")
    }
  )
})
