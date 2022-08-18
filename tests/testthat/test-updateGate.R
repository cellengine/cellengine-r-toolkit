context("updateGate")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "PATCH")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/gates/592640aa298f1480900e10e4") # nolint
      body <- rawToChar(req$options$postfields)
      expect_equal(body, '{"name":"new name"}')

      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"rectangle":{"y2":214399.74226804124,"x2":182870.51546391752,"y1":190978.03092783503,"x1":118010.39175257733},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"RectangleGate","name":"new name","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false}', # nolint
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- updateGate("591a3b441d725115208a6fda", "592640aa298f1480900e10e4", list("name" = "new name"))
      expect_equal(resp$name, "new name")
    }
  )
})
