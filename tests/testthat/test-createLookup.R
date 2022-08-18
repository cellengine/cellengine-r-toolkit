test_that("Correct HTTP request is made, parentPopulation specified", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/593b44a7ff5925084dd96ed1/gates?query=eq%28name%2C%20%22my%20gate%22%29&limit=2" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[{"name":"Tiny plate","created":"2017-06-10T01:00:23.638Z","__v":0,"_id":"593b44a7ff5925084dd96ed1","public":false,"uploader":{"_id":"57e497d9e3f1430e16805d17","lastName":"Bjornson","email":"zbjornson@primitybio.com","username":"zbjornson","firstName":"Zach","fullName":"Zach Bjornson"},"permissions":{},"primaryResearcher":{"_id":"57e497d9e3f1430e16805d17","lastName":"Bjornson","email":"zbjornson@primitybio.com","username":"zbjornson","firstName":"Zach","fullName":"Zach Bjornson"},"updated":"2017-06-10T01:00:25.632Z"}]', # nolint
            status_code = 200,
            headers = list(`Content-Type` = "application/json")
          )
        },
        "https://my.server.com/api/v1/experiments/593b44a7ff5925084dd96ed1/gates/593b44a7ff5925084dd96ed1/" = {
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            # Fixed GID, not the one passed in
            content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[37836.07,971.51],[1588732.12,154.646],[8139.405,664.78],[9441.949,781.32]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"my gate","parentPopulationId":"591a3b5f1d725115208a7087","yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false}', # nolint
            status_code = 200,
            headers = list(`Content-Type` = "application/json")
          )
          return(response)
        },
        {
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      lookup <- createLookup("593b44a7ff5925084dd96ed1")
      resp <- lookup("gates", "my gate")
      expect_equal(resp$name, "my gate")
      expect_equal(resp$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$`_id`, "592640aa298f1480900e10e4")
    }
  )
})
