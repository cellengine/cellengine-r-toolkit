context("createPolygonGate")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/gates")
      body <- rawToChar(req$options$postfields)
      # Literal except for the gid value
      expect_match(body, '{"model":{"locked":false,"polygon":{"vertices":\\[\\[37836.07,971.51\\],\\[1588732.12,154.646\\],\\[8139.405,664.78\\],\\[9441.949,781.32\\]\\]},"label":\\[411037.386,643.064\\]},"xChannel":"FSC-A","yChannel":"FSC-W","type":"PolygonGate","parentPopulationId":null,"name":"my gate","gid":"[0-9A-Za-z]{24}","tailoredPerFile":false}', perl = TRUE) # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        # Fixed GID, not the one passed in
        content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[37836.07,971.51],[1588732.12,154.646],[8139.405,664.78],[9441.949,781.32]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"my gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}', # nolint
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- createPolygonGate("591a3b441d725115208a6fda", "FSC-A", "FSC-W", "my gate",
        list(c(37836.07, 971.51), c(1588732.12, 154.646), c(8139.405, 664.78), c(9441.949, 781.32)),
        createPopulation = FALSE
      )
      resp <- resp$gate
      expect_equal(resp$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$xChannel, "FSC-A")
      expect_equal(resp$yChannel, "FSC-W")
      expect_equal(resp$name, "my gate")
      expect_equal(resp$model$label, c(150440.453608247, 202688.886597938))
      expect_equal(resp$model$polygon$vertices, matrix(c(
        37836.07, 971.51,
        1588732.12, 154.646,
        8139.405, 664.78,
        9441.949, 781.32
      ), ncol = 2, byrow = TRUE))
      expect_equal(resp$model$locked, FALSE)
      expect_equal(resp$gid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$parentPopulationId, NULL) # default assigned client-side
      expect_equal(resp$type, "PolygonGate")
      expect_equal(resp$tailoredPerFile, FALSE)
    }
  )
})

test_that("Correct HTTP request is made, fcsFileId specified", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/gates")
      body <- rawToChar(req$options$postfields)
      # Literal except for the gid value
      expect_match(body, '{"model":{"locked":false,"polygon":{"vertices":\\[\\[37836.07,971.51\\],\\[1588732.12,154.646\\],\\[8139.405,664.78\\],\\[9441.949,781.32\\]\\]},"label":\\[411037.386,643.064\\]},"xChannel":"FSC-A","yChannel":"FSC-W","type":"PolygonGate","parentPopulationId":null,"name":"my gate","gid":"[0-9A-Za-z]{24}","tailoredPerFile":true,"fcsFileId":"591a3b441d725115208a6fdf"}', perl = TRUE) # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        # Fixed GID, not the one passed in
        content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[37836.07,971.51],[1588732.12,154.646],[8139.405,664.78],[9441.949,781.32]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"my gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":true,"id":"592640aa298f1480900e10e4","fcsFileId":"591a3b441d725115208a6fdf"}', # nolint
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- createPolygonGate("591a3b441d725115208a6fda", "FSC-A", "FSC-W", "my gate",
        list(c(37836.07, 971.51), c(1588732.12, 154.646), c(8139.405, 664.78), c(9441.949, 781.32)),
        tailoredPerFile = TRUE, fcsFileId = "591a3b441d725115208a6fdf", createPopulation = FALSE
      )
      resp <- resp$gate
      expect_equal(resp$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$xChannel, "FSC-A")
      expect_equal(resp$yChannel, "FSC-W")
      expect_equal(resp$name, "my gate")
      expect_equal(resp$model$label, c(150440.453608247, 202688.886597938))
      expect_equal(resp$model$polygon$vertices, matrix(c(
        37836.07, 971.51,
        1588732.12, 154.646,
        8139.405, 664.78,
        9441.949, 781.32
      ), ncol = 2, byrow = TRUE))
      expect_equal(resp$model$locked, FALSE)
      expect_equal(resp$gid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$parentPopulationId, NULL) # default assigned client-side
      expect_equal(resp$type, "PolygonGate")
      expect_equal(resp$tailoredPerFile, TRUE)
      expect_equal(resp$fcsFileId, "591a3b441d725115208a6fdf")
    }
  )
})

test_that("Correct HTTP request is made, parentPopulation specified", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?query=eq%28name%2C%20%22singlets%22%29" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {
                "_id":"591a3b5f1d725115208a7087",
                "experimentId":"591a3b441d725115208a6fda","name":"singlets",
                "gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33a\\"]}",
                "parentId":null,"terminalGateGid":"591a3b5961a8a2302d15a33a","__v":0,
                "id":"591a3b5f1d725115208a7087"}
            ]',
            status_code = 200,
            headers = list(`Content-Type` = "application/json")
          )
        },
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/gates" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          # Literal except for the gid value
          expect_match(body, '{"model":{"locked":false,"polygon":{"vertices":\\[\\[37836.07,971.51\\],\\[1588732.12,154.646\\],\\[8139.405,664.78\\],\\[9441.949,781.32\\]\\]},"label":\\[411037.386,643.064\\]},"xChannel":"FSC-A","yChannel":"FSC-W","type":"PolygonGate","parentPopulationId":"591a3b5f1d725115208a7087","name":"my gate","gid":"[0-9A-Za-z]{24}","tailoredPerFile":false}', perl = TRUE) # nolint
          response <- httptest::fake_response(
            req$url,
            req$method,
            # Fixed GID, not the one passed in
            content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[37836.07,971.51],[1588732.12,154.646],[8139.405,664.78],[9441.949,781.32]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"my gate","parentPopulationId":"591a3b5f1d725115208a7087","yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}', # nolint
            status_code = 201,
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
      resp <- createPolygonGate("591a3b441d725115208a6fda", "FSC-A", "FSC-W", "my gate",
        list(c(37836.07, 971.51), c(1588732.12, 154.646), c(8139.405, 664.78), c(9441.949, 781.32)),
        parentPopulation = "singlets", createPopulation = FALSE
      )
      resp <- resp$gate
      expect_equal(resp$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$xChannel, "FSC-A")
      expect_equal(resp$yChannel, "FSC-W")
      expect_equal(resp$name, "my gate")
      expect_equal(resp$model$label, c(150440.453608247, 202688.886597938))
      expect_equal(resp$model$polygon$vertices, matrix(c(
        37836.07, 971.51,
        1588732.12, 154.646,
        8139.405, 664.78,
        9441.949, 781.32
      ), ncol = 2, byrow = TRUE))
      expect_equal(resp$model$locked, FALSE)
      expect_equal(resp$gid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$parentPopulationId, "591a3b5f1d725115208a7087")
      expect_equal(resp$type, "PolygonGate")
      expect_equal(resp$tailoredPerFile, FALSE)
    }
  )
})
