context("createQuadrantGate")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/gates")
      body <- rawToChar(req$options$postfields)
      # Literal except for the gid values
      expect_match(body, '{"model":{"locked":false,"quadrant":{"x":118010.391752577,"y":182870.515463918,"angles":\\[1.5707963267949,3.14159265358979,4.71238898038469,0\\]},"gids":\\["[0-9A-Za-z]{24}","[0-9A-Za-z]{24}","[0-9A-Za-z]{24}","[0-9A-Za-z]{24}"\\],"labels":\\[\\[196608,196608\\],\\[0.75,196608\\],\\[0.75,0.75\\],\\[196608,0.75\\]\\],"skewable":false},"xChannel":"FSC-A","yChannel":"FSC-W","type":"QuadrantGate","names":\\["my gate \\(UR\\)","my gate \\(UL\\)","my gate \\(LL\\)","my gate \\(LR\\)"\\],"gid":"[0-9A-Za-z]{24}","tailoredPerFile":false,"fcsFileId":null}', perl = T) # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        # Fixed GID, not the one passed in
        content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"locked":false,"labels":[[196608.00, 196608], [0.75, 196608.00], [0.75, 0.75], [196608, 0.75]],"quadrant":{"x":118010.391752577,"y":182870.515463918,"angles":[1.5707963267949,3.14159265358979,4.71238898038469,0]},"gids":["5d30960a417e4bc767a428a3","5d30960a417e4bc767a428a4","5d30960a417e4bc767a428a5","5d30960a417e4bc767a428a6"]},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","yChannel":"FSC-W","type":"QuadrantGate","names":["my gate (UR)","my gate (UL)","my gate (LL)","my gate (LR)"],"_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"fcsFileId":null}', # nolint
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- createQuadrantGate("591a3b441d725115208a6fda", "FSC-A", "FSC-W", "my gate",
        118010.39175257733, 182870.51546391752,
        labels = list(c(196608.00, 196608.00), c(0.75, 196608.00), c(0.75, 0.75), c(196608.00, 0.75)),
        createPopulation = FALSE
      )
      expect_equal(resp$gate$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$gate$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$gate$xChannel, "FSC-A")
      expect_equal(resp$gate$yChannel, "FSC-W")
      expect_equal(resp$gate$names, c("my gate (UR)","my gate (UL)","my gate (LL)","my gate (LR)"))
      expect_equal(
        resp$gate$model$labels,
        matrix(c(c(196608.00, 196608.00), c(0.75, 196608.00), c(0.75, 0.75), c(196608.00, 0.75)), byrow = TRUE, ncol = 2)
      )
      expect_equal(resp$gate$model$quadrant$x, 118010.391752577)
      expect_equal(resp$gate$model$quadrant$y, 182870.515463918)
      expect_equal(resp$gate$model$locked, FALSE)
      expect_equal(resp$gate$model$gids, c("5d30960a417e4bc767a428a3", "5d30960a417e4bc767a428a4", "5d30960a417e4bc767a428a5", "5d30960a417e4bc767a428a6")) # nolint
      expect_equal(resp$gate$gid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$gate$type, "QuadrantGate")
      expect_equal(resp$gate$tailoredPerFile, FALSE)
    }
  )
})

test_that("Correct HTTP request is made, fcsFileId specified", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/gates")
      body <- rawToChar(req$options$postfields)
      # Literal except for the gid values
      expect_match(body, '{"model":{"locked":false,"quadrant":{"x":118010.391752577,"y":182870.515463918,"angles":\\[1.5707963267949,3.14159265358979,4.71238898038469,0\\]},"gids":\\["[0-9A-Za-z]{24}","[0-9A-Za-z]{24}","[0-9A-Za-z]{24}","[0-9A-Za-z]{24}"\\],"labels":\\[\\[196608,196608\\],\\[0.75,196608\\],\\[0.75,0.75\\],\\[196608,0.75\\]\\],"skewable":false},"xChannel":"FSC-A","yChannel":"FSC-W","type":"QuadrantGate","names":\\["my gate \\(UR\\)","my gate \\(UL\\)","my gate \\(LL\\)","my gate \\(LR\\)"\\],"gid":"[0-9A-Za-z]{24}","tailoredPerFile":true,"fcsFileId":"591a3b441d725115208a6fdf"}', perl = T) # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        # Fixed GID, not the one passed in
        content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"locked":false,"labels":[[196608.00, 196608], [0.75, 196608.00], [0.75, 0.75], [196608, 0.75]],"quadrant":{"x":118010.391752577,"y":182870.515463918,"angles":[1.5707963267949,3.14159265358979,4.71238898038469,0]},"gids":["5d30960a417e4bc767a428a3","5d30960a417e4bc767a428a4","5d30960a417e4bc767a428a5","5d30960a417e4bc767a428a6"]},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","yChannel":"FSC-W","type":"QuadrantGate","names":["my gate (UR)","my gate (UL)","my gate (LL)","my gate (LR)"],"_id":"592640aa298f1480900e10e4","tailoredPerFile":true,"fcsFileId":"591a3b441d725115208a6fdf"}', # nolint
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- createQuadrantGate("591a3b441d725115208a6fda", "FSC-A", "FSC-W", "my gate",
        118010.39175257733, 182870.51546391752,
        labels = list(c(196608.00, 196608.00), c(0.75, 196608.00), c(0.75, 0.75), c(196608.00, 0.75)),
        tailoredPerFile = TRUE, fcsFileId = "591a3b441d725115208a6fdf", createPopulation = FALSE
      )
      expect_equal(resp$gate$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$gate$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$gate$xChannel, "FSC-A")
      expect_equal(resp$gate$yChannel, "FSC-W")
      expect_equal(resp$gate$names, c("my gate (UR)","my gate (UL)","my gate (LL)","my gate (LR)"))
      expect_equal(
        resp$gate$model$labels,
        matrix(c(c(196608.00, 196608.00), c(0.75, 196608.00), c(0.75, 0.75), c(196608.00, 0.75)), byrow = T, ncol = 2)
      )
      expect_equal(resp$gate$model$quadrant$x, 118010.391752577)
      expect_equal(resp$gate$model$quadrant$y, 182870.515463918)
      expect_equal(resp$gate$model$locked, FALSE)
      expect_equal(
        resp$gate$model$gids,
        c(
          "5d30960a417e4bc767a428a3",
          "5d30960a417e4bc767a428a4",
          "5d30960a417e4bc767a428a5",
          "5d30960a417e4bc767a428a6"
        )
      )
      expect_equal(resp$gate$gid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$gate$type, "QuadrantGate")
      expect_equal(resp$gate$tailoredPerFile, TRUE)
      expect_equal(resp$gate$fcsFileId, "591a3b441d725115208a6fdf")
    }
  )
})


test_that("Correct HTTP request is made, createPopulation=TRUE, parentPopulationId=byName()", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?query=eq%28name%2C%20%22singlets%22%29&limit=2" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {
                "_id":"591a3b5f1d725115208a7087","experimentId":"591a3b441d725115208a6fda","name":"singlets",
                "gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33a\\"]}","parentId":null,
                "terminalGateGid":"591a3b5961a8a2302d15a33a","__v":0
              }
            ]',
            status_code = 200,
            headers = list(`Content-Type` = "application/json")
          )
        },
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/gates?createPopulation=true&parentPopulationId=591a3b5f1d725115208a7087" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          # Literal except for the gid values
          expect_match(body, '{"model":{"locked":false,"quadrant":{"x":118010.391752577,"y":182870.515463918,"angles":\\[1.5707963267949,3.14159265358979,4.71238898038469,0\\]},"gids":\\["[0-9A-Za-z]{24}","[0-9A-Za-z]{24}","[0-9A-Za-z]{24}","[0-9A-Za-z]{24}"\\],"labels":\\[\\[196608,196608\\],\\[0.75,196608\\],\\[0.75,0.75\\],\\[196608,0.75\\]\\],"skewable":false},"xChannel":"FSC-A","yChannel":"FSC-W","type":"QuadrantGate","names":\\["my gate \\(UR\\)","my gate \\(UL\\)","my gate \\(LL\\)","my gate \\(LR\\)"\\],"gid":"[0-9A-Za-z]{24}","tailoredPerFile":false,"fcsFileId":null}', perl = T) # nolint
          response <- httptest::fake_response(
            req$url,
            req$method,
            # Fixed GID, not the one passed in
            content = '{
              "gate":{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"locked":false,"labels":[[196608.00, 196608], [0.75, 196608.00], [0.75, 0.75], [196608, 0.75]],"quadrant":{"x":118010.391752577,"y":182870.515463918,"angles":[1.5707963267949,3.14159265358979,4.71238898038469,0]},"gids":["5d30960a417e4bc767a428a3","5d30960a417e4bc767a428a4","5d30960a417e4bc767a428a5","5d30960a417e4bc767a428a6"]},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","yChannel":"FSC-W","type":"QuadrantGate","names":["my gate (UR)","my gate (UL)","my gate (LL)","my gate (LR)"],"_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"fcsFileId":null},
              "populations": [
                {"parentId":"591a3b5f1d725115208a7087","_id":"62fc44da8500a029b981e350","name":"my gate","gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33a\\",\\"5d30960a417e4bc767a428a3\\"]}","terminalGateGid":"5d30960a417e4bc767a428a3","experimentId":"591a3b441d725115208a6fda"},
                {"parentId":"591a3b5f1d725115208a7087","_id":"62fc44da8500a029b981e350","name":"my gate","gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33a\\",\\"5d30960a417e4bc767a428a4\\"]}","terminalGateGid":"5d30960a417e4bc767a428a4","experimentId":"591a3b441d725115208a6fda"},
                {"parentId":"591a3b5f1d725115208a7087","_id":"62fc44da8500a029b981e350","name":"my gate","gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33a\\",\\"5d30960a417e4bc767a428a5\\"]}","terminalGateGid":"5d30960a417e4bc767a428a5","experimentId":"591a3b441d725115208a6fda"},
                {"parentId":"591a3b5f1d725115208a7087","_id":"62fc44da8500a029b981e350","name":"my gate","gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33a\\",\\"5d30960a417e4bc767a428a6\\"]}","terminalGateGid":"5d30960a417e4bc767a428a6","experimentId":"591a3b441d725115208a6fda"}
              ]
            }', # nolint
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
      resp <- createQuadrantGate("591a3b441d725115208a6fda", "FSC-A", "FSC-W", "my gate",
        118010.39175257733, 182870.51546391752,
        labels = list(c(196608.00, 196608.00), c(0.75, 196608.00), c(0.75, 0.75), c(196608.00, 0.75)),
        parentPopulationId = byName("singlets"), createPopulation = TRUE, tailoredPerFile = FALSE
      )
      expect_equal(resp$gate$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$gate$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$gate$xChannel, "FSC-A")
      expect_equal(resp$gate$yChannel, "FSC-W")
      expect_equal(resp$gate$names, c("my gate (UR)","my gate (UL)","my gate (LL)","my gate (LR)"))
      expect_equal(
        resp$gate$model$labels,
        matrix(c(c(196608.00, 196608.00), c(0.75, 196608.00), c(0.75, 0.75), c(196608.00, 0.75)), byrow = TRUE, ncol = 2)
      )
      expect_equal(resp$gate$model$quadrant$x, 118010.391752577)
      expect_equal(resp$gate$model$quadrant$y, 182870.515463918)
      expect_equal(resp$gate$model$locked, FALSE)
      expect_equal(
        resp$gate$model$gids,
        c(
          "5d30960a417e4bc767a428a3",
          "5d30960a417e4bc767a428a4",
          "5d30960a417e4bc767a428a5",
          "5d30960a417e4bc767a428a6"
        )
      )
      expect_equal(resp$gate$gid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$gate$type, "QuadrantGate")
      expect_equal(resp$gate$tailoredPerFile, FALSE)
    }
  )
})
