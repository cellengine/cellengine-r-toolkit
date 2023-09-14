context("createSplitGate")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/gates")
      body <- rawToChar(req$options$postfields)
      # Literal except for the gid values
      expect_match(body, '{"model":{"locked":false,"split":{"x":144000,"y":0.5},"gids":\\["[0-9A-Za-z]{24}","[0-9A-Za-z]{24}"\\],"labels":\\[\\[26215.4,0.95\\],\\[235929.6,0.95\\]\\]},"xChannel":"FSC-A","names":\\["my gate \\(L\\)","my gate \\(R\\)"\\],"type":"SplitGate","parentPopulationId":null,"gid":"[0-9A-Za-z]{24}","tailoredPerFile":false}', perl = T) # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        # Fixed GID, not the one passed in
        content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"locked":false,"labels":[[26215.4,0.95], [235929.6, 0.95]],"split":{"x":144000,"y":0.5},"gids":["5d30960a417e4bc767a428a3","5d30960a417e4bc767a428a4"]},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"SplitGate","name":"my gate","parentPopulationId":null,"_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}', # nolint
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- createSplitGate(
        "591a3b441d725115208a6fda", "FSC-A", "my gate", 144000,
        labels = list(c(26215.4, 0.95), c(235929.6, 0.95)), createPopulation = FALSE
      )
      resp <- resp$gate
      expect_equal(resp$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$xChannel, "FSC-A")
      expect_equal(resp$name, "my gate")
      expect_equal(resp$model$labels, matrix(c(c(26215.4, 0.95), c(235929.6, 0.95)), byrow = TRUE, ncol = 2))
      expect_equal(resp$model$split$x, 144000)
      expect_equal(resp$model$split$y, 0.5)
      expect_equal(resp$model$locked, FALSE)
      expect_equal(resp$model$gids, c("5d30960a417e4bc767a428a3", "5d30960a417e4bc767a428a4"))
      expect_equal(resp$gid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$parentPopulationId, NULL) # default assigned client-side
      expect_equal(resp$type, "SplitGate")
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
      # Literal except for the gid values
      expect_match(body, '{"model":{"locked":false,"split":{"x":144000,"y":0.5},"gids":\\["[0-9A-Za-z]{24}","[0-9A-Za-z]{24}"\\],"labels":\\[\\[26215.4,0.95\\],\\[235929.6,0.95\\]\\]},"xChannel":"FSC-A","names":\\["my gate \\(L\\)","my gate \\(R\\)"\\],"type":"SplitGate","parentPopulationId":null,"gid":"[0-9A-Za-z]{24}","tailoredPerFile":true,"fcsFileId":"591a3b441d725115208a6fdf"}', perl = T) # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        # Fixed GID, not the one passed in
        content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"locked":false,"labels":[[26215.4,0.95], [235929.6, 0.95]],"split":{"x":144000,"y":0.5},"gids":["5d30960a417e4bc767a428a3","5d30960a417e4bc767a428a4"]},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"SplitGate","name":"my gate","parentPopulationId":null,"_id":"592640aa298f1480900e10e4","tailoredPerFile":true,"id":"592640aa298f1480900e10e4","fcsFileId":"591a3b441d725115208a6fdf"}', # nolint
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- createSplitGate("591a3b441d725115208a6fda", "FSC-A", "my gate", 144000,
        labels = list(c(26215.4, 0.95), c(235929.6, 0.95)),
        tailoredPerFile = TRUE, fcsFileId = "591a3b441d725115208a6fdf", createPopulation = FALSE
      )
      resp <- resp$gate
      expect_equal(resp$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$xChannel, "FSC-A")
      expect_equal(resp$name, "my gate")
      expect_equal(resp$model$labels, matrix(c(c(26215.4, 0.95), c(235929.6, 0.95)), byrow = TRUE, ncol = 2))
      expect_equal(resp$model$split$x, 144000)
      expect_equal(resp$model$split$y, 0.5)
      expect_equal(resp$model$locked, FALSE)
      expect_equal(resp$model$gids, c("5d30960a417e4bc767a428a3", "5d30960a417e4bc767a428a4"))
      expect_equal(resp$gid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$parentPopulationId, NULL) # default assigned client-side
      expect_equal(resp$type, "SplitGate")
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
                "_id":"591a3b5f1d725115208a7087","experimentId":"591a3b441d725115208a6fda","name":"singlets",
                "gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33a\\"]}","parentId":null,
                "terminalGateGid":"591a3b5961a8a2302d15a33a","__v":0,"id":"591a3b5f1d725115208a7087"
              }
            ]',
            status_code = 200,
            headers = list(`Content-Type` = "application/json")
          )
        },
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/gates" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          # Literal except for the gid values
          expect_match(body, '{"model":{"locked":false,"split":{"x":144000,"y":0.5},"gids":\\["[0-9A-Za-z]{24}","[0-9A-Za-z]{24}"\\],"labels":\\[\\[26215.4,0.95\\],\\[235929.6,0.95\\]\\]},"xChannel":"FSC-A","names":\\["my gate \\(L\\)","my gate \\(R\\)"\\],"type":"SplitGate","parentPopulationId":"591a3b5f1d725115208a7087","gid":"[0-9A-Za-z]{24}","tailoredPerFile":false}', perl = T) # nolint
          response <- httptest::fake_response(
            req$url,
            req$method,
            # Fixed GID, not the one passed in
            content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"locked":false,"labels":[[26215.4,0.95], [235929.6, 0.95]],"split":{"x":144000,"y":0.5},"gids":["5d30960a417e4bc767a428a3","5d30960a417e4bc767a428a4"]},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"SplitGate","name":"my gate","parentPopulationId":"591a3b5f1d725115208a7087","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4","fcsFileId":"591a3b441d725115208a6fdf"}', # nolint
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
      resp <- createSplitGate("591a3b441d725115208a6fda", "FSC-A", "my gate", 144000,
        labels = list(c(26215.4, 0.95), c(235929.6, 0.95)),
        parentPopulation = "singlets", createPopulation = FALSE, tailoredPerFile = FALSE
      )
      resp <- resp$gate
      expect_equal(resp$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$xChannel, "FSC-A")
      expect_equal(resp$name, "my gate")
      expect_equal(resp$model$labels, matrix(c(c(26215.4, 0.95), c(235929.6, 0.95)), byrow = TRUE, ncol = 2))
      expect_equal(resp$model$split$x, 144000)
      expect_equal(resp$model$split$y, 0.5)
      expect_equal(resp$model$locked, FALSE)
      expect_equal(resp$model$gids, c("5d30960a417e4bc767a428a3", "5d30960a417e4bc767a428a4"))
      expect_equal(resp$gid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$parentPopulationId, "591a3b5f1d725115208a7087")
      expect_equal(resp$type, "SplitGate")
      expect_equal(resp$tailoredPerFile, FALSE)
    }
  )
})
