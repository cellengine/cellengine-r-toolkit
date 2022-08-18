context("createEllipseGate")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/gates")
      body <- rawToChar(req$options$postfields)
      # Literal except for the gid value
      expect_match(body, '{"model":{"locked":false,"ellipse":{"center":\\[106299.536082474,85580.3298969073\\],"angle":0.703952917888142,"major":166096.63099403,"minor":102655.519773813},"label":\\[106299.536082474,85580.3298969073\\]},"xChannel":"FSC-A","yChannel":"FSC-W","type":"EllipseGate","name":"my gate","gid":"[0-9A-Za-z]{24}","tailoredPerFile":false,"fcsFileId":null}', perl = TRUE) # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        # Fixed GID, not the one passed in
        content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-A","type":"EllipseGate","name":"my gate","yChannel":"FSC-W","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":false,"fcsFileId":null}', # nolint
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- createEllipseGate("591a3b441d725115208a6fda", "FSC-A", "FSC-W", "my gate",
        106299.536082474, 85580.3298969073, 0.7039529178881421, 166096.6309940297, 102655.51977381333,
        createPopulation = FALSE
      )
      expect_equal(resp$gate$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$gate$`_id`, "59289ff59989cc7704ada3c0") # assigned server-side
      expect_equal(resp$gate$xChannel, "FSC-A")
      expect_equal(resp$gate$yChannel, "FSC-W")
      expect_equal(resp$gate$name, "my gate")
      expect_equal(resp$gate$model$label, c(106299.536082474, 85580.3298969073))
      expect_equal(resp$gate$model$ellipse$center, c(106299.53608247427, 85580.32989690728))
      expect_equal(resp$gate$model$ellipse$angle, 0.7039529178881421)
      expect_equal(resp$gate$model$ellipse$major, 166096.6309940297)
      expect_equal(resp$gate$model$ellipse$minor, 102655.51977381333)
      expect_equal(resp$gate$model$locked, FALSE)
      expect_equal(resp$gate$gid, "59289ff2461f1fd925fca4ff")
      expect_equal(resp$gate$type, "EllipseGate")
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
      # Literal except for the gid value
      expect_match(body, '{"model":{"locked":false,"ellipse":{"center":\\[106299.536082474,85580.3298969073\\],"angle":0.703952917888142,"major":166096.63099403,"minor":102655.519773813},"label":\\[106299.536082474,85580.3298969073\\]},"xChannel":"FSC-A","yChannel":"FSC-W","type":"EllipseGate","name":"my gate","gid":"[0-9A-Za-z]{24}","tailoredPerFile":true,"fcsFileId":"59289ff2461f1fd925fca4aa"}', perl = TRUE) # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        # Fixed GID, not the one passed in
        content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-A","type":"EllipseGate","name":"my gate","yChannel":"FSC-W","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":true,"fcsFileId":"59289ff2461f1fd925fca4aa"}', # nolint
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- createEllipseGate("591a3b441d725115208a6fda", "FSC-A", "FSC-W", "my gate",
        106299.536082474, 85580.3298969073, 0.7039529178881421, 166096.6309940297, 102655.51977381333,
        tailoredPerFile = TRUE, fcsFileId = "59289ff2461f1fd925fca4aa", createPopulation = FALSE
      )
      expect_equal(resp$gate$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$gate$`_id`, "59289ff59989cc7704ada3c0") # assigned server-side
      expect_equal(resp$gate$xChannel, "FSC-A")
      expect_equal(resp$gate$yChannel, "FSC-W")
      expect_equal(resp$gate$name, "my gate")
      expect_equal(resp$gate$model$label, c(106299.536082474, 85580.3298969073))
      expect_equal(resp$gate$model$ellipse$center, c(106299.53608247427, 85580.32989690728))
      expect_equal(resp$gate$model$ellipse$angle, 0.7039529178881421)
      expect_equal(resp$gate$model$ellipse$major, 166096.6309940297)
      expect_equal(resp$gate$model$ellipse$minor, 102655.51977381333)
      expect_equal(resp$gate$model$locked, FALSE)
      expect_equal(resp$gate$gid, "59289ff2461f1fd925fca4ff")
      expect_equal(resp$gate$type, "EllipseGate")
      expect_equal(resp$gate$tailoredPerFile, TRUE)
      expect_equal(resp$gate$fcsFileId, "59289ff2461f1fd925fca4aa")
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
                "_id":"591a3b5f1d725115208a7087",
                "experimentId":"591a3b441d725115208a6fda",
                "name":"singlets","gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33a\\"]}","parentId":null,
                "terminalGateGid":"591a3b5961a8a2302d15a33a","__v":0}
            ]',
            status_code = 200,
            headers = list(`Content-Type` = "application/json")
          )
        },
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/gates?createPopulation=true&parentPopulationId=591a3b5f1d725115208a7087" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          # Literal except for the gid value
          expect_match(body, '{"model":{"locked":false,"ellipse":{"center":\\[106299.536082474,85580.3298969073\\],"angle":0.703952917888142,"major":166096.63099403,"minor":102655.519773813},"label":\\[106299.536082474,85580.3298969073\\]},"xChannel":"FSC-A","yChannel":"FSC-W","type":"EllipseGate","name":"my gate","gid":"[0-9A-Za-z]{24}","tailoredPerFile":false,"fcsFileId":null}', perl = TRUE) # nolint
          response <- httptest::fake_response(
            req$url,
            req$method,
            # Fixed GID, not the one passed in
            content = '{
              "gate":{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-A","type":"EllipseGate","name":"my gate","yChannel":"FSC-W","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":false,"fcsFileId":null},
              "population":{"parentId":"591a3b5f1d725115208a7087","_id":"62fc44da8500a029b981e350","name":"my gate","gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33a\\",\\"592640a5a6a1d6256ec9b08a\\"]}","terminalGateGid":"592640a5a6a1d6256ec9b08a","experimentId":"591a3b441d725115208a6fda"}
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
      resp <- createEllipseGate("591a3b441d725115208a6fda", "FSC-A", "FSC-W", "my gate",
        106299.536082474, 85580.3298969073, 0.7039529178881421, 166096.6309940297, 102655.51977381333,
        parentPopulationId = byName("singlets"), createPopulation = TRUE
      )
      expect_equal(resp$gate$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$gate$`_id`, "59289ff59989cc7704ada3c0") # assigned server-side
      expect_equal(resp$gate$xChannel, "FSC-A")
      expect_equal(resp$gate$yChannel, "FSC-W")
      expect_equal(resp$gate$name, "my gate")
      expect_equal(resp$gate$model$label, c(106299.536082474, 85580.3298969073))
      expect_equal(resp$gate$model$ellipse$center, c(106299.53608247427, 85580.32989690728))
      expect_equal(resp$gate$model$ellipse$angle, 0.7039529178881421)
      expect_equal(resp$gate$model$ellipse$major, 166096.6309940297)
      expect_equal(resp$gate$model$ellipse$minor, 102655.51977381333)
      expect_equal(resp$gate$model$locked, FALSE)
      expect_equal(resp$gate$gid, "59289ff2461f1fd925fca4ff")
      expect_equal(resp$gate$type, "EllipseGate")
      expect_equal(resp$gate$tailoredPerFile, FALSE)
      expect_equal(resp$population$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$population$parentId, "591a3b5f1d725115208a7087")
      expect_equal(resp$population$name, "my gate")
      expect_equal(resp$population$terminalGateGid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$population$`_id`, "62fc44da8500a029b981e350")
    }
  )
})
