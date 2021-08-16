context("setFcsFilePanel")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "PATCH")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/591a3b441d725115208a6fdc") # nolint
      body <- rawToChar(req$options$postfields)
      expect_equal(body, '{"panelName":"Panel 1","panel":[{"index":1,"channel":"FSC-A"},{"index":7,"channel":"Blue530-A","reagent":"CD3"}]}') # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '{"filename":"Specimen_001_A2_A02_MeOHperm(DL350neg).fcs","gridId":"593f3ae211005114b1bf03e2","hasFileInternalComp":true,"panelName":"Panel 1","__v":0,"_id":"591a3b441d725115208a6fdc","spillString":"8,Blue530-A,Vio450-A,Vio605-A,UV450-A,Red670-A,YG582-A,YG610-A,YG780-A,1,0,0,0,0,0,0,0,0,1,0.019999998552000027,0.055000000000000014,0,0,0,0,0.003000000000000001,0.012999999058800013,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1","crc32c":"76633d32","eventCount":164684,"md5":"4a0ded33c1f55c6ff52c90dbe3c4894d","experimentId":"591a3b441d725115208a6fda","panel":[{"index":1,"channel":"FSC-A"},{"index":7,"reagent":"CD3","channel":"Blue530-A"}],"annotations":[{"type":"any","value":"myvalue","name":"annotation 1"},{"type":"any","value":"2.12","name":"annotation 2"}]}', # nolint
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      panel <- list(
        list("index" = 1, "channel" = "FSC-A"),
        list("index" = 7, "channel" = "Blue530-A", "reagent" = "CD3")
      )
      resp <- setFcsFilePanel("591a3b441d725115208a6fda", "591a3b441d725115208a6fdc", "Panel 1", panel)
    }
  )
})
