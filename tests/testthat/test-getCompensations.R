context("getCompensations")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/compensations")
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '[{"_id": "62a41fb7b72926ab549680db", "channels": ["Ax488-A","PE-A","PE-TR-A"],"spillMatrix": [1,0.2,0, 0,1,0, 0,0,1], "experimentId": "62a41fb7b72926ab5496809a", "name": "Comp 1"}]', # nolint
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getCompensations("591a3b441d725115208a6fda")
      expect_equal(resp[[1]]$name, "Comp 1")
      expect_true(is.matrix(resp[[1]]$spillMatrix))
      expect_equal(resp[[1]]$spillMatrix, matrix(
        c(1,0.2,0, 0,1,0, 0,0,1),
        nrow=3,
        ncol=3,
        byrow=TRUE,
        dimnames=list(
          c("Ax488-A","PE-A","PE-TR-A"),
          c("Ax488-A","PE-A","PE-TR-A"))
      ))
    }
  )
})
