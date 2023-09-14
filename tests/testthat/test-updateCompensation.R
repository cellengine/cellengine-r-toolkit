context("updateCompensation")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "PATCH")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/compensations/62a41fb7b72926ab549680db")
      body <- rawToChar(req$options$postfields)
      expect_equal(body, '{"spillMatrix":[1,0.1,0,0,1,0,0,0,1]}')
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '{"_id": "62a41fb7b72926ab549680db", "channels": ["Ax488-A","PE-A","PE-TR-A"],"spillMatrix": [1,0.1,0, 0,1,0, 0,0,1], "experimentId": "62a41fb7b72926ab5496809a", "name": "Comp 1"}', # nolint
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- updateCompensation(
        "591a3b441d725115208a6fda",
        "62a41fb7b72926ab549680db",
        list(spillMatrix = c(1,0.1,0, 0,1,0, 0,0,1))
      )
      expect_equal(resp$name, "Comp 1")
      expect_true(is.matrix(resp$spillMatrix))
      expect_equal(resp$spillMatrix, matrix(
        c(1,0.1,0, 0,1,0, 0,0,1),
        nrow=3,
        ncol=3,
        byrow=TRUE,
        dimnames=list(
          c("Ax488-A","PE-A","PE-TR-A"),
          c("Ax488-A","PE-A","PE-TR-A")
        )
      ))
    }
  )
})
