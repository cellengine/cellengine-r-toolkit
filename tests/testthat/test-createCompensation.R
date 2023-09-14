context("createCompensation")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/compensations")
      body <- rawToChar(req$options$postfields)
      expect_equal(body, '{"name":"Comp 1","channels":["Ax488-A","PE-A"],"spillMatrix":[1,0.1,0,1]}')
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '{"_id": "62a41fb7b72926ab549680db", "channels": ["Ax488-A","PE-A"],"spillMatrix": [1,0.1, 0,1], "experimentId": "591a3b441d725115208a6fdb", "name": "Comp 1"}',
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- createCompensation(
        "591a3b441d725115208a6fda",
        name="Comp 1",
        channels=c("Ax488-A", "PE-A"),
        spillMatrix=c(1, 0.1, 0, 1)
      )
      expect_equal(resp$`experimentId`, "591a3b441d725115208a6fdb")
      expect_equal(resp$`_id`, "62a41fb7b72926ab549680db")
      expect_equal(resp$name, "Comp 1")
      expect_equal(resp$spillMatrix, matrix(
        c(1,0.1, 0,1),
        nrow=2,
        ncol=2,
        byrow=TRUE,
        dimnames=list(
          c("Ax488-A", "PE-A"),
          c("Ax488-A", "PE-A")
        )
      ))
    }
  )
})

test_that("Correct HTTP request is made given a matrix", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/compensations")
      body <- rawToChar(req$options$postfields)
      expect_equal(body, '{"name":"Comp 1","channels":["Ax488-A","PE-A"],"spillMatrix":[1,0.1,0,1]}')
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '{"_id": "62a41fb7b72926ab549680db", "channels": ["Ax488-A","PE-A"],"spillMatrix": [1,0.1, 0,1], "experimentId": "591a3b441d725115208a6fdb", "name": "Comp 1"}',
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- createCompensation(
        "591a3b441d725115208a6fda",
        name="Comp 1",
        matrix(
          c(1,0.1, 0,1),
          nrow=2,
          ncol=2,
          byrow=TRUE,
          dimnames=list(
            c("Ax488-A", "PE-A"),
            c("Ax488-A", "PE-A")
          )
        )
      )
      expect_equal(resp$`experimentId`, "591a3b441d725115208a6fdb")
      expect_equal(resp$`_id`, "62a41fb7b72926ab549680db")
      expect_equal(resp$name, "Comp 1")
      expect_equal(resp$spillMatrix, matrix(
        c(1,0.1, 0,1),
        nrow=2,
        ncol=2,
        byrow=TRUE,
        dimnames=list(
          c("Ax488-A", "PE-A"),
          c("Ax488-A", "PE-A")
        )
      ))
    }
  )
})
