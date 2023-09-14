context("authenticate")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/signin")
      body <- rawToChar(req$options$postfields)
      expect_equal(body, '{"username":"user1","password":"p@ssword"}')
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '{"token":"s:abcdefgh.ijklmnop/pqr","userId":"592799bd14ac0ad59699cb77","admin":false,"flags":{}}',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      authenticate("user1", "p@ssword")
    }
  )
})

test_that("Correct HTTP request is made with OTP", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/signin")
      body <- rawToChar(req$options$postfields)
      expect_equal(body, '{"username":"user1","password":"p@ssword","otp":"012345"}')
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '{"token":"s:abcdefgh.ijklmnop/pqr","userId":"592799bd14ac0ad59699cb77","admin":false,"flags":{}}',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      authenticate("user1", "p@ssword", "012345")
    }
  )
})

test_that("Authenticating with an access token sets token on subsequent requests", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments")
      expect_equal(req$headers, c("Authorization" = "Bearer cep_mypersonalaccesstoken"))
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = "[]",
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      authenticate(token = "cep_mypersonalaccesstoken")
      getExperiments()
    }
  )
})
