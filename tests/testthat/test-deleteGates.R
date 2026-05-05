context("deleteGates")

test_that("Correct HTTP request is made", {
  local_mocked_bindings(
    request_perform = function(req, handle, refresh) {
      expect_equal(req$method, "DELETE")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/gates?gid=591a3b441d725115208a6fdc") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = "",
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    .package = "httr"
  )
  setServer("https://my.server.com")
  resp <- deleteGates("591a3b441d725115208a6fda", "591a3b441d725115208a6fdc")
})
