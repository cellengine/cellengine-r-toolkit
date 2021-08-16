context("createPopulation")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations")
      body <- rawToChar(req$options$postfields)
      expect_equal(body, "{\"name\":\"Singlets\",\"gates\":\"{\\\"$and\\\":[\\\"59262d84b1a1fc1193f12b0e\\\"]}\",\"terminalGateGid\":\"59262d84b1a1fc1193f12b0e\",\"parentId\":null}") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = "{\"_id\":\"59263d09b1a1fc1193f12b0f\",\"name\":\"Singlets\",\"gates\":\"{\\\"$and\\\":[\\\"59262d84b1a1fc1193f12b0e\\\"]}\",\"terminalGateGid\":\"59262d84b1a1fc1193f12b0e\",\"parentId\":null}", # nolint
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- createPopulation("591a3b441d725115208a6fda", "Singlets", list(`$and` = c("59262d84b1a1fc1193f12b0e")), "59262d84b1a1fc1193f12b0e") # nolint
      expect_equal(resp$`_id`, "59263d09b1a1fc1193f12b0f") # assigned server-side
      expect_equal(resp$parentId, NULL) # default assigned client-side
      expect_equal(resp$name, "Singlets")
      expect_equal(resp$gates, "{\"$and\":[\"59262d84b1a1fc1193f12b0e\"]}") # string, not parsed
      expect_equal(resp$terminalGateGid, "59262d84b1a1fc1193f12b0e")
    }
  )
})
