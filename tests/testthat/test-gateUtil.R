context("parseFcsFileArgs")

test_that("works if tailoredPerFile=FALSE", {
  result <- parseFcsFileArgs(list(), FALSE, NULL, "591a3b441d725115208a6fda")
  expect_equal(result, list(
    tailoredPerFile = jsonlite::unbox(FALSE),
    fcsFileId = jsonlite::unbox(NULL)
  ))
})

test_that("works if tailoredPerFile=TRUE and fcsFileId is an ID", {
  result <- parseFcsFileArgs(list(), TRUE, "591a3b5f1d725115208a7088", "591a3b441d725115208a6fda")
  expect_equal(result, list(
    tailoredPerFile = jsonlite::unbox(TRUE),
    fcsFileId = jsonlite::unbox("591a3b5f1d725115208a7088")
  ))
})

test_that("assigns the ID of a matching fcsFile given in byName()", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$url, "https://cellengine.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles?query=eq%28filename%2C%20%22name%22%29&limit=2") # nolint
      expect_equal(req$method, "GET")
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '[
          {"_id":"591a3b5f1d725115208a7088"}
        ]',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://cellengine.com")
      result <- parseFcsFileArgs(list(), TRUE, byName("name"), "591a3b441d725115208a6fda")
      expect_equal(result, list(
        tailoredPerFile = jsonlite::unbox(TRUE),
        fcsFileId = jsonlite::unbox("591a3b5f1d725115208a7088")
      ))
    }
  )
})
