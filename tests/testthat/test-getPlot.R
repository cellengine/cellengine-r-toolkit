context("getPlot")

test_that("makes expected HTTP request", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/plot?fcsFileId=591a3b441d725115208a6fdc&plotType=dot&xChannel=FSC-A&yChannel=FSC-W&zChannel=&axisLabelsQ=true&ticksQ=true") # nolint

      response <- httptest::fake_response(
        req$url,
        req$method,
        content = readBin("../tinyimage.png", "raw", 10e6),
        status_code = 200,
        headers = list(`Content-Type` = "image/png")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getPlot("591a3b441d725115208a6fda", "591a3b441d725115208a6fdc", "dot", "FSC-A", "FSC-W", display = FALSE)
    }
  )
})

test_that("writes png to expected file path", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/plot?fcsFileId=591a3b441d725115208a6fdc&plotType=dot&xChannel=FSC-A&yChannel=FSC-W&zChannel=&axisLabelsQ=true&ticksQ=true") # nolint

      response <- httptest::fake_response(
        req$url,
        req$method,
        content = readBin("../tinyimage.png", "raw", 10e6),
        status_code = 200,
        headers = list(`Content-Type` = "image/png")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getPlot(
        "591a3b441d725115208a6fda",
        "591a3b441d725115208a6fdc",
        "dot",
        "FSC-A",
        "FSC-W",
        destination = "test_png"
      )

      expect_true(file.exists("test_png.png"))
      file.remove("test_png.png")
      expect_true(!file.exists("test_png.png"))
    }
  )
})

test_that("warns and returns bytes if not in RStudio or Jupyter", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/plot?fcsFileId=591a3b441d725115208a6fdc&plotType=dot&xChannel=FSC-A&yChannel=FSC-W&zChannel=&axisLabelsQ=true&ticksQ=true") # nolint

      response <- httptest::fake_response(
        req$url,
        req$method,
        content = readBin("../tinyimage.png", "raw", 10e6),
        status_code = 200,
        headers = list(`Content-Type` = "image/png")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")

      expect_warning(
        resp <- getPlot(
          "591a3b441d725115208a6fda",
          "591a3b441d725115208a6fdc",
          "dot",
          "FSC-A",
          "FSC-W",
          display = TRUE
        )
      )
      expect_equal(length(resp), 103)  # length of tinyimage.png bytes
    }
  )
})
