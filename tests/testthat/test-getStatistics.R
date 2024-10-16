context("getStatistics")

test_that("throws an error if both fcsFileIds and fcsFiles is specified", {
  expect_error(
    getStatistics("eid",
      statistics = c(), compensationId = 0,
      fcsFileIds = c("a"), fcsFiles = c("b")
    ),
    "only one of 'fcsFiles"
  )
})

test_that("looks up fcsFiles by name; unambiguous match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles?fields=%2Bfilename&query=in%28filename%2C%20%5B%22filename1.fcs%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {"_id":"591a3b5f1d725115208a7088","filename":"filename1.fcs"}
            ]',
            status_code = 200,
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
      # Specify both populationIds and populations so the call dies after the lookup
      expect_error(
        getStatistics(
          "591a3b441d725115208a6fda",
          populationIds = "some ID",
          populations = "some population",
          statistics = c(),
          compensationId = 0,
          fcsFiles = c("filename1.fcs")
        ),
        "Please specify only one of 'populations' or 'populationIds'."
      )
    }
  )
})

test_that("looks up fcsFiles by name; errors with ambiguous results", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles?fields=%2Bfilename&query=in%28filename%2C%20%5B%22filename1.fcs%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {"_id":"591a3b5f1d725115208a7088","filename":"filename1.fcs"},
              {"_id":"591a3b5f1d725115208a7089","filename":"filename1.fcs"}
            ]',
            status_code = 200,
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
      expect_error(
        getStatistics("591a3b441d725115208a6fda", statistics = c(), compensationId = 0, fcsFiles = c("filename1.fcs")),
        "same filenames"
      )
    }
  )
})

test_that("looks up fcsFiles by name; errors with too few results", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles?fields=%2Bfilename&query=in%28filename%2C%20%5B%22filename1.fcs%22%2C%22filename2.fcs%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {"_id":"591a3b5f1d725115208a7089","filename":"filename1.fcs"}
            ]',
            status_code = 200,
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
      expect_error(
        getStatistics("591a3b441d725115208a6fda",
          statistics = c(), compensationId = 0,
          fcsFiles = c("filename1.fcs", "filename2.fcs")
        ),
        "1 file\\(s\\) were not found"
      )
    }
  )
})

test_that("looks up fcsFiles by name; errors with zero results", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles?fields=%2Bfilename&query=in%28filename%2C%20%5B%22filename1.fcs%22%2C%22filename2.fcs%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
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
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      expect_error(
        getStatistics("591a3b441d725115208a6fda",
          statistics = c(), compensationId = 0,
          fcsFiles = c("filename1.fcs", "filename2.fcs")
        ),
        "2 file\\(s\\) were not found"
      )
    }
  )
})

test_that("looks up fcsFiles by name; errors with too few and ambiguous results", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles?fields=%2Bfilename&query=in%28filename%2C%20%5B%22filename1.fcs%22%2C%22filename2.fcs%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {"_id":"591a3b5f1d725115208a7088","filename":"filename1.fcs"},
              {"_id":"591a3b5f1d725115208a7089","filename":"filename1.fcs"}
            ]',
            status_code = 200,
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
      expect_error(
        getStatistics("591a3b441d725115208a6fda",
          statistics = c(), compensationId = 0,
          fcsFiles = c("filename1.fcs", "filename2.fcs")
        ),
        "1 file\\(s\\) were not found"
      )
    }
  )
})

test_that("throws an error if both populationIds and populations is specified", {
  expect_error(
    getStatistics("eid",
      statistics = c(), compensationId = 0, fcsFileIds = c("a"),
      populationIds = c("a"), populations = c("b")
    ),
    "only one"
  )
})

test_that("looks up population by name; unambiguous match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?fields=%2Bname&query=in%28name%2C%20%5B%22pname1%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {"_id":"591a3b5f1d725115208a7088","name":"pname1"}
            ]',
            status_code = 200,
            headers = list(`Content-Type` = "application/json")
          )
          return(response)
        },
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {
                "fcsFileId":"591a3b441d725115208a6fdb","filename":"abc.fcs","populationId":"591a3b5f1d725115208a7088",
                "population":"pname1","annotations":{"row":"A","column":"1"},"parentPopulation":"singlets",
                "parentPopulationId":"591a3b441d725115208a6fde","percent":21.89535144846171
              }
            ]',
            status_code = 200,
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
      res = getStatistics("591a3b441d725115208a6fda",
        statistics = c("percent"), compensationId = 0,
        fcsFileIds = c("591a3b441d725115208a6fdb"), populations = c("pname1")
      )
      expect_true(is.data.frame(res))
    }
  )
})

test_that("looks up multiple populations by name", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?fields=%2Bname&query=in%28name%2C%20%5B%22pname1%22%2C%22pname2%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {"_id":"591a3b5f1d725115208a7088","name":"pname1"},
              {"_id":"591a3b5f1d725115208a7089","name":"pname2"}
            ]',
            status_code = 200,
            headers = list(`Content-Type` = "application/json")
          )
          return(response)
        },
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          expect_equal(body, "{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b5f1d725115208a7088\",\"591a3b5f1d725115208a7089\"],\"compensationId\":0,\"q\":0.5,\"format\":\"json\",\"annotations\":true,\"layout\":\"medium\"}") # nolint
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {
                "fcsFileId":"591a3b441d725115208a6fdb","filename":"abc.fcs","populationId":"591a3b5f1d725115208a7088",
                "population":"pname1","annotations":{"row":"A","column":"1"},"parentPopulation":"singlets",
                "parentPopulationId":"591a3b441d725115208a6fde","percent":21.89535144846171
              },
              {
                "fcsFileId":"591a3b441d725115208a6fdb","filename":"abc.fcs","populationId":"591a3b5f1d725115208a7089",
                "population":"pname2","annotations":{"row":"A","column":"1"},"parentPopulation":"singlets",
                "parentPopulationId":"591a3b441d725115208a6fde","percent":22.234
              }
            ]',
            status_code = 200,
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
      res = getStatistics(
        "591a3b441d725115208a6fda",
        statistics = c("percent"),
        compensationId = 0,
        fcsFileIds = c("591a3b441d725115208a6fdb"),
        populations = c("pname1", "pname2")
      )
      expect_true(is.data.frame(res))
      expect_equal(nrow(res), 2)
    }
  )
})

test_that("looks up populations by name; errors with ambiguous results", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?fields=%2Bname&query=in%28name%2C%20%5B%22pname1%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {"_id":"591a3b5f1d725115208a7088","name":"pname1"},
              {"_id":"591a3b5f1d725115208a7089","name":"pname1"}
            ]',
            status_code = 200,
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
      expect_error(
        getStatistics("591a3b441d725115208a6fda",
          statistics = c(), compensationId = 0,
          fcsFileIds = c("fid1"), populations = c("pname1")
        ),
        "same names"
      )
    }
  )
})

test_that("looks up populations by name; errors with too few results", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?fields=%2Bname&query=in%28name%2C%20%5B%22pname1%22%2C%22pname2%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {"_id":"591a3b5f1d725115208a7089","name":"pname1"}
            ]',
            status_code = 200,
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
      expect_error(
        getStatistics("591a3b441d725115208a6fda",
          statistics = c(), compensationId = 0,
          fcsFileIds = c("fid1"), populations = c("pname1", "pname2")
        ),
        "1 population\\(s\\) were not found"
      )
    }
  )
})

test_that("looks up populations by name; errors with zero results", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?fields=%2Bname&query=in%28name%2C%20%5B%22pname1%22%2C%22pname2%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
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
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      expect_error(
        getStatistics("591a3b441d725115208a6fda",
          statistics = c(), compensationId = 0,
          fcsFileIds = c("fid1"), populations = c("pname1", "pname2")
        ),
        "2 population\\(s\\) were not found"
      )
    }
  )
})

test_that("looks up populations by name; errors with too few and ambiguous results", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?fields=%2Bname&query=in%28name%2C%20%5B%22pname1%22%2C%22pname2%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {"_id":"591a3b5f1d725115208a7088","name":"pname1"},
              {"_id":"591a3b5f1d725115208a7089","name":"pname1"}
            ]',
            status_code = 200,
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
      expect_error(
        getStatistics("591a3b441d725115208a6fda",
          statistics = c(), compensationId = 0,
          fcsFileIds = c("fid1"), populations = c("pname1", "pname2")
        ),
        "1 population\\(s\\) were not found"
      )
    }
  )
})

test_that("works, percentOf specified as single value", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b441d725115208a6fdc\"],\"compensationId\":0,\"q\":0.5,\"format\":\"json\",\"annotations\":true,\"layout\":\"medium\",\"percentOf\":\"591a3b441d725115208a6fde\"}') # nolint
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {
                "fcsFileId":"591a3b441d725115208a6fdb","filename":"abc.fcs","populationId":"591a3b441d725115208a6fdc",
                "population":"positivePop","annotations":{"row":"A","column":"1"},"parentPopulation":"singlets",
                "parentPopulationId":"591a3b441d725115208a6fde","percent":21.89535144846171
              }
            ]',
            status_code = 200,
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
      res <- getStatistics("591a3b441d725115208a6fda",
        statistics = c("percent"), compensationId = 0,
        fcsFileIds = c("591a3b441d725115208a6fdb"), populationIds = c("591a3b441d725115208a6fdc"),
        percentOf = "591a3b441d725115208a6fde"
      )
      expect_true(is.data.frame(res))
      expect_equal(nrow(res), 1)
      expect_equal(res[1, "filename"], "abc.fcs")
      expect_equal(res[1, "annotations"]$row, "A")
    }
  )
})

test_that("works, percentOf 'PARENT'", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b441d725115208a6fdc\"],\"compensationId\":0,\"q\":0.5,\"format\":\"json\",\"annotations\":true,\"layout\":\"medium\",\"percentOf\":[\"591a3b441d725115208a6fde\",\"PARENT\"]}') # nolint
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {
                "fcsFileId":"591a3b441d725115208a6fdb","filename":"abc.fcs","populationId":"591a3b441d725115208a6fdc",
                "population":"positivePop","annotations":{"row":"A","column":"1"},"parentPopulation":"singlets",
                "parentPopulationId":"591a3b441d725115208a6fde","percent":21.89535144846171
              }
            ]',
            status_code = 200,
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
      res <- getStatistics("591a3b441d725115208a6fda",
        statistics = c("percent"), compensationId = 0,
        fcsFileIds = c("591a3b441d725115208a6fdb"), populationIds = c("591a3b441d725115208a6fdc"),
        percentOf = c("591a3b441d725115208a6fde", "PARENT")
      )
      expect_true(is.data.frame(res))
      expect_equal(nrow(res), 1)
      expect_equal(res[1, "filename"], "abc.fcs")
      expect_equal(res[1, "annotations"]$row, "A")
    }
  )
})

test_that("works, percentOf not specified", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b441d725115208a6fdc\"],\"compensationId\":0,\"q\":0.5,\"format\":\"json\",\"annotations\":true,\"layout\":\"medium\"}') # nolint
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = "[
            ]",
            status_code = 200,
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
      getStatistics("591a3b441d725115208a6fda",
        statistics = c("percent"), compensationId = 0,
        fcsFileIds = c("591a3b441d725115208a6fdb"), populationIds = c("591a3b441d725115208a6fdc")
      )
    }
  )
})

test_that("works, percentOf specified as an array", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b441d725115208a6fdc\",\"591a3b441d725115208a6fd1\"],\"compensationId\":0,\"q\":0.5,\"format\":\"json\",\"annotations\":true,\"layout\":\"medium\",\"percentOf\":[\"591a3b441d725115208a6fde\",\"591a3b441d725115208a6fd2\"]}') # nolint
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
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      getStatistics("591a3b441d725115208a6fda",
        statistics = c("percent"), compensationId = 0,
        fcsFileIds = c("591a3b441d725115208a6fdb"),
        populationIds = c("591a3b441d725115208a6fdc", "591a3b441d725115208a6fd1"),
        percentOf = c("591a3b441d725115208a6fde", "591a3b441d725115208a6fd2")
      )
    }
  )
})

test_that("works, percentOf specified as a single name", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?fields=%2Bname&query=in%28name%2C%20%5B%22pname1%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {"_id":"591a3b5f1d725115208a7088","name":"pname1"}
            ]',
            status_code = 200,
            headers = list(`Content-Type` = "application/json")
          )
          return(response)
        },
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b441d725115208a6fdc\",\"591a3b441d725115208a6fd1\"],\"compensationId\":0,\"q\":0.5,\"format\":\"json\",\"annotations\":true,\"layout\":\"medium\",\"percentOf\":"591a3b5f1d725115208a7088"}') # nolint
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
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      getStatistics("591a3b441d725115208a6fda",
        statistics = c("percent"), compensationId = 0,
        fcsFileIds = c("591a3b441d725115208a6fdb"),
        populationIds = c("591a3b441d725115208a6fdc", "591a3b441d725115208a6fd1"),
        percentOf = "pname1"
      )
    }
  )
})

test_that("works, percentOf specified as an array of names", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?fields=%2Bname&query=in%28name%2C%20%5B%22pname1%22%2C%22pname2%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {"_id":"591a3b5f1d725115208a7088","name":"pname1"},
              {"_id":"591a3b5f1d725115208a7090","name":"pname2"}
            ]',
            status_code = 200,
            headers = list(`Content-Type` = "application/json")
          )
          return(response)
        },
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b441d725115208a6fdc\",\"591a3b441d725115208a6fd1\"],\"compensationId\":0,\"q\":0.5,\"format\":\"json\",\"annotations\":true,\"layout\":\"medium\",\"percentOf\":["591a3b5f1d725115208a7088","591a3b5f1d725115208a7090"]}') # nolint
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
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      getStatistics("591a3b441d725115208a6fda",
        statistics = c("percent"), compensationId = 0,
        fcsFileIds = c("591a3b441d725115208a6fdb"),
        populationIds = c("591a3b441d725115208a6fdc", "591a3b441d725115208a6fd1"),
        percentOf = c("pname1", "pname2")
      )
    }
  )
})

test_that("works, percentOf specified as a mixed array of names, IDs and UNGATED", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?fields=%2Bname&query=in%28name%2C%20%5B%22pname1%22%5D%29" = { # nolint
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {"_id":"591a3b5f1d725115208a7088","name":"pname1"}
            ]',
            status_code = 200,
            headers = list(`Content-Type` = "application/json")
          )
          return(response)
        },
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":["591a3b441d725115208a6fdc","591a3b441d725115208a6fd1","591a3b441d725115208a6fe1"],\"compensationId\":0,\"q\":0.5,\"format\":\"json\",\"annotations\":true,\"layout\":\"medium\",\"percentOf\":["591a3b5f1d725115208a7088","591a3b5f1d725115208a7090",null]}') # nolint
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
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      getStatistics("591a3b441d725115208a6fda",
        statistics = c("percent"), compensationId = 0,
        fcsFileIds = c("591a3b441d725115208a6fdb"),
        populationIds = c("591a3b441d725115208a6fdc", "591a3b441d725115208a6fd1", "591a3b441d725115208a6fe1"), # nolint
        percentOf = c("pname1", "591a3b5f1d725115208a7090", UNGATED)
      )
    }
  )
})

test_that("works, percentOf specified as UNGATED", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b441d725115208a6fdc\",\"591a3b441d725115208a6fd1\"],\"compensationId\":0,\"q\":0.5,\"format\":\"json\",\"annotations\":true,\"layout\":\"medium\",\"percentOf\":null}') # nolint
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
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      getStatistics("591a3b441d725115208a6fda",
        statistics = c("percent"), compensationId = 0,
        fcsFileIds = c("591a3b441d725115208a6fdb"),
        populationIds = c("591a3b441d725115208a6fdc", "591a3b441d725115208a6fd1"),
        percentOf = UNGATED
      )
    }
  )
})

test_that("works, gets statistics for all FCS files", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          expect_equal(body, '{\"fcsFileIds\":[],\"statistics\":[\"percent\"],\"populationIds\":null,\"compensationId\":0,\"q\":0.5,\"format\":\"json\",\"annotations\":true,\"layout\":\"medium\",\"percentOf\":null}') # nolint
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
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      getStatistics("591a3b441d725115208a6fda",
        statistics = c("percent"), compensationId = 0,
        fcsFileIds = NULL, populationIds = NULL,
        percentOf = UNGATED
      )
    }
  )
})

test_that("handles null populationId, parentPopulationId, reagent in response", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {
                "parentPopulationId": null,
                "populationId": null,
                "parentPopulation": "Ungated",
                "population": "Ungated",
                "percent": 1.2,
                "channel": "Ce140",
                "reagent": null,
                "filename": "test.fcs",
                "fcsFileId": "619555fa3f6700533c6f9006"
              }
            ]',
            status_code = 200,
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
      res <- getStatistics("591a3b441d725115208a6fda",
        statistics = c("percent"), compensationId = 0,
        fcsFileIds = NULL, populationIds = NULL,
        percentOf = UNGATED
      )
      expect_equal(res$populationId[1], UNGATED)
      expect_equal(res$parentPopulationId[1], UNGATED)
      expect_equal(res$reagent[1], NA)
    }
  )
})
