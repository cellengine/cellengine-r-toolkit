context("getStatistics")

test_that("throws an error if both fcsFileIds and fcsFiles is specified", {{ expect_error(
  getStatistics("eid",
    statistics = c(), compensationId = 0,
    fcsFileIds = c("a"), fcsFiles = c("b")
  ),
  "only one of 'fcsFiles"
) }})

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

test_that("throws an error if both populationIds and populations is specified", {{ expect_error(
  getStatistics("eid",
    statistics = c(), compensationId = 0, fcsFileIds = c("a"),
    populationIds = c("a"), populations = c("b")
  ),
  "only one"
) }})

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
        {
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      # Specify fake stat so the call dies after the lookup
      expect_error(
        getStatistics("591a3b441d725115208a6fda",
          statistics = c("fake"), compensationId = 0,
          fcsFileIds = c("fid1"), populations = c("pname1")
        ),
        "not allowed"
      )
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

test_that("whitelists statistics", {{ expect_error(
  getStatistics("eid",
    statistics = c("MEAN", "median", "quantile", "stdDev", "CV", "MAD", "eventcount", "percent", "fake1", "fake2"),
    compensationId = 0,
    fcsFileIds = c("fid1", "fid2"),
    populationIds = c("p1", "p2")
  ),
  "Statistics \\[fake1, fake2\\] are not allowed"
) }})

test_that("looks up default scaleset; single match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/scalesets?fields=%2B_id" = {
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
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      # Specify bad percentOf arg so fn stops after lookup
      expect_error(
        getStatistics("591a3b441d725115208a6fda",
          statistics = c("percent"), compensationId = 0,
          fcsFileIds = c("fid1"), populationIds = c("pname1"), percentOf = c("a", "b")
        ),
        "same length"
      )
    }
  )
})

test_that("looks up default scaleset; no matches", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/scalesets?fields=%2B_id" = {
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
          statistics = c("percent"), compensationId = 0,
          fcsFileIds = c("fid1"), populationIds = c("pname1")
        ),
        "No scalesets"
      )
    }
  )
})

test_that("looks up default scaleset; more than one match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/scalesets?fields=%2B_id" = {
          expect_equal(req$method, "GET")
          response <- httptest::fake_response(
            req$url,
            req$method,
            content = '[
              {"_id":"591a3b5f1d725115208a7088"},
              {"_id":"591a3b5f1d725115208a7089"}
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
          statistics = c("percent"), compensationId = 0,
          fcsFileIds = c("fid1"), populationIds = c("pname1")
        ),
        "More than one scaleset"
      )
    }
  )
})

test_that("validates percentOf array is same length as populationIds", {{ setServer("https://my.server.com")
  expect_error(
    getStatistics("591a3b441d725115208a6fda",
      statistics = c("percent"), compensationId = 0,
      fcsFileIds = c("fid1"), populationIds = c("pname1"), scaleSetId = "abc", percentOf = c("a", "b")
    ),
    "same length"
  ) }})

test_that("works, percentOf specified as single value", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b441d725115208a6fdc\"],\"compensationId\":0,\"q\":0.5,\"scaleSetId\":\"591a3b441d725115208a6fdd\",\"format\":\"json\",\"annotations\":true,\"percentOf\":\"591a3b441d725115208a6fde\"}') # nolint
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
        scaleSetId = "591a3b441d725115208a6fdd", percentOf = "591a3b441d725115208a6fde"
      )
      expect_true(is.data.frame(res))
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
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b441d725115208a6fdc\"],\"compensationId\":0,\"q\":0.5,\"scaleSetId\":\"591a3b441d725115208a6fdd\",\"format\":\"json\",\"annotations\":true}') # nolint
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
        fcsFileIds = c("591a3b441d725115208a6fdb"), populationIds = c("591a3b441d725115208a6fdc"),
        scaleSetId = "591a3b441d725115208a6fdd"
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
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b441d725115208a6fdc\",\"591a3b441d725115208a6fd1\"],\"compensationId\":0,\"q\":0.5,\"scaleSetId\":\"591a3b441d725115208a6fdd\",\"format\":\"json\",\"annotations\":true,\"percentOf\":[\"591a3b441d725115208a6fde\",\"591a3b441d725115208a6fd2\"]}') # nolint
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
        fcsFileIds = c("591a3b441d725115208a6fdb"), populationIds = c("591a3b441d725115208a6fdc", "591a3b441d725115208a6fd1"), # nolint
        scaleSetId = "591a3b441d725115208a6fdd", percentOf = c("591a3b441d725115208a6fde", "591a3b441d725115208a6fd2")
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
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b441d725115208a6fdc\",\"591a3b441d725115208a6fd1\"],\"compensationId\":0,\"q\":0.5,\"scaleSetId\":\"591a3b441d725115208a6fdd\",\"format\":\"json\",\"annotations\":true,\"percentOf\":"591a3b5f1d725115208a7088"}') # nolint
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
        fcsFileIds = c("591a3b441d725115208a6fdb"), populationIds = c("591a3b441d725115208a6fdc", "591a3b441d725115208a6fd1"), # nolint
        scaleSetId = "591a3b441d725115208a6fdd", percentOf = "pname1"
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
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b441d725115208a6fdc\",\"591a3b441d725115208a6fd1\"],\"compensationId\":0,\"q\":0.5,\"scaleSetId\":\"591a3b441d725115208a6fdd\",\"format\":\"json\",\"annotations\":true,\"percentOf\":["591a3b5f1d725115208a7088","591a3b5f1d725115208a7090"]}') # nolint
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
        scaleSetId = "591a3b441d725115208a6fdd", percentOf = c("pname1", "pname2")
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
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":["591a3b441d725115208a6fdc","591a3b441d725115208a6fd1","591a3b441d725115208a6fe1"],\"compensationId\":0,\"q\":0.5,\"scaleSetId\":\"591a3b441d725115208a6fdd\",\"format\":\"json\",\"annotations\":true,\"percentOf\":["591a3b5f1d725115208a7088","591a3b5f1d725115208a7090",""]}') # nolint
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
        fcsFileIds = c("591a3b441d725115208a6fdb"), populationIds = c("591a3b441d725115208a6fdc", "591a3b441d725115208a6fd1", "591a3b441d725115208a6fe1"), # nolint
        scaleSetId = "591a3b441d725115208a6fdd", percentOf = c("pname1", "591a3b5f1d725115208a7090", UNGATED)
      )
    }
  )
})

test_that("works, percentOf specified as null (ungated)", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          expect_equal(body, '{\"fcsFileIds\":[\"591a3b441d725115208a6fdb\"],\"statistics\":[\"percent\"],\"populationIds\":[\"591a3b441d725115208a6fdc\",\"591a3b441d725115208a6fd1\"],\"compensationId\":0,\"q\":0.5,\"scaleSetId\":\"591a3b441d725115208a6fdd\",\"format\":\"json\",\"annotations\":true,\"percentOf\":""}') # nolint
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
        fcsFileIds = c("591a3b441d725115208a6fdb"), populationIds = c("591a3b441d725115208a6fdc", "591a3b441d725115208a6fd1"), # nolint
        scaleSetId = "591a3b441d725115208a6fdd", percentOf = UNGATED
      )
    }
  )
})

test_that("works, gets statistics for all FCS files and populations", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/bulkstatistics" = {
          expect_equal(req$method, "POST")
          body <- rawToChar(req$options$postfields)
          expect_equal(body, '{\"fcsFileIds\":null,\"statistics\":[\"percent\"],\"populationIds\":null,\"compensationId\":0,\"q\":0.5,\"scaleSetId\":\"591a3b441d725115208a6fdd\",\"format\":\"json\",\"annotations\":true,\"percentOf\":""}') # nolint
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
        scaleSetId = "591a3b441d725115208a6fdd", percentOf = UNGATED
      )
    }
  )
})
