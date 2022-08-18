context("getEvents")

test_that("makes expected HTTP request", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/591a3b441d725115208a6fdc.FCS?populationId=null&compensationId=0&compensatedQ=false&headers=true&addEventNumber=true") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = readBin("../5k.fcs", raw(), file.info("../5k.fcs")$size),
        status_code = 200,
        headers = list(`Content-Type` = "application/vnd.isac.fcs")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getEvents("591a3b441d725115208a6fda", "591a3b441d725115208a6fdc", addEventNumber = TRUE)
    }
  )
})

test_that("makes expected HTTP request with subsampling", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/591a3b441d725115208a6fdc.FCS?populationId=null&compensationId=0&compensatedQ=false&headers=true&addEventNumber=false&preSubsampleN=50&seed=2.25") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = readBin("../5k.fcs", raw(), file.info("../5k.fcs")$size),
        status_code = 200,
        headers = list(`Content-Type` = "application/vnd.isac.fcs")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getEvents(
        "591a3b441d725115208a6fda",
        "591a3b441d725115208a6fdc",
        subsampling = list(preSubsampleN = 50, seed = 2.25)
      )
    }
  )
})

test_that("doesn't mangle column names", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "GET")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/591a3b441d725115208a6fdc.TSV?populationId=null&compensationId=0&compensatedQ=false&headers=true&addEventNumber=false") # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = "FSC-A\tCD3 (Ax647-A)\r\n1.23\t2.34\r\n3.45\t4.56\r\n",
        status_code = 200,
        headers = list(`Content-Type` = "application/vnd.isac.fcs")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getEvents("591a3b441d725115208a6fda", "591a3b441d725115208a6fdc", format = "TSV")
      expect_equal(colnames(resp), c("FSC-A", "CD3 (Ax647-A)"))
    }
  )
})

test_that("makes expected HTTP request for S3 transfer", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/591a3b441d725115208a6fdc.FCS?populationId=null&compensationId=0&compensatedQ=false&headers=true&addEventNumber=false") # nolint
      body <- rawToChar(req$options$postfields)
      expect_equal(body, '{"dest":{"host":"ce-test-s3-b.s3.us-east-2.amazonaws.com","path":"/","accessKey":"access key","secretKey":"secret key","headers":{"x-amz-storage-class":"REDUCED_REDUNDANCY","x-amz-server-side-encryption":"AES256"}}}') # nolint
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '{
          "s3Status":200,
          "s3StatusMessage":"OK",
          "s3Headers":{
            "x-amz-id-2":"MigW9a2LcKC/DJucA0rVt2vqwwUJQJuJxlzMsmzdjTafoA9T+sMSK/Vt5paFlIPnEheq2sWXE3ekkUtopKoP8g==",
            "x-amz-request-id":"RJT8S9PGDPJ9N0Y3",
            "date":"Thu, 18 Aug 2022 03:41:55 GMT",
            "etag":"\\"844da3ef3926fdfe1d5e2fc35a34d6b4\\"",
            "server":"AmazonS3",
            "content-length":"0"
          },
          "s3Response":""
        }',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp <- getEvents(
        "591a3b441d725115208a6fda",
        "591a3b441d725115208a6fdc",
        destination=list(
          host="ce-test-s3-b.s3.us-east-2.amazonaws.com",
          path="/",
          accessKey="access key",
          secretKey="secret key",
          headers=list(
            `x-amz-storage-class`="REDUCED_REDUNDANCY",
            `x-amz-server-side-encryption`="AES256"
          )
        )
      )
    }
  )
})