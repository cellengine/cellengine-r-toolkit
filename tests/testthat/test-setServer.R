context("setServer")

test_that("Sets the baseURL pkg variable", {
  setServer("https://my.server.com")
  expect_equal(pkg.env$baseURL, "https://my.server.com/api/v1")
})

test_that("Tolerates trailing /", {
  setServer("https://my.server.com/")
  expect_equal(pkg.env$baseURL, "https://my.server.com/api/v1")
})

test_that("Requires https", {
  expect_error((function() setServer("http://my.server.com/"))())
})
