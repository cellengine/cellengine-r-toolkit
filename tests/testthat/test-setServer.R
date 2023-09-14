context("setServer")

test_that("Sets the baseURL pkg variable", {
  setServer("https://my.server.com")
  expect_equal(pkg.env$baseURL, "https://my.server.com")
})

test_that("Trims trailing /", {
  setServer("https://my.server.com/")
  expect_equal(pkg.env$baseURL, "https://my.server.com")
})
