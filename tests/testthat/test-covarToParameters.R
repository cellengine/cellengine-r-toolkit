context("covarToParameters")

test_that("returns ellipse parameters from covariance matrix", {
  covar <- matrix(c(6879, 3612, 3612, 5215), nrow = 2, ncol = 2, byrow = TRUE,
         dimnames = list(c("FSC-A", "FSC-W"),
                         c("FSC-A", "FSC-W")))
  params <- covarToParameters(covar)

  expect_equal(params$major, 98.8, tolerance = 0.001)
  expect_equal(params$minor, 48.4, tolerance = 0.001)
  expect_equal(params$angle, 0.672, tolerance = 0.001)
})

test_that("returns parameters for test matrix", {
  # https://cookierobotics.com/007/
  covar <- matrix(c(9, 5, 5, 4), nrow = 2, ncol = 2, byrow = TRUE,
         dimnames = list(c("FSC-A", "FSC-W"),
                         c("FSC-A", "FSC-W")))
  params <- covarToParameters(covar)

  expect_equal(params$major, 3.477, tolerance = 0.001)
  expect_equal(params$minor, 0.954, tolerance = 0.001)
  expect_equal(params$angle, 0.554, tolerance = 0.001)
})

test_that("returns ellipse parameters for a unit circle", {
  covar <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE,
         dimnames = list(c("FSC-A", "FSC-W"),
                         c("FSC-A", "FSC-W")))
  params <- covarToParameters(covar)

  expect_equal(params$major, 1, tolerance = 0.001)
  expect_equal(params$minor, 1, tolerance = 0.001)
  expect_equal(params$angle, 0, tolerance = 0.001)
})
