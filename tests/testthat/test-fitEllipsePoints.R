context("fitEllipsePoints")

test_that("fits an ellipse to given points", {
  points <- t(data.frame(list(
    c(232913.3, 193084.0),
    c(148847.1, 216930.3),
    c(39857.19, 163833.57),
    c(-30211.74, 64897.12),
    c(-20314.21, -21923.39),
    c(63751.93, -45769.68),
    c(172741.885, 7327.087),
    c(242810.8, 106263.5)
  )))

  results <- fitEllipsePoints(points, 1e-5, 10000)
  expected <- matrix(as.numeric(list(20445626846, 8412116525, 8412116525, 17680614553)), ncol = 2)
  expect_equal(expected, results$covar)
  expect_equal(106299.5, results$x, tolerance = 0.005)
  expect_equal(85580.32, results$y, tolerance = 0.005)
  expect_equal(166096.6, results$major, tolerance = 0.005)
  expect_equal(102655.5, results$minor, tolerance = 0.005)
  expect_equal(0.703953, results$angle, tolerance = 0.005)
})

test_that("fits an ellipse to unit circle", {
  points <- t(data.frame(list(
    c(0, 1),
    c(1, 0),
    c(-1, 0),
    c(0, -1)
  )))

  results <- fitEllipsePoints(points, 1e-5, 10000)
  expected <- matrix(as.numeric(list(1, 0, 0, 1)), ncol = 2)
  expect_equal(expected, results$covar)
  expect_equal(0, results$x, tolerance = 0.005)
  expect_equal(0, results$y, tolerance = 0.005)
  expect_equal(1, results$major, tolerance = 0.005)
  expect_equal(1, results$minor, tolerance = 0.005)
  expect_equal(0, results$angle, tolerance = 0.005)
})
