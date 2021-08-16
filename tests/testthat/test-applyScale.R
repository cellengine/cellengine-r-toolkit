context("applyScale")

test_that("applies linear scale to list", {
  data <- c(10.0, 7.0, 1.2, 9.0, 40.0)
  scale <- list(
    minimum = 1,
    maximum = 10,
    type = "LinearScale"
  )

  result = applyScale(scale, data, clampQ=FALSE)
  expect_equal(data, result)
})

test_that("clamps linear scale", {
  data <- c(10.0, 7.0, 1.2, 9.0, 40.0)
  scale <- list(
    minimum = 5,
    maximum = 10,
    type = "LinearScale"
  )

  result = applyScale(scale, data, clampQ=TRUE)
  expected = c(10.0, 7.0, 5.0, 9.0, 10.0)
  expect_equal(expected, result)
})

test_that("applies log scale", {
  data <- c(10.0, -1, 7.0, 1.2, 9.0, 40.0)
  scale <- list(
    minimum = 2,
    maximum = 10,
    type = "LogScale"
  )

  result = applyScale(scale, data, clampQ=FALSE)
  expected = c(1.0, 0, 0.845098, 0.07918125, 0.9542425, 1.60206)
  expect_equal(expected, result, tolerance=0.001)
})

test_that("applies clamped log scale", {
  data <- c(10.0, 7.0, 1.2, 9.0, 40.0)
  scale <- list(
    minimum = 2,
    maximum = 10,
    type = "LogScale"
  )

  result = applyScale(scale, data, clampQ=TRUE)
  expected = c(1.0, 0.845098, 0.30103, 0.9542425, 1.0)
  expect_equal(expected, result, tolerance=0.001)
})

test_that("applies arcsinh scale", {
  data <- c(-250, -20, -2, -0.01, 0, 0.2, 0.5, 1)
  scale <- list(
    minimum = -200,
    maximum = 5000,
    cofactor = 5,
    type = "ArcSinhScale"
  )

  result = applyScale(scale, data, clampQ=FALSE)
  expected = c(-4.60527, -2.094713, -0.3900353, -0.001999999, 0, 0.03998934, 0.09983408, 0.1986901)
  expect_equal(expected, result, tolerance=0.001)
})

test_that("applies clamped arcsinh scale", {
  data <- c(-250, -20, -2, -0.01, 0, 0.2, 0.5, 1)
  scale <- list(
    minimum = -200,
    maximum = 5000,
    cofactor = 5,
    type = "ArcSinhScale"
  )

  result = applyScale(scale, data, clampQ=TRUE)
  expected = c(-4.382183, -2.094713, -0.3900353, -0.001999999, 0, 0.03998934, 0.09983408, 0.1986901)
  expect_equal(expected, result, tolerance=0.001)
})
