context("applyScaleSet")

test_that("applies scaleset with linear scale", {
  data <- data.frame("FSC-A" = c(10.0, 7.0, 1.2, 9.0, 40.0), check.names = FALSE)
  # FSC-A: min = 1, max = 10, type = LinearScale
  scaleSet <- jsonlite::fromJSON('[{"scales":[{"channelName":"FSC-A","scale":{"type":"LinearScale","minimum":1,"maximum":10}}],"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","__v":1,"updated":"2020-10-27T18:38:55.554Z"}]') # nolint

  result <- applyScaleSet(scaleSet, data)
  expect_equal(data, result)
})

test_that("clamps linear scale", {
  data <- data.frame("FSC-A" = c(10.0, 7.0, 1.2, 9.0, 40.0), check.names = FALSE)
  # FSC-A: min = 5, max = 10, type = LinearScale
  scaleSet <- jsonlite::fromJSON('[{"scales":[{"channelName":"FSC-A","scale":{"type":"LinearScale","minimum":5,"maximum":10}}],"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","__v":1,"updated":"2020-10-27T18:38:55.554Z"}]') # nolint

  result <- applyScaleSet(scaleSet, data, clamp_q = TRUE)
  expected <- data.frame("FSC-A" = c(10.0, 7.0, 5.0, 9.0, 10.0), check.names = FALSE)
  expect_equal(expected, result)
})

test_that("applies log scale", {
  data <- data.frame("FSC-A" = c(10.0, -1, 7.0, 1.2, 9.0, 40.0), check.names = FALSE)
  # FSC-A: min = 2, max = 10, type = LogScale
  scaleSet <- jsonlite::fromJSON('[{"scales":[{"channelName":"FSC-A","scale":{"type":"LogScale","minimum":2,"maximum":10}}],"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","__v":1,"updated":"2020-10-27T18:38:55.554Z"}]') # nolint

  result <- applyScaleSet(scaleSet, data)
  expected <- data.frame("FSC-A" = c(1.0, 0, 0.845098, 0.07918125, 0.9542425, 1.60206), check.names = FALSE)
  expect_equal(expected, result, tolerance = 0.001)
})

test_that("applies clamped log scale", {
  data <- data.frame("FSC-A" = c(10.0, 7.0, 1.2, 9.0, 40.0), check.names = FALSE)
  # FSC-A: min = 2, max = 10, type = LogScale
  scaleSet <- jsonlite::fromJSON('[{"scales":[{"channelName":"FSC-A","scale":{"type":"LogScale","minimum":2,"maximum":10}}],"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","__v":1,"updated":"2020-10-27T18:38:55.554Z"}]') # nolint

  result <- applyScaleSet(scaleSet, data, clamp_q = TRUE)
  expected <- data.frame("FSC-A" = c(1.0, 0.845098, 0.30103, 0.9542425, 1.0), check.names = FALSE)
  expect_equal(expected, result, tolerance = 0.001)
})

test_that("applies arcsinh scale", {
  data <- data.frame("FSC-A" = c(-250, -20, -2, -0.01, 0, 0.2, 0.5, 1), check.names = FALSE)
  # FSC-A: min = -200, max = 5000, cofactor = 5, type = LogScale
  scaleSet <- jsonlite::fromJSON('[{"scales":[{"channelName":"FSC-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":5000,"cofactor":5}}],"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","__v":1,"updated":"2020-10-27T18:38:55.554Z"}]') # nolint

  result <- applyScaleSet(scaleSet, data)
  expected <- data.frame(
    "FSC-A" = c(-4.60527, -2.094713, -0.3900353, -0.001999999, 0, 0.03998934, 0.09983408, 0.1986901),
    check.names = FALSE
  )
  expect_equal(expected, result, tolerance = 0.001)
})

test_that("applies clamped arcsinh scale", {
  data <- data.frame("FSC-A" = c(-250, -20, -2, -0.01, 0, 0.2, 0.5, 1), check.names = FALSE)
  # FSC-A: min = -200, max = 5000, cofactor = 5, type = LogScale
  scaleSet <- jsonlite::fromJSON('[{"scales":[{"channelName":"FSC-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":5000,"cofactor":5}}],"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","__v":1,"updated":"2020-10-27T18:38:55.554Z"}]') # nolint

  result <- applyScaleSet(scaleSet, data, clamp_q = TRUE)
  expected <- data.frame(
    "FSC-A" = c(-4.382183, -2.094713, -0.3900353, -0.001999999, 0, 0.03998934, 0.09983408, 0.1986901),
    check.names = FALSE
  )
  expect_equal(expected, result, tolerance = 0.001)
})

test_that("prints correct warning message", {
  data <- data.frame("FSC-A" = c(1, 1), "SSC-A" = c(2, 2), check.names = FALSE)
  # SSC-H: wrong channel name
  scaleSet <- jsonlite::fromJSON('[{"scales":[{"channelName":"SSC-H","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":5000,"cofactor":5}}],"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","__v":1,"updated":"2020-10-27T18:38:55.554Z"}]') # nolint

  expect_error(applyScaleSet(scaleSet, data), "FSC-A.*SSC-A")
})
