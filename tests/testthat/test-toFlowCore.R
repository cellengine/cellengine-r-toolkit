context("toFlowCore")
library("mockthat")

test_that("rectangle gate is converted to flowCore", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"rectangle":{"y2":214399.74226804124,"x2":182870.51546391752,"y1":190978.03092783503,"x1":118010.39175257733},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"RectangleGate","name":"my gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false}' # nolint
  gate <- jsonlite::fromJSON(content)

  flowGate <- toFlowCore(gate)
  expect_equal(flowGate@filterId, "my gate")
  expect_equal(flowGate@parameters[[1]]@parameters, gate$xChannel)
  expect_equal(flowGate@parameters[[2]]@parameters, gate$yChannel)
  expect_true(flowGate@min[1] == gate$model$rectangle$x1)
  expect_true(flowGate@min[2] == gate$model$rectangle$y1)
  expect_true(flowGate@max[1] == gate$model$rectangle$x2)
  expect_true(flowGate@max[2] == gate$model$rectangle$y2)
})


test_that("ellipse gate is converted to flowCore", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-A","type":"EllipseGate","name":"my gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":false}' # nolint
  gate <- jsonlite::fromJSON(content)

  flowGate <- toFlowCore(gate)

  expect_equal(flowGate@filterId, "my gate")
  expect_equal(flowGate@mean[1], gate$model$ellipse$center[1], tolerance = 0.001, check.names = FALSE)
  expect_equal(flowGate@mean[2], gate$model$ellipse$center[2], tolerance = 0.001, check.names = FALSE)
  expect_equal(flowGate@parameters[[1]]@parameters, gate$xChannel)
  expect_equal(flowGate@parameters[[2]]@parameters, gate$yChannel)

  params <- covarToParameters(flowGate@cov)
  expect_equal(2 * params$major, gate$model$ellipse$major)
  expect_equal(2 * params$minor, gate$model$ellipse$minor)
  expect_equal(params$angle, gate$model$ellipse$angle)
})

test_that("polygon gate is converted to flowCore", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[37836.07,971.51],[1588732.12,154.646],[8139.405,664.78],[9441.949,781.32]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"my gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false}' # nolint
  gate <- jsonlite::fromJSON(content)

  flowGate <- toFlowCore(gate)
  expect_equal(flowGate@filterId, "my gate")
  expect_equal(flowGate@parameters[[1]]@parameters, gate$xChannel)
  expect_equal(flowGate@parameters[[2]]@parameters, gate$yChannel)
  expect_true(all(flowGate@boundaries == gate$model$polygon$vertices))
})


test_that("ellipse gate makes correct round-trip: CE -> flowCore -> CE", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  # given
  experimentId <- "5d2f8b4b21fd0676fb3a6a70"
  content <- '{"__v":0,"experimentId":"5d2f8b4b21f0676fb3a6a70","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-H","type":"EllipseGate","name":"my gate","parentPopulationId":null,"yChannel":"SSC-H","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":false}' # nolint
  gate <- jsonlite::fromJSON(content)
  mock <- mock(gate)

  with_mock(`cellengine::createEllipseGate` = mock, {
    # when
    flowGate <- toFlowCore(gate)
    newCEGate <- fromFlowCore(flowGate, experimentId, "hek 2")
  })

  # then
  m1 <- gate$model$ellipse
  m2 <- newCEGate$model$ellipse
  expect_equal(m1$angle, m2$angle)
  expect_equal(m1$major, m2$major)
  expect_equal(m1$minor, m2$minor)
  expect_equal(unlist(m1$center)[1], unlist(m2$center)[1])
  expect_equal(unlist(m1$center)[2], unlist(m2$center)[2])
})

test_that("polygon gate makes correct round-trip: CE -> flowCore -> CE", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  # given
  experimentId <- "5d2f8b4b21fd0676fb3a6a70"
  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[37836.07,971.51],[1588732.12,154.646],[8139.405,664.78],[9441.949,781.32]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-H","type":"PolygonGate","name":"my gate","parentPopulationId":null,"yChannel":"SSC-H","_id":"592640aa298f1480900e10e4","tailoredPerFile":false}' # nolint
  gate <- jsonlite::fromJSON(content)
  mock <- mock(gate)

  with_mock(`cellengine::createPolygonGate` = mock, {
    # when
    flowGate <- toFlowCore(gate)
    newCEGate <- fromFlowCore(flowGate, experimentId, "test polygon gate")
  })

  # then
  m1 <- gate$model$polygon
  m2 <- newCEGate$model$polygon # this is newGate$gate$model$population when hitting the real API
  expect_equal(m1, m2)
})

test_that("rectangle gate makes correct round-trip: CE -> flowCore -> CE", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  # given
  experimentId <- "5d2f8b4b21fd0676fb3a6a70"
  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"rectangle":{"y2":214399.74226804124,"x2":182870.51546391752,"y1":190978.03092783503,"x1":118010.39175257733},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-H","type":"RectangleGate","name":"my gate","parentPopulationId":null,"yChannel":"SSC-H","_id":"592640aa298f1480900e10e4","tailoredPerFile":false}' # nolint
  gate <- jsonlite::fromJSON(content)
  mock <- mock(gate)

  with_mock(`cellengine::createRectangleGate` = mock, {
    # when
    flowGate <- toFlowCore(gate)
    newCEGate <- fromFlowCore(flowGate, experimentId, "test rectangle gate")
  })

  # then
  m1 <- gate$model$rectangle
  m2 <- newCEGate$model$rectangle
  expect_equal(m1$x1, m2$x1)
  expect_equal(m1$x2, m2$x2)
  expect_equal(m1$y1, m2$y1)
  expect_equal(m1$y2, m2$y2)
})

test_that("range gate is converted to flowCore", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[56440.46,0.5],"range":{"x1":12502.34,"x2":95102.78,"y":0.5},"locked":false},"gid":"592640a5a6a1d6256ec9b08b","xChannel":"FSC-A","type":"RangeGate","name":"my range gate","parentPopulationId":null,"_id":"592640aa298f1480900e10e5","tailoredPerFile":false}' # nolint
  gate <- jsonlite::fromJSON(content)

  flowGate <- toFlowCore(gate)
  expect_equal(class(flowGate)[1], "rectangleGate")
  expect_equal(flowGate@filterId, "my range gate")
  expect_equal(length(flowGate@min), 1L)
  expect_equal(names(flowGate@min), gate$xChannel)
  expect_true(flowGate@min[[1]] == gate$model$range$x1)
  expect_true(flowGate@max[[1]] == gate$model$range$x2)
})

test_that("range gate makes correct round-trip: CE -> flowCore -> CE", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  # given
  experimentId <- "5d2f8b4b21fd0676fb3a6a70"
  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[56440.46,0.5],"range":{"x1":12502.34,"x2":95102.78,"y":0.5},"locked":false},"gid":"592640a5a6a1d6256ec9b08b","xChannel":"FSC-H","type":"RangeGate","name":"my range gate","parentPopulationId":null,"_id":"592640aa298f1480900e10e5","tailoredPerFile":false}' # nolint
  gate <- jsonlite::fromJSON(content)
  mock <- mock(gate)

  with_mock(`cellengine::createRangeGate` = mock, {
    # when
    flowGate <- toFlowCore(gate)
    newCEGate <- fromFlowCore(flowGate, experimentId, "my range gate")
  })

  # then
  m1 <- gate$model$range
  m2 <- newCEGate$model$range
  expect_equal(m1$x1, m2$x1)
  expect_equal(m1$x2, m2$x2)
})

test_that("split gate is converted to flowCore", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"labels":[[50000,0.95],[200000,0.95]],"split":{"x":144000,"y":0.5},"gids":["aaa111","bbb222"],"locked":false},"gid":"top123","xChannel":"FSC-A","type":"SplitGate","names":["my gate (L)","my gate (R)"],"parentPopulationId":null,"_id":"abc123","tailoredPerFile":false}' # nolint
  gate <- jsonlite::fromJSON(content)

  result <- toFlowCore(gate)

  expect_true(is.list(result))
  expect_equal(length(result), 2L)
  expect_equal(names(result), gate$names)

  leftGate <- result[[1]]
  rightGate <- result[[2]]

  expect_equal(class(leftGate)[1], "rectangleGate")
  expect_equal(class(rightGate)[1], "rectangleGate")

  expect_equal(leftGate@filterId, gate$names[1])
  expect_equal(rightGate@filterId, gate$names[2])

  expect_equal(names(leftGate@min), gate$xChannel)
  expect_equal(names(rightGate@min), gate$xChannel)

  expect_true(leftGate@min[[gate$xChannel]] == -Inf)
  expect_true(leftGate@max[[gate$xChannel]] == gate$model$split$x)
  expect_true(rightGate@min[[gate$xChannel]] == gate$model$split$x)
  expect_true(rightGate@max[[gate$xChannel]] == Inf)
})

test_that("quadrant gate is converted to flowCore", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"labels":[[200000,200000],[-200000,200000],[-200000,-200000],[200000,-200000]],"quadrant":{"x":144000,"y":96000,"angles":[1.5707963,3.1415927,4.7123890,0.0]},"gids":["g1","g2","g3","g4"],"locked":false,"skewable":false},"gid":"top456","xChannel":"FSC-A","yChannel":"SSC-A","type":"QuadrantGate","names":["my gate (UR)","my gate (UL)","my gate (LL)","my gate (LR)"],"parentPopulationId":null,"_id":"def456","tailoredPerFile":false}' # nolint
  gate <- jsonlite::fromJSON(content)

  result <- toFlowCore(gate)

  expect_true(is.list(result))
  expect_equal(length(result), 4L)
  expect_equal(names(result), gate$names)

  ur <- result[[1]]
  ul <- result[[2]]
  ll <- result[[3]]
  lr <- result[[4]]

  expect_equal(class(ur)[1], "rectangleGate")

  x <- gate$model$quadrant$x
  y <- gate$model$quadrant$y

  expect_equal(ur@filterId, gate$names[1])
  expect_equal(ul@filterId, gate$names[2])
  expect_equal(ll@filterId, gate$names[3])
  expect_equal(lr@filterId, gate$names[4])

  # UR: [x, Inf] x [y, Inf]
  expect_equal(ur@min[[gate$xChannel]], x)
  expect_equal(ur@max[[gate$xChannel]], Inf)
  expect_equal(ur@min[[gate$yChannel]], y)
  expect_equal(ur@max[[gate$yChannel]], Inf)

  # UL: [-Inf, x] x [y, Inf]
  expect_equal(ul@min[[gate$xChannel]], -Inf)
  expect_equal(ul@max[[gate$xChannel]], x)
  expect_equal(ul@min[[gate$yChannel]], y)
  expect_equal(ul@max[[gate$yChannel]], Inf)

  # LL: [-Inf, x] x [-Inf, y]
  expect_equal(ll@min[[gate$xChannel]], -Inf)
  expect_equal(ll@max[[gate$xChannel]], x)
  expect_equal(ll@min[[gate$yChannel]], -Inf)
  expect_equal(ll@max[[gate$yChannel]], y)

  # LR: [x, Inf] x [-Inf, y]
  expect_equal(lr@min[[gate$xChannel]], x)
  expect_equal(lr@max[[gate$xChannel]], Inf)
  expect_equal(lr@min[[gate$yChannel]], -Inf)
  expect_equal(lr@max[[gate$yChannel]], y)
})

test_that("quadrant gate with skewable=TRUE warns on conversion", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"labels":[[200000,200000],[-200000,200000],[-200000,-200000],[200000,-200000]],"quadrant":{"x":144000,"y":96000,"angles":[1.5707963,3.1415927,4.7123890,0.0]},"gids":["g1","g2","g3","g4"],"locked":false,"skewable":true},"gid":"top456","xChannel":"FSC-A","yChannel":"SSC-A","type":"QuadrantGate","names":["my gate (UR)","my gate (UL)","my gate (LL)","my gate (LR)"],"parentPopulationId":null,"_id":"def456","tailoredPerFile":false}' # nolint
  gate <- jsonlite::fromJSON(content)

  expect_warning(toFlowCore(gate), "skewable=TRUE")
})

test_that("toFlowCore converts a ScaleSet", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  scaleData <- '[{"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","scales":[{"channelName":"FSC-H","scale":{"type":"LogScale","minimum":1,"maximum":100000}},{"channelName":"SSC-H","scale":{"type":"LinearScale","minimum":1,"maximum":262144}},{"channelName":"SSC-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax488-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":300}},{"channelName":"PE-A","scale":{"type":"ArcSinhScale","minimum":-500,"maximum":262144,"cofactor":150}},{"channelName":"PE-TR-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PerCP-Cy55-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PE-Cy7-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax647-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax700-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax750-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacBlu-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot525-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacOrange-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot605-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot655-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot705-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Time","scale":{"type":"LinearScale","minimum":1,"maximum":262144}}],"__v":1,"updated":"2020-10-27T18:38:55.554Z"}]' # nolint
  ss <- jsonlite::fromJSON(scaleData)
  tl <- toFlowCore(ss)
  expect_equal(tl@transformationId, ss$name)
  # LogScale:
  expect_equal(tl@transforms$`FSC-H`@f(400), log10(400)) # nolint
  # LinearScale:
  expect_equal(tl@transforms$`SSC-H`@f(10), 10) # nolint
  # ArcSinhScale:
  expect_equal(tl@transforms$`Ax488-A`@f(10), asinh(10 / 300)) # nolint
  # ArcSinhScale (ensure cofactor captured correctly):
  expect_equal(tl@transforms$`PE-A`@f(10), asinh(10 / 150)) # nolint
})

test_that("toFlowCore converts a Compensation", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  ce = list(
    `_id` = "62fc374ebe488d038956aa86",
    channels = c("PE-A", "QDot605-A", "PE-Cy55-A", "APC-A", "Pacific Blue-A", "AmCyan-A"),
    spillMatrix = matrix(
      c(1, 0, 0, 0, 0, 0,
        0.1, 1, 0, 0, 0, 0,
        0, 0, 1, 0, 0, 0,
        0.2, 0, 0, 1, 0, 0,
        0, 0, 0, 0, 1, 0,
        0, 0, 0, 0, 0, 1
      ),
      nrow = 6,
      ncol = 6,
      byrow = TRUE,
      dimnames = list(
        c("PE-A", "QDot605-A", "PE-Cy55-A", "APC-A", "Pacific Blue-A", "AmCyan-A"),
        c("PE-A", "QDot605-A", "PE-Cy55-A", "APC-A", "Pacific Blue-A", "AmCyan-A")
      )
    ),
    experimentId = "6297cdd1eff31f50bbdc76c1",
    name = "Test comp"
  )
  fc <- toFlowCore(ce)
  expect_equal(fc@compensationId, "Test comp")
  expect_equal(fc@spillover, matrix(
    c(1, 0, 0, 0, 0, 0,
      0.1, 1, 0, 0, 0, 0,
      0, 0, 1, 0, 0, 0,
      0.2, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 1, 0,
      0, 0, 0, 0, 0, 1
    ),
    nrow = 6,
    ncol = 6,
    byrow = TRUE,
    dimnames = list(
      c("PE-A", "QDot605-A", "PE-Cy55-A", "APC-A", "Pacific Blue-A", "AmCyan-A"),
      c("PE-A", "QDot605-A", "PE-Cy55-A", "APC-A", "Pacific Blue-A", "AmCyan-A")
    )
  ))
})

