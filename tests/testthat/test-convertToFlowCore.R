context("toFlowCore")
library("mockthat")

test_that("rectangle gate is converted to flowCore", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"rectangle":{"y2":214399.74226804124,"x2":182870.51546391752,"y1":190978.03092783503,"x1":118010.39175257733},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"RectangleGate","name":"my gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}' # nolint
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

  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-A","type":"EllipseGate","name":"my gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":false,"id":"59289ff59989cc7704ada3c0"}' # nolint
  gate <- jsonlite::fromJSON(content)

  flowGate <- toFlowCore(gate)

  expect_equal(flowGate@filterId, "my gate")
  expect_equal(flowGate@mean[1], gate$model$ellipse$center[1], tolerance = 0.001, check.names = F)
  expect_equal(flowGate@mean[2], gate$model$ellipse$center[2], tolerance = 0.001, check.names = F)
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

  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[37836.07,971.51],[1588732.12,154.646],[8139.405,664.78],[9441.949,781.32]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"my gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}' # nolint
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
  content <- '{"__v":0,"experimentId":"5d2f8b4b21f0676fb3a6a70","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-H","type":"EllipseGate","name":"my gate","parentPopulationId":null,"yChannel":"SSC-H","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":false,"id":"59289ff59989cc7704ada3c0"}' # nolint
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
  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[37836.07,971.51],[1588732.12,154.646],[8139.405,664.78],[9441.949,781.32]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-H","type":"PolygonGate","name":"my gate","parentPopulationId":null,"yChannel":"SSC-H","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}' # nolint
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
  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"rectangle":{"y2":214399.74226804124,"x2":182870.51546391752,"y1":190978.03092783503,"x1":118010.39175257733},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-H","type":"RectangleGate","name":"my gate","parentPopulationId":null,"yChannel":"SSC-H","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}' # nolint
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
