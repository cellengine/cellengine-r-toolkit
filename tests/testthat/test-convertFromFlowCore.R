context("fromFlowCore")
library("mockthat")


test_that("fromFlowCore accepts `gid` as a splat keyword arg and passes it to the next function", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  mock <- mock("good")
  with_mock(`cellengine::convertRectangleGate` = mock, {

    # given: flowCore rectangleGate gate
    flowObject <- rectangleGate(
      filterId = "name",
      "a" = c(0, 0),
      "b" = c(0, 0)
    )

    # when:
    res <- fromFlowCore(flowObject, "some-experiment-id", "converted gate", gid = "parent-pop-id")

    # then: convertRectangleGate should be called with `gid`
    args <- mock_args(mock)
    expect_equal(args$gid, "parent-pop-id")
  })
})

test_that("fromFlowCore overloads for rectangle gate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  with_mock(`cellengine::convertRectangleGate` = mock("good"), {
    # given: flowCore rectangleGate gate
    flowObject <- rectangleGate(filterId = "myRectGate", "FSC-H" = c(200, 600), "SSC-H" = c(0, 400))

    # when:
    res <- fromFlowCore(flowObject, "some-experiment-id", "converted gate")

    # then: convertRectangleGate should be called
    expect_equal(res, "good")
  })
})

test_that("fromFlowCore overloads for polygon gate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  with_mock(`cellengine::convertPolygonGate` = mock("good"), {
    # given: flowCore gate
    sqrcut <- matrix(c(300, 300, 600, 600, 50, 300, 300, 50), ncol = 2, nrow = 4)
    colnames(sqrcut) <- c("FSC-H", "SSC-H")
    pg <- polygonGate(filterId = "nonDebris", .gate = sqrcut)

    # when:
    res <- fromFlowCore(pg, "some-experiment-id", "converted gate", gid = "parent-pop")

    # then: convertPolygonGate should be called
    expect_equal(res, "good")
  })
})

test_that("fromFlowCore overloads for ellipsoid gate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  with_mock(`cellengine::convertEllipsoidGate` = mock("good"), {
    # given: flowCore ellipsoidGate
    cov <- matrix(c(6879, 3612, 3612, 5215),
      ncol = 2,
      dimnames = list(c("FSC-H", "SSC-H"), c("FSC-H", "SSC-H"))
    )
    mean <- c("FSC-H" = 430, "SSC-H" = 175)
    flowObject <- ellipsoidGate(filterId = "myEllipsoidGate", .gate = cov, mean = mean)

    # when
    res <- fromFlowCore(flowObject, "some-experiment-id", "converted gate")

    # then: convertRectangleGate should be called
    expect_equal(res, "good")
  })
})

test_that("fromFlowCore overloads for flowDensity gate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  skip_if_not_installed("flowDensity")
  library("flowDensity")

  with_mock(`cellengine::convertFlowDensity` = mock("good"), {
    # given: flowDensity object
    experimentId <- "5d2f8b4b21fd0676fb3a6a70"
    f <- read.FCS("../5k.fcs", transformation = "linearize")
    flow <- flowDensity(f,
      channels = c("FSC-A", "FSC-W"), position = c(FALSE, FALSE),
      percentile = c(.99999, .99999), use.percentile = c(TRUE, TRUE),
      ellip.gate = TRUE, scale = .99
    )

    # when:
    res <- fromFlowCore(flow, experimentId, "converted gate")

    # then: convertFlowDensity should be called
    expect_equal(res, "good")
  })
})

test_that("rectangleGate is correctly converted to CE RectangleGate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  # given: the correct response from createRectangleGateGate
  content <- '{"__v":0,"experimentId":"5d2f8b4b21fd0676fb3a6a70","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[300, 50], [300, 300], [600, 300], [600, 50]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"converted gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}' # nolint
  mock <- mock(jsonlite::fromJSON(content))

  with_mock(`cellengine::createRectangleGate` = mock, {
    # given: flowCore rectangleGate gate
    rg <- rectangleGate(filterId = "myRectGate", "FSC-H" = c(200, 600), "SSC-H" = c(0, 400))

    # when:
    experimentId <- "5d2f8b4b21fd0676fb3a6a70"
    res <- fromFlowCore(rg, experimentId, "converted gate")
  })

  # then: createRectangleGate called with correct arguments
  expect_equal(experimentId, mock_arg(mock, "experimentId"))
  expect_equal("converted gate", mock_arg(mock, "name"))
  expect_equal(names(rg@min)[1], mock_arg(mock, "xChannel"))
  expect_equal(names(rg@min)[2], mock_arg(mock, "yChannel"))
  expect_equal(200, mock_arg(mock, "x1"))
  expect_equal(600, mock_arg(mock, "x2"))
  expect_equal(0, mock_arg(mock, "y1"))
  expect_equal(400, mock_arg(mock, "y2"))
})

test_that("polygonGate is correctly converted to CE PolygonGate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  # given: the correct response from createPolygonGate
  content <- '{"__v":0,"experimentId":"5d2f8b4b21fd0676fb3a6a70","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[300, 50], [300, 300], [600, 300], [600, 50]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"converted gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}' # nolint
  mock <- mock(jsonlite::fromJSON(content))

  with_mock(`cellengine::createPolygonGate` = mock, {
    # given: polygonGate gate
    sqrcut <- matrix(c(300, 300, 600, 600, 50, 300, 300, 50), ncol = 2, nrow = 4)
    colnames(sqrcut) <- c("FSC-H", "SSC-H")
    pg <- polygonGate(filterId = "nonDebris", .gate = sqrcut)

    # when:
    experimentId <- "5d2f8b4b21fd0676fb3a6a70"
    polygonGate <- fromFlowCore(pg, experimentId, "converted gate", gid = "parent-pop")
  })

  # then: createPolygonGate called with correct arguments
  expect_equal(experimentId, mock_arg(mock, "experimentId"))
  expect_equal(colnames(pg@boundaries)[1], mock_arg(mock, "xChannel"))
  expect_equal(colnames(pg@boundaries)[2], mock_arg(mock, "yChannel"))
  expect_equal("converted gate", mock_arg(mock, "name"))
  expect_equal(sqrcut, mock_arg(mock, "vertices")[[1]])
})

test_that("ellipsoidGate is correctly converted to CE EllipseGate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  # given: the correct response from createEllipseGate
  content <- '{"__v":0,"experimentId":"5d2f8b4b21fd0676fb3a6a70","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-H","type":"EllipseGate","name":"my gate","parentPopulationId":null,"yChannel":"SSC-H","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":false,"id":"59289ff59989cc7704ada3c0"}' # nolint
  mock <- mock(jsonlite::fromJSON(content))

  with_mock(`cellengine::createEllipseGate` = mock, {
    # given also: flowCore ellipsoidGate
    experimentId <- "5d2f8b4b21fd0676fb3a6a70"
    cov <- matrix(c(6879, 3612, 3612, 5215),
      ncol = 2,
      dimnames = list(c("FSC-H", "SSC-H"), c("FSC-H", "SSC-H"))
    )
    mean <- c("FSC-H" = 430, "SSC-H" = 175)
    flowObject <- ellipsoidGate(filterId = "myEllipsoidGate", .gate = cov, mean = mean)

    # when:
    ellipseGate <- fromFlowCore(flowObject, experimentId, "converted gate")
  })

  # then
  expect_equal(experimentId, mock_arg(mock, "experimentId"))
  expect_equal(colnames(flowObject@cov)[1], mock_arg(mock, "xChannel"))
  expect_equal(colnames(flowObject@cov)[2], mock_arg(mock, "yChannel"))
  expect_equal("converted gate", mock_arg(mock, "name"))
  expect_equal(430, mock_arg(mock, "x"))
  expect_equal(175, mock_arg(mock, "y"))
  expect_equal(0.672201, mock_arg(mock, "angle"), tolerance = 0.001)
  expect_equal(197.5205, mock_arg(mock, "major"), tolerance = 0.001)
  expect_equal(96.75568, mock_arg(mock, "minor"), tolerance = 0.001)
})

test_that("flowDensity is converted to polygonGate", {
  skip_if_not_installed("flowCore")
  library("flowCore")
  skip_if_not_installed("flowDensity")
  library("flowDensity")

  # given: the correct response from createPolygonGate
  content <- '{"__v":0,"experimentId":"5d2f8b4b21fd0676fb3a6a70","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[222400,94720],[216128,85568],[201344,77696],[194752,75264],[19584,67904],[19072,68864],[20800,75648],[25920,93312],[37184,101120],[51648,104576],[82816,109824],[95040,111488],[107712,112512],[144384,114304],[172224,114240],[176640,113920],[190336,112512],[197632,111040],[211072,104448],[222400,94720]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"converted gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}' # nolint
  mock <- mock(jsonlite::fromJSON(content))

  with_mock(`cellengine::createPolygonGate` = mock, {
    # given: a flowDensity object
    experimentId <- "5d2f8b4b21fd0676fb3a6a70"
    f <- read.FCS("../5k.fcs", transformation = "linearize")
    flow <- flowDensity(f,
      channels = c("FSC-A", "FSC-W"), position = c(FALSE, FALSE),
      percentile = c(.99999, .99999), use.percentile = c(TRUE, TRUE),
      ellip.gate = TRUE, scale = .99
    )

    # when
    gate <- fromFlowCore(flow, experimentId, "converted gate")
  })

  # then: createPolygonGate called with correct arguments
  expect_equal(experimentId, mock_arg(mock, "experimentId"))
  expect_equal(gate$name, "converted gate") # this is returned as [gate: {}, population: {}] by CE
  expect_equal("converted gate", mock_arg(mock, "name"))
  expect_equal(flow@filter, mock_arg(mock, "vertices")[[1]])
  expect_true(all((flow@filter == gate$model$polygon$vertices)))
})
