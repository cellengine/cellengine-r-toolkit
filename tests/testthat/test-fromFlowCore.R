context("fromFlowCore")


test_that("fromFlowCore accepts `gid` as a splat keyword arg and passes it to the next function", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  captured <- NULL
  local_mocked_bindings(
    convertRectangleGate = function(fcGate, experimentId, name, ...) {
      captured <<- list(...)
      "good"
    },
    .package = "cellengine"
  )

  # given: flowCore rectangleGate gate
  flowObject <- rectangleGate(
    filterId = "name",
    "a" = c(0, 0),
    "b" = c(0, 0)
  )

  # when:
  res <- fromFlowCore(flowObject, "some-experiment-id", "converted gate", gid = "parent-pop-id")

  # then: convertRectangleGate should be called with `gid`
  expect_equal(captured$gid, "parent-pop-id")
})

test_that("fromFlowCore overloads for rectangle gate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  local_mocked_bindings(
    convertRectangleGate = function(...) "good",
    .package = "cellengine"
  )
  # given: flowCore rectangleGate gate
  flowObject <- rectangleGate(filterId = "myRectGate", "FSC-H" = c(200, 600), "SSC-H" = c(0, 400))

  # when:
  res <- fromFlowCore(flowObject, "some-experiment-id", "converted gate")

  # then: convertRectangleGate should be called
  expect_equal(res, "good")
})

test_that("fromFlowCore routes 1D rectangleGate to createRangeGate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  local_mocked_bindings(
    createRangeGate = function(...) "good",
    .package = "cellengine"
  )
  # given: a 1D flowCore rectangleGate
  flowObject <- rectangleGate(filterId = "myRangeGate", "FSC-H" = c(200, 600))

  # when:
  res <- fromFlowCore(flowObject, "some-experiment-id", "my range gate")

  # then: createRangeGate should be called
  expect_equal(res, "good")
})

test_that("fromFlowCore overloads for polygon gate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  local_mocked_bindings(
    convertPolygonGate = function(...) "good",
    .package = "cellengine"
  )
  # given: flowCore gate
  sqrcut <- matrix(c(300, 300, 600, 600, 50, 300, 300, 50), ncol = 2, nrow = 4)
  colnames(sqrcut) <- c("FSC-H", "SSC-H")
  pg <- polygonGate(filterId = "nonDebris", .gate = sqrcut)

  # when:
  res <- fromFlowCore(pg, "some-experiment-id", "converted gate", gid = "parent-pop")

  # then: convertPolygonGate should be called
  expect_equal(res, "good")
})

test_that("fromFlowCore overloads for ellipsoid gate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  local_mocked_bindings(
    convertEllipsoidGate = function(...) "good",
    .package = "cellengine"
  )
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

test_that("rectangleGate is correctly converted to CE RectangleGate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  # given: the correct response from createRectangleGateGate
  content <- '{"__v":0,"experimentId":"5d2f8b4b21fd0676fb3a6a70","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[300, 50], [300, 300], [600, 300], [600, 50]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"converted gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false}' # nolint
  captured <- NULL
  local_mocked_bindings(
    createRectangleGate = function(experimentId, xChannel, yChannel, name,
                                   x1, x2, y1, y2, ...) {
      captured <<- list(
        experimentId = experimentId, xChannel = xChannel, yChannel = yChannel,
        name = name, x1 = x1, x2 = x2, y1 = y1, y2 = y2
      )
      jsonlite::fromJSON(content)
    },
    .package = "cellengine"
  )
  # given: flowCore rectangleGate gate
  rg <- rectangleGate(filterId = "myRectGate", "FSC-H" = c(200, 600), "SSC-H" = c(0, 400))

  # when:
  experimentId <- "5d2f8b4b21fd0676fb3a6a70"
  res <- fromFlowCore(rg, experimentId, "converted gate")

  # then: createRectangleGate called with correct arguments
  expect_equal(experimentId, captured$experimentId)
  expect_equal("converted gate", captured$name)
  expect_equal(names(rg@min)[1], captured$xChannel)
  expect_equal(names(rg@min)[2], captured$yChannel)
  expect_equal(200, captured$x1)
  expect_equal(600, captured$x2)
  expect_equal(0, captured$y1)
  expect_equal(400, captured$y2)
})

test_that("polygonGate is correctly converted to CE PolygonGate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  # given: the correct response from createPolygonGate
  content <- '{"__v":0,"experimentId":"5d2f8b4b21fd0676fb3a6a70","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[300, 50], [300, 300], [600, 300], [600, 50]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"converted gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false}' # nolint
  captured <- NULL
  local_mocked_bindings(
    createPolygonGate = function(experimentId, xChannel, yChannel, name,
                                 vertices, ...) {
      captured <<- list(
        experimentId = experimentId, xChannel = xChannel, yChannel = yChannel,
        name = name, vertices = vertices
      )
      jsonlite::fromJSON(content)
    },
    .package = "cellengine"
  )
  # given: polygonGate gate
  sqrcut <- matrix(c(300, 300, 600, 600, 50, 300, 300, 50), ncol = 2, nrow = 4)
  colnames(sqrcut) <- c("FSC-H", "SSC-H")
  pg <- polygonGate(filterId = "nonDebris", .gate = sqrcut)

  # when:
  experimentId <- "5d2f8b4b21fd0676fb3a6a70"
  polygonGate <- fromFlowCore(pg, experimentId, "converted gate", gid = "parent-pop")

  # then: createPolygonGate called with correct arguments
  expect_equal(experimentId, captured$experimentId)
  expect_equal(colnames(pg@boundaries)[1], captured$xChannel)
  expect_equal(colnames(pg@boundaries)[2], captured$yChannel)
  expect_equal("converted gate", captured$name)
  expect_equal(sqrcut, captured$vertices[[1]])
})

test_that("ellipsoidGate is correctly converted to CE EllipseGate", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  # given: the correct response from createEllipseGate
  content <- '{"__v":0,"experimentId":"5d2f8b4b21fd0676fb3a6a70","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-H","type":"EllipseGate","name":"my gate","parentPopulationId":null,"yChannel":"SSC-H","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":false}' # nolint
  captured <- NULL
  local_mocked_bindings(
    createEllipseGate = function(experimentId, xChannel, yChannel, name,
                                 x, y, angle, major, minor, ...) {
      captured <<- list(
        experimentId = experimentId, xChannel = xChannel, yChannel = yChannel,
        name = name, x = x, y = y, angle = angle, major = major, minor = minor
      )
      jsonlite::fromJSON(content)
    },
    .package = "cellengine"
  )
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

  # then
  expect_equal(experimentId, captured$experimentId)
  expect_equal(colnames(flowObject@cov)[1], captured$xChannel)
  expect_equal(colnames(flowObject@cov)[2], captured$yChannel)
  expect_equal("converted gate", captured$name)
  expect_equal(430, captured$x)
  expect_equal(175, captured$y)
  expect_equal(0.672201, captured$angle, tolerance = 0.001)
  expect_equal(197.5205, captured$major, tolerance = 0.001)
  expect_equal(96.75568, captured$minor, tolerance = 0.001)
})

test_that("compensation is correctly converted to CE Compensation", {
  skip_if_not_installed("flowCore")
  library("flowCore")

  local_mocked_bindings(
    request_perform = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://cellengine.com/api/v1/experiments/591a3b441d725115208a6fda/compensations")
      body <- rawToChar(req$options$postfields)
      expect_equal(body, '{"name":"defaultCompensation","channels":["Ax488-A","PE-A"],"spillMatrix":[1,0.1,0,1]}')
      response <- httptest::fake_response(
        req$url,
        req$method,
        content = '{"_id": "62a41fb7b72926ab549680db", "channels": ["Ax488-A","PE-A"],"spillMatrix": [1,0.1, 0,1], "experimentId": "591a3b441d725115208a6fdb", "name": "defaultCompensation"}', # nolint
        status_code = 201,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    .package = "httr"
  )
  setServer("https://cellengine.com")
  experimentId <- "591a3b441d725115208a6fda"
  fc <- flowCore::compensation(
    matrix(
      c(1, 0.1, 0, 1),
      nrow=2,
      ncol=2,
      byrow=TRUE,
      dimnames=list(
        c("Ax488-A", "PE-A"),
        c("Ax488-A", "PE-A")
      )
    )
  )
  ce <- fromFlowCore(fc, experimentId)
  expect_equal(ce$experimentId, "591a3b441d725115208a6fdb")
  expect_equal(ce$name, "defaultCompensation")
  expect_equal(ce$`_id`, "62a41fb7b72926ab549680db")
  expect_equal(ce$spillMatrix, matrix(
    c(1, 0.1, 0, 1),
    nrow=2,
    ncol=2,
    byrow=TRUE,
    dimnames=list(
      c("Ax488-A", "PE-A"),
      c("Ax488-A", "PE-A")
    )
  ))
})
