context("getEllipsePoints")

test_that("returns 8 points for a unit circle", {
  angle <- 0
  major <- 1
  minor <- 1
  x <- 0
  y <- 0

  points <- getEllipsePoints(angle, major, minor, x, y)
  expect_equal(8, length(points), tolerance = 0.001)
  expect_equal(2, length(points[[1]]), tolerance = 0.001)

  # point at theta = 0
  expect_equal(points[[1]][1], 1, tolerance = 0.001)
  expect_equal(points[[1]][2], 0, tolerance = 0.001)

  # point at theta = 3 * pi / 4
  expect_equal(points[[4]][1], (-sqrt(2) / 2), tolerance = 0.001)
  expect_equal(points[[4]][2], (sqrt(2) / 2), tolerance = 0.001)

  # point at theta = 3 * pi / 2
  expect_equal(points[[7]][1], 0, tolerance = 0.001)
  expect_equal(points[[7]][2], -1, tolerance = 0.001)
})


test_that("returns 8 points for an ellipse", {
  angle <- 0
  major <- 2
  minor <- 1
  x <- 0
  y <- 0

  points <- getEllipsePoints(angle, major, minor, x, y)
  expect_equal(8, length(points))
  expect_equal(2, length(points[[1]]))

  # point at theta = 0
  expect_equal(points[[1]][1], major * cos(0), tolerance = 0.001)
  expect_equal(points[[1]][2], minor * sin(0), tolerance = 0.001)

  # point at theta = 3 * pi / 4
  expect_equal(points[[4]][1], major * cos(3 * pi / 4), tolerance = 0.001)
  expect_equal(points[[4]][2], minor * sin(3 * pi / 4), tolerance = 0.001)

  # point at theta = 3 * pi / 2
  expect_equal(points[[7]][1], major * cos(3 * pi / 2), tolerance = 0.001)
  expect_equal(points[[7]][2], minor * sin(3 * pi / 2), tolerance = 0.001)
})

test_that("returns 8 points on an ellipse", {
  content <- '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-A","type":"EllipseGate","name":"my gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":false}' # nolint
  gate <- jsonlite::fromJSON(content)
  ell <- gate$model$ellipse

  points <- getEllipsePoints(ell$angle, ell$major, ell$minor, ell$center[1], ell$center[2])
  expect_equal(8, length(points))
  expect_equal(2, length(points[[1]]))

  # position of first ellipse point
  x1 <- points[[1]][1]
  y1 <- points[[1]][2]

  # position of ellipse center
  x2 <- ell$center[1]
  y2 <- ell$center[2]

  # distance formula
  dist <- sqrt((x2 - x1)^2 + (y2 - y1)^2)

  # distance between ellipse center and first point (theta = 0)
  # should be equivalent to major axis, regardless of angle
  expect_equal(dist, ell$major, tolerance = 0.001)
})
