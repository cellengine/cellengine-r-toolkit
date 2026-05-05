library("stats")

#' Convert a CellEngine object to flowCore
#'
#' Converts a CellEngine object to its flowCore analogue.
#'
#' Notes: Split gates and quadrant gates will be converted to lists of two
#' and four rectangle gates, respectively. Skewed quadrant gates from CellEngine
#' will be converted to orthogonal quadrant gates, as flowCore's quadGate class
#' does not support skewing.
#'
#' @param cellengineObject The CellEngine object to be converted.
#' @examples
#' \dontrun{
#' # Converting a compensation:
#' ceComp <- getCompensation(experimentId, byName("My comp"))
#' fcComp <- toFlowCore(ceComp)
#' }
#' @export
toFlowCore <- function(cellengineObject) {
  if (!requireNamespace("flowCore"))
    stop("This function requires the 'flowCore' package.")

  if ("scales" %in% names(cellengineObject)) {
    class_ <- "ScaleSet"
  } else if ("type" %in% names(cellengineObject)) {
    class_ <- cellengineObject$type
  } else if ("spillMatrix" %in% names(cellengineObject)) {
    class_ <- "Compensation"
  } else {
    stop("Unknown/unsupported CellEngine object")
  }

  switch(class_,
    "RectangleGate" = toFlowCoreRectangleGate(cellengineObject),
    "EllipseGate" = toFlowCoreEllipsoidGate(cellengineObject),
    "PolygonGate" = toFlowCorePolygonGate(cellengineObject),
    "QuadrantGate" = toFlowCoreQuadrantGate(cellengineObject),
    "SplitGate" = toFlowCoreSplitGate(cellengineObject),
    "RangeGate" = toFlowCoreRangeGate(cellengineObject),
    "ScaleSet" = scaleSetToTransformList(cellengineObject),
    "Compensation" = toFlowCoreCompensation(cellengineObject)
  )
}

toFlowCoreRectangleGate <- function(gate) {
  flowCore::rectangleGate(
    filterId = gate$name,
    stats::setNames(
      list(
        c(gate$model$rectangle$x1, gate$model$rectangle$x2),
        c(gate$model$rectangle$y1, gate$model$rectangle$y2)
      ),
      c(gate$xChannel, gate$yChannel)
    )
  )
}

toFlowCoreEllipsoidGate <- function(gate) {
  ellipse <- gate$model$ellipse
  x <- unlist(ellipse$center)[1]
  y <- unlist(ellipse$center)[2]
  points <- getEllipsePoints(ellipse$angle, 0.5 * ellipse$major, 0.5 * ellipse$minor, x, y)
  points <- t(data.frame(points))

  result <- fitEllipsePoints(points)

  cov <- result$covar
  colnames(cov) <- c(gate$xChannel, gate$yChannel)
  rownames(cov) <- c(gate$xChannel, gate$yChannel)

  return(flowCore::ellipsoidGate(filterId = gate$name, .gate = cov, mean = c(result$x, result$y)))
}

toFlowCorePolygonGate <- function(gate) {
  m <- gate$model$polygon$vertices
  if (is.null(dim(m))) {
    m <- m[[1]]
  }
  colnames(m) <- c(gate$xChannel, gate$yChannel)
  flowCore::polygonGate(filterId = gate$name, m)
}

toFlowCoreRangeGate <- function(gate) {
  flowCore::rectangleGate(
    filterId = gate$name,
    stats::setNames(
      list(c(gate$model$range$x1, gate$model$range$x2)),
      gate$xChannel
    )
  )
}

toFlowCoreSplitGate <- function(gate) {
  x <- gate$model$split$x
  sector_names <- gate$names
  left <- flowCore::rectangleGate(
    filterId = sector_names[1],
    stats::setNames(list(c(-Inf, x)), gate$xChannel)
  )
  right <- flowCore::rectangleGate(
    filterId = sector_names[2],
    stats::setNames(list(c(x, Inf)), gate$xChannel)
  )
  stats::setNames(list(left, right), sector_names)
}

toFlowCoreQuadrantGate <- function(gate) {
  if (isTRUE(gate$model$skewable)) {
    warning(
      "CellEngine QuadrantGate '", gate$gid,
      "' has skewable=TRUE. flowCore only supports orthogonal quadrant gates; ",
      "the gate will be converted using the center point only, ignoring skew."
    )
  }
  x <- gate$model$quadrant$x
  y <- gate$model$quadrant$y
  sector_names <- gate$names  # order: UR, UL, LL, LR
  make_sector <- function(filterId, xmin, xmax, ymin, ymax) {
    flowCore::rectangleGate(
      filterId = filterId,
      stats::setNames(
        list(c(xmin, xmax), c(ymin, ymax)),
        c(gate$xChannel, gate$yChannel)
      )
    )
  }
  gates <- list(
    make_sector(sector_names[1],  x,   Inf,  y,   Inf),  # UR
    make_sector(sector_names[2], -Inf,  x,   y,   Inf),  # UL
    make_sector(sector_names[3], -Inf,  x,  -Inf,  y),   # LL
    make_sector(sector_names[4],  x,   Inf, -Inf,  y)    # LR
  )
  stats::setNames(gates, sector_names)
}

#' Convert ScaleSet
#'
#' Convert a CellEngine ScaleSet to a flowCore transformList
#'
#' @param scaleSet The CellEngine scaleSet to be converted
#' @noRd
scaleSetToTransformList <- function(scaleSet) {
  scales <- scaleSet$scales[[1]]

  funs <- sapply(seq_len(nrow(scales)), function(i) {
    x <- scales$scale[i, ]
    switch(x$type,
      "LinearScale" = function(a) a,
      "LogScale" = function(a) log10(pmax(1, a)),
      "ArcSinhScale" = function(a) asinh(a / x$cofactor)
    )
  })

  flowCore::transformList(
    from = scales$channelName,
    tfun = funs,
    transformationId = scaleSet$name
  )
}

toFlowCoreCompensation <- function(comp) {
  flowCore::compensation(
    comp$spillMatrix,
    compensationId = comp$name
  )
}
