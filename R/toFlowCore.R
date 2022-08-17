library("stats")

#' Convert a CellEngine object to flowCore
#'
#' Converts a CellEngine object to its flowCore analogue.
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
    "QuadrantGate" = stop("This gate representation is not yet implemented"),
    "SplitGate" = stop("This gate representation is not yet implemented"),
    "RangeGate" = stop("This gate representation is not yet implemented"),
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
