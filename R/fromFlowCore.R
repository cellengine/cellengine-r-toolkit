library("stats")

#' Convert a flowCore object to a CellEngine object
#'
#' Creates a CellEngine object from a flowCore object, saving it to CellEngine.
#'
#' @param flowCoreObj A flowCore object.
#' @param experimentId The ID of the experiment in which to create the object.
#' @param name The name to use for the CellEngine object. If not set, it will
#' default to the identifier of the flowCore object.
#' @param ... Other parameters passed to \code{createRectangleGate},
#' \code{createPolygonGate}, etc.
#' @export
#' @examples
#' \dontrun{
#' # Converting a flowCore rectangleGate:
#' rectGate <- rectangleGate(
#'   filterId = "Gate 1",
#'   "FL1-H" = c(0, 12), "FL2-H" = c(0, 12)
#' )
#' experimentId <- "5d2f8b4b21fd0676fb3a6a8c"
#' fromFlowCore(rectGate, experimentId, name = "my gate")
#' # "my gate" should now exist in the CellEngine experiment.
#' }
fromFlowCore <- function(flowCoreObj, experimentId, name = NULL, ...) {
  if (is.null(name)) {
    name <- flowCore::identifier(flowCoreObj)
  }

  switch(class(flowCoreObj)[1],
    "ellipsoidGate" = convertEllipsoidGate(flowCoreObj, experimentId, name, ...),
    "polygonGate" = convertPolygonGate(flowCoreObj, experimentId, name, ...),
    "rectangleGate" = convertRectangleGate(flowCoreObj, experimentId, name, ...),
    "compensation" = convertCompensation(flowCoreObj, experimentId, name),
    stop(paste0("Unsupported class ", class(flowCoreObj)[1]))
  )
}

convertPolygonGate <- function(fcGate, experimentId, name, ...) {
  boundaries <- fcGate@boundaries
  createPolygonGate(
    experimentId,
    colnames(boundaries)[1],
    colnames(boundaries)[2],
    name,
    vertices = list(boundaries),
    ...
  )
}

convertEllipsoidGate <- function(fcGate, experimentId, name, ...) {
  params <- covarToParameters(fcGate@cov)

  createEllipseGate(
    experimentId,
    colnames(fcGate@cov)[1],
    colnames(fcGate@cov)[2],
    name,
    fcGate@mean[[1]],
    fcGate@mean[[2]],
    params$angle,
    2 * params$major,
    2 * params$minor,
    ...
  )
}

convertRectangleGate <- function(fcGate, experimentId, name, ...) {
  createRectangleGate(
    experimentId,
    names(fcGate@min)[1],
    names(fcGate@min)[2],
    name,
    fcGate@min[[1]],
    fcGate@max[[1]],
    fcGate@min[[2]],
    fcGate@max[[2]],
    ...
  )
}

convertCompensation <- function(fcComp, experimentId, name) {
  createCompensation(
    experimentId,
    name = name,
    spillMatrix = fcComp@spillover
  )
}
