library("stats")

#' Convert a flowCore object to a CellEngine object
#'
#' Creates a CellEngine object from a flowCore object, saving it to CellEngine.
#'
#' @param flowObject A flowCore object.
#' @param experimentId The ID of the experiment in which to create the object.
#' @param name The name to use for the CellEngine object. If not set, it will
#' default to the identifier of the flowCore object.
#' @param ... Other parameters passed to \code{createRectangleGate}, \code{createPolygonGate}, etc.
#' @export
#' @examples
#' \dontrun{
#' rectGate <- rectangleGate(
#'   filterId = "Gate 1",
#'   "FL1-H" = c(0, 12), "FL2-H" = c(0, 12)
#' )
#' experimentId <- "5d2f8b4b21fd0676fb3a6a8c"
#' fromFlowCore(rectGate, experimentId, name = "my gate")
#' }
fromFlowCore <- function(flowObject, experimentId, name = NULL, ...) {
  if (is.null(name)) {
    name <- flowCore::identifier(flowObject)
  }

  switch(class(flowObject)[1],
    "ellipsoidGate" = {
      convertEllipsoidGate(flowObject, experimentId, name, ...)
    },
    "polygonGate" = {
      convertPolygonGate(flowObject, experimentId, name, ...)
    },
    "rectangleGate" = {
      convertRectangleGate(flowObject, experimentId, name, ...)
    }
  )
}

convertPolygonGate <- function(flowGate, experimentId, name = NULL, ...) {
  boundaries <- flowGate@boundaries
  createPolygonGate(
    experimentId,
    colnames(boundaries)[1],
    colnames(boundaries)[2],
    name,
    vertices = list(boundaries),
    ...
  )
}

convertEllipsoidGate <- function(flowGate, experimentId, name = NULL, ...) {
  params <- covarToParameters(flowGate@cov)

  createEllipseGate(
    experimentId,
    colnames(flowGate@cov)[1],
    colnames(flowGate@cov)[2],
    name,
    flowGate@mean[[1]],
    flowGate@mean[[2]],
    params$angle,
    2 * params$major,
    2 * params$minor,
    ...
  )
}

convertRectangleGate <- function(flowGate, experimentId, name = NULL, ...) {
  createRectangleGate(
    experimentId,
    names(flowGate@min)[1],
    names(flowGate@min)[2],
    name,
    flowGate@min[[1]],
    flowGate@max[[1]],
    flowGate@min[[2]],
    flowGate@max[[2]],
    ...
  )
}
