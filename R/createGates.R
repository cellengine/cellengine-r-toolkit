#' Create gates in bulk.
#'
#' Creates multiple gates in a single API call. This is faster than calling
#' \code{createXxxGate} in a loop.
#'
#' Each gate must be specified as a \code{list()} object conforming to our
#' \href{https://docs.cellengine.com/api/#resource-representation_4}{API documentation};
#' however, some properties will have defaults automatically set if not
#' specified (\code{model.locked}, \code{model.label}, \code{gid},
#' \code{parentPopulationId}, \code{tailoredPerFile}, \code{fcsFileId},
#' \code{experimentId}).
#'
#' Up to ~1,000 gates may be created at once.
#'
#' Unlike \code{createXxxGate}, this function cannot automatically create
#' populations corresponding to each gate. This function is primarily suited to
#' tailoring a large number of gates.
#'
#' @param experimentId The ID of the experiment to which to add the gate.
#' @param gates \code{list()} of gates.
#' @export
#' @examples
#' \dontrun{
#' g1 <- list(
#'   type = "RectangleGate", xChannel = "FSC-A", yChannel = "SSC-A",
#'   model = list(rectangle = list(x1 = 1, x2 = 100, y1 = 1, y2 = 100))
#' )
#' g2 <- list(
#'   type = "PolygonGate", xChannel = "FSC-A", yChannel = "SSC-A",
#'   model = list(polygon = list(vertices = c(c(1, 2), c(30, 40), c(50, 60))))
#' )
#' g3 <- list(
#'   type = "RangeGate", xChannel = "V450-480-A",
#'   model = list(range = list(x1 = 1, x2 = 100, y = 0.5))
#' )
#'
#' createGates(experimentId, c(g1, g2, g3))
#' }
createGates <- function(experimentId, gates) {
  # This function could be friendlier in terms of valdiating gates, but it is
  # an advanced function.

  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)

  body <- lapply(gates, function(g) {
    if (!("label" %in% names(g$model))) {
      switch(g$type,
        RectangleGate = {
          g$model$label <- c(
            mean(c(g$model$rectangle$x1, g$model$rectangle$x2)),
            mean(c(g$model$rectangle$y1, g$model$rectangle$y2))
          )
        },
        PolygonGate = {
          g$model$label <- c(
            mean(g$model$polygon$vertices[, 1]),
            mean(g$model$polygon$vertices[, 2])
          )
        },
        EllipseGate = {
          g$model$label <- c(g$model$center[1], g$model$center[2])
        },
        RangeGate = {
          g$model$label <- c(
            mean(g$model$range$x1, g$model$range$x2),
            g$model$range.y
          )
        }
      )
    }

    if (!("locked" %in% names(g["model"]))) g$model$locked <- FALSE

    if (!("gid" %in% names(g))) g["gid"] <- generateId()

    if (!("parentPopulationId" %in% names(g))) g["parentPopulationId"] <- list(NULL)

    if (!("tailoredPerFile" %in% names(g))) g["tailoredPerFile"] <- FALSE

    g
  })

  body <- jsonlite::toJSON(body, null = "null", auto_unbox = TRUE)
  basePost(paste("experiments", experimentId, "gates", sep = "/"), body)
}
