#' Create ellipse gate
#'
#' Creates an ellipse gate.
#'
#' @param experimentId The ID of the experiment to which to add the gate, or a
#'   \code{byName} expression.
#' @param xChannel The name of the x channel to which the gate applies.
#' @param yChannel The name of the y channel to which the gate applies.
#' @param name The name of the gate.
#' @param x The x centerpoint of the gate.
#' @param y The y centerpoint of the gate.
#' @param angle The angle of the ellipse in radians.
#' @param major The major radius of the ellipse.
#' @param minor The minor radius of the ellipse.
#' @param label Position of the label. Defaults to the midpoint of the gate.
#' @param gid Group ID of the gate, used for tailoring. If this is not specified,
#'   then a new Group ID will be created.
#' @param parentPopulationId ID of the parent population or a \code{byName}
#'   expression. Use \code{UNGATED} for the "ungated" population. Only used if
#'   \code{createPopulation} is \code{TRUE}.
#' @param tailoredPerFile Whether or not this gate is tailored per FCS file.
#' @param fcsFileId ID of FCS file or a \code{byName} expression, if tailored
#'   per file. Use \code{NULL} for the global gate in a tailored gate group.
#' @param locked Prevents modification of the gate via the web interface.
#' @param createPopulation Automatically create corresponding population.
#'   Specify \code{parentPopulationId} if set to \code{TRUE}.
#' @export
#' @examples
#' \dontrun{
#' createEllipseGate(experimentId, "FSC-A", "FSC-W", "my gate", c(1, 2, 3), c(4, 5, 6))
#' }
createEllipseGate <- function(experimentId, xChannel, yChannel, name,
                              x, y, angle, major, minor,
                              label = c(x, y),
                              gid = generateId(),
                              parentPopulationId = NULL,
                              tailoredPerFile = FALSE, fcsFileId = NULL,
                              locked = FALSE, createPopulation = is.null(fcsFileId)) {
  body <- list(
    model = list(
      locked = jsonlite::unbox(locked),
      ellipse = list(
        center = c(x, y),
        angle = jsonlite::unbox(angle),
        major = jsonlite::unbox(major),
        minor = jsonlite::unbox(minor)
      ),
      label = label
    ),
    xChannel = jsonlite::unbox(xChannel),
    yChannel = jsonlite::unbox(yChannel),
    type = jsonlite::unbox("EllipseGate")
  )

  commonGateCreate(
    body, gid, experimentId, parentPopulationId,
    tailoredPerFile, fcsFileId, createPopulation,
    name = name
  )
}
