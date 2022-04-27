#' Create ellipse gate
#'
#' Creates an ellipse gate.
#'
#' @param experimentId The ID of the experiment to which to add the gate.
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
#' @param parentPopulationId ID of the parent population. Use \code{NULL} for
#'   the "ungated" population. If specified, do not specify \code{parentPopulation}.
#' @param parentPopulation Name of the parent population. An attempt will be made
#'   to find the population by name. If zero or more than one population exists
#'   with the name, an error will be thrown. If specified, do not specify
#'   \code{parentPopulationId}.
#' @param tailoredPerFile Whether or not this gate is tailored per FCS file.
#' @param fcsFileId ID of FCS file, if tailored per file. Use \code{NULL} for
#'   the global gate in a tailored gate group. If specified, do not specify
#'   \code{fcsFile}.
#' @param fcsFile Name of FCS file, if tailored per file. An attempt will be made
#'   to find the file by name. If zero or more than one file exists with the name,
#'   an error will be thrown. If specified, do not specify \code{fcsFileId}.
#' @param locked Prevents modification of the gate via the web interface.
#' @param createPopulation Automatically create corresponding population.
#' @export
#' @examples
#' \dontrun{
#' createEllipseGate(experimentId, "FSC-A", "FSC-W", "my gate", c(1, 2, 3), c(4, 5, 6))
#' }
createEllipseGate <- function(experimentId, xChannel, yChannel, name,
                              x, y, angle, major, minor,
                              label = c(x, y),
                              gid = generateId(),
                              parentPopulationId = NULL, parentPopulation = NULL,
                              tailoredPerFile = FALSE, fcsFileId = NULL, fcsFile = NULL,
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
    body, name, gid, experimentId, parentPopulationId, parentPopulation,
    tailoredPerFile, fcsFileId, fcsFile, createPopulation
  )
}
