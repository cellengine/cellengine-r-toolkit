#' Create polygon gate
#'
#' Creates a polygon gate.
#'
#' @param experimentId The ID of the experiment to which to add the gate.
#' @param xChannel The name of the x channel to which the gate applies.
#' @param yChannel The name of the y channel to which the gate applies.
#' @param name The name of the gate.
#' @param vertices List of vectors of coordinates, like
#' \code{list(c(x,y), c(x,y), ...)}.
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
#' createPolygonGate(experimentId, "FSC-A", "FSC-W", "my gate", list(c(1, 2), c(4, 5), c(7, 8)))
#' }
createPolygonGate <- function(experimentId, xChannel, yChannel, name,
                              vertices,
                              label = NULL,
                              gid = generateId(),
                              parentPopulationId = NULL, parentPopulation = NULL,
                              tailoredPerFile = FALSE, fcsFileId = NULL, fcsFile = NULL,
                              locked = FALSE, createPopulation = is.null(fcsFileId)) {
  if (is.null(label)) {
    label <- c(mean(sapply(vertices, "[[", 1)), mean(sapply(vertices, "[[", 2)))
    vertices <- do.call(rbind, vertices)
  }

  body <- list(
    model = list(
      locked = jsonlite::unbox(locked),
      polygon = list(
        vertices = vertices
      ),
      label = label
    ),
    xChannel = jsonlite::unbox(xChannel),
    yChannel = jsonlite::unbox(yChannel),
    type = jsonlite::unbox("PolygonGate")
  )

  commonGateCreate(
    body, name, gid, experimentId, parentPopulationId, parentPopulation,
    tailoredPerFile, fcsFileId, fcsFile, createPopulation
  )
}
