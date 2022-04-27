#' Create rectangle gate
#'
#' Creates a rectangle gate.
#'
#' @param experimentId The ID of the experiment to which to add the gate.
#' @param xChannel The name of the x channel to which the gate applies.
#' @param yChannel The name of the y channel to which the gate applies.
#' @param name The name of the gate.
#' @param x1 The first x coordinate (after the channel's scale has been applied).
#' @param x2 The second x coordinate (after the channel's scale has been applied).
#' @param y1 The first y coordinate (after the channel's scale has been applied).
#' @param y2 The second y coordinate (after the channel's scale has been applied).
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
#' createRectangleGate(experimentId, "FSC-A", "FSC-W", "my gate", 12.502, 95.102, 1020, 32021.2)
#' }
createRectangleGate <- function(experimentId, xChannel, yChannel, name,
                                x1, x2, y1, y2,
                                label = c(mean(c(x1, x2)), mean(c(y1, y2))),
                                gid = generateId(),
                                parentPopulationId = NULL, parentPopulation = NULL,
                                tailoredPerFile = FALSE, fcsFileId = NULL, fcsFile = NULL,
                                locked = FALSE, createPopulation = is.null(fcsFileId)) {
  body <- list(
    model = list(
      locked = jsonlite::unbox(locked),
      rectangle = list(
        x1 = jsonlite::unbox(x1),
        x2 = jsonlite::unbox(x2),
        y1 = jsonlite::unbox(y1),
        y2 = jsonlite::unbox(y2)
      ),
      label = label
    ),
    xChannel = jsonlite::unbox(xChannel),
    yChannel = jsonlite::unbox(yChannel),
    type = jsonlite::unbox("RectangleGate")
  )

  commonGateCreate(
    body, name, gid, experimentId, parentPopulationId, parentPopulation,
    tailoredPerFile, fcsFileId, fcsFile, createPopulation
  )
}
