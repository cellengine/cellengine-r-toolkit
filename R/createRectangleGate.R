#' Create rectangle gate
#'
#' Creates a rectangle gate.
#'
#' @param experimentId The ID of the experiment to which to add the gate, or a
#'   \code{byName} expression.
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
#' createRectangleGate(experimentId, "FSC-A", "FSC-W", "my gate", 12.502, 95.102, 1020, 32021.2)
#' }
createRectangleGate <- function(experimentId, xChannel, yChannel, name,
                                x1, x2, y1, y2,
                                label = c(mean(c(x1, x2)), mean(c(y1, y2))),
                                gid = generateId(),
                                parentPopulationId = NULL,
                                tailoredPerFile = FALSE, fcsFileId = NULL,
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
    body, gid, experimentId, parentPopulationId,
    tailoredPerFile, fcsFileId, createPopulation,
    name = name
  )
}
