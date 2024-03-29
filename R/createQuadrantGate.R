#' Create quadrant gate
#'
#' Creates a quadrant gate. Quadrant gates have four sectors (upper-right,
#' upper-left, lower-left, lower-right), each with a name and unique gid.
#'
#' @param experimentId The ID of the experiment to which to add the gate, or a
#'   \code{byName} expression.
#' @param xChannel The name of the x channel to which the gate applies.
#' @param yChannel The name of the y channel to which the gate applies.
#' @param name The name of the gate. Each sector is named with this parameter
#' and a quadrant flag (UR, UL, LL, LR).
#' @param x The x coordinate (after the channel's scale has been applied).
#' @param y The y coordinate (after the channel's scale has been applied).
#' @param labels Positions of the quadrant labels. A list of four length-2 vectors in
#' the order: UR, UL, LL, LR. These are set automatically to the plot corners.
#' @param gid Top-level group ID of the gate, used for tailoring. If this is not specified,
#'   then a new Group ID will be created. For compound gates, "gid" refers to the
#'   top-level GID. Each sector has a unique model gid and name to which
#'   populations must refer.
#' @param gids Group IDs of each sector, assigned to model.gids.
#' @param parentPopulationId ID of the parent population or a \code{byName}
#'   expression. Use \code{UNGATED} for the "ungated" population. Only used if
#'   \code{createPopulation} is \code{TRUE}.
#' @param tailoredPerFile Whether or not this gate is tailored per FCS file.
#' @param fcsFileId ID of FCS file or a \code{byName} expression, if tailored
#'   per file. Use \code{NULL} for the global gate in a tailored gate group.
#' @param locked Prevents modification of the gate via the web interface.
#' @param createPopulations Automatically create corresponding populations.
#'   Specify \code{parentPopulationId} if set to \code{TRUE}. Because R performs
#'   partial matching of argument names, this can also be specified as
#'   \code{createPopulation} for consistency with the other \code{create__Gate}
#'   functions.
#' @export
#' @examples
#' \dontrun{
#' createQuadrantGate(experimentId, "FSC-A", "FSC-W", "my gate", 160000, 200000)
#' }
createQuadrantGate <- function(experimentId, xChannel, yChannel, name,
                               x, y, labels = NULL,
                               gid = generateId(), gids = replicate(4, generateId()),
                               parentPopulationId = NULL,
                               tailoredPerFile = FALSE, fcsFileId = NULL,
                               locked = FALSE, createPopulations = is.null(fcsFileId)) {
  # future args:
  skewable <- FALSE
  angles <- c(pi / 2, pi, 3 / 2 * pi, 0.000000)
  # @param angles Angles at which the quadrant lines appear, in radians.
  # Zero (0) points horizontally to the right; angles proceed counter-clockwise.
  # Currently these must be 0, pi / 2, pi and 3 * pi / 2.
  # TODO this is supported now

  if (length(labels) == 0) {
    # TODO update to huge label coordinates
    scales <- data.frame(getScaleSets(experimentId)$scales)
    labels <- list(
      c(
        scales[scales$channelName == xChannel, ]$scale$maximum, # upper right
        scales[scales$channelName == yChannel, ]$scale$maximum
      ),
      c(
        scales[scales$channelName == xChannel, ]$scale$minimum, # upper left
        scales[scales$channelName == yChannel, ]$scale$maximum
      ),
      c(
        scales[scales$channelName == xChannel, ]$scale$minimum, # lower left
        scales[scales$channelName == yChannel, ]$scale$minimum
      ),
      c(
        scales[scales$channelName == xChannel, ]$scale$maximum, # lower right
        scales[scales$channelName == yChannel, ]$scale$minimum
      )
    )
  } else if (all(lapply(labels,length) == c(2,2,2,2))) {
    # okay
  } else {
    stop("Labels must be a list of 4 length-2 vectors.")
  }

  names <- paste(name, c("(UR)", "(UL)", "(LL)", "(LR)"))

  body <- list(
    model = list(
      locked = jsonlite::unbox(locked),
      quadrant = list(
        x = jsonlite::unbox(x),
        y = jsonlite::unbox(y),
        angles = angles
      ),
      gids = gids,
      labels = labels,
      skewable = jsonlite::unbox(skewable) # future: as arg in function call
    ),
    xChannel = jsonlite::unbox(xChannel),
    yChannel = jsonlite::unbox(yChannel),
    type = jsonlite::unbox("QuadrantGate")
  )

  commonGateCreate(
    body, gid, experimentId, parentPopulationId,
    tailoredPerFile, fcsFileId, createPopulations,
    names = names
  )
}
