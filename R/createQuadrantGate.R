#' Create quadrant gate
#'
#' Creates a quadrant gate. Quadrant gates have four sectors (upper-right,
#' upper-left, lower-left, lower-right), each with a unique gid and name.
#'
#' @param experimentId The ID of the experiment to which to add the gate.
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
#' @param parentPopulationId ID of the parent population. Use \code{NULL} for
#'   the "ungated" population. If specified, do not specify \code{parentPopulation}.
#' @param parentPopulation Name of the parent population. An attempt will be made
#'   to find the population by name. If zero or more than one population exists
#'   with the name, an error will be thrown. If specified, do not specify
#'   \code{parentPopulationId}.
#' @param tailoredPerFile Whether this gate is tailored per FCS file.
#' @param fcsFileId ID of FCS file, if tailored per file. Use \code{NULL} for
#'   the global gate in a tailored gate group. If specified, do not specify
#'   \code{fcsFile}.
#' @param fcsFile Name of FCS file, if tailored per file. An attempt will be made
#'   to find the file by name. If zero or more than one file exists with the name,
#'   an error will be thrown. If specified, do not specify \code{fcsFileId}.
#' @param locked Prevents modification of the gate via the web interface.
#' @param createPopulations Automatically create corresponding population.
#' @export
#' @examples
#' \dontrun{
#' createQuadrantGate(experimentId, "FSC-A", "FSC-W", "my gate", 160000, 200000)
#' }
createQuadrantGate <- function(experimentId, xChannel, yChannel, name,
                               x, y, labels = NULL,
                               gid = generateId(), gids = replicate(4, generateId()),
                               parentPopulationId = NULL, parentPopulation = NULL,
                               tailoredPerFile = FALSE, fcsFileId = NULL, fcsFile = NULL,
                               locked = FALSE, createPopulations = is.null(fcsFileId)) {
  # future args:
  skewable <- FALSE
  angles <- c(pi / 2, pi, 3 / 2 * pi, 0.000000)
  # @param angles Angles at which the quadrant lines appear, in radians.
  # Zero (0) points horizontally to the right; angles proceed counter-clockwise.
  # Currently these must be 0, pi / 2, pi and 3 * pi / 2.

  if (length(labels) == 0) {
    scales <- data.frame(getScaleSets(experimentId)$scales)
    labels <- list(
      c(
        scales[scales$channelName == xChannel, ]$scale$maximum, # upper right
        scales[scales$channelName == yChannel, ]$scale$maximum
      ), # upper right
      c(
        scales[scales$channelName == xChannel, ]$scale$minimum, # upper left
        scales[scales$channelName == yChannel, ]$scale$maximum
      ), # upper left
      c(
        scales[scales$channelName == xChannel, ]$scale$minimum, # lower left
        scales[scales$channelName == yChannel, ]$scale$minimum
      ), # lower left
      c(
        scales[scales$channelName == xChannel, ]$scale$maximum, # lower right
        scales[scales$channelName == yChannel, ]$scale$minimum
      ) # lower right
    )
  } else if (all(dim(data.frame(labels)) == list(2, 4))) {
    labels <- labels
  } else {
    stop("Labels must be a list of 4 length-2 vectors.")
  }

  names <- paste(name, (c("(UR)", "(UL)", "(LL)", "(LR)")))

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
    names = names,
    type = jsonlite::unbox("QuadrantGate")
  )

  compoundGateCreate(
    body, names, gid, gids, experimentId, parentPopulationId, parentPopulation,
    tailoredPerFile, fcsFileId, fcsFile, createPopulations
  )
}
