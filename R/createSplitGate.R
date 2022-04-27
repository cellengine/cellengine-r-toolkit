#' Create split gate
#'
#' Creates a split gate. split gates have two sectors (right and left),
#' each with a unique gid and name.
#'
#' @param experimentId The ID of the experiment to which to add the gate.
#' @param xChannel The name of the x channel to which the gate applies.
#' @param name The name of the gate. Each sector is named with this parameter
#' and a split flag (L, R).
#' @param x The x coordinate of the center point (after the channel's scale has been applied).
#' @param y The y position of the dashed line extending from the center point (as a fraction
#'  of the height).
#' @param labels Positions of the quadrant labels. A list of two length-2 vectors in
#' the order: L, R. These are set automatically to the top corners.
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
#' @param createPopulation Automatically create corresponding population.
#' @export
#' @examples
#' \dontrun{
#' createSplitGate(experimentId, "FSC-A", "my gate", 144000, 100000)
#' }
createSplitGate <- function(experimentId, xChannel, name, x, y = 0.5,
                            gid = generateId(), gids = replicate(2, generateId()), labels = NULL,
                            parentPopulationId = NULL, parentPopulation = NULL,
                            tailoredPerFile = FALSE, fcsFileId = NULL, fcsFile = NULL,
                            locked = FALSE, createPopulation = is.null(fcsFileId)) {



  # set labels based on axis scale
  if (length(labels) == 0) {
    scales <- data.frame(getScaleSets(experimentId)$scales)
    min <- scales[scales$channelName == xChannel, ]$scale$minimum
    max <- scales[scales$channelName == xChannel, ]$scale$maximum
    labels <- list(
      c(min + 0.1 * max, 0.95),
      c(max - 0.1 * max, 0.95)
    )
  } else if (all(dim(data.frame(labels)) == list(2, 2))) {
    labels <- labels
  } else {
    stop("Labels must be a list of 2 length-2 vectors.")
  }

  names <- paste(name, (c("(L)", "(R)")))

  body <- list(
    model = list(
      locked = jsonlite::unbox(locked),
      split = list(
        x = jsonlite::unbox(x),
        y = jsonlite::unbox(y)
      ),
      gids = gids,
      labels = labels
    ),
    xChannel = jsonlite::unbox(xChannel),
    names = names,
    type = jsonlite::unbox("SplitGate")
  )

  compoundGateCreate(
    body, names, gid, gids, experimentId, parentPopulationId, parentPopulation,
    tailoredPerFile, fcsFileId, fcsFile, createPopulation
  )
}
