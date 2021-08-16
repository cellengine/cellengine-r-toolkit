#' Delete gate
#'
#' Deletes a gate or a tailored gate family
#'
#' This is an advanced function, primarily used for deleting tailored gates. It
#' does not delete populations. Use \code{deletePopulationAuto} to delete gates
#' and their associated populations.
#'
#' Works for compound gates if you specify the top-level gid. Specifying the gid
#' of a sector (i.e. one listed in model.gids) will result in no gates being
#' deleted. If gateId is specified, only that gate will be deleted, regardless
#' of the other parameters specified.
#'
#' Currently, \code{byName} does not work for the gateId in this function (it does work
#' for the experimentId).
#' @param experimentId ID of experiment.
#' @param gid ID of gate family.
#' @param gateId ID of gate.
#' @param exclude Gate ID to exclude from deletion.
#' @export
#' @examples
#' \dontrun{
#' deleteGate(experimentId, gid = gateFamilyID)
#' deleteGate(experimentId, gateId = gateID)
#' }
#'
deleteGates <- function(experimentId, gid = NULL, gateId = NULL, exclude = NULL) {
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  base <- paste("experiments", experimentId, "gates", sep = "/")
  # TODO:(ge) Add byName functionality for gateId

  if ((is.null(gid) & is.null(gateId)) | (!is.null(gid) & !is.null(gateId))) {
    stop("Either the gid or the gateId must be specified")
  }

  if (!is.null(gateId)) {
    checkDefined(gateId)
    url <- sprintf("%s/%s", base, gateId)
  } else if (!is.null(gid)) {
    url <- sprintf("%s?gid=%s", base, gid)
    if (!is.null(exclude)) {
      url <- sprintf("$s&exclude=%s", url, exclude)
    }
  }
  baseDelete(url)
}
