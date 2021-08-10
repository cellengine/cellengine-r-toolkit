#' Get a gate
#'
#' Retrieves a gate from an experiment.
#'
#' @param experimentId ID of experiment.
#' @param gateId ID of gate.
#' @export
#' @examples
#' \dontrun{
#' getGate(experimentId, gateId)
#' }
getGate = function(experimentId, gateId) {
  checkDefined(experimentId)
  experimentId = lookupByName("experiments", experimentId)
  checkDefined(gateId)
  gateId = lookupByName(paste("experiments", experimentId, "gates", sep = "/"), gateId)
  baseGet(paste("experiments", experimentId, "gates", gateId, sep = "/"))
}
