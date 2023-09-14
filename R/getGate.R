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
getGate <- function(experimentId, gateId) {
  stopIfParamIsNull(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  stopIfParamIsNull(gateId)
  gateId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/gates"), gateId)
  baseGet(paste0("/api/v1/experiments/", experimentId, "/gates/", gateId))
}
