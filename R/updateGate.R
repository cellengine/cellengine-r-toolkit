#' Update gate
#'
#' Updates a gate.
#'
#' @param experimentId ID of experiment.
#' @param gateId ID of gate or a \code{byName} expression.
#' @param properties Properties to set on the gate.
#' @export
#' @examples
#' \dontrun{
#' updateGate(experimentId, gateId, list("name" = "new gate name"))
#' }
#'
updateGate <- function(experimentId, gateId, properties = list()) {
  checkDefined(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  checkDefined(gateId)
  gateId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/gates"), gateId)
  body <- jsonlite::toJSON(properties, null = "null", auto_unbox = TRUE)
  basePatch(paste0("/api/v1/experiments/", experimentId, "/gates/", gateId), body)
}
