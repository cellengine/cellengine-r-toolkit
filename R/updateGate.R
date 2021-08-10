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

updateGate = function(experimentId, gateId, properties = list()) {
  checkDefined(experimentId)
  experimentId = lookupByName("experiments", experimentId)
  checkDefined(gateId)
  gateId = lookupByName(paste("experiments", experimentId, "gates", sep = "/"), gateId)
  body = jsonlite::toJSON(properties, null = "null", auto_unbox = TRUE)
  basePatch(paste("experiments", experimentId, "gates", gateId, sep = "/"), body)
}
