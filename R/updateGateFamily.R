#' Update gate family
#'
#' Updates a gate family.
#'
#' @param experimentId ID of experiment.
#' @param gid Gate family ID
#' @param properties Properties to set on the gate.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' updateGateFamily(experimentId, gid, list("name" = "new gate name"))
#' }

updateGateFamily = function(experimentId, gid, properties = list(), params = list()) {
  checkDefined(experimentId)
  experimentId = lookupByName("experiments", experimentId)
  checkDefined(gid)
  body = jsonlite::toJSON(properties, null = "null", auto_unbox = TRUE)
  url = sprintf("experiments/%s/gates?gid=%s", experimentId, gid)
  basePatch(url, body, params)
}
