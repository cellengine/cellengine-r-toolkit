#' Update compensation
#'
#' Updates a compensation.
#'
#' @param experimentId ID of experiment or a \code{byName} expression.
#' @param compensationId ID of compensation or a \code{byName} expression.
#' @param properties Properties to set on the compensation.
#' @export
#' @examples
#' \dontrun{
#' updateCompensation(experimentId, compensationId, list("name" = "new name"))
#' }
#'
updateCompensation <- function(experimentId, compensationId, properties = list()) {
  checkDefined(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  checkDefined(compensationId)
  compensationId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/compensations"),
                                 compensationId)
  body <- jsonlite::toJSON(properties, null = "null", auto_unbox = TRUE)
  r = basePatch(
    paste0("/api/v1/experiments/", experimentId, "/compensations/", compensationId),
    body
  )
  formatCompensationResponse(r)
}
