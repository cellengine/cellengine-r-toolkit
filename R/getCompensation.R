#' Get compensation
#'
#' Retrieves a compensation. The returned object has a \code{spillMatrix}
#' property that is a \code{matrix}.
#'
#' @param experimentId ID of experiment or a \code{byName} expression.
#' @param compensationId ID of compensation or a \code{byName} expression.
#' @export
#' @examples
#' \dontrun{
#' # Retrieve by ID
#' getCompensation(experimentId, compensationId)
#'
#' # Lookup by name
#' getCompensation(experimentId, byName("Comp 1"))
#' }
getCompensation <- function(experimentId, compensationId) {
  checkDefined(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  checkDefined(compensationId)
  compensationId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/compensations"),
                                 compensationId)
  r <- baseGet(paste0("/api/v1/experiments/", experimentId, "/compensations/", compensationId))
  formatCompensationResponse(r)
}
