#' Delete compensation
#'
#' Deletes a compensation.
#'
#' @param experimentId ID of experiment or a \code{byName} expression.
#' @param compensationId ID of compensation or a \code{byName} expression.
#' @export
#' @examples
#' \dontrun{
#' deleteCompensation(experimentId, compensationId)
#' }
deleteCompensation <- function(experimentId, compensationId) {
  stopIfParamIsNull(experimentId)
  experimentId <- lookupByName("/api/v/1/experiments", experimentId)
  stopIfParamIsNull(compensationId)
  compensationId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/compensations"), compensationId)
  baseDelete(paste0("/api/v1/experiments/", experimentId, "/compensations/", compensationId))
}
