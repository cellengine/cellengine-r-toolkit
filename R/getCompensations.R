#' Get compensations
#'
#' Retrieves the list of compensations in an experiment.
#'
#' @param experimentId ID of experiment or a \code{byName} expression.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' getCompensations(experimentId)
#' getCompensations(experimentId, params = list("limit" = "5"))
#' }
getCompensations <- function(experimentId, params = list()) {
  stopIfParamIsNull(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  cs <- baseGet(paste0("/api/v1/experiments/", experimentId, "/compensations"),
                params)
  lapply(cs, formatCompensationResponse)
}
