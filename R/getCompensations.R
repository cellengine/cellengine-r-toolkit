#' Get compensations
#'
#' Retrieves the list of compensations in an experiment.
#'
#' @param experimentId ID of experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' getCompensations(experimentId)
#' getCompensations(experimentId, params = list("limit" = "5"))
#' }
getCompensations <- function(experimentId, params = list()) {
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  baseGet(paste("experiments", experimentId, "compensations", sep = "/"), params)
}
