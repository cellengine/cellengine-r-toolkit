#' Get gates
#'
#' Retrieves the list of gates in an experiment.
#'
#' @param experimentId ID of experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' # List all gates in the experiment
#' getGates(experimentId)
#'
#' # List the name and GID of the first five gates
#' getGates(experimentId, params = list("limit" = "5", "fields" = "+name,+gid"))
#' }
getGates <- function(experimentId, params = list()) {
  checkDefined(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  res <- baseGet(paste0("/api/v1/experiments/", experimentId, "/gates"), params)
  res
}
