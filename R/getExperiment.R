#' Get experiment
#'
#' Retrieves an experiment.
#'
#' @param experimentId ID of experiment.
#' @export
#' @examples
#' \dontrun{
#' # Retrieve by ID
#' getExperiment(experimentId)
#'
#' # Lookup by name
#' getExperiment(byName("my experiment"))
#' }
getExperiment <- function(experimentId) {
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  baseGet(paste("experiments", experimentId, sep = "/"))
}
