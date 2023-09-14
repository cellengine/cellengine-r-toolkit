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
  stopIfParamIsNull(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  baseGet(paste0("/api/v1/experiments/", experimentId))
}
