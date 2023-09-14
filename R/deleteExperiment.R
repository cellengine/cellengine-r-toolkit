#' Delete experiment
#'
#' Deletes an experiment.
#'
#' @param experimentId ID of experiment.
#' @export
#' @examples
#' \dontrun{
#' deleteExperiment(experimentId)
#' }
deleteExperiment <- function(experimentId) {
  stopIfParamIsNull(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  baseDelete(paste0("/api/v1/experiments/", experimentId))
}
