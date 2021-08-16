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
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  baseDelete(paste("experiments", experimentId, sep = "/"))
}
