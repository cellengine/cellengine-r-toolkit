#' Update experiment
#'
#' Updates an experiment.
#'
#' @param experimentId ID of experiment.
#' @param properties Properties to set on the experiment.
#' @export
#' @examples
#' \dontrun{
#' updateExperiment(experimentId, list("name" = "my experiment"))
#' }
updateExperiment <- function(experimentId, properties = list()) {
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  body <- jsonlite::toJSON(properties, null = "null", auto_unbox = TRUE)
  basePatch(paste("experiments", experimentId, sep = "/"), body)
}
