#' Auto Delete Population
#'
#' Deletes a population, its corresponding gates, and all descendant gates and
#' populations.
#'
#' @param experimentId ID of experiment.
#' @param populationId ID of population.
#' @export
#' @examples
#' \dontrun{
#' deletePopulationAuto(experimentId, populationId)
#' }
deletePopulationAuto <- function(experimentId, populationId) {
  checkDefined(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  checkDefined(populationId)
  populationId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/populations"), populationId)
  baseDelete(
    paste0("/api/v1/experiments/", experimentId, "/populations/", populationId),
    params = list(deleteBranch = "true")
  )
}
