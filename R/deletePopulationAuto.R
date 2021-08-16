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
  experimentId <- lookupByName("experiments", experimentId)
  checkDefined(populationId)
  populationId <- lookupByName(paste("experiments", experimentId, "populations", sep = "/"), populationId)
  baseDelete(
    paste("experiments", experimentId, "populations", populationId, sep = "/"),
    params = list(deleteBranch = "true")
  )
}
