#' Get population
#'
#' Retrieves a population by ID
#'
#' @param experimentId ID of experiment.
#' @param populationId ID of the population.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' # Retrieve by ID
#' getPopulation(experimentId, populationId)
#'
#' # Lookup by name
#' getPopulation(experimentId, byName("Singlets"))
#' }
getPopulation <- function(experimentId, populationId, params = list()) {
  stopIfParamIsNull(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  stopIfParamIsNull(populationId)
  populationId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/populations"), populationId)
  baseGet(paste0("/api/v1/experiments/", experimentId, "/populations/", populationId), params)
}
