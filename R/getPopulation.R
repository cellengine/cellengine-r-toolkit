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
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  checkDefined(populationId)
  populationId <- lookupByName(paste("experiments", experimentId, "populations", sep = "/"), populationId)
  baseGet(paste("experiments", experimentId, "populations", populationId, sep = "/"), params)
}
