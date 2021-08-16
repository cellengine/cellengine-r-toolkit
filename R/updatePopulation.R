#' Update population
#'
#' Updates a population.
#'
#' @param experimentId ID of experiment.
#' @param populationId Population ID or a \code{byName()} expression.
#' @param properties Properties to set on the population.
#' @export
#' @examples
#' \dontrun{
#' updatePopulation(experimentId, populationId, list("name" = "new pop name"))
#' }
#'
updatePopulation <- function(experimentId,
                             populationId,
                             properties = list()) {
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  checkDefined(populationId)
  populationId <- lookupByName(paste("experiments", experimentId, "populations", sep = "/"), populationId)
  body <- jsonlite::toJSON(properties, null = "null", auto_unbox = TRUE)
  basePatch(paste("experiments", experimentId, "populations", populationId, sep = "/"), body)
}
