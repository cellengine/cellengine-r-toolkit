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
  stopIfParamIsNull(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  stopIfParamIsNull(populationId)
  populationId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/populations"), populationId)
  body <- jsonlite::toJSON(properties, null = "null", auto_unbox = TRUE)
  basePatch(paste0("/api/v1/experiments/", experimentId, "/populations/", populationId), body)
}
