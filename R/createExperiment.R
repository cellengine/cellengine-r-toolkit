#' Create experiment
#'
#' Creates a new experiment.
#'
#' @param properties Optional properties for the new experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' createExperiment() # creates a blank experiment
#' createExperiment(list("name" = "my experiment"))
#' }
createExperiment <- function(properties = list(), params = list()) {
  body <- jsonlite::toJSON(properties, null = "null", auto_unbox = TRUE)
  basePost("/api/v1/experiments", body, params)
}
