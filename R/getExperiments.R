#' Get experiments
#'
#' Retrieves the list of experiments to which you have access.
#'
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' # List all accessible experiments
#' getExperiments()
#'
#' # List the names of the first five experiments
#' getExperiments(params = list("limit" = "5", "fields" = "+name"))
#' }
getExperiments <- function(params = list()) {
  baseGet("/api/v1/experiments", params)
}
