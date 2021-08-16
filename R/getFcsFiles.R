#' Get FCS files
#'
#' Retrieves the list of FCS files in an experiment. This does not download
#' the FCS files themselves; this only returns information about the FCS files.
#' Only returns non-control FCS files unless "includeControls"=T is passed in
#' params.
#'
#' @param experimentId ID of experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' # List all FCS files in the experiment
#' getFcsFiles(experimentId)
#'
#' # List the filename of the the first five files
#' getFcsFiles(experimentId, params = list("limit" = "5", "fields" = "+filename"))
#' }
getFcsFiles <- function(experimentId, params = list()) {
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  baseGet(paste("experiments", experimentId, "fcsfiles", sep = "/"), params)
}
