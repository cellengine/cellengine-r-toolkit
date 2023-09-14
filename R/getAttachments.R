#' Get experiment attachments
#'
#' Retrieves the list of attachments in an experiment.
#'
#' @param experimentId ID of experiment.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' getAttachments(experimentId)
#' getAttachments(experimentId, params = list("limit" = "5"))
#' }
getAttachments <- function(experimentId, params = list()) {
  checkDefined(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  baseGet(paste0("/api/v1/experiments/", experimentId, "/attachments"), params)
}
