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
  experimentId <- lookupByName("experiments", experimentId)
  baseGet(paste("experiments", experimentId, "attachments", sep = "/"), params)
}
