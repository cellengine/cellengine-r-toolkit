#' Upload attachment
#'
#' Uploads an attachment to an experiment.
#'
#' @param experimentId ID of experiment to which to upload.
#' @param attachmentPath Path to attachment.
#' @export
#' @examples
#' \dontrun{
#' uploadAttachment(experimentId, "/path/to/file")
#' }
uploadAttachment <- function(experimentId, attachmentPath) {
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  body <- list("file" = httr::upload_file(attachmentPath))
  ensureBaseUrl()
  fullURL <- paste(pkg.env$baseURL, "experiments", experimentId, "attachments", sep = "/")
  handleResponse(httr::POST(fullURL, body = body, httr::user_agent(ua)))
}
