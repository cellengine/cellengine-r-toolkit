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
  stopIfParamIsNull(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  body <- list("file" = httr::upload_file(attachmentPath))
  ensureBaseUrl()
  fullURL <- paste0(pkg.env$baseURL, "/api/v1/experiments/", experimentId, "/attachments")
  handleResponse(httr::POST(fullURL, body = body, httr::user_agent(ua)))
}
