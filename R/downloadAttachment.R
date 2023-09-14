#' Download Attachment
#'
#' Downloads an attachment and returns it as a binary blob or saves it to disk.
#'
#' @param experimentId ID of the experiment.
#' @param attachmentId ID of the attachment.
#' @param destination Optional. If specified, write the file to the specified
#'   path instead of returning it as a binary blob.
#' @param overwrite Optional. If a destination is specified, allows destination
#'   file to be overwritten.
#' @export
#' @examples
#' \dontrun{
#' # Returns the attachment as a binary blob:
#' attachment <- downloadAttachment(experimentId, attachmentId)
#' # Parse it as text:
#' readBin(attachment, character())
#'
#' # Saves the attachment to disk:
#' downloadAttachment(experimentId, attachmentId, destination = "dir/file.txt")
#'
#' # Use the byName helper to avoid finding the attachment ID:
#' downloadAttachment(experimentId, byName("file.txt"))
#' }
downloadAttachment <- function(experimentId,
                               attachmentId,
                               destination = NULL,
                               overwrite = FALSE) {
  checkDefined(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  checkDefined(attachmentId)
  attachmentId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/attachments"), attachmentId, "filename")
  ensureBaseUrl()

  fullURL <- paste0(pkg.env$baseURL, "/api/v1/experiments/", experimentId, "/attachments/", attachmentId)

  if (is.null(destination)) {
    response <- httr::GET(fullURL, httr::user_agent(ua))
    httr::stop_for_status(response, "download attachment")
    content <- httr::content(response, "raw")
    return(content)
  } else {
    response <- httr::GET(fullURL, httr::user_agent(ua), httr::write_disk(destination, overwrite))
    httr::stop_for_status(response, "download attachment")
  }
}
