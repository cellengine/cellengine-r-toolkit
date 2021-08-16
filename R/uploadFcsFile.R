#' Upload FCS file
#'
#' Uploads an FCS file to an experiment.
#'
#' @param experimentId ID of experiment to which to upload.
#' @param fcsFilePath Path to FCS file.
#' @export
#' @examples
#' \dontrun{
#' uploadFcsFile(experimentId, "/path/to/file.fcs")
#' }
uploadFcsFile <- function(experimentId, fcsFilePath) {
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  body <- list("file" = httr::upload_file(fcsFilePath))
  ensureBaseUrl()
  fullURL <- paste(pkg.env$baseURL, "experiments", experimentId, "fcsfiles", sep = "/")
  handleResponse(httr::POST(fullURL, body = body, httr::user_agent(ua)))
}
