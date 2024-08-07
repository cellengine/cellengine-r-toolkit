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
  stopIfParamIsNull(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  body <- list("file" = httr::upload_file(fcsFilePath))
  ensureBaseUrl()
  fullURL <- paste0(pkg.env$baseURL, "/api/v1/experiments/", experimentId, "/fcsfiles")
  r <- httr::POST(fullURL,
                  body = body,
                  httr::user_agent(ua),
                  httr::add_headers(.headers = pkg.env$auth))
  handleResponse(r)
}
