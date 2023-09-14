#' Delete FCS file
#'
#' Deletes an FCS file.
#'
#' @param experimentId ID of experiment.
#' @param fcsFileId ID of FCS file.
#' @export
#' @examples
#' \dontrun{
#' deleteFcsFile(experimentId, fcsFileId)
#' }
deleteFcsFile <- function(experimentId, fcsFileId) {
  stopIfParamIsNull(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  stopIfParamIsNull(fcsFileId)
  fcsFileId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/fcsfiles"), fcsFileId, "filename")
  baseDelete(paste0("/api/v1/experiments/", experimentId, "/fcsfiles/", fcsFileId))
}
