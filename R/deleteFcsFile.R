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
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  checkDefined(fcsFileId)
  fcsFileId <- lookupByName(paste("experiments", experimentId, "fcsfiles", sep = "/"), fcsFileId, "filename")
  baseDelete(paste("experiments", experimentId, "fcsfiles", fcsFileId, sep = "/"))
}
