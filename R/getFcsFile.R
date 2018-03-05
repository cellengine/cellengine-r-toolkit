#' Get FCS File
#'
#' Retrieves an FCS file. This retrieves metadata about the file, but does not
#' retrieve the file's contents.
#'
#' @param experimentId ID of experiment.
#' @param fcsFileId ID of FCS file.
#' @param params Optional query parameters.
#' @export
#' @examples
#' \dontrun{
#' getFcsFile(experimentId, fcsFileId)
#' getFcsFile(experimentId, fcsFileId, params = list("fields" = "+filename"))
#' }
getFcsFile = function(experimentId, fcsFileId, params = list()) {
  baseGet(paste("experiments", experimentId, "fcsfiles", fcsFileId, sep = "/"), params)
}