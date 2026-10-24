#' Create FCS file
#'
#' Creates an FCS file by copying, concatenating and/or subsampling existing
#' files, or by importing a file from an S3-compatible service.
#'
#' @param experimentId ID of the experiment to which to add the file.
#' @param fcsFiles A character vector of FCS file IDs, or a list of source
#'   objects. Source objects may specify \code{_id} and \code{experimentId} to
#'   copy a file from another experiment, or \code{host} and \code{path} to
#'   import a file from an S3-compatible service.
#' @param addFileNumber If concatenating files, whether to add a file number
#'   channel.
#' @param addEventNumber Whether to add an event number column.
#' @param filename Filename to use for the created file. Required when
#'   concatenating or subsampling.
#' @param monotonicChannels Channels whose values should remain monotonically
#'   increasing when concatenating files.
#' @param preSubsampleN Randomly subsample each input file to this number of
#'   events before concatenating.
#' @param preSubsampleP Randomly subsample each input file to this fraction of
#'   events before concatenating.
#' @param seed Seed for deterministic subsampling.
#' @export
#' @examples
#' \dontrun{
#' createFcsFile(experimentId, c("fcsFileId1", "fcsFileId2"),
#'               filename = "combined.fcs")
#' }
createFcsFile <- function(experimentId, fcsFiles, addFileNumber = NULL,
                          addEventNumber = NULL, filename = NULL,
                          monotonicChannels = NULL, preSubsampleN = NULL,
                          preSubsampleP = NULL, seed = NULL) {
  stopIfParamIsNull(experimentId)
  stopIfParamIsNull(fcsFiles)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)

  if (is.character(fcsFiles)) fcsFiles <- as.list(fcsFiles)

  body <- list(fcsFiles = fcsFiles)
  optional <- list(
    addFileNumber = addFileNumber,
    addEventNumber = addEventNumber,
    filename = filename,
    monotonicChannels = monotonicChannels,
    preSubsampleN = preSubsampleN,
    preSubsampleP = preSubsampleP,
    seed = seed
  )
  body <- c(body, optional[!vapply(optional, is.null, logical(1))])

  body <- jsonlite::toJSON(body, null = "null", auto_unbox = TRUE)
  basePost(paste0("/api/v1/experiments/", experimentId, "/fcsfiles"), body)
}
