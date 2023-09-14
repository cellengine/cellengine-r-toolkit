#' Annotate FCS file
#'
#' Sets the annotations on an FCS file.
#'
#' This function overwrites any existing annotations. To add new annotations
#' while keeping the existing annotations, first use \code{getFcsFile} to get
#' the existing annotations. Append new annotations with \code{rbind}.
#'
#' Note: if you retrieve FCS files in bulk, such as with \code{getFcsFiles},
#' \code{file$annotations} will return a nested list. Be sure to extract the
#' annotations from this list before appending new ones (see examples below).
#'
#' @param experimentId ID of experiment.
#' @param fcsFileId ID of FCS file.
#' @param annotations List of annotations
#' @export
#' @examples
#' \dontrun{
#' annotations <- list(
#'   list(name = "annotations 1", value = 1),
#'   list(name = "annotation 2", value = "myValue")
#' )
#' annotateFcsFile(experimentId, fcsFileId, annotations)
#'
#' # or, to append annotations
#' files <- getFcsFiles(id)
#' file <- files[1, ]
#' annos <- file$annotations[[1]]
#' annos[nrow(annos) + 1, ] <- list("abc", "def")
#' annotateFcsFile(experimentId, file$`_id`, annos)
#' }
annotateFcsFile <- function(experimentId, fcsFileId, annotations) {
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  checkDefined(fcsFileId)
  fcsFileId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/fcsfiles"), fcsFileId, "filename")
  body <- jsonlite::toJSON(list("annotations" = annotations), auto_unbox = TRUE)
  basePatch(paste0("/api/v1/experiments/", experimentId, "/fcsfiles/", fcsFileId), body)
}
