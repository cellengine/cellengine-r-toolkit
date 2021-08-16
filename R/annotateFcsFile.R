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
#' annotations from this list before appending new ones.
#'
#' @param experimentId ID of experiment.
#' @param fcsFileId ID of FCS file.
#' @param annotations List of annotations
#' @export
#' @examples
#' \dontrun{
#' annotations <- list(list(name = "annotations 1", value = 1), list(name = "annotation 2", value = "myValue"))
#' annotateFcsFile(experimentId, fcsFileId, annotations)
#' }
annotateFcsFile <- function(experimentId, fcsFileId, annotations) {
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  checkDefined(fcsFileId)
  fcsFileId <- lookupByName(paste("experiments", experimentId, "fcsfiles", sep = "/"), fcsFileId, "filename")
  body <- jsonlite::toJSON(list("annotations" = annotations), auto_unbox = TRUE)
  basePatch(paste("experiments", experimentId, "fcsfiles", fcsFileId, sep = "/"), body)
}
