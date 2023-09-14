#' Update Fcs File
#'
#' Updates an fcs file
#'
#' @param experimentId character ID of experiment.
#' @param fcsFileId character ID of file or a \code{byName} expression.
#' @param properties list Properties to set on the fcs file.
#'
#' Accepted properties:
#' - filename character The name of the file
#' - panel list List with properties:
#'   - channel character
#'   - index character
#'   - reagent character Optional; only channel and index are required.
#' - panelName character
#' - isControl logical
#' - compensation character Compensation ID
#' - deleted string ISO 8601 date string, "%Y-%m-%dT%H:%M:%S.%fZ"
#' - gatesLocked logical
#' - annotations list List with properties:
#'   - name character
#'   - value character
#' @md
#' @export
#' @examples
#' \dontrun{
#' updateFcsFile(experimentId, list("filename" = "my fcs file"))
#' }
updateFcsFile <- function(experimentId, fcsFileId, properties = list()) {
  checkDefined(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  checkDefined(fcsFileId)
  fcsFileId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/fcsfiles"), fcsFileId, "filename")
  body <- jsonlite::toJSON(properties, null = "null", auto_unbox = TRUE)
  basePatch(paste0("/api/v1/experiments/", experimentId, "/fcsfiles/", fcsFileId), body)
}
