#' Set FCS file panel
#'
#' Updates the panel properties of an FCS file.
#'
#' A panel must be specified as a list of lists. Each inner list must have at
#' least \code{channel} and \code{index} properties, and may optionally have a
#' \code{reagent} property.
#'
#' The \code{index} value corresponds to the parameter index (i.e. the n in
#' "$PnN" from the FCS file header). This property allows the list of channels
#' to be in any order.
#'
#' The \code{channel} value corresponds to the channel name. Channel names are
#' used for matching channels between FCS files, gates, scales and compensations.
#'
#' The \code{reagent} value is used for various displays, including plot axis
#' labels, and is typically the reagent description (e.g. "CD3-PacBlu").
#'
#' The \code{panelName} property is used to group files by panel.
#'
#' @param experimentId ID of experiment.
#' @param fcsFileId ID of FCS file.
#' @param panelName Name of panel.
#' @param panel List of channel properties.
#' @export
#' @examples
#' \dontrun{
#' panel <- list(
#'   list("index" = 1, "channel" = "FSC-A"),
#'   list("index" = 7, "channel" = "Blue530-A", "reagent" = "CD3")
#' )
#' setFcsFilePanel(experimentId, fcsFileId, "Panel 1", panel)
#' }
setFcsFilePanel <- function(experimentId, fcsFileId, panelName, panel) {
  stopIfParamIsNull(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  stopIfParamIsNull(fcsFileId)
  fcsFileId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/fcsfiles"), fcsFileId, "filename")
  body <- jsonlite::toJSON(list(
    panelName = panelName,
    panel = panel
  ), null = "null", auto_unbox = TRUE)
  basePatch(paste0("/api/v1/experiments/", experimentId, "/fcsfiles/", fcsFileId), body)
}
