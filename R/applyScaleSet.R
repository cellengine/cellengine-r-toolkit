#' Apply a ScaleSet
#'
#' Applies a ScaleSet to a dataframe of events
#'
#' @param scaleset ScaleSet. Accepts a ScaleSet or \code{scaleSet$scales}.
#' @param data Events data from an FcsFile, i.e. \code{events@expr}
#' @param clampQ If true, values will be clamped to fall within the scale's
#' minimum and maximum. Applies clamping to all scales in a ScaleSet
#' @export
#' @examples
#' \dontrun{
#' getEvents(experimentId, fcsFileId, destination = "myFile.fcs")
#' events <- flowCore::read.FCS("myFile.fcs")
#' data <- events@exprs
#' scaleSet <- getScaleSets(experimentId)[1, ]
#' applyScaleSet(scaleSet, data)
#' }
applyScaleSet <- function(scaleset, data, clampQ = FALSE) {
  if ("scales" %in% names(scaleset)) {
    scaleset <- scaleset$scales[[1]]
  }
  missing <- list()
  cols <- lapply(colnames(data), function(cname) {
    idx <- match(cname, scaleset[, "channelName"])
    if (!is.na(idx)) {
      return(applyScale(scaleset[idx, "scale"], data[, cname], clampQ = clampQ))
    } else {
      missing <<- append(missing, cname)
    }
  })

  if (length(missing) > 0) {
    stop(c("Scales do not exist in the scaleset for the following channels:", missing))
  }

  df <- as.data.frame(do.call(cbind, cols))
  colnames(df) <- colnames(data)
  return(df)
}
