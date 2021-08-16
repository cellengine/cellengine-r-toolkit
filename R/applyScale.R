#' Apply a scale
#'
#' Applies a scale to a vector of channel values
#'
#' @param scale Scale (named list with keys "type", "maximum", "minimum",
#' and "cofactor").
#' @param data Vector of values for a channel.
#' @param clampQ If true, values will be clamped to fall within the scale's
#' minimum and maximum.
#' @export
#' @examples
#' \dontrun{
#' applyScale(list(type = "LinearScale", minimum = 1, maximum = 10), c(1, 2, 3, 4, 5))
#'
#' # Using a Scale from a CellEngine ScaleSet
#' scaleSet <- getScaleSets(experimentId)
#' chanIdx <- 5
#' applyScale(scaleSet$scales[[1]][chanIdx, "scale"], c(1, 2, 3, 4, 5))
#' }
applyScale <- function(scale, data, clampQ = FALSE) {
  fn <- switch(scale$type,
    "LinearScale" = function(a) {
      a
    },
    "LogScale" = function(a) {
      log10(pmax(1, a))
    },
    "ArcSinhScale" = function(a) {
      asinh(a / scale$cofactor)
    },
  )

  if (clampQ) {
    data <- pmax(pmin(data, scale$maximum), scale$minimum)
  }
  fn(data)
}
