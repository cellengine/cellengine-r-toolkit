#' Apply a scale
#'
#' Applies a scale to a list of channel values
#'
#' @param scale Scale (named list with keys ['type', 'maximum', 'minimum', 'cofactor']).
#' @param data List of values for a channel.
#' @param clamp_q If true, values will be clamped to fall within the scale's
#' minimum and maximum.
#' @export
#' @examples
#' \dontrun{
#' applyScale(list(type='LinearScale', minimum=1, maximum=10), list(1, 2, 3, 4, 5))
#' }
applyScale = function(scale, data, clamp_q=FALSE) {

  fn = switch(
    scale$type,
    "LinearScale" = function(a) { a },
    "LogScale" = function(a) { if (a <= 1) 0 else log10(a) },
    "ArcSinhScale" = function(a) { asinh(a / scale$cofactor) },
    )

  if (clamp_q) {
    lapply(pmax(pmin(data, scale$maximum), scale$minimum), fn)
  } else {
    lapply(data, fn)
  }
}
