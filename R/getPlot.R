#' Get a plot
#'
#' Gets a plot for an FcsFile. If in RStudio or Jupyter and `display` is `TRUE`
#' (default), displays the image. If `destination` is provided, also saves the
#' image as a PNG file. In all cases, returns the PNG-encoded bytes (invisibly,
#' if `display` is `TRUE`).
#'
#' @param experimentId character ID of experiment.
#' @param fcsFileId character ID of FcsFile
#' @param plotType character Plot type, one of: "contour", "dot", "density", or "histogram"
#' @param xChannel character X channel name
#' @param yChannel character Y channel name
#' @param zChannel character Z channel name
#' @param params list Optional query parameters:
#' - axesQ logical Display axes lines. Defaults to `TRUE`.
#' - axisLabelsQ logical Display axis labels. Defaults to `TRUE`.
#' - color character Case-insensitive string in the format #rgb, #rgba,
#'   #rrggbb or #rrggbbaa. The foreground color, i.e. color of curves in
#'   "histogram" and "contour" plots, dots in "dot" plots.
#' - compensation character ID of compensation to use for gating and
#'   display, or one of [`UNCOMPENSATED`] or [`FILE_INTERNAL`].
#' - gateLabel character One of "eventcount", "mean", "median", "percent",
#'   "mad", "cv", "stddev", "geometricmean", "name", "none".
#' - gateLabelFontSize double Font size for gate labels.
#' - height double Image height. Defaults to 228.
#' - percentileStart double For contour plots, the percentile of the first
#'   contour.
#' - percentileStep double For contour plots, the percentile step between
#'   each contour.
#' - populationId character Population ID. Defaults to ungated.
#' - postSubsampleN double Randomly subsample the file to contain this many
#'   events after gating.
#' - postSubsampleP double Randomly subsample the file to contain this
#'   percent of events (0 to 1) after gating.
#' - renderGates logical Render gates into the image.
#' - seed double Seed for random number generator used for subsampling. Use
#'   for deterministic reproducible subsampling. If omitted, a pseudo-random
#'   value is used.
#' - smoothing double For density and contour plots, adjusts the amount of
#'   smoothing. Defaults to 0 (no smoothing). Set to 1 for typical smoothing.
#'   Higher values (up to 10) increase smoothing.
#' - strokeThickness double The thickness of histogram and contour plot
#'   lines. Defaults to 1.
#' - tickLabelsQ logical Display tick labels. Defaults to `FALSE`.
#' - ticksQ logical Display ticks. Defaults to `TRUE`.
#' - width Integer Image width. Defaults to 228.
#' - xAxisLabelQ logical Display x axis label. Overrides axisLabelsQ.
#' - xAxisQ logical Display x axis line. Overrides axesQ.
#' - xTickLabelsQ logical Display x tick labels. Overrides tickLabelsQ.
#' - xTicksQ logical Display x ticks. Overrides ticksQ.
#' - yAxisLabel logical Display y axis label. Overrides axisLabelsQ.
#' - yAxisQ logical Display y axis line. Overrides axesQ.
#' - yTickLabelsQ logical Display y tick labels. Overrides tickLabelsQ.
#' - yTicksQ logical Display y ticks. Overrides ticksQ.
#' @param destination character If specified, write the image to file.
#' @param overwrite logical If a destination is specified, allows destination
#'   file to be overwritten.
#' @param display logical Display the image. Defaults to `TRUE`.
#'
#' @md
#' @export
#' @examples
#' \dontrun{
#' parameters <- list(
#'   "width" = 400,
#'   "height" = 400,
#'   "axesQ" = FALSE,
#'   "ticksQ" = FALSE,
#'   "color" = "#ff0000",
#'   "renderGates" = TRUE,
#'   "smoothing" = 0.75,
#' )
#' getPlot(experimentId,
#'   fcsFileId,
#'   plotType = "dot",
#'   xChannel = "FSC-A",
#'   yChannel = "FSC-W",
#'   params = parameters
#' )
#' }
getPlot <- function(experimentId,
                    fcsFileId,
                    plotType,
                    xChannel,
                    yChannel = "",
                    zChannel = "",
                    params = list(),
                    destination = NULL,
                    overwrite = FALSE,
                    display = TRUE) {
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)
  checkDefined(fcsFileId)
  fcsFileId <- lookupByName(paste("experiments", experimentId, "fcsfiles", sep = "/"), fcsFileId, "filename")

  args <- list(
    fcsFileId = fcsFileId,
    plotType = plotType,
    xChannel = xChannel,
    yChannel = yChannel,
    zChannel = zChannel,
    axisLabelsQ = TRUE,
    ticksQ = TRUE
  )

  args[names(params)] <- params
  bytes <- baseGet(paste("experiments", experimentId, "plot", sep = "/"), args)

  if (!is.null(destination)) {
    if (!overwrite && file.exists(destination)) {
      stop("Path exists and overwrite is FALSE")
    }
    if (!endsWith(destination, ".png")) destination <- paste0(destination, ".png")
    writeBin(bytes, destination)
  }

  if (display) {
    if (rstudioapi::isAvailable()) {
      tmp <- file.path(tempdir(), "plot.png")
      writeBin(bytes, tmp)
      rstudioapi::viewer(tmp)
    } else if (isTRUE(getOption("jupyter.in_kernel"))) {
      IRdisplay::display_png(bytes)
    } else {
      warning("Not in RStudio or Jupyter. Can't display plot.")
    }
    return(invisible(bytes))
  } else {
    return(bytes)
  }
}
