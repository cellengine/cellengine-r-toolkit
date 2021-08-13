#' Get FCS File Events
#'
#' Retrieves events from an FCS file or gated population as either an FCS or
#' TSV file.
#'
#' Tip: FCS files are more compact than TSV files. Use that format for faster
#' downloads. Use a library such as flowCore to parse FCS files.
#'
#' @param experimentId ID of experiment.
#' @param fcsFileId ID of FCS file.
#' @param populationId Optional ID of population, for gated events.
#' @param compensation ID of compensation or special compensation type
#'   (\code{UNCOMPENSATED} or \code{FILE_INTERNAL}) to use for gating. This
#'   should generally match what you used to create your gates. Not required if
#'   \code{compensatedQ} is \code{FALSE} and exporting ungated files.
#' @param compensatedQ If \code{TRUE}, applies the compensation specified in
#'   compensation to the exported events. For TSV format, the numerical values
#'   will be the compensated values. For FCS format, the numerical values will
#'   be unchanged, but the file header will contain the compensation as the
#'   spill string (file-internal compensation).
#' @param headerQ for TSV format only: if true, file will contain a header row.
#' @param format One of "TSV" or "FCS".
#' @param destination Optional, if specified, write the file to the specified
#'   path instead of returning it as a binary blob (FCS) or data.frame (TSV).
#' @param overwrite Optional, if a destination is specified, allows destination
#'   file to be overwritten.
#' @param subsampling Optional, subsampling parameters in the form
#'   \code{list(preSubsampleN = number, preSubsampleP = number,
#'   postSubsampleN = number, postSubsampleP = number, seed = number)}.
#'   Subsample-N options specify absolute subsampling values, and subsample-P
#'   options specify a fractional subsampling value (0 to 1); specify only one
#'   pre- and/or one post-subsample option. Specify a \code{seed} for
#'   reproducible downsampling.
#' @param addEventNumber Optional. Add an event number column to the exported
#'   file. When a `populationId` is specified (when gating), this number
#'   corresponds to the index of the event in the original file.
#' @export
#' @examples
#' \dontrun{
#' # Returns the FCS file as a binary blob (requires parsing before use):
#' getEvents(experimentId, fcsFileId)
#'
#' # Returns a data.frame:
#' getEvents(experimentId, fcsFileId, populationId, format = "TSV")
#'
#' # Saves as an FCS file to disk:
#' getEvents(experimentId, fcsFileId, destination = "/path/to/output.fcs")
#'
#' # Saves as a TSV file to disk:
#' getEvents(experimentId, fcsFileId, destination = "/path/to/output.tsv", format = "TSV")
#'
#' # Subsamples and gates to only contain events in the specified population:
#' subsampling <- list(preSubsampleN = 5000, seed = 1.5)
#' getEvents(experimentId, fcsFileId, populationId, subsampling = subsampling)
#' }
getEvents <- function(experimentId,
                      fcsFileId,
                      populationId = NULL,
                      compensation = NULL,
                      compensatedQ = FALSE,
                      headerQ = TRUE,
                      format = "FCS",
                      destination = NULL,
                      overwrite = FALSE,
                      subsampling = list(),
                      addEventNumber = FALSE) {
  checkDefined(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  checkDefined(fcsFileId)
  fcsFileId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/fcsfiles"), fcsFileId, "filename")

  if (is.null(compensation) && !is.null(populationId)) {
    stop("'compensation' parameter is required for gated populations.")
  } else if (is.null(compensation) && isTRUE(compensatedQ)) {
    stop("'compensation' parameter is required when returning compensated data.")
  }

  if (!is.null(subsampling$preSubsampleN) && !is.null(subsampling$preSubsampleP)) {
    stop("Specify only one of preSubsampleN or preSubsampleP.")
  }

  if (!is.null(subsampling$postSubsampleN) && !is.null(subsampling$postSubsampleP)) {
    stop("Specify only one of postSubsampleN or postSubsampleP.")
  }

  if (!is.null(populationId)) {
    populationId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/populations"), populationId)
  }

  ensureBaseUrl()

  fullURL <- paste0(pkg.env$baseURL, "/api/v1/experiments/", experimentId, "/fcsfiles/", fcsFileId, ".", format)

  params <- list(
    populationId = populationId,
    compensationId = compensation,
    compensatedQ = compensatedQ,
    headers = headerQ,
    addEventNumber = addEventNumber
  )

  subsamplingParams <- Filter(Negate(is.null), subsampling[
    c("preSubsampleN", "preSubsampleP", "postSubsampleN", "postSubsampleP", "seed")
  ])

  params <- c(params, subsamplingParams)

  if (is.null(destination)) {
    response <- httr::GET(fullURL, query = params, httr::user_agent(ua))
    httr::warn_for_status(response)
    if (format == "TSV") {
      content <- httr::content(response, "text")
      content <- utils::read.table(text = content, header = headerQ, sep = "\t", check.names = F)
    } else {
      content <- httr::content(response, "raw")
    }
    return(content)
  } else {
    response <- httr::GET(fullURL, query = params, httr::user_agent(ua), httr::write_disk(destination, overwrite))
    httr::warn_for_status(response)
  }
}
