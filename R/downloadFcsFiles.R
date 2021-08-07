#' Download multiple FCS files
#'
#' Downloads multiple files bundled into a ZIP archive.
#'
#' @param experimentId ID of experiment.
#' @param format Specifies the file format ("fcs", "tsv (with header)" or
#' "tsv (without header)")
#' @param destination [String] Write the files to the specified destination.
#' @param overwrite [Logical] Allows a destination file to be overwritten.
#' @param fcsFileIds [Array<String>] Optional. Defaults to returning all
#' non-control files in the experiment.
#' @param populationIds [Array<String>] Optional. If provided, only events from
#' these populations will be included in the output files. Defaults to ungated.
#' @param compensationId [String] Required if populationIds is specified.
#' Compensation to use for gating.
#' @param compensatedQ [Logical] Optional. If true, applies the compensation
#' specified in compensationId to the exported events. For TSV format, the
#' numerical values will be the compensated values. For FCS format, the
#' numerical values will be unchanged, but the file header will contain the
#' compensation as the spill string (file-internal compensation).
#' @param preSubsampleN [Integer] Randomly subsample the file to contain this
#' many events before gating.
#' @param preSubsampleP [Numeric] Randomly subsample the file to contain this
#' percent of events (0 to 1) before gating.
#' @param postSubsampleN [Integer] Randomly subsample the file to contain this
#' many events after gating.
#' @param postSubsampleP [Numeric] Randomly subsample the file to contain this
#' percent of events (0 to 1) after gating.
#' @param seed [Numeric] Seed for random number generator used for
#' subsampling. Use for deterministic (reproducible) subsampling. If omitted, a
#' pseudo-random value is used.
#' @param filenameTemplate [String]	Tokenized template to dynamically name
#' each file in the resulting archive, like "{file.name}-{population.name}"
#' @export
#' @examples
#' \dontrun{
#' # Download all FCS files in the experiment
#' downloadFcsFiles(experimentId, "fcs", "archive.zip", overwrite = T)
#' # Download specific FCS files
#' fcsFileIds=c("5d2f8b4b21fd0676fb3a6a72", "5d2f8b4b21fd0676fb3a6a74")
#' downloadFcsFiles(experimentId, "fcs", "archive.zip", fcsFileIds=fcsFileIds)
#' }
downloadFcsFiles <- function(
  experimentId,
  format,
  destination,
  overwrite=F,
  fcsFileIds=NULL,
  populationIds=NULL,
  compensationId=NULL,
  compensatedQ=NULL,
  preSubsampleN=NULL,
  preSubsampleP=NULL,
  postSubsampleN=NULL,
  postSubsampleP=NULL,
  seed=NULL,
  filenameTemplate=NULL) {
  checkDefined(experimentId)
  experimentId = lookupByName("experiments", experimentId)

  body <- list(
    format = jsonlite::unbox(format),
    fcsFileIds = fcsFileIds,
    populationIds = populationIds,
    compensationId = jsonlite::unbox(compensationId),
    compensatedQ = jsonlite::unbox(compensatedQ),
    preSubsampleN = jsonlite::unbox(preSubsampleN),
    preSubsampleP = jsonlite::unbox(preSubsampleP),
    postSubsampleN = jsonlite::unbox(preSubsampleN),
    postSubsampleP = jsonlite::unbox(postSubsampleP),
    seed = jsonlite::unbox(seed),
    filenameTemplate = jsonlite::unbox(filenameTemplate)
  )
  body <- body[-which(lapply(body,is.null) == T)]
  body <- jsonlite::toJSON(body, null = "null", digits = NA)

  fullURL <- paste(
    paste(
      pkg.env$baseURL,
      "experiments",
      experimentId,
      "fcsfiles", "zip", sep = "/"
    ),
    sep = "."
  )

  response <- httr::POST(
    fullURL,
    body = body,
    httr::content_type_json(),
    httr::user_agent(ua),
    httr::write_disk(destination, overwrite)
  )
  httr::warn_for_status(response)
}
