#' Export populations to a new experiment
#'
#' Exports populations and FCS files from an experiment into a new experiment.
#'
#' @param experimentId ID of the source experiment or a \code{byName} expression.
#' @param compensationId ID of compensation, special compensation type
#'   (\code{UNCOMPENSATED}, \code{FILE_INTERNAL} \code{PER_FILE}) or a
#'   \code{byName} expression to use for gating. This should generally match
#'   what you used to create your gates.
#' @param name Name of the new experiment.
#' @param fcsFileIds FCS file IDs to export, as a character vector or list.
#'   List elements may be \code{byName} expressions, which are matched against
#'   FCS filenames. If omitted or empty, all non-control files are used.
#' @param filenameTemplate Template for the names of the exported files.
#' @param populationIds Population IDs to export, as a character vector or
#'   list. List elements may be \code{byName} expressions, which are matched
#'   against population names. If omitted, the ungated population is exported.
#' @param preSubsampleN Number of events to subsample before gating.
#' @param postSubsampleN Number of events to subsample after gating.
#' @param preSubsampleP Fraction of events to subsample before gating.
#' @param postSubsampleP Fraction of events to subsample after gating.
#' @param seed Seed for deterministic subsampling.
#' @param addEventNumber Whether to add an event number corresponding to the
#'   index in the source file.
#' @export
#' @examples
#' \dontrun{
#' exportPopulationsToNewExperiment(
#'   experimentId,
#'   compensationId,
#'   name = "Exported populations",
#'   populationIds = c("populationId1", "populationId2")
#' )
#' }
exportPopulationsToNewExperiment <- function(
    experimentId,
    compensationId,
    name = NULL,
    fcsFileIds = NULL,
    filenameTemplate = NULL,
    populationIds = NULL,
    preSubsampleN = NULL,
    postSubsampleN = NULL,
    preSubsampleP = NULL,
    postSubsampleP = NULL,
    seed = NULL,
    addEventNumber = FALSE) {
  stopIfParamIsNull(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  stopIfParamIsNull(compensationId)
  compensationId <- lookupByName(
    paste0("/api/v1/experiments/", experimentId, "/compensations"),
    compensationId
  )

  if (!is.null(fcsFileIds)) {
    fcsFileIds <- unlist(
      lapply(as.list(fcsFileIds), function(id) {
        lookupByName(
          paste0("/api/v1/experiments/", experimentId, "/fcsfiles"),
          id,
          "filename"
        )
      }),
      recursive = TRUE,
      use.names = FALSE
    )
  }
  if (!is.null(populationIds)) {
    populationIds <- unlist(
      lapply(as.list(populationIds), function(id) {
        lookupByName(
          paste0("/api/v1/experiments/", experimentId, "/populations"),
          id
        )
      }),
      recursive = TRUE,
      use.names = FALSE
    )
  }

  body <- list(compensationId = jsonlite::unbox(compensationId))
  optional <- list(
    name = if (is.null(name)) NULL else jsonlite::unbox(name),
    fcsFileIds = fcsFileIds,
    filenameTemplate = if (is.null(filenameTemplate)) NULL else jsonlite::unbox(filenameTemplate),
    populationIds = populationIds,
    preSubsampleN = if (is.null(preSubsampleN)) NULL else jsonlite::unbox(preSubsampleN),
    postSubsampleN = if (is.null(postSubsampleN)) NULL else jsonlite::unbox(postSubsampleN),
    preSubsampleP = if (is.null(preSubsampleP)) NULL else jsonlite::unbox(preSubsampleP),
    postSubsampleP = if (is.null(postSubsampleP)) NULL else jsonlite::unbox(postSubsampleP),
    seed = if (is.null(seed)) NULL else jsonlite::unbox(seed),
    addEventNumber = if (is.null(addEventNumber)) NULL else jsonlite::unbox(addEventNumber)
  )
  body <- c(body, optional[!vapply(optional, is.null, logical(1))])
  body <- jsonlite::toJSON(body, null = "null", digits = NA)
  basePost(
    paste0("/api/v1/experiments/", experimentId, "/populationsToNew"),
    body
  )
}
