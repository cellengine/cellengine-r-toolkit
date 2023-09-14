#' Get statistics
#'
#' Retrieves statistics from FCS files, returning the results as a data frame.
#'
#' The quick syntax accepts FCS files and populations by names. This is often
#' easier to use, but is slower to execute because additional API requests and
#' validation logic must be run. Thus, for performance-critical applications,
#' provide IDs instead of names in the \code{fcsFileIds}, \code{populationIds}
#' and \code{percentOf} parameters. IDs are also guaranteed to reference unique
#' files and populations, while names may be non-unique, in which case an error
#' will be raised.
#' Specify neither \code{fcsFileIds} nor {fcsFiles} to calculate statistics for
#' all non-control FCS files.
#'
#' @param experimentId ID of experiment.
#' @param fcsFileIds IDs of FCS files. If specified, do not specify \code{fcsFiles}.
#' @param fcsFiles Names of FCS files. An attempt will be made to find the files
#'   by name. If zero or more than one file exists with a given filename, an
#'   error will be thrown. If specified, do not specify \code{fcsFileIds}.
#' @param channels Names of channels (for statistic types "mean", "median",
#'   "quantile", "stddev", "cv" and "mad"). Use channel short names, not reagents.
#' @param statistics Statistics to export. Valid options: "mean", "median",
#'   "quantile", "stddev", "cv", "eventcount", "percent", "mad".
#' @param compensationId Compensation to apply. May be an ID,
#'   \code{cellengine::UNCOMPENSATED} or \code{cellengine::FILE_INTERNAL}.
#' @param populationIds IDs of populations. If specified, do not specify
#'   \code{populations}.
#' @param populations Names of populations. An attempt will be made to find the
#'   population by name. If zero or more than one population exists with the
#'   name, an error will be thrown. If specified, do not specify
#'   \code{populationIds}.
#' @param scaleSetId Optional ID of scale set. If omitted and if a single
#'   scale set exists in the experiment, that scale set will be used.
#' @param q Quantile to calculate for "quantile" statistic, in the range 0 to 1.
#' @param percentOf Single population ID or name, or list of population IDs or
#'   names the same length as \code{populationIds} parameter.
#'
#'   \itemize{
#'     \item If omitted or the string "PARENT", then the percent of parent will
#'       be calculated for each population.
#'
#'     \item If a single ID or name is provided, then the percent of that
#'        population will be calculated for each population specified in
#'        \code{populations} or \code{populationIds} (useful for calculating
#'        e.g. percent of singlets or leukocytes).
#'
#'     \item If an array of IDs or names is provided, then the percent of each
#'        of those populations will be calculated.
#'   }
#'
#'   In the latter two cases, if a name or list of names instead of IDs are
#'   provided, an attempt will be made to find those populations by name. IDs
#'   are detected as matching a 24-character string comprised of the characters
#'   \code{A-Fa-f0-9}.
#' @param includeAnnotations Includes FCS file annotations in the returned data
#'   frame.
#'
#' @return Statistics as a data frame, including file annotations and
#'   information about the statistics such as the channel name and population.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Quick syntax, using population names and file names instead of IDs:
#' fcsFiles <- c("file1.fcs")
#' channels <- c("SSC-A", "YG780/60-A")
#' statistics <- c("median", "mean", "quantile", "percent")
#' populations <- c("Leukocytes")
#' stats <- getStatistics(experimentId,
#'   fcsFiles = fcsFiles, channels = channels,
#'   statistics = statistics, populations = populations, q = 0.95,
#'   compensationId = cellengine::FILE_INTERNAL
#' )
#' # Returns a data.frame of statistics, including the file annotations.
#' # Because percentOf is not specified, "percent" will be percent of parent.
#'
#' # Explicit syntax, using IDs instead of population and file names, and
#' # specifying a scaleSetId:
#' fcsFileIds <- c("9ab5c6d7a8cf24a5c4f9a6c2")
#' statistics <- c("percent")
#' populationIds <- c("9ab5c6d7a8cf24a5c4f9face")
#' scaleSetId <- "9ab5c6d7a8cf24a5c4f9fdde"
#' stats <- getStatistics(experimentId,
#'   fcsFileIds = fcsFileIds,
#'   statistics = statistics, populationIds = populationIds,
#'   compensationId = cellengine::UNCOMPENSATED, scaleSetId = scaleSetId
#' )
#'
#' # Percent of ungated:
#' getStatistics(experimentId,
#'   fcsFileIds = fcsFileIds, statistics = statistics,
#'   populationIds = populationIds, compensationId = cellengine::UNCOMPENSATED,
#'   percentOf = cellengine::UNGATED
#' )
#' }
getStatistics <- function(experimentId,
                          fcsFileIds = NULL, fcsFiles = NULL,
                          channels = c(),
                          statistics,
                          compensationId,
                          populationIds = NULL, populations = NULL,
                          scaleSetId = NULL,
                          q = 0.5,
                          percentOf = NULL,
                          includeAnnotations = TRUE) {
  checkDefined(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)

  fcsFileIds <- lookupFilesByName(experimentId, fcsFileIds, fcsFiles)
  populationIds <- lookupPopulationsByName(experimentId, populationIds, populations)

  # statistics argument
  allowedStatistics <- c("mean", "median", "quantile", "stddev", "cv", "eventcount", "percent", "mad")
  statsDiff <- setdiff(tolower(statistics), allowedStatistics)
  if (length(statsDiff) > 0) {
    stop(sprintf("Statistics [%s] are not allowed.", paste0(statsDiff, collapse = ", ")))
  }

  # TODO remove, this was removed from the API
  scaleSetId <- verifyScaleSets(experimentId, scaleSetId)

  # percentOf argument
  if (length(percentOf) > 1 && length(percentOf) != length(populationIds)) {
    stop(paste0(
      "If an array is specified for 'percentOf', it must have the ",
      "same length as 'populations' or 'populationIds'."
    ))
  }
  percentofNonIds <- !grepl("^[A-Fa-f0-9]{24}$|^$", percentOf) # not ID or UNGATED
  if (any(percentofNonIds)) { # one or more values are not IDs; lookup by name
    queryPopulations <- percentOf[percentofNonIds]
    quotedQueryPopulations <- paste0(shQuote(queryPopulations, type = "cmd"), collapse = ",")
    serverPops <- getPopulations(experimentId, params = list(
      fields = "+name",
      query = sprintf("in(name, [%s])", quotedQueryPopulations)
    ))
    if (!is.data.frame(serverPops)) { # zero-length results are not data.frames
      pkg.env$lastError <- queryPopulations
      stop(sprintf(
        "%i population(s) were not found. Call getErrorInfo() for a list of missing percentOf populations.",
        length(queryPopulations)
      ))
    }
    diff <- setdiff(queryPopulations, serverPops$name) # populations absent from server
    if (length(diff) != 0) {
      pkg.env$lastError <- diff
      stop(sprintf(
        "%i population(s) were not found. Call getErrorInfo() for a list of missing percentOf populations.",
        length(diff)
      ))
    }
    if (anyDuplicated(serverPops$name)) { # populations with non-unique names
      pkg.env$lastError <- serverPops$name[duplicated(serverPops$name)]
      stop(paste0(
        "One or more populations have the same names and cannot be selected unambiguously. ",
        "Call getErrorInfo() for a list of duplicate names."
      ))
    }
    # Finally, pull out _ids
    if (all(percentofNonIds)) { # fast path: all names
      percentOf <- serverPops$`_id`
    } else { # slow path: mixed names and IDs
      percentOf <- sapply(percentOf, function(v) {
        idx <- match(v, serverPops$name)
        if (is.na(idx)) {
          return(v)
        } # with above checks, already an ID
        serverPops[idx]$`_id`
      })
    }
  }

  body <- list(
    fcsFileIds = fcsFileIds,
    statistics = statistics,
    populationIds = populationIds,
    compensationId = jsonlite::unbox(compensationId),
    q = jsonlite::unbox(q),
    scaleSetId = jsonlite::unbox(scaleSetId),
    format = jsonlite::unbox("json"),
    annotations = jsonlite::unbox(includeAnnotations)
  )

  if (!is.null(percentOf)) {
    if (length(percentOf) == 1) percentOf <- jsonlite::unbox(percentOf)
    body <- c(body, list(percentOf = percentOf))
  }

  if (length(channels) > 0) {
    body <- c(body, list(channels = channels))
  }

  path <- paste0("/api/v1/experiments/", experimentId, "/bulkstatistics")
  body <- jsonlite::toJSON(body, null = "null", digits = NA)
  basePost(path, body, list())
}

lookupFilesByName <- function(experimentId, fcsFileIds, fcsFiles) {
  if (!is.null(fcsFileIds) && !is.null(fcsFiles)) {
    stop("Please specify only one of 'fcsFiles' or 'fcsFileIds'.")
  }
  if (is.null(fcsFileIds) && !is.null(fcsFiles)) {
    queryFilenames <- paste0(shQuote(fcsFiles, type = "cmd"), collapse = ",")
    serverFiles <- getFcsFiles(experimentId, params = list(
      fields = "+filename",
      query = sprintf("in(filename, [%s])", queryFilenames)
    ))
    if (!is.data.frame(serverFiles)) { # zero-length results are not data.frames
      pkg.env$lastError <- fcsFiles
      stop(sprintf(
        "%i file(s) were not found. Call getErrorInfo() for a list of missing files.",
        length(fcsFiles)
      ))
    }
    diff <- setdiff(fcsFiles, serverFiles$filename) # files absent from server
    if (length(diff) != 0) {
      pkg.env$lastError <- diff
      stop(sprintf(
        "%i file(s) were not found. Call getErrorInfo() for a list of missing files.",
        length(diff)
      ))
    }
    if (anyDuplicated(serverFiles$filename)) { # files with non-unique names
      pkg.env$lastError <- serverFiles$filename[duplicated(serverFiles$filename)]
      stop(paste0(
        "One or more files have the same filenames and cannot be selected unambiguously. ",
        "Call getErrorInfo() for a list of duplicate filenames."
      ))
    }
    fcsFileIds <- serverFiles$`_id`
  }
  return(fcsFileIds)
}

lookupPopulationsByName <- function(experimentId, populationIds, populations) {
  if (!is.null(populationIds) && !is.null(populations)) {
    stop("Please specify only one of 'populations' or 'populationIds'.")
  }
  if (is.null(populationIds) && !is.null(populations)) { # lookup populations by name
    queryPopulations <- paste0(shQuote(populations, type = "cmd"), collapse = ",")
    serverPops <- getPopulations(experimentId, params = list(
      fields = "+name",
      query = sprintf("in(name, [%s])", queryPopulations)
    ))
    if (!is.data.frame(serverPops)) { # zero-length results are not data.frames
      pkg.env$lastError <- populations
      stop(sprintf(
        "%i population(s) were not found. Call getErrorInfo() for a list of missing populations.",
        length(populations)
      ))
    }
    diff <- setdiff(populations, serverPops$name) # populations absent from server
    if (length(diff) != 0) {
      pkg.env$lastError <- diff
      stop(sprintf(
        "%i population(s) were not found. Call getErrorInfo() for a list of missing populations.",
        length(diff)
      ))
    }
    if (anyDuplicated(serverPops$name)) { # populations with non-unique names
      pkg.env$lastError <- serverPops$name[duplicated(serverPops$name)]
      stop(paste0(
        "One or more populations have the same names and cannot be selected unambiguously. ",
        "Call getErrorInfo() for a list of duplicate names."
      ))
    }
    populationIds <- serverPops$`_id`
  }
  return(populationIds)
}

verifyScaleSets <- function(experimentId, scaleSetId) {
  if (is.null(scaleSetId)) {
    serverScaleSets <- getScaleSets(experimentId, params = list(fields = "+_id"))
    if (!is.data.frame(serverScaleSets)) { # zero-length results are not data.frames
      stop("No scalesets found in experiment.")
    }
    if (nrow(serverScaleSets) > 1) {
      stop("More than one scaleset exists in experiment. Please specify a scaleSetId to select one.")
    }
    scaleSetId <- serverScaleSets$`_id`
  }
  return(scaleSetId)
}
