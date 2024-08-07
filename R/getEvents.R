#' Get FCS File Events
#'
#' Retrieves events from an FCS file or gated population as either an FCS or
#' TSV file. Optionally transfers directly to S3-compatible storage.
#'
#' Tip: FCS files are more compact than TSV files. Use that format for faster
#' downloads. Use a library such as flowCore to parse FCS files.
#'
#' @param experimentId ID of experiment or a \code{byName} expression.
#' @param fcsFileId ID of FCS file or a \code{byName} expression.
#' @param populationId Optional ID of population or a \code{byName} expression,
#'   for gated events.
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
#' @param destination Optional destination for the file.
#'
#'   If omitted, the result will be returned as a binary blob (if \code{format}
#'   is "FCS") or a data.frame (if \code{format} is "TSV").
#'
#'   If a string, the result will be written to disk.
#'
#'   If a \code{list} with a \code{host} value, transfer the file to
#'   S3-compatible storage. S3 transfers occur "in the cloud," directly from
#'   CellEngine to the S3 provider, without transferring the file to your local
#'   computer. S3 transfer specifications accept the following:
#'
#'   \itemize{
#'     \item \code{host} Required. The full S3 host, including the bucket and
#'       region as applicable. For example, for AWS, this would look like
#'       \code{"mybucket.s3.us-east-2.amazonaws.com"}.
#'
#'     \item \code{path} The path prefix for the object. This is concatenated
#'       directly with the file name. Use a trailing "/" to specify a
#'       "directory" in S3. For example, if the path is \code{"/Study001/"} and
#'       the file is called \code{"Specimen01.fcs"}, the object key will be
#'       \code{"/Study001/Specimen01.fcs"}. Use no trailing slash to specify a
#'       file prefix. For example, if the path is \code{"/transfer1_"}, the
#'       object key will be \code{"/transfer1_Specimen01.fcs"}. Note that S3
#'       object keys must start with "/"; a path of "" will result in an
#'       \code{Invalid URI} response. Defaults to "/".
#'
#'     \item \code{accessKey} Required for private buckets. The S3 access key.
#'       Must be associated with an account with appropriate S3 permissions
#'       (e.g. \code{PutObject}).
#'
#'     \item \code{secretKey} Required for private buckets. The S3 secret key.
#'
#'     \item \code{headers} Custom headers to include in the S3 request. Common
#'       examples are \code{x-amz-storage-class}/\code{x-goog-storage-class} and
#'       \code{x-amz-server-side-encryption}. Some headers, such as
#'       \code{Content-Length}, \code{Content-MD5} and \code{Content-Type},
#'       cannot be modified. Refer to https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html
#'       for AWS header documentation and https://cloud.google.com/storage/docs/migrating#custommeta
#'       regarding compatibility with Google Cloud Platform.
#'   }
#'
#' @param overwrite Optional, if a destination is a string, allows destination
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
                      # originalQ = FALSE, TODO
                      format = "FCS",
                      destination = NULL,
                      overwrite = FALSE,
                      subsampling = list(),
                      addEventNumber = FALSE) {
  stopIfParamIsNull(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)
  stopIfParamIsNull(fcsFileId)
  fcsFileId <- lookupByName(paste0("/api/v1/experiments/", experimentId, "/fcsfiles"), fcsFileId, "filename")

  if (is.null(compensation) && !is.null(populationId)) {
    stop("'compensation' parameter is required for gated populations.")
  } else if (is.null(compensation) && isTRUE(compensatedQ)) {
    stop("'compensation' parameter is required when returning compensated data.")
  } else if (is.null(compensation)) {
    # compensation=null in URL query currently causes API error
    compensation = UNCOMPENSATED
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

  urlPath <- paste0("/api/v1/experiments/", experimentId, "/fcsfiles/", fcsFileId, ".", format)
  fullURL <- paste0(pkg.env$baseURL, urlPath)

  params <- list(
    populationId = populationId,
    compensationId = compensation,
    compensatedQ = compensatedQ,
    headers = headerQ,
    addEventNumber = addEventNumber
  )

  params <- coerceParameters(params)

  subsamplingParams <- Filter(Negate(is.null), subsampling[
    c("preSubsampleN", "preSubsampleP", "postSubsampleN", "postSubsampleP", "seed")
  ])

  params <- c(params, subsamplingParams)

  if (is.null(destination)) {
    response <- httr::GET(fullURL,
                          query = params,
                          httr::user_agent(ua),
                          httr::add_headers(.headers = pkg.env$auth))
    httr::stop_for_status(response)
    if (format == "TSV") {
      content <- httr::content(response, "text")
      content <- utils::read.table(text = content, header = headerQ, sep = "\t", check.names = F)
    } else {
      content <- httr::content(response, "raw")
    }
    return(content)
  } else if (is.list(destination) && "host" %in% names(destination)) {
    body <- list(host = jsonlite::unbox(destination$host))
    if ("path" %in% names(destination))
      body <- c(body, list(path = jsonlite::unbox(destination$path)))
    if ("accessKey" %in% names(destination))
      body <- c(body, list(accessKey = jsonlite::unbox(destination$accessKey)))
    if ("secretKey" %in% names(destination))
      body <- c(body, list(secretKey = jsonlite::unbox(destination$secretKey)))
    if ("headers" %in% names(destination))
      body <- c(body, list(headers = destination$headers))
    body <- jsonlite::toJSON(list(dest = body), null = "null", auto_unbox=TRUE)
    basePost(urlPath, body, params)
  } else if (is.character(destination)) {
    response <- httr::GET(fullURL,
                          query = params,
                          httr::user_agent(ua),
                          httr::write_disk(destination, overwrite),
                          httr::add_headers(.headers = pkg.env$auth))
    httr::stop_for_status(response)
  } else {
    stop("'destination' must be NULL, an S3 transfer specification list, or a file path.")
  }
}
