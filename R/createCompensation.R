#' Create compensation
#'
#' Creates a new compensation.
#'
#' @param experimentId The ID of the experiment of a \code{byName} expression.
#' @param name Name of the compensation matrix.
#' @param spillMatrix The spillover matrix. This can either be a 1-dimensional
#' vector of size \code{length(channels) ^ 2}, or a
#' \code{matrix}.
#' @param channels The channel names. Not required if \code{spillMatrix} is a
#' \code{matrix} with dimnames set.
#' @export
#' @examples
#' \dontrun{
#' createCompensation(
#'   experimentId,
#'   name="Comp 1",
#'   channels=c("Ax488-A", "PE-A"),
#'   spillMatrix=c(1, 0,
#'                 0, 1)
#' )
#' # same as:
#' createCompensation(
#'   experimentId,
#'   name="Comp 1",
#'   spillMatrix=matrix(
#'     c(1, 0,
#'       0, 1),
#'     nrow=2,
#'     ncol=2,
#'     byrow=TRUE,
#'     dimnames=list(c("Ax488-A", "PE-A"), c("Ax488-A", "PE-A"))
#'   )
#' )
#' }
createCompensation <- function(experimentId, name, spillMatrix, channels = NULL) {
  checkDefined(experimentId)
  experimentId <- lookupByName("/api/v1/experiments", experimentId)

  if (is.null(channels)) {
    if (!is.matrix(spillMatrix) || is.null(dimnames(spillMatrix)))
      stop("spillMatrix must be a matrix with dimnames if channels is not specified.")

    channels = dimnames(spillMatrix)[[1]]
  }

  if (is.matrix(spillMatrix))
    spillMatrix = as.vector(t(spillMatrix))

  body <- jsonlite::toJSON(
    list(name=name, channels=channels, spillMatrix=spillMatrix),
    null = "null",
    auto_unbox = TRUE
  )
  r <- basePost(paste0("/api/v1/experiments/", experimentId, "/compensations"),
                body,
                list(),
                simplifyDataFrame = FALSE)
  formatCompensationResponse(r)
}
