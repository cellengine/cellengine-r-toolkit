#' Get default compensation
#'
#' Initially (until a value is selected in the UI), experiments have an
#' \code{activeCompensation} value of \code{NULL}, which means "use a default
#' value that depends on the experiment state," as follows:
#' \itemize{
#'   \item If per-file compensations are enabled, then the default is
#'     \code{PER_FILE}.
#'   \item If all files have a file-internal compensation matrix, then the
#'     default is \code{FILE_INTERNAL}.
#'   \item Otherwise, the default is \code{UNCOMPENSATED}.
#' }
#'
#' This function provides that logic.
#'
#' @param experimentId ID of experiment.
#'
#' @export
getDefaultCompensation <- function(experimentId) {
  experiment <- getExperiment(experimentId)

  if (experiment$perFileCompensationsEnabled) return(PER_FILE)

  fcsFiles <- getFcsFiles(
    experiment$`_id`,
    params=list("files" = "+hasFileInternalComp,-_id")
  )

  hasFileInternalComp <- vapply(
    fcsFiles,
    function(fcsFile) isTRUE(fcsFile$hasFileInternalComp),
    logical(1)
  )
  if (length(hasFileInternalComp) > 0 && all(hasFileInternalComp)) {
    return(FILE_INTERNAL)
  }

  UNCOMPENSATED
}
