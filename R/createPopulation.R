#' Create population
#'
#' Creates a population.
#'
#' @param experimentId The ID of the experiment to which to add the population.
#' @param name The name of the population.
#' @param gates The list of gates comprising the population. This should be in
#'   the form \code{list(`$and` = c("gid1", "gid2"))}, where \code{"gid1"} and
#'   \code{"gid2"} are the Group IDs (\code{gid}s) of the gates. Any combination
#'   of \code{$and}, \code{$or}, \code{$not} and \code{$xor} can be used;
#'   however, only populations with the form \code{list(`$and` = c(...gates))}
#'   can be displayed in the Web UI at this time. Alternatively, this may be a
#'   JSON string (may be easier than constructing the correct R expression).
#' @param terminalGateGid The Group ID (\code{gid}) of the gate that
#'   differentiates this population from its parent. Because complex populations
#'   that deviate from the \code{list(`$and` = c(...gates))} form cannot currently
#'   be displayed in the Web UI, those populations may set this to \code{NULL}.
#'   In the future, this will be required to be a list of GIDs for those
#'   populations.
#' @param parentId ID of the parent population. Use \code{NULL} for the
#'   "ungated" population.
#' @export
#' @examples
#' \dontrun{
#' gid1 <- "59262d84b1a1fc1193f12b0e"
#' createPopulation(experimentId, "Singlets", list(`$and` = c(gid1)), gid1)
#' }
createPopulation <- function(experimentId, name, gates, terminalGateGid,
                             parentId = NULL) {
  stopIfParamIsNull(experimentId)

  if (!is.character(gates)) gates <- jsonlite::toJSON(gates)

  body <- jsonlite::toJSON(list(
    name = jsonlite::unbox(name),
    gates = jsonlite::unbox(gates),
    terminalGateGid = jsonlite::unbox(terminalGateGid),
    parentId = jsonlite::unbox(parentId)
  ), null = "null", digits = NA)

  basePost(paste0("/api/v1/experiments/", experimentId, "/populations"), body, list())
}
