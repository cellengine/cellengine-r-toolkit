parseFcsFileArgs <- function(body, tailoredPerFile, fcsFileId, experimentId) {
  if (is.null(fcsFileId)) {
    # not tailored, or global tailored gate
    body <- c(body, list(
      tailoredPerFile = jsonlite::unbox(isTRUE(tailoredPerFile)),
      fcsFileId = jsonlite::unbox(NULL)
    ))
    return(body)
  }

  # tailored per file
  fcsFileId <- lookupByName(
    paste0("/api/v1/experiments/", experimentId, "/fcsfiles"),
    fcsFileId,
    "filename"
  )
  body <- c(body, list(
    tailoredPerFile = jsonlite::unbox(TRUE),
    fcsFileId = jsonlite::unbox(fcsFileId)
  ))
  return(body)
}

# Assigns common properties to the body, then makes the request.
commonGateCreate <- function(body, gid,
                             experimentId,
                             parentPopulationId,
                             tailoredPerFile, fcsFileId,
                             createPopulation,
                             name = NULL, names = NULL) {
  checkDefined(experimentId)
  experimentId <- lookupByName("experiments", experimentId)

  # name or names
  if (is.null(name))
    body <- c(body, list(names = names))
  else
    body <- c(body, list(name = jsonlite::unbox(name)))

  body <- c(body, list(gid = jsonlite::unbox(gid)))

  body <- parseFcsFileArgs(body, tailoredPerFile, fcsFileId, experimentId)

  body <- jsonlite::toJSON(body, null = "null", digits = NA)
  path <- paste0("/api/v1/experiments/", experimentId, "/gates")

  if (createPopulation) {
    parentPopulationId <- lookupByName(
      paste0("/api/v1/experiments/", experimentId, "/populations"),
      parentPopulationId
    )
    if (parentPopulationId == UNGATED)
      parentPopulationId = NULL
    gateResp <- basePost(
      path,
      body,
      params = list(
        createPopulation = TRUE,
        parentPopulationId = jsonlite::unbox(parentPopulationId)
      )
    )
    return(gateResp)
  } else {
    gateResp <- basePost(path, body)
    return(list(gate = gateResp))
  }
}
