# Returns _id of population,using either of the parentPopulationId or parentPopulation params.
parsePopulationArgs <- function(parentPopulationId, parentPopulation, experimentId) {
  if (!is.null(parentPopulationId) && !is.null(parentPopulation)) {
    stop("Please specify only one of 'parentPopulation' or 'parentPopulationId'.")
  }

  if (!is.null(parentPopulation)) { # attempt to lookup by name
    pops <- getPopulations(experimentId, params = list(
      query = sprintf("eq(name, \"%s\")", parentPopulation)
    ))
    if (!is.data.frame(pops)) {
      stop(sprintf("Population with the name '%s' does not exist in the experiment.", parentPopulation))
    }
    if (nrow(pops) > 1) {
      stop(sprintf(paste0(
        "More than one population with the name '%s' exists in the experiment. ",
        "Cannot find unambiguous parent population."
      ), parentPopulation))
    }
    parentPopulationId <- pops$`_id`
  }

  return(parentPopulationId)
}

# Assigns fcsFileId to body, using either of the fcsFileId or fcsFile params.
parseFcsFileArgs <- function(body, tailoredPerFile, fcsFileId, fcsFile, experimentId) {
  body <- c(body, list(tailoredPerFile = jsonlite::unbox(tailoredPerFile)))

  if (!tailoredPerFile) {
    return(body)
  } # not tailored
  if (is.null(fcsFileId) && is.null(fcsFile)) {
    return(body)
  } # global tailored gate

  if (!is.null(fcsFileId) && !is.null(fcsFile)) {
    stop("Please specify only one of 'fcsFile' or 'fcsFileId'.")
  }

  if (!is.null(fcsFile)) { # attempt to lookup by name
    files <- getFcsFiles(experimentId, params = list(
      query = sprintf("eq(filename, \"%s\")", fcsFile),
      fields = "+_id"
    ))
    if (!is.data.frame(files)) {
      stop(sprintf("FCS file with the name '%s' does not exist in the experiment.", fcsFile))
    }
    if (nrow(files) > 1) {
      stop(sprintf(paste0(
        "More than one FCS file with the name '%s' exists in the experiment. ",
        "Cannot find unambiguous file to create tailored gate."
      ), fcsFile))
    }
    fcsFileId <- files$`_id`
  }

  body <- c(body, list(fcsFileId = jsonlite::unbox(fcsFileId)))

  return(body)
}

# Assigns common properties to the body, then makes the request.
commonGateCreate <- function(body, name, gid,
                             experimentId,
                             parentPopulationId, parentPopulation,
                             tailoredPerFile, fcsFileId, fcsFile,
                             createPopulation) {
  checkDefined(experimentId)

  parentPopulationId <- parsePopulationArgs(
    parentPopulationId, parentPopulation,
    experimentId
  )

  body <- c(body, list(
    parentPopulationId = jsonlite::unbox(parentPopulationId),
    name = jsonlite::unbox(name),
    gid = jsonlite::unbox(gid)
  ))

  body <- parseFcsFileArgs(body, tailoredPerFile, fcsFileId, fcsFile, experimentId)

  body <- jsonlite::toJSON(body, null = "null", digits = NA)
  path <- paste0("/api/v1/experiments/", experimentId, "/gates")

  if (createPopulation) {
    gateResp <- basePost(path, body, params = list("createPopulation" = TRUE))
    return(gateResp)
  } else {
    gateResp <- basePost(path, body, list())
    return(list(gate = gateResp))
  }
}

# Assigns common properties to body of compound gate, then makes the request.
compoundGateCreate <- function(body, names, gid, gids,
                               experimentId,
                               parentPopulationId, parentPopulation,
                               tailoredPerFile, fcsFileId, fcsFile,
                               createPopulation) {
  checkDefined(experimentId)

  parentPopulationId <- parsePopulationArgs(
    parentPopulationId, parentPopulation,
    experimentId
  )

  body <- c(body, list(
    parentPopulationId = jsonlite::unbox(parentPopulationId),
    gid = jsonlite::unbox(gid)
  ))

  body <- parseFcsFileArgs(body, tailoredPerFile, fcsFileId, fcsFile, experimentId)

  body <- jsonlite::toJSON(body, null = "null", digits = NA)
  path <- paste0("/api/v1/experiments/", experimentId, "/gates")

  if (createPopulation) {
    gateResp <- basePost(path, body, params = list("createPopulation" = TRUE))
    return(gateResp)
  } else {
    gateResp <- basePost(path, body, list())
    return(list(gate = gateResp))
  }
}
