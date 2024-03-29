% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCompensations.R
\name{getCompensations}
\alias{getCompensations}
\title{Get compensations}
\usage{
getCompensations(experimentId, params = list())
}
\arguments{
\item{experimentId}{ID of experiment or a \code{byName} expression.}

\item{params}{Optional query parameters.}
}
\description{
Retrieves the list of compensations in an experiment.
}
\examples{
\dontrun{
getCompensations(experimentId)
getCompensations(experimentId, params = list("limit" = "5"))
}
}
