#' Covert covariance matrix to ellipse parameters
#'
#' Returns list("major" = major, "minor" = minor, "angle" = angle)
#'
#' @param covar Covariance matrix
#' @noRd
covarToParameters <- function(covar) {
  eigens <- eigen(covar, symmetric = TRUE)
  eigenvalues <- eigens$values

  a <- covar[1]
  b <- covar[2]
  c <- covar[4]

  major <- sqrt(eigenvalues[1])
  minor <- sqrt(eigenvalues[2])

  if ((b == 0) && a >= c) {
    alpha <- 0
  } else if ((b == 0) && a < c) {
    alpha <- pi / 2
  } else {
    alpha <- atan2(eigenvalues[1] - a, b)
  }

  return(list("major" = major, "minor" = minor, "angle" = alpha))
}
