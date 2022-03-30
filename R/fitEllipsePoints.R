#' Fit ellipse points
#'
#' Calculate the minimum volume enclosing ellipsoid for a set of points
#'
#' Uses the Khachiyan Algorithm to find the the minimum volume enclosing
#' ellipsoid (MVEE) of a set of points. In two dimensions, this is just
#' the bounding ellipse (this function is limited to two dimensions).
#' Adapted from https://stat.ethz.ch/pipermail/r-help/2011-March/272997.html
#'
#' @param xy a two-column data frame containing x and y coordinates.
#' @param tolerance a tolerance value (default = 0.005).
#' @param max.iter Maximum algorithm iterations.
#'
#' @return A named list containing:
#'  - "covar", the 2x2 covariance matrix of the ellipse
#'  - "x", the x-coordinate of the ellipse center
#'  - "y", the y-coordinate of the ellipse center
#'  - "major", the length of the major axis
#'  - "minor", the length of the minor axis
#'  - "angle", the angle of rotation
#' @noRd
fitEllipsePoints <- function(xy, tolerance = 1e-20, max.iter = 1000) {
  if (ncol(xy) != 2) {
    stop("xy must be a two-column data frame")
  }
  n <- nrow(xy)
  d <- ncol(xy)
  if (n <= d) stop("There cannot be more columns than rows")

  ## Add a column of 1s to the (n x 2) matrix xy - so it is now (n x 3)
  Q <- t(cbind(xy, rep(1, n))) # nolint

  ## Initialize
  count <- 1
  err <- 1
  u <- rep(1 / n, n)
  ## Khachiyan Algorithm
  while (err > tolerance) {
    X <- Q %*% diag(u) %*% t(Q) # nolint
    M <- diag(t(Q) %*% solve(X) %*% Q) # nolint
    maximum <- max(M)
    j <- which(M == maximum)
    step_size <- (maximum - d - 1) / ((d + 1) * (maximum - 1))
    new_u <- (1 - step_size) * u
    new_u[j] <- new_u[j] + step_size
    err <- sqrt(sum((new_u - u)^2))
    count <- count + 1
    if (count > max.iter) {
      message <- paste(
        "Iterated ", max.iter, " times and
       still can't find the bounding
       ellipse. \n Either increase the
       tolerance or the maximum number of
       iterations. \n",
        sep = ""
      )
      stop(message)
    }
    u <- new_u
  }
  # Put the elements of the vector u into the diagonal of a matrix
  U <- diag(u) # nolint
  # Take the transpose of xy
  P <- t(xy) # nolint
  # Compute the center, adding back the shifted values
  c <- as.vector((P %*% u))
  x <- c[1]
  y <- c[2]
  # Compute the A-matrix
  A <- (1 / d) * solve(P %*% U %*% t(P) - (P %*% u) %*% t(P %*% u)) # nolint
  # covar is the covariance matrix of the ellipse
  covar <- solve(A)
  # Find the Eigenvalues of matrix A which will be used to get the major and minor axes
  A.eigen <- eigen(A) # nolint
  # Calculate the length of the semi-major and semi-minor axes
  # from the Eigenvalues of A.
  semi.axes <- sort(1 / sqrt(A.eigen$values), decreasing = TRUE) # nolint
  major <- semi.axes[1]
  minor <- semi.axes[2]
  # Calculate the rotation angle from the first Eigenvector
  alpha <- atan2(A.eigen$vectors[2, 1], A.eigen$vectors[1, 1]) - pi / 2


  params <- covarToParameters(covar)

  list("covar" = covar, "x" = x, "y" = y, "major" = params$major, "minor" = params$minor, angle = params$angle)
}
