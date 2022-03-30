#' Get ellipse points
#'
#' Returns 8 perimeter points at angular increments of pi / 4.
#'
#' @param x X-coordinate of the ellipse center.
#' @param y Y-coordinate of the ellipse center.
#' @param angle Angle of the ellipse.
#' @param major Length of the major axis.
#' @param minor Length of the minor axis.
#' @noRd
getEllipsePoints <- function(angle, major, minor, x, y) {
  points <- list()
  phi <- 0

  while (phi < (pi * 2)) {
    p <- c(
      x + major * cos(phi) * cos(angle) - minor * sin(phi) * sin(angle),
      y + major * cos(phi) * sin(angle) + minor * sin(phi) * cos(angle)
    )

    points[length(points) + 1] <- list(p)
    phi <- phi + (pi / 4)
  }
  points
}
