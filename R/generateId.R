#' Generate ID
#'
#' Generates an ID string suitable for use as a \code{gid} when creating gates.
#'
#' @return an ID string
#' @export
#' @examples
#' generateId()
generateId <- (function() {
  seg1 <- floor(runif(1, min = 0, max = 0xFFFFFF))
  seg2 <- floor(runif(1, min = 0, max = 0xFFFF))
  incr <- floor(runif(1, min = 0, max = 0xFFFFFF))

  function() {
    time <- as.integer(Sys.time())
    incr <<- incr + 1
    if (incr > 0xFFFFFF) {
      incr <- 0
    }

    timeStr <- format(as.hexmode(time), width = 8)
    seg1Str <- format(as.hexmode(seg1), width = 6)
    seg2Str <- format(as.hexmode(seg2), width = 4)
    incrStr <- format(as.hexmode(incr), width = 6)

    id <- paste(timeStr, seg1Str, seg2Str, incrStr, sep = "")
    return(id)
  }
})()
