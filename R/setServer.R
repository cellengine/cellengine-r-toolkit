#' Set server address
#'
#' Sets the address of the server to which to connect.
#'
#' @param host Address of API server.
#' @export
#' @examples
#' setServer("https://mycompany.cellengine.com")
setServer <- function(host) {
  host <- sub("/$", "", host)
  if (!grepl("^https://[0-9a-zA-Z\\.\\-]+\\.[0-9a-zA-Z\\.]+$", host, ignore.case = TRUE)) {
    stop("Argument 'host' must be a valid HTTPS url")
  }
  pkg.env$baseURL <- paste(host, "/api/v1", sep = "")
}
