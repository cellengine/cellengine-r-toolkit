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
  pkg.env$baseURL <- paste(host, "/api/v1", sep = "")
}
